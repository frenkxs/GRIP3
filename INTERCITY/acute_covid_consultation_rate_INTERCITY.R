# _____________________________________________________________________________________________
# Libraries and working directory -------------------------------------------------------------
# _____________________________________________________________________________________________

# install missing packages
packages_needed <- c("tidyverse", "lubridate", "forecast", "runner", "colorspace", "rstudioapi",
                     "pbapply", "DBI", "RpostgreSQL", "here")
install.packages(setdiff(packages_needed, rownames(installed.packages())))
rm(packages_needed)

library(tidyverse)
library(lubridate)
library(forecast)
library(runner)
library(colorspace)
library(rstudioapi)

# libraries to fetch data from the RG database
library(DBI)
library(RPostgreSQL)

# start here
setwd(here::here())

# .-----
# _____________________________________________________________________________________________
# Load data -----------------------------------------------------------------------------------
# _____________________________________________________________________________________________

# load data into a separate environment
data <- new.env(parent = globalenv())

local(
    {

        #' connection details. Fill what's needed to connect to your database.
        #' You'll be prompted to fill in user name and password after running the script.
        #' If any of the above data are not needed in your case (eg. password), delete them
        db_name <- "fill this in"
        host_name <- "fill this in"
        port_number <- "fill this in"
        user_name <- rstudioapi::askForPassword("Database user name ")
        db_password <- rstudioapi::askForPassword("Database password")



        # open connection with the db
        con <- dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         host = host_name,
                         port = port_number,
                         user = user_name,
                         password = db_password
        )

        patients_query <- dbSendQuery(con, "SELECT * FROM grip3.patient;")
        patients <- dbFetch(patients_query)
        dbClearResult(patients_query)

        comorbidities_query <- dbSendQuery(con, "SELECT * FROM grip3.comorbidity;")
        comorbidities <- dbFetch(comorbidities_query)
        dbClearResult(comorbidities_query)

        consultations_query <- dbSendQuery(con, "SELECT * FROM grip3.episode_consultations;")
        consultations <- dbFetch(consultations_query)
        dbClearResult(consultations_query)

        acute_covid_query <- dbSendQuery(con, "SELECT * FROM grip3.episodes;")
        acute_covid <- dbFetch(acute_covid_query)
        dbClearResult(acute_covid_query)

        # close conection with the db
        dbDisconnect(con)

        rm(acute_covid_query)
        rm(patients_query)
        rm(comorbidities_query)
        rm(consultations_query)
        rm(con)

        rm(db_name)
        rm(host_name)
        rm(port_number)
    },
    env = data
)

#' Alternativelly - and depending how you store the output of the SQL script - you can load the raw
#' data from csv files. The above code assumes that the csv files are stored in your
#' working directory in a series of subforlders:
#'
#' In the example below the working directory has a subfolder "covid_cohort" in which
#' the csv file is stored

data[["consults"]] <- read_csv(here::here("covid_cohort", "consults.csv"))
data[["acute_covid"]] <- read_csv(here::here("covid_cohort", "episodes.csv"))
data[["patients"]] <- read_csv(here::here("covid_cohort", "patients.csv"))


# .-----
# _____________________________________________________________________________________________
# Formatting ----------------------------------------------------------------------------------
# _____________________________________________________________________________________________


# Population  will be computed in person-weeks, ie. if  a practice joins the database mid-week,
# their population size will scaled depending how many days they contributed to the week.

# The reference day for a weekly population size is Monday, practices will only be included if they
# contribute to the follow-up with the full week. ie. if a practice joins the database on Thursday, this practice
# will be included in the study from the following Monday.
acute_covid <- data$acute_covid %>%
    mutate(
        start_episode_week = floor_date(start_episode,
            unit = "weeks",
            week_start = getOption("lubridate.week.start", 1)
        ),
        age = round(2 * (interval(patbird, start_episode)) / years(1), 0) / 2,
        age_g = factor(cut(age,
            breaks = c(-Inf, 19, 39, 59, 79, Inf),
            labels = c("0-19", "20-39", "40-59", "60-79", "80+")
        )),
        sex = factor(gender, levels = c(1, 2), labels = c("Male", "Female"))
    )


# Some practices don't have assigned ID. Those will be removed from the cohort
patients <- data$patients %>%
    drop_na(practice) %>%
    mutate(sex = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
           ipciper = interval(ymd(ipcistd), ymd(ipciendd))) %>%
    dplyr::select(
        practice, municipality_code_gp, municipality_gp, exppatidx, sex,
        patbird, patdead, ipciper
    ) %>%
    drop_na(ipciper)

acute_covid <- acute_covid %>%
    drop_na(practice)


# consultations - count the number of consultations (including repeat consults within COVID
# episode)
consults <- data$consultations %>%
  left_join(., acute_covid %>% select(exppatidx, start_episode, end_episode, age_g, sex),
            by = "exppatidx") %>%
  distinct(exppatidx, contactd, .keep_all = TRUE) %>%
  mutate(consult_week = floor_date(contactd,
                                   unit = "weeks",
                                   week_start = getOption("lubridate.week.start", 1))) %>%
  drop_na(practice)


# .-----
# _____________________________________________________________________________________________
# Population characteristic: visual check -----------------------------------------------------
# _____________________________________________________________________________________________

ggplot(acute_covid, aes(age)) +
    geom_histogram(color = "azure3", fill = "azure4") +
    theme_minimal(base_size = 22) +
    ggtitle("Patients who sought medical assistance for COVID-19",
        subtitle = "Age distribution"
    )

ggplot(acute_covid, aes(x = sex, y = age, fill = sex)) +
    geom_boxplot() +
    # scale_color_manual(values = c("deepskyblue", "brown"), name = NULL, guide = "none") +
    scale_fill_manual(values = c("deepskyblue", "brown"), name = NULL, guide = "none") +
    scale_x_discrete(name = NULL) +
    theme_minimal(base_size = 22) +
    ggtitle("Patients who sought medical assistance for COVID-19",
        subtitle = "Age distribution by sex"
    )


# check the follow up time (up to the date of the first COVID-19 consult)
acute_covid %>%
    mutate(
        fu = follow_down_years + 1,
        fu5 = case_when(
            fu < 1 ~ "< 1 year",
            fu >= 1 & fu < 2 ~ "less than 2",
            fu >= 2 & fu < 4 ~ "between 2 and 4",
            TRUE ~ "4 or more"
        )
    ) %>%
    ggplot(aes(fu5)) +
    geom_bar(color = "azure3", fill = "azure4", stat = "count") +
    theme_minimal(base_size = 22) +
    ggtitle("Number of years of medical history prior to COVID-19 episode")


# .-----
# _____________________________________________________________________________________________
# Population characteristics: Table 1 ---------------------------------------------------------
# _____________________________________________________________________________________________
# Number of patients in the database with at least one week of follow-up between September 2020 and the end of the study period

# set up the study periods
sp_start <- ymd("2020-08-31")

# the end date is simply the latest date of the episode start
sp_end <- max(acute_covid$start_episode_week)

study_per <- interval(sp_start, sp_end)

study_per / weeks(1)

database_size <- patients %>%
    dplyr::filter(int_overlaps(study_per, ipciper),
                  ipciper / days(1) >= 6) %>%
    nrow()



# Mean age at start and end of study period
date <- sp_start
age_start <- patients %>%
    dplyr::filter(date %within% ipciper) %>%
    mutate(age = round(2 * (interval(patbird, date)) / years(1), 0) / 2) %>%
    summarise(age_sd = sd(age),
              age = mean(age),
    ) %>%
    round(., 1)


date <- sp_end
age_end <- patients %>%
    dplyr::filter(date %within% ipciper) %>%
    mutate(age = round(2 * (interval(patbird, date)) / years(1), 0) / 2) %>%
    summarise(age_sd = sd(age),
              age = mean(age),
    ) %>%
    round(., 1)


# Sex ratio at start and end of study period
date <- sp_start
sex_start <- patients %>%
    dplyr::filter(date %within% ipciper) %>%
    group_by(sex) %>%
    count()

sex_start <- round(as.numeric(sex_start[2, 2] / sex_start[1, 2]), 2)


date <- sp_end
sex_end <- patients %>%
    dplyr::filter(date %within% ipciper) %>%
    group_by(sex) %>%
    count()

sex_end <- round(as.numeric(sex_end[2, 2]) / as.numeric(sex_end[1, 2]), 2)



# .-----
# _____________________________________________________________________________________________
# COVID cohort characteristics: Table 2 -----------------------------------------------------
# _____________________________________________________________________________________________

ggplot(acute_covid, aes(age)) +
    geom_histogram(color = "azure3", fill = "azure4") +
    theme_minimal(base_size = 22) +
    ggtitle("Patients who sought medical assistance for COVID-19",
            subtitle = "Age distribution"
    )

ggplot(acute_covid, aes(x = sex, y = age, fill = sex)) +
    geom_boxplot() +
    # scale_color_manual(values = c("deepskyblue", "brown"), name = NULL, guide = "none") +
    scale_fill_manual(values = c("deepskyblue", "brown"), name = NULL, guide = "none") +
    scale_x_discrete(name = NULL) +
    theme_minimal(base_size = 22) +
    ggtitle("Patients who sought medical assistance for COVID-19",
            subtitle = "Age distribution by sex"
    )

# number of people who consulted GP for covid
covid_cohort_size <- nrow(acute_covid)
covid_cohort_size_perc <- round((covid_cohort_size / database_size) * 100, 1)


covid_cohort_age <- mean(acute_covid$age)
covid_cohort_age_sd <- sd(acute_covid$age)


covid_cohort_sex_ratio <- acute_covid %>%
    group_by(sex) %>%
    count()

covid_cohort_sex_ratio <- round(as.numeric(covid_cohort_sex_ratio[2, 2] /
                                               covid_cohort_sex_ratio[1, 2]), 2)


# check the follow up time (up to the date of the first COVID-19 consult)
acute_covid %>%
    mutate(
        fu = follow_down_years + 1,
        fu5 = case_when(
            fu < 1 ~ "< 1 year",
            fu >= 1 & fu < 2 ~ "less than 2",
            fu >= 2 & fu < 4 ~ "between 2 and 4",
            TRUE ~ "4 or more"
        )
    ) %>%
    ggplot(aes(fu5)) +
    geom_bar(color = "azure3", fill = "azure4", stat = "count") +
    theme_minimal(base_size = 22) +
    ggtitle("Number of years of medical history prior to COVID-19 episode")


# .-----
# _____________________________________________________________________________________________
# Practice size -------------------------------------------------------------------------------
# _____________________________________________________________________________________________


# function to count the number of patients within each practice and for each week in
count_patients <- function(date, df) {
    df %>%
        mutate(
            age = round(2 * (interval(patbird, date)) / years(1), 0) / 2,
            age_g = factor(cut(age,
                breaks = c(-Inf, 19, 39, 59, 79, Inf),
                labels = c("0-19", "20-39", "40-59", "60-79", "80+")
            ))
        ) %>%
        group_by(practice, age_g, sex, .drop = FALSE) %>%
        summarise(gp_size = sum(date %within% ipciper)) %>%
        mutate(date = date)
}

# the end date depends on the follow up time in your database. Our followup time
# ends at the end of July 2022 but it may be different in your case
weeks <- seq(sp_start, sp_end, by = "weeks")


# Calculate denominators --------------------------------------------------

# compute the overall population size by sex and age
# !!! NOTE: this may take a while to run
denominators <- new.env(parent = globalenv())

local(
    {
        # compute the overall population size by sex and age
        practice_age_sex <- lapply(weeks, count_patients, df = patients) %>%
            map(as_tibble) %>%
            reduce(bind_rows)

        # aggregate by age
        practice_age <- practice_age_sex %>%
            group_by(practice, date, age_g) %>%
            summarise(gp_size = sum(gp_size))

        # aggregate by sex
        practice_sex <- practice_age_sex %>%
            group_by(practice, date, sex) %>%
            summarise(gp_size = sum(gp_size))

        # aggregate over sex and age
        practice <- practice_age_sex %>%
            group_by(practice, date) %>%
            summarise(gp_size = sum(gp_size))

        # aggregate over sex and age
        total <- practice_age_sex %>%
            group_by(date) %>%
            summarise(gp_size = sum(gp_size))
    },
    env = denominators
)

# .-----
# _____________________________________________________________________________________________
# COVID-19 contacts ---------------------------------------------------------------------------
# _____________________________________________________________________________________________

covid_contacts <- new.env(parent = globalenv())

local(
    {
        # covid contacts per practice by sex and age
        practice_age_sex <- acute_covid %>%
            filter(start_episode >= sp_start) %>%
            group_by(practice, age_g, sex, start_episode_week, .drop = FALSE) %>%
            summarise(n = length(start_episode_week)) %>%
            rename(
                date = start_episode_week,
                covid_contacts = n
            ) %>%
            left_join(denominators$practice_age_sex, .,
                      by = c("practice", "date", "sex", "age_g")) %>%
            replace_na(list(covid_contacts = 0)) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1)) %>%

            # add municipality to practice so we could do double nested mixed models
            left_join(., acute_covid %>% select(municipality_gp, practice) %>%
                          distinct(),
                      by = "practice"
            ) %>%
            rename(municipality = municipality_gp) %>%
            filter(gp_size > 0)

        # aggregate over sex
        practice_age <- practice_age_sex %>%
            group_by(practice, date, age_g, municipality) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

        # aggregate over age
        practice_sex <- practice_age_sex %>%
            group_by(practice, date, sex, municipality) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

        # aggregate over sex and age
        practice <- practice_age_sex %>%
            group_by(practice, date, municipality) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

        # ______________________________________________________________________________


        # covid contacts per municipality and sex and age
        municipality_age_sex <- practice_age_sex %>%
            group_by(municipality, age_g, sex, date, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                sample_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / sample_size) * 1000, 1))

        # aggregate by age
        municipality_age <- practice_age_sex %>%
            group_by(municipality, age_g, date, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                sample_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / sample_size) * 1000, 1))


        # aggregate by sex
        municipality_sex <- practice_age_sex %>%
            group_by(municipality, sex, date, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                sample_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / sample_size) * 1000, 1))


        # aggregate over sex and age
        municipality <- practice_age_sex %>%
            group_by(municipality, date, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                sample_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / sample_size) * 1000, 1))

        # ______________________________________________________________________________

        # aggregate over sex and age and practice
        total <- practice_age_sex %>%
            group_by(date, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


        # aggregate over age and practice
        sex <- practice_age_sex %>%
            group_by(date, sex, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


        # aggregate over sex and practice
        age <- practice_age_sex %>%
            group_by(date, age_g, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


        # aggregate over practice
        age_sex <- practice_age_sex %>%
            group_by(date, sex, age_g, .drop = FALSE) %>%
            summarise(
                covid_contacts = sum(covid_contacts),
                gp_size = sum(gp_size)
            ) %>%
            mutate(rate = round((covid_contacts / gp_size) * 1000, 1))
    },
    env = covid_contacts
)


# .-----
# _____________________________________________________________________________________________
# COVID-19 contacts including repeat consults--------------------------------------------------
# _____________________________________________________________________________________________

covid_consults <- new.env(parent = globalenv())

local(
  {
    # covid contacts per practice by sex and age
    practice_age_sex <- consults %>%
      filter(start_episode >= sp_start) %>%
      group_by(practice, age_g, sex, consult_week, .drop = FALSE) %>%
      summarise(n = n()) %>%
      rename(
        date = consult_week,
        covid_consults = n
      ) %>%
      left_join(denominators$practice_age_sex, .,
                by = c("practice", "date", "sex", "age_g")) %>%
      replace_na(list(covid_consults = 0)) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1)) %>%

      # add municipality to practice so we could do double nested mixed models
      left_join(., acute_covid %>% select(municipality_gp, practice) %>%
                  distinct(),
                by = "practice"
      ) %>%
      rename(municipality = municipality_gp) %>%
      filter(gp_size > 0)

    # aggregate over sex
    practice_age <- practice_age_sex %>%
      group_by(practice, date, age_g, municipality) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))

    # aggregate over age
    practice_sex <- practice_age_sex %>%
      group_by(practice, date, sex, municipality) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))

    # aggregate over sex and age
    practice <- practice_age_sex %>%
      group_by(practice, date, municipality) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))

    # ______________________________________________________________________________


    # covid contacts per municipality and sex and age
    municipality_age_sex <- practice_age_sex %>%
      group_by(municipality, age_g, sex, date, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        sample_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / sample_size) * 1000, 1))

    # aggregate by age
    municipality_age <- practice_age_sex %>%
      group_by(municipality, age_g, date, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        sample_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / sample_size) * 1000, 1))


    # aggregate by sex
    municipality_sex <- practice_age_sex %>%
      group_by(municipality, sex, date, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        sample_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / sample_size) * 1000, 1))


    # aggregate over sex and age
    municipality <- practice_age_sex %>%
      group_by(municipality, date, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        sample_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / sample_size) * 1000, 1))

    # ______________________________________________________________________________

    # aggregate over sex and age and practice
    total <- practice_age_sex %>%
      group_by(date, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))


    # aggregate over age and practice
    sex <- practice_age_sex %>%
      group_by(date, sex, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))


    # aggregate over sex and practice
    age <- practice_age_sex %>%
      group_by(date, age_g, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))


    # aggregate over practice
    age_sex <- practice_age_sex %>%
      group_by(date, sex, age_g, .drop = FALSE) %>%
      summarise(
        covid_consults = sum(covid_consults),
        gp_size = sum(gp_size)
      ) %>%
      mutate(rate = round((covid_consults / gp_size) * 1000, 1))
  },
  env = covid_consults
)

# .-----
# _____________________________________________________________________________________________
# Save ---------------------------------------------------------------------------
# _____________________________________________________________________________________________

# this creates the folder called "clean_data"
if (!dir.exists(file.path(here::here("clean_data")))) {
    dir.create(file.path(here::here("clean_data")), recursive = TRUE)
}

saveRDS(covid_contacts, file = here::here("clean_data", "covid_contacts.RData"))
saveRDS(covid_consults, file = here::here("clean_data", "covid_consults.RData"))
saveRDS(denominators, file = here::here("clean_data", "denominators.RData"))

# save basic information about the covid cohort
save(covid_cohort_size , covid_cohort_size_perc, covid_cohort_age,
     covid_cohort_age_sd, covid_cohort_sex_ratio,
     file = here::here("clean_data", "table2.RData"))

# save basic information about the study cohort (the database)
save(database_size, age_start, age_end, sex_start, sex_end,
     file = here::here("clean_data", "table1.RData"))

# .-----
# _____________________________________________________________________________________________
# covid contacts all  -------------------------------------------------------------------------
# _____________________________________________________________________________________________

y_axis <- "Rate per 1,000 patients"
titl <- "GP episodes of care for acute COVID-19"

covid_contacts <- readRDS(here::here("clean_data", "covid_contacts.RData"))
infections <- readRDS(here::here("clean_data", "covid_infections_all_regions.RData"))

# select your database / region

# use for STIZON database as it's non- region specific
# STIZON: "all"
# Groningen: "north"
# Maastricht: "maastrich"
# Utrecht: "utrecht"
# Rotterdam: "rotterdam"


select_infection_data <- function(region = c("all" ,
                                     "north",
                                     "maastrich",
                                     "utrecht",
                                     "rotterdam")) {
  region <- match.arg(region)

  if (region == "north"){
    res <- infections$infections_north
  } else if (region == "maastricht") {
    res <- infections$infections_maastricht
  } else if (region == "utrecht") {
    res <- infections$infections_utrecht
  } else if (region == "rotterdam") {
    res <- infections$infections_rotterdam
  } else {
    res <- infections$infections_all
  }
  res
}

# select the right infection dataset
inf_df <- select_infection_data(region = "select your regio or select all if STIZON")


# Contact rate by municipalities
covid_contacts$municipality %>%
    dplyr::filter(!is.na(municipality)) %>%
    ggplot(aes(x = date, y = rate)) +
    geom_line(aes(
        size = factor(highlight),
        group = municipality, colour = municipality,
        alpha = factor(highlight)
    )) +
    geom_point(
        aes(
            group = municipality, colour = municipality,
            alpha = factor(highlight)
        ),
        size = 3
    ) +
    scale_alpha_manual(values = c(0.3, 1), guide = "none") +
    scale_size_manual(values = c(0.4, 1.5), guide = "none") +
    scale_x_date(date_breaks = "5 weeks", date_labels = "%b %y", name = NULL) +
    scale_y_continuous(name = y_axis, limits = c(0, 20)) +
    scale_color_discrete(name = "Municipality:") +
    theme_minimal(base_size = 22) +
    ggtitle(titl) +
    geom_point(
        data = inf_df$infection_total,
        aes(x = date, y = rate), colour = "black",
        size = 1, alpha = 0.5
    ) +
    geom_line(
        data = inf_df$infection_total,
        aes(x = date, y = rate), colour = "black",
        size = 1, linetype = "dotted", alpha = 0.7
    ) +
    annotate("curve",
             x = ymd("2021-10-21"), y = 9,
             xend = ymd("2021-11-10"), yend = 8,
             color = "darkslategray", curvature = 0.4, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate("text",
             x = ymd("2021-10-21"), y = 9.3, hjust = 1, colour = "grey20",
             label = "COVID-19\ninfection rate"
    )


# overall convid-19 contacts
covid_contacts$age %>%
    ggplot(aes(x = date, y = rate)) +
    geom_line(aes(group = age_g, colour = age_g)) +
    geom_point(aes(group = age_g, colour = age_g), size = 3) +
    scale_x_date(date_breaks = "5 weeks", date_labels = "%b %y", name = NULL) +
    scale_y_continuous(name = y_axis) +
    theme_minimal(base_size = 22) +
    ggtitle(titl) +


    geom_point(
        data = inf_df$infection_age,
        aes(x = date, y = rate, group = age_g, colour = age_g), colour = "black",
        size = 1, alpha = 0.5) +
    geom_line(
        data = inf_df$infection_age,
        aes(x = date, y = rate, group = age_g, colour = age_g), colour = "black",
        size = 1, linetype = "dotted", alpha = 0.7
    ) +
    facet_wrap(~ age_g) +

    annotate("curve",
             x = ymd("2021-10-21"), y = 9,
             xend = ymd("2021-11-10"), yend = 8,
             color = "darkslategray", curvature = 0.4, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate("text",
             x = ymd("2021-10-21"), y = 9.3, hjust = 1, colour = "grey20",
             label = "COVID-19\ninfection rate"
    )


covid_contacts$total %>%
    ggplot(aes(x = date, y = rate)) +
    geom_line() +
    geom_point(size = 3) +
    scale_x_date(date_breaks = "5 weeks", date_labels = "%b %y", name = NULL) +
    scale_y_continuous(name = y_axis, limits = c(0, 20)) +
    theme_minimal(base_size = 22) +
    ggtitle(titl) +
    geom_point(
        data = inf_df$infection_total, aes(x = date, y = rate), colour = "black",
        size = 1, alpha = 0.5
    ) +
    geom_line(
        data = inf_df$infection_total, aes(x = date, y = rate), colour = "black",
        size = 1, linetype = "dotted", alpha = 0.7
    ) +
    annotate("curve",
             x = ymd("2021-10-21"), y = 9,
             xend = ymd("2021-11-10"), yend = 8,
             color = "darkslategray", curvature = 0.4, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate("text",
             x = ymd("2021-10-21"), y = 9.3, hjust = 1, colour = "grey20",
             label = "COVID-19\ninfection rate"
    )

# proportion by age of patients seeking GP consultation
covid_contacts$age %>%
    group_by(date, age_g) %>%
    mutate(percent = (covid_contacts / sum(gp_size)) * 1000) %>%
    ggplot(aes(x = date, y = percent, fill = age_g)) +
    geom_area(alpha = 0.6, size = 0.5, colour = "white") +
    scale_fill_viridis_d() +
    scale_x_date(date_breaks = "5 weeks", date_labels = "%b %y", name = NULL) +
    theme_minimal(base_size = 22) +
    ggtitle(titl)


covid_contacts$age %>%
    group_by(date, age_g) %>%
    summarise(
        n = sum(covid_contacts),
        size = sum(gp_size)
    ) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(x = date, y = percent, fill = age_g)) +
    geom_area(alpha = 0.6, size = 0.5, colour = "white") +
    scale_fill_viridis_d() +
    theme_minimal(base_size = 22) +
    ggtitle(titl)


covid_contacts$municipality %>%
    dplyr::filter(!is.na(municipality)) %>%
    ggplot(aes(x = date, y = rate)) +
    geom_line(aes(
        group = municipality, colour = municipality
    ), alpha = 0.5) +
    geom_point(
        aes(
            group = municipality, colour = municipality
        ),
        size = 3, alpha = 0.5
    ) +
    scale_alpha_manual(values = c(0.3, 1), guide = "none") +
    scale_size_manual(values = c(0.4, 1.5), guide = "none") +
    scale_x_date(date_breaks = "5 weeks", date_labels = "%b %y", name = NULL) +
    scale_y_continuous(name = y_axis) +
    scale_color_discrete(name = "Municipality:") +
    theme_minimal(base_size = 22) +
    ggtitle(titl)

# box plot with number of COVID-19 consults
acute_covid %>%
    mutate(age_g = relevel(age_g, ref = "0-19")) %>%
    ggplot(aes(x = age_g, y = as.numeric(number_of_consults), fill = age_g)) +
    geom_boxplot() +
    scale_fill_discrete_sequential(palette = "viridis", name = NULL, alpha = 0.5, guide = "none") +
    scale_y_continuous(name = "Number of COVID-19 contacts") +
    scale_x_discrete(name = "Age groups") +
    theme_minimal(base_size = 22) +
    # theme(legend.position = "none") +
    ggtitle(label = "Number of GP contacts for COVID-19 by age")


