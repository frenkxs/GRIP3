# _____________________________________________________________________________________________
# Libraries and working directory -------------------------------------------------------------
# _____________________________________________________________________________________________

# install missing packages
packages_needed <- c("tidyverse", "lubridate", "forecast", "runner", "colorspace", "rstudioapi",
                     "pbapply", "DBI", "RpostgreSQL")
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

unique_consults <- data$consultations %>%
    distinct(exppatidx, contactd) %>%
    group_by(exppatidx) %>%
    count() %>%
    ungroup()

acute_covid <- acute_covid %>%
    left_join(., unique_consults, by = "exppatidx") %>%
    rename(unique_consults = n)

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


mean(acute_covid$age)
median(acute_covid$age)
sd(acute_covid$age)

sum((as.integer(acute_covid$sex) - 1)) / nrow(acute_covid)

range(acute_covid$age)

quantile(acute_covid$age)

nrow(acute_covid)

quantile(acute_covid$follow_down_years + 1)

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
# ends at the end of July 2022 but it may be difference in your case
weeks <- seq(ymd("2020-08-31"), ymd("2022-07-31"), by = "weeks")

# compute the overall population size by sex and age
# !!! NOTE: this may take a while to run
practice_size_age_sex <- pbapply::pblapply(weeks, count_patients, df = patients, cl = 4L) %>%
    map(as_tibble) %>%
    reduce(bind_rows)

# aggregate by age
practice_size_age <- practice_size_age_sex %>%
    group_by(practice, date, age_g) %>%
    summarise(gp_size = sum(gp_size))

# aggregate by sex
practice_size_sex <- practice_size_age_sex %>%
    group_by(practice, date, sex) %>%
    summarise(gp_size = sum(gp_size))

# aggregate over sex and age
practice_size_total <- practice_size_age_sex %>%
    group_by(practice, date) %>%
    summarise(gp_size = sum(gp_size))


# aggregate over sex and age
database_size_total <- practice_size_age_sex %>%
    group_by(date) %>%
    summarise(gp_size = sum(gp_size))

# .-----
# _____________________________________________________________________________________________
# COVID-19 contacts ---------------------------------------------------------------------------
# _____________________________________________________________________________________________

# Rate is computed as number of contact per 1000 person-weeks

# covid contacts per practice by sex and age
n_contacts_age_sex <- acute_covid %>%
    filter(start_episode > ymd("2020-08-30")) %>%
    group_by(practice, age_g, sex, start_episode_week, .drop = FALSE) %>%
    summarise(n = length(start_episode_week)) %>%
    rename(
        date = start_episode_week,
        covid_contacts = n
    ) %>%
    left_join(practice_size_age_sex, ., by = c("practice", "date", "sex", "age_g")) %>%
    replace_na(list(covid_contacts = 0)) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1)) %>%
    left_join(., acute_covid %>% select(municipality_gp, practice) %>%
        distinct(),
    by = "practice"
    ) %>%
    rename(municipality = municipality_gp) %>%
    filter(gp_size > 0)

# aggregate over age
n_contacts_age <- n_contacts_age_sex %>%
    group_by(practice, date, age_g, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

# aggregate over sex
n_contacts_sex <- n_contacts_age_sex %>%
    group_by(practice, date, sex, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

# aggregate over sex and age
n_contacts_total <- n_contacts_age_sex %>%
    group_by(practice, date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))

# ______________________________________________________________________________


# covid contacts per municipality and sex and age
n_contacts_mun_age_sex <- n_contacts_age_sex %>%
    group_by(municipality, age_g, sex, date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        sample_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / sample_size) * 1000, 1))

# aggregate by age
n_contacts_mun_age <- n_contacts_age_sex %>%
    group_by(municipality, age_g, date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        sample_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / sample_size) * 1000, 1))


# aggregate by sex
n_contacts_mun_sex <- n_contacts_age_sex %>%
    group_by(municipality, sex, date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        sample_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / sample_size) * 1000, 1))


# aggregate over sex and age
n_contacts_mun_total <- n_contacts_age_sex %>%
    group_by(municipality, date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        sample_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / sample_size) * 1000, 1))

# ______________________________________________________________________________

# This is the overall number of contacts regardless of practice or municipality

# aggregate over sex and age and practice
n_contacts_total_database <- n_contacts_age_sex %>%
    group_by(date, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


# aggregate over age and practice
n_contacts_total_sex <- n_contacts_age_sex %>%
    group_by(date, sex, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


# aggregate over sex and practice
n_contacts_total_age <- n_contacts_age_sex %>%
    group_by(date, age_g, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


# aggregate over practice
n_contacts_total_age_sex <- n_contacts_age_sex %>%
    group_by(date, sex, age_g, .drop = FALSE) %>%
    summarise(
        covid_contacts = sum(covid_contacts),
        gp_size = sum(gp_size)
    ) %>%
    mutate(rate = round((covid_contacts / gp_size) * 1000, 1))


# .-----
# _____________________________________________________________________________________________
# covid contacts all  -------------------------------------------------------------------------
# _____________________________________________________________________________________________

y_axis <- "Rate per 1,000 patients"
titl <- "GP episodes of care for acute COVID-19"

# with Rotterdam highlighted
n_contacts_mun_total %>%
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
    ggplot(aes(x = age_g, y = unique_consults, fill = age_g)) +
    geom_boxplot() +
    scale_fill_discrete_sequential(palette = "viridis", name = NULL, alpha = 0.5, guide = "none") +
    scale_y_continuous(name = "Number of COVID-19 contacts") +
    scale_x_discrete(name = "Age groups") +
    theme_minimal(base_size = 22) +
    # theme(legend.position = "none") +
    ggtitle(label = "Number of GP contacts for COVID-19 by age")


