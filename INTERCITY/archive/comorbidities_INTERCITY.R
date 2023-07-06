# _____________________________________________________________________________________________
# Libraries and working directory -------------------------------------------------------------
# _____________________________________________________________________________________________

# install missing packages
packages_needed <- c("tidyverse", "lubridate", "forecast", "runner", "colorspace", "rstudioapi")
install.packages(setdiff(packages_needed, rownames(installed.packages())))

library(tidyverse)
library(lubridate)
library(forecast)
library(runner)
library(colorspace)

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
data_com <- new.env(parent = globalenv())

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
  env = data_com
)

#' Alternatively - and depending how you store the output of the SQL script - you can load the raw
#' data from csv files. The above code assumes that the csv files are stored in your
#' working directory in a series of subfolders:
#'
#' In the example below the working directory has a subfolder "covid_cohort" in which
#' the csv file is stored

data_com[["comorbidities"]] <- read_csv(here::here("covid_cohort", "comorbidity.csv"))
data_com[["acute_covid"]] <- read_csv(here::here("covid_cohort", "episodes.csv"))
data_com[["patients"]] <- read_csv(here::here("covid_cohort", "patients.csv"))
data_com[["consults"]] <- read_csv(here::here("covid_cohort", "consults.csv"))

# .-----
# ------- Set global variables ----------------------------

# list of covid patients ids
covid_patients <- unique(data_com$acute_covid$exppatidx)

# reference date to calculate prevalences for non-covid patients
ref_date <- ymd("2021-07-01")

# .-----
# -------------- Denominators------------------------

# formatting patients data
patients <- data_com$patients %>%
    mutate(is_covid_patient = if_else(exppatidx %in% covid_patients, 1, 0)) %>%
    
    #' add the reference date to the patient data. the date is the date of the first
    #' covid consult for covid patients, and 1 July 2021 for non-covid patients
    left_join(., data_com$acute_covid %>% dplyr::select(exppatidx, start_episode),
              by = "exppatidx") %>%
    rename(referd = start_episode) %>%
    mutate(referd = replace_na(referd, ref_date),
           age = round(time_length(interval(patbird, referd), unit = "years"), 2),
           age_g = cut(age, breaks = c(seq(0, 80, by = 5), Inf)),
           ipciper = interval(ipcistd, ipciendd)
    ) %>%
    
    #' only select those patients with valid ipci time on the reference data
    #' By definition, this will be all covid patients, but not all non-covid patients
    filter(referd %within% ipciper)

# calculate age group sizes for covid patients
age_size_covid <- data_com$acute_covid %>%
    mutate(
        age = round(time_length(interval(patbird, start_episode), unit = "years"), 2),
        age_g = cut(age, breaks = c(seq(0, 80, by = 5), Inf))
    ) %>%
    distinct(exppatidx, .keep_all = TRUE) %>%
    select(exppatidx, age_g) %>%
    group_by(age_g) %>%
    tally()


# calculate the age group sizes for non covid patients
age_size_all <- patients %>%
    # remove covid patients
    filter(!(exppatidx %in% covid_patients)) %>%
    select(exppatidx, age_g) %>%
    filter(!is.na(age_g)) %>%
    group_by(age_g) %>%
    tally()



# .-----
# ---------------  Prevalence ------------------------
# function to calculate prevalence for 10 chronic diseases.
# Need to specify whether we want covid or non-covid patients and time (either five or all)
get_prevalence <- function(is_covid = TRUE, df) {
    if (is_covid) {
        size <- age_size_covid$n
    } else {
        size <- age_size_all$n
        
        # only select patients that had valid ipci time on the reference date
        df <- df %>%
            mutate(ipciper = interval(ipcistd, ipciendd)) %>%
            filter(ref_date %within% ipciper)
    }
    
    
    res <- df %>%
        # for non-covid patients, contactd variable is NA, we replace it with a reference date
        # of 2021-07-01
        mutate(start_episode = replace_na(start_episode, ref_date)) %>%
        # select either covid or non-covid patients
        filter(is_covid_patient == (is_covid * 1)) %>%
        # only select the first three letters in the icpc code
        mutate(icpc = substring(cod_com, 1, 3)) %>%
        # calculate age at the reference date
        mutate(
            age = round(time_length(interval(patbird, start_episode), unit = "years"), 2),
            age_g = cut(age, breaks = c(seq(0, 80, by = 5), Inf))
        ) %>%
        select(exppatidx, icpc, age_g) %>%
        mutate(disease = case_when(
            icpc == "P76" ~ "Depression",
            icpc == "P70" ~ "Dementia",
            icpc == "R96" ~ "Asthma",
            icpc == "R95" ~ "COPD",
            icpc == "N87" ~ "Parkinsonism",
            icpc == "K77" ~ "Heart failure",
            icpc == "K90" ~ "Stroke",
            icpc == "K75" ~ "Heart attack",
            icpc == "T90" ~ "Diabetes",
            icpc == "U99" ~ "Chronic kidney disease",
            icpc == "T82" ~ "Obesity",
            icpc %in% c(
                "A79", "B72", "B73", "B74", "D74", "D75", "D76", "D77", "F74",
                "H75", "K72", "L71", "N74", "R84", "R85", "S77", "T71", "U75",
                "U76", "U77", "W72", "X75", "X76", "X77", "Y77", "Y78"
            ) ~ "Cancer"
        )) %>%
        distinct(exppatidx, disease, .keep_all = TRUE) %>%
        group_by(disease, age_g, .drop = FALSE) %>%
        count()
    
    res$prevalence <- res$n / size
    
    res
}


# Prevalence: 5 years prior to reference date
prev_covid_5 <- get_prevalence(is_covid = TRUE, df = data_com$comorbidities) %>%
    mutate(cohort = "covid")
prev_all_5 <- get_prevalence(is_covid = FALSE, df = data_com$comorbidities) %>%
    mutate(cohort = "non-covid")

prev_5 <- bind_rows(prev_all_5, prev_covid_5) %>%
    mutate(
        disease = as.factor(disease),
        cohort = as.factor(cohort)
    )

# Get all patients (covid and non-covid) who have none of the comorbidities
healthy_pat <- setdiff(unique(patients$exppatidx), unique(data_com$comorbidities$exppatidx))

get_counts <- . %>%
    dplyr::filter(exppatidx %in% healthy_pat) %>%
    dplyr::filter(!is.na(age_g)) %>%
    group_by(age_g, is_covid_patient) %>%
    count() %>%
    ungroup()

#  first covid patients
healthy_covid <- patients %>% get_counts %>%
    dplyr::filter(is_covid_patient == 1) %>%
    left_join(., age_size_covid, by = "age_g", suffix = c(".healthy", ".all")) %>%
    mutate(prevalence = n.healthy / n.all) %>%
    dplyr::select(age_g, prevalence) %>%
    add_column(cohort = "covid",
               disease = "healthy")

#  then non-covid patients
healthy_noncovid <- patients %>% get_counts %>%
    dplyr::filter(is_covid_patient == 0) %>%
    left_join(., age_size_all, by = "age_g", suffix = c(".healthy", ".all")) %>%
    mutate(prevalence = n.healthy / n.all) %>%
    dplyr::select(age_g, prevalence) %>%
    add_column(cohort = "non-covid",
               disease = "healthy")

healthy <- bind_rows(healthy_covid, healthy_noncovid) %>%
    mutate(
        disease = as.factor(disease),
        cohort = as.factor(cohort)
    )

# put it all together
prev_5 <- bind_rows(healthy, prev_5)

# .-----
# ----------------- Plots ---------------------------
ggplot(prev_5, aes(x = age_g, y = prevalence, group = cohort, color = cohort)) +
    geom_line(linewidth = 1.5) +
    scale_x_discrete(labels = c(
        "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
        "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
        "70-75", "75-80", ">80"
    ), name = "age groups") +
    scale_color_manual(values = c("deepskyblue", "brown"), name = NULL) +
    facet_wrap(~ disease) +
    # scale_colour_hue() +
    theme_minimal(base_size = 16) +
    ggtitle("Prevalence of 12 chronic diseases among COVID-19 patients in primary care",
            subtitle = "5 years prior to COVID-19 diagnosis"
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10))

# .-----
# Comorbidity vs contacts -------------------------------------------------

comorb_clean <- data_com$comorbidities %>%
    mutate(icpc = substring(cod_com, 1, 3)) %>%
    mutate(disease = case_when(
        icpc == "P76" ~ "Depression",
        icpc == "P70" ~ "Dementia",
        icpc == "R96" ~ "Asthma",
        icpc == "R95" ~ "COPD",
        icpc == "N87" ~ "Parkinsonism",
        icpc == "K77" ~ "Heart failure",
        icpc == "K90" ~ "Stroke",
        icpc == "K75" ~ "Heart attack",
        icpc == "T90" ~ "Diabetes",
        icpc == "U99" ~ "Chronic kidney disease",
        icpc == "T82" ~ "Obesity",
        icpc %in% c(
            "A79", "B72", "B73", "B74", "D74", "D75", "D76", "D77", "F74",
            "H75", "K72", "L71", "N74", "R84", "R85", "S77", "T71", "U75",
            "U76", "U77", "W72", "X75", "X76", "X77", "Y77", "Y78"
        ) ~ "Cancer"
    )) %>%
    select(exppatidx, disease) %>%
    distinct() %>%
    pivot_wider(names_from = disease, values_from = disease, values_fn = ~ 1, values_fill = 0)


# left join with acute covid
acute_covid <- data_com$acute_covid %>%
    left_join(., comorb_clean, by = "exppatidx") %>%
    replace(is.na(.), 0)

# count number of comorbidities
acute_covid <- acute_covid %>%
    rowwise() %>%
    mutate(n_diseases = sum(across(Depression:Parkinsonism)))

unique_consults <- data_com$consultations %>%
    distinct(exppatidx, consultid) %>%
    group_by(exppatidx) %>%
    count() %>%
    ungroup()

acute_covid <- acute_covid %>%
    left_join(., unique_consults, by = "exppatidx") %>%
    rename(unique_consults = n)


# box plot with number of COVID-19 consults and comorbidities count
acute_covid %>%
    mutate(n_diseases_cat = as.character(n_diseases)) %>%
    mutate(n_diseases_cat = if_else(n_diseases > 4, "5+", n_diseases_cat)) %>%
    ggplot(aes(
        x = as.factor(n_diseases_cat), y = unique_consults,
        fill = as.factor(n_diseases_cat)
    )) +
    geom_boxplot() +
    scale_fill_discrete_sequential(palette = "viridis", name = NULL, alpha = 0.5, guide = "none") +
    scale_y_continuous(name = "Number of COVID-19 contacts") +
    scale_x_discrete(name = "Number of comorbidities") +
    theme_minimal(base_size = 22) +
    # theme(legend.position = "none") +
    ggtitle(label = "Number of GP contacts for COVID-19 by comorbidities count")



# .-----
# Odds of seeking GP consult for COVID-19 ---------------------------------

# get a table with all patients with their comorbidities

# first all patients who have any of the 10 comorbidities
res <- data_com$comorbidities %>%
    
    # for non-covid patients, contactd variable is NA, we replace it with a reference date
    # of 2021-07-01, then select only those with valid IPCI time on the reference date
    mutate(start_episode = replace_na(start_episode, ref_date),
           ipciper = interval(ipcistd, ipciendd)) %>%
    
    filter(start_episode %within% ipciper) %>%
    
    # only select the first three letters in the icpc code
    mutate(icpc = substring(cod_com, 1, 3)) %>%
    
    # calculate age at the reference date
    mutate(
        age = round(time_length(interval(patbird, start_episode), unit = "years"), 2),
        age_g = cut(age, breaks = c(seq(0, 80, by = 5), Inf))
    ) %>%
    
    dplyr::rename(ref_date = start_episode) %>%
    select(exppatidx, icpc, age, is_covid_patient) %>%
    
    mutate(disease = case_when(
        icpc == "P76" ~ "Depression",
        icpc == "P70" ~ "Dementia",
        icpc == "R96" ~ "Asthma",
        icpc == "R95" ~ "COPD",
        icpc == "N87" ~ "Parkinsonism",
        icpc == "K77" ~ "Heart failure",
        icpc == "K90" ~ "Stroke",
        icpc == "K75" ~ "Heart attack",
        icpc == "T90" ~ "Diabetes",
        icpc == "U99" ~ "Chronic kidney disease",
        icpc == "T82" ~ "Obesity",
        icpc %in% c(
            "A79", "B72", "B73", "B74", "D74", "D75", "D76", "D77", "F74",
            "H75", "K72", "L71", "N74", "R84", "R85", "S77", "T71", "U75",
            "U76", "U77", "W72", "X75", "X76", "X77", "Y77", "Y78"
        ) ~ "Cancer"
    )) %>%
    select(exppatidx, disease, is_covid_patient, age) %>%
    pivot_wider(names_from = disease, values_from = disease, values_fn = ~ 1, values_fill = 0)

# then the rest - patients (covid and non-covid) who have none of the comorbidities
patients <- data_com$patients %>%
    
    left_join(., data_com$acute_covid %>% select(exppatidx, start_episode), by = "exppatidx") %>%
    mutate(start_episode = replace_na(start_episode, ref_date),
           ipciper = interval(ipcistd, ipciendd)
    ) %>%
    filter(start_episode %within% ipciper) %>%
    
    mutate(
        age = round(time_length(interval(patbird, start_episode), unit = "years"), 2),
        is_covid_patient = if_else(exppatidx %in% covid_patients, 1, 0)
    ) %>%
    select(exppatidx, age, ipciper, is_covid_patient, gender)


pat_com <- patients %>%
    left_join(., res %>% select(-c(is_covid_patient, age)), by = "exppatidx") %>%
    mutate(across(everything(), ~ replace_na(.x, 0)))


# odds of contact for people with any given comorbidity
odds_contact <- function(disease = c(
    "Depression",
    "Dementia",
    "Asthma",
    "COPD",
    "Parkinsonism",
    "Heart failure",
    "Stroke",
    "Heart attack",
    "Diabetes",
    "Chronic kidney disease",
    "Obesity",
    "Cancer"
),
data = pat_com)
{
    fit <- glm(
        as.formula(paste("is_covid_patient ~ age + gender +", disease)),
        family = "binomial", data = data)
    
    as.data.frame(exp(confint(fit))) %>%
        add_column(pe = exp(coef(fit))) %>%
        round(., 2)
    
}

odds_contact("Depression")
odds_contact("Dementia")
odds_contact("Asthma")
odds_contact("COPD")
odds_contact("Parkinsonism")
odds_contact("Heart failure")
odds_contact("Stroke")
odds_contact("Heart attack")
odds_contact("Diabetes")
odds_contact("Chronic kidney disease")
odds_contact("Obesity")
odds_contact("Cancer")