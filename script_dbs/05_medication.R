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

# _____________________________________________________________________________________________

# Load data -----------------------------------------------------------------------------------
# _____________________________________________________________________________________________


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

    medication_query <- dbSendQuery(con, "SELECT * FROM grip3.episode_medication;")
    medication <- dbFetch(medication_query)
    dbClearResult(medication_query)

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
    rm(medication_query)
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

data[["consults"]] <- read_csv(here::here("covid_cohort", "episode_consultations.csv"))
data[["acute_covid"]] <- read_csv(here::here("covid_cohort", "episodes.csv"))
data[["patients"]] <- read_csv(here::here("covid_cohort", "patients.csv"))
data[["medication"]] <- read_csv(here::here("covid_cohort", "episode_medication.csv"))


# _____________________________________________________________________________________________

# data cleaning -------------------------------------------------------------------------------
# _____________________________________________________________________________________________

load("<path to the atc_names.RData file>")
medication <- data$medication %>%
  left_join(., atc_names %>% dplyr::select(atc, txteng, categorie_eng_n4), by = "atc") %>%

  # medication is the name of the medication
  # category_medication is the level 4 ATC category of the medication
  dplyr::rename(medication = txteng,
                category_medication = categoerie_eng_n4)



medication <- medication %>%
    drop_na(medication) %>%

    # Only select those after August 2020
    dplyr::filter(start_episode > lubridate::ymd("2020-08-15")) %>%
    rowwise %>%
    mutate(not_linked = sum(across(linked_covid_icpc:linked_respiratory_icpc))) %>%

    # remove covid-19 vaccines as they are not really a covid medication
    dplyr::filter(medication != "COVID-19 VACCINES") %>%

    # if there isn't an icpc code and it was not prescribed on the same day, it'll be 'not linked'
    mutate(not_linked = if_else(is.na(medication_episprcod) & same_day_as_covid_consult == 0, 1, 0)) %>%

    # if there is an icpc code other than covid or covid-like symptoms
    mutate(linked_other_icpc = if_else(!is.na(medication_episprcod) &
                                           sum(linked_covid_icpc,linked_covidlikesymptom_icpc) == 0, 1, 0)) %>%

    # make the categories mutually exclusive, prescribed on the same day as covid contact will be true only if
    # not linked to icpc code or covid_like symptoms and not linked to any other icpc code
    mutate(same_day_as_covid_consult = if_else(is.na(medication_episprcod) &
                                                       same_day_as_covid_consult == 1, 1, 0)) %>%

    # re-assign some medication to other
    mutate(medication = if_else(medication == "OTHER EMOLLIENTS AND PROTECTIVES",
                                "OTHER", medication))


# to check if the categories now are mutually exclusive: there can only be one TRUE value among all
# the categories that links medication to icpc
medication <- medication %>%
    rowwise %>%
    mutate(check = sum(linked_covid_icpc,
                       linked_covidlikesymptom_icpc,
                       not_linked,
                       linked_other_icpc,
                       same_day_as_covid_consult))


medication_long <- medication %>%
    pivot_longer(cols = c(linked_covid_icpc, linked_covidlikesymptom_icpc,
                          linked_other_icpc, same_day_as_covid_consult, not_linked)) %>%
    filter(value == 1) %>%
    select(- value)


medication_long$name = factor(medication_long$name,
                              levels = c("linked_covid_icpc",
                                         "linked_covidlikesymptom_icpc",
                                         "same_day_as_covid_consult",
                                         "linked_other_icpc",
                                         "not_linked"),
                              ordered = TRUE)

# save frequency table
save(table(medication_long$medication),
     file = here::here("clean_data", "medication_freq.RData"))


# Plotting ----------------------------------------------------------------

# long names will be broken into multiple lines
medication_long$medication[medication_long$medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR"] <-
  "AMOXICILLIN & BETA-LACTAMASE INHIB."

#  Most frequent at the level of medicines
ggplot(medication_long, aes(x = reorder(medication, medication,
                                            function(x) - length(x)), fill = name)) +
        geom_bar(position = "stack") +
        xlim(names(sort(table(medication_long$medication), decreasing = TRUE)[1:20])) +
        scale_fill_discrete_sequential(palette = "Viridis", order = 5:1, name = NULL,
                                       labels = c("Linked to COVID-19",
                                                    "Linked to COVID-19-like symptoms",
                                                    "Presribed on the same day as COVID contact",
                                                    "Linked to other diagnoses",
                                                    "Not linked / Unknown")) +
        ggtitle("Most frequent medication prescribed to COVID-19 patients in primary care") +
        theme_minimal(base_size = 22) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank())

#  Most frequent one ATC level up
ggplot(medication_long, aes(x = reorder(category_medication, category_medication,
                                        function(x) - length(x)), fill = name)) +
    geom_bar(position = "stack") +
    xlim(names(sort(table(medication_long$category_medication), decreasing = TRUE)[1:20])) +
    scale_fill_discrete_sequential(palette = "Viridis", order = 5:1, name = NULL,
                                   labels = c("Linked to COVID-19",
                                              "Linked to COVID-19-like symptoms",
                                              "Presribed on the same day as COVID contact",
                                              "Linked to other diagnoses",
                                              "Not linked / Unknown")) +
    ggtitle("Most frequent medication prescribed to COVID-19 patients in primary care") +
    theme_minimal(base_size = 22) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())

ggplot(medication, aes(x = medication_episprcod)) +
    geom_bar() +
    xlim(names(sort(table(medication$medication_episprcod, useNA = "ifany"),
                    decreasing = TRUE, na.last = FALSE)[1:20])) +
    ggtitle("Most frequent diagnosis for which a medication is prescribed during COVID-19 acute phase in primary care") +
    theme_minimal(base_size = 22) +
    theme(axis.title.x = element_blank())



# Prescription patterns over time -----------------------------------------
# We will track the first six most frequent medications (those above 500 in total prescriptions)
# everything else will be grouped together as other

#' 1. Codeine
#' 2. Amoxicillin
#' 3. Salbutamol
#' 4. Metoclopramide
#' 5. Prednisolone
#' 5. MACROGOL, COMBINATIONS

medication_time <- medication_long %>%
    mutate(med_cat = case_when(
        medication == "CODEINE" ~ medication,
        medication == "AMOXICILLIN" ~ medication,
        medication == "SALBUTAMOL" ~ medication,
        medication == "METOCLOPRAMIDE" ~ medication,
        medication == "PREDNISOLON" ~ medication,
        medication == "MACROGOL, COMBINATIONS" ~ medication,
        TRUE ~ "OTHER PRESCRIPTION"
      )) %>%

  # conver prescription date to prescription week
    mutate(week = lubridate::floor_date(medicationdate, unit = "week",
                                      week_start = getOption("lubridate.week.start", 1))) %>%
    group_by(med_cat, week, .drop = FALSE) %>%
    count() %>%

    mutate(med_cat = factor(med_cat, levels = c("CODEINE",
                                                "AMOXICILLIN",
                                                "SALBUTAMOL",
                                                "METOCLOPRAMIDE",
                                                "PREDNISOLON",
                                                "MACROGOL, COMBINATIONS",
                                                "OTHER PRESCRIPTION")))


#  Most frequent presriptions over time
#  without consults with no presriptions
ggplot(medication_time,
       aes(x = week, y = n,  fill = med_cat)) +
    geom_area(position = "fill") +
    scale_x_date(breaks = "4 weeks") +
    scale_fill_discrete_diverging(name = NULL) +
    scale_y_continuous(name = "Proportion of presctriptions", labels = scales::percent) +
    ggtitle("Most frequent medication prescribed to COVID-19 patients in primary care") +
    theme_minimal(base_size = 22) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())


# Tree map -----------

# medication tree map
med_tree <- medication %>%
    dplyr::select(exppatidx, atc, category_medication, medication, age, gender, start_episode)


# get all ids in the cohort
acute_covid <- data$acute_covid
all <- acute_covid$exppatidx

# those without any meds
no_meds <- acute_covid %>%
    filter(exppatidx %in% setdiff(all, med_tree$exppatidx)) %>%
    dplyr::select(exppatidx, age, gender, start_episode) %>%
    add_column(atc = "NONE",
               category_medication = "NONE",
               medication = "NONE")

med_tree <- bind_rows(med_tree, no_meds)

library(treemapify)

plot_tree <- list(
    geom_treemap(layout = "scol"),
    geom_treemap_text(colour = "white", place = "centre", size = 15, layout = "scol"),
    theme_minimal(base_size = 22),
    theme(legend.position = "none"),
    scale_fill_viridis_d())


med_tree %>%
    group_by(category_medication) %>%
    count() %>%
    mutate(category_medication = if_else((n < 400 | category_medication == "OTHER"), "OTHER", category_medication)) %>%
    summarise(n = sum(n)) %>%
    ggplot(aes(area = n, fill = category_medication,
               label = paste(category_medication, n, sep = "\n"))) +
    plot_tree

med_plot <- med_tree %>%
    group_by(medication) %>%
    count() %>%
    mutate(medication = if_else((n < 300 | medication == "OTHER"), "OTHER", medication)) %>%
    summarise(n = sum(n)) %>%
    mutate(medication = if_else(medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR",
                                "AMOXICILLIN AND \nBETA-LACTAMASE \nINHIBITOR",
                                medication)) %>%
    ggplot(aes(area = n, fill = medication,
               label = paste(medication, n, sep = "\n"))) +
    plot_tree

save(med_plot, file = "med_plot.RData")

# Do the same but only for those with contact in 2020
twenty20 <- med_tree %>%
    dplyr::filter(start_episode < ymd("2020-12-31")) %>%
    group_by(medication) %>%
    count() %>%
    mutate(medication = if_else((n < 100 | medication == "OTHER"), "OTHER", medication)) %>%
    summarise(n = sum(n)) %>%
    mutate(medication = if_else(medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR",
                                "AMOXICILLIN AND \nBETA-LACTAMASE \nINHIBITOR",
                                medication)) %>%
    ggplot(aes(area = n, fill = medication,
               label = paste(medication, n, sep = "\n"))) +
    plot_tree +
    ggtitle("Most frequent diagnosis for which a medication is prescribed during
            COVID-19 acute phase in primary care",
            subtitle = "year 2020 only")


twenty22 <- med_tree %>%
    dplyr::filter(start_episode > ymd("2021-12-31")) %>%
    group_by(medication) %>%
    count() %>%
    mutate(medication = if_else((n < 100 | medication == "OTHER"), "OTHER", medication)) %>%
    summarise(n = sum(n)) %>%
    mutate(medication = if_else(medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR",
                                "AMOXICILLIN AND \nBETA-LACTAMASE \nINHIBITOR",
                                medication)) %>%
    ggplot(aes(area = n, fill = medication,
               label = paste(medication, n, sep = "\n"))) +
    plot_tree +
    ggtitle("Most frequent diagnosis for which a medication is prescribed during
            COVID-19 acute phase in primary care",
            subtitle = "year 2022 only")


library(cowplot)

plot_grid(twenty20, twenty22)

med_tree %>%
    group_by(medication) %>%
    count() %>%
    mutate(medication = if_else((n < 300 | medication == "OTHER"), "OTHER", medication)) %>%
    summarise(n = sum(n)) %>%
    mutate(medication = if_else(medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR",
                                "AMOXICILLIN AND \nBETA-LACTAMASE \nINHIBITOR",
                                medication)) %>%
    ggplot(aes(area = n, fill = medication,
               label = paste(medication, n, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre", size = 15) +
    ggtitle("Most frequent diagnosis for which a medication is prescribed during COVID-19 acute phase in primary care") +
    theme_minimal(base_size = 22) +
    theme(legend.position = "none") +
    scale_fill_viridis_d()


  # Correlation between COVID-19 code and day of prescription -----------------------------------

# proportion of medication linked to covid which was prescribed at the same day as covid consult
table(covid = medication$linked_covid_icpc, date = medication$same_day_as_covid_consult)

# most frequent icpc codes linked to medication prescribed at the same day as covid consult
table(meds = medication$medication_episprcod, date = medication$same_day_as_covid_consult, useNA = "ifany") %>%
    as.data.frame() %>%
    arrange(desc(Freq))
