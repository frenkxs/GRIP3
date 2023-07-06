# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# GRIP3: GP COVID-19 contact rate estimation ------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------


#' This script takes aggregated GP covid contact data and estimate the GP contact rate
#' for covid-related complaints for each week for which the data are available
#'
# Contact Premysl Velek, p.velek@erasmusmc.nl for any questions!


# Libraries

# ' Installing packages
packages_needed <- c(
  "tidyverse", "here", "lme4", "svDialogs", "tcltk"
)

# this will install all the necessary packages
packages_to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(packages_to_install) > 0) install.packages(packages_to_install)
rm(packages_to_install, packages_needed)


library(tidyverse)
library(lme4)


# Load data ---------------------------------------------------------------

#' You'll be using data previously extracted and cleaned and also COVID-19 infection
#' data provided to you. To load the data you will be prompted to first locate those
#' file on your computer


# First you'll select the data you need
select_read_data <- function(type) {
  svDialogs::dlg_message(paste("Please select the", type, "data.
                               Continue by pressing OK"), type = c("ok"), gui = .GUI)
  path <- tcltk::tk_choose.files(caption = paste("Select the", type, "data"))
  readRDS(path)
}


# The first data is the dataset created in the 'acute_covid_consultation_rate'
# script. If no changes were made to the code, it should sit in the "clean_data" folder.
covid_contacts <- select_read_data("covid contact")


# Estimate contact rate ---------------------------------------------------

#' This function produces two sets of results. One contains only the estimated values and
#' not the observed data. Hence it can be shared among partners. The other set of results
#' is more comprehensive and contains also data used to estimate the contact rate. This is
#' important to store locally but will not be shared.
#'
#' this will run very long time, so it's good to test it first. You can test it by changeing the
#' value of the 'test' argument. If it is set to yes, it will run with much smaller sample and should
#' finish within a couple of minutes.
get_covid_contact_rate <- function(var = c("all", "0-19", "20-39", "40-59", "60-79", "80+"),
                                   contact_data = covid_contacts) {

  if (var == "all") {
    df <- contact_data$practice
  } else {
    df <- contact_data$practice_age
    df <- df %>%
      dplyr::filter(age_g == var)
  }

  # remove unknown municipalities
  df <- df %>%
    dplyr::filter(municipality != "Unknown")

  # sequence of dates for which to estimate the covid contact rate
  dates <- unique(df$date)

  # store estimates
  results <- data.frame(
    date = dates,
    p = rep(0, length(dates)),
    p_err = rep(0, length(dates)),

    sd_prak = rep(0, length(dates))

    # sd_mun = rep(0, length(dates))

  )

  # store fits
  results_fit <- vector(mode = "list")

  for (i in 1:length(dates)) {
    df_temp <- df %>%
      dplyr::filter(date == dates[i])


    # if there were no covid contact in any of the municipalities or practices, skip
    # The data will be recorded as NA in the estimates table and in the list of fits
    if (sum(df_temp$covid_contacts) == 0) {
      results[i, -1] <- rep(NA, 3)
      results_fit[[as.character(dates[i])]] <- NA
      next
    }

    fit <- glmer(data = df_temp,
                 family = "binomial",
                 formula = cbind(covid_contacts, gp_size - covid_contacts)  ~
                   1 + (1 | practice),
                 control = glmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun = 2e5)))

    coefs <- summary(fit)

    # save results
    p <- c(
      coefs$coefficients[, c(1:2)],
      as.data.frame(coefs$varcor)[, 5]
    )

    results[i, -1] <- p
    results_fit[[as.character(dates[i])]] <- fit
  }

  list(estimates = results, fits = results_fit)

}


# run it for real.
# This will take a loooong time !! Better keep it running overnight
covid_contact_all_north <- get_covid_contact_rate(var = "all")
covid_contact_19_north <- get_covid_contact_rate(var = "0-19")
covid_contact_39_north <- get_covid_contact_rate(var = "20-39")
covid_contact_59_north <- get_covid_contact_rate(var = "40-59")
covid_contact_79_north <- get_covid_contact_rate(var = "60-79")
covid_contact_80_north <- get_covid_contact_rate(var = "80+")


# # Rotterdam
# covid_contact_all_rot <- get_covid_contact_rate(var = "all", database =  "Rotterdam")
# covid_contact_19_rot <- get_covid_contact_rate(var = "0-19", database =  "Rotterdam")
# covid_contact_39_rot <- get_covid_contact_rate(var = "20-39", database = "Rotterdam")
# covid_contact_59_rot <- get_covid_contact_rate(var = "40-59", database = "Rotterdam")
# covid_contact_79_rot <- get_covid_contact_rate(var = "60-79", database = "Rotterdam")
# covid_contact_80_rot <- get_covid_contact_rate(var = "80+", database =   "Rotterdam")
#
# # Maastricht
# covid_contact_all_rot <- get_covid_contact_rate(var = "all", database =  "Maastricht")
# covid_contact_19_rot <- get_covid_contact_rate(var = "0-19", database =  "Maastricht")
# covid_contact_39_rot <- get_covid_contact_rate(var = "20-39", database = "Maastricht")
# covid_contact_59_rot <- get_covid_contact_rate(var = "40-59", database = "Maastricht")
# covid_contact_79_rot <- get_covid_contact_rate(var = "60-79", database = "Maastricht")
# covid_contact_80_rot <- get_covid_contact_rate(var = "80+", database =   "Maastricht")
#
# # Nijmegen
# covid_contact_all_rot <- get_covid_contact_rate(var = "all", database =  "Nijmegen")
# covid_contact_19_rot <- get_covid_contact_rate(var = "0-19", database =  "Nijmegen")
# covid_contact_39_rot <- get_covid_contact_rate(var = "20-39", database = "Nijmegen")
# covid_contact_59_rot <- get_covid_contact_rate(var = "40-59", database = "Nijmegen")
# covid_contact_79_rot <- get_covid_contact_rate(var = "60-79", database = "Nijmegen")
# covid_contact_80_rot <- get_covid_contact_rate(var = "80+", database =   "Nijmegen")
#
# # Utrecht
# covid_contact_all_rot <- get_covid_contact_rate(var = "all", database =  "Utrecht")
# covid_contact_19_rot <- get_covid_contact_rate(var = "0-19", database =  "Utrecht")
# covid_contact_39_rot <- get_covid_contact_rate(var = "20-39", database = "Utrecht")
# covid_contact_59_rot <- get_covid_contact_rate(var = "40-59", database = "Utrecht")
# covid_contact_79_rot <- get_covid_contact_rate(var = "60-79", database = "Utrecht")
# covid_contact_80_rot <- get_covid_contact_rate(var = "80+", database =   "Utrecht")




# Save results ------------------------------------------------------------

# this file can be shared. It contains only estimates of the contact rate
save(
  covid_contact_all_north[[1]],
  covid_contact_19_north[[1]],
  covid_contact_39_north[[1]],
  covid_contact_59_north[[1]],
  covid_contact_79_north[[1]],
  covid_contact_80_north[[1]],
  file = "covid_contact_rate.RData"
)

# This is more extensive results. Please keep this on your computer but do not share
save(
  covid_contact_all_north[[2]],
  covid_contact_19_north[[2]],
  covid_contact_39_north[[2]],
  covid_contact_59_north[[2]],
  covid_contact_79_north[[2]],
  covid_contact_80_north[[2]],
  file = "covid_contact_rate_fit.RData"
)
