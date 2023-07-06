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

#' You'll be using data previously extracted and cleaned.
#'  To load the data you will be prompted to first locate those
#' files on your computer


# First you'll select the data you need
select_read_data <- function(type) {
  svDialogs::dlg_message(paste("Please select the", type, "data.
                               Continue by pressing OK"), type = c("ok"), gui = .GUI)
  path <- tcltk::tk_choose.files(caption = paste("Select the", type, "data"))
  readRDS(path)
}

# !!!!! covid_contacts.RData !!!!!
# The first data is a dataset created in the 'acute_covid_consultation_rate'
# script. If no changes were made to the code, it should be the covid_contacts.RData file
# stored in the "clean_data" folder
covid_contacts <- select_read_data("covid contact")

# !!!!! covid_consults.RData !!!!!
# The second data is again a dataset created in the 'acute_covid_consultation_rate'
# script. If no changes were made to the code, it should be the covid_consults.RData file
# stored in the "clean_data" folder
covid_consults <- select_read_data("covid consults")


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
    if (sum(df_temp$covid_contacts) < 10) {
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



# contact
covid_contact_all <- get_covid_contact_rate(var = "all")
covid_contact_19 <- get_covid_contact_rate(var = "0-19")
covid_contact_39 <- get_covid_contact_rate(var = "20-39")
covid_contact_59 <- get_covid_contact_rate(var = "40-59")
covid_contact_79 <- get_covid_contact_rate(var = "60-79")
covid_contact_80 <- get_covid_contact_rate(var = "80+")


# consults
covid_consult_all <- get_covid_contact_rate(var = "all", contact_data = covid_consults)
covid_consult_19 <- get_covid_contact_rate(var = "0-19", contact_data = covid_consults)
covid_consult_39 <- get_covid_contact_rate(var = "20-39", contact_data = covid_consults)
covid_consult_59 <- get_covid_contact_rate(var = "40-59", contact_data = covid_consults)
covid_consult_79 <- get_covid_contact_rate(var = "60-79", contact_data = covid_consults)
covid_consult_80 <- get_covid_contact_rate(var = "80+", contact_data = covid_consults)



# Save results ------------------------------------------------------------

# this file can be shared. It contains only estimates of the contact rate
save(
  covid_contact_all[[1]],
  covid_contact_19[[1]],
  covid_contact_39[[1]],
  covid_contact_59[[1]],
  covid_contact_79[[1]],
  covid_contact_80[[1]],


  covid_consult_all[[1]],
  covid_consult_19[[1]],
  covid_consult_39[[1]],
  covid_consult_59[[1]],
  covid_consult_79[[1]],
  covid_consult_80[[1]],

  file = "covid_contact_rate.RData"
)

# This is more extensive results. Please keep this on your computer but do not share
save(
  covid_contact_all[[2]],
  covid_contact_19[[2]],
  covid_contact_39[[2]],
  covid_contact_59[[2]],
  covid_contact_79[[2]],
  covid_contact_80[[2]],


  covid_consult_all[[2]],
  covid_consult_19[[2]],
  covid_consult_39[[2]],
  covid_consult_59[[2]],
  covid_consult_79[[2]],
  covid_consult_80[[2]],

  file = "covid_contact_rate_fit.RData"
)
