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
  "brms", "tidyverse", "ProbBayes", "LearnBayes", "here",
  "svDialogs", "tcltk"
)

# this will install all the necessary packages
packages_to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(packages_to_install) > 0) install.packages(packages_to_install)
rm(packages_to_install, packages_needed)

library(brms)
library(tidyverse)
library(ProbBayes)
library(LearnBayes)


#' Installing and running brms might get a bit tricky. Please uncomment the code
#' below and run it to test everything is working fine

# ------------------------ run this test code
# data("epilepsy")
# fit_test <- brm(count ~ zAge + zBase * Trt + (1 | patient),
#             data = epilepsy, family = poisson())
# summary(fit_test)

# rm(epilepsy, fit_test)
# ----------------------- end

# You should get a results similar to what you see at:
# https://cran.r-project.org/web/packages/brms/readme/README.html

# For help with installing brms, please refer to:
# https://learnb4ss.github.io/learnB4SS/articles/install-brms.html

#

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

# The second data is the file 'covid_infections_all_regions.RData' that you were sent. You can also
# download it from the Erasmus MC One Drive at:
# https://erasmusmc-my.sharepoint.com/:u:/g/personal/p_velek_erasmusmc_nl/EewDlNojvXRHrm-KbHpjt3sBPLlV31CqMmBOY9Pzz6PC7w?e=md3zht
infections <- select_read_data("infection")




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
                                   infection_data = infections,
                                   contact_data = covid_contacts,
                                   database = c(
                                     "Groningen",
                                     "Maastricht",
                                     "Nijmegen",
                                     "Rotterdam",
                                     "Utrecht"
                                   ),
                                   test = FALSE) {
  database <- match.arg(database)

  infection_data <- case_when(
    database == "Groningen" ~ infection_data$infections_north,
    database == "Maastricht" ~ infection_data$infections_maastricht,
    database == "Nijmegen" ~ infection_data$infections_nijmegen,
    database == "Rotterdam" ~ infection_data$infections_rotterdam,
    database == "Utrecht" ~ infection_data$infections_utrecht,
  )

  if (var == "all") {
    df <- contact_data$practice
    df <- left_join(df, infection_data$infection_total,
      by = "date",
      suffix = c("_contact", "_infection")
    )
  } else {
    df <- contact_data$practice_age
    df <- df %>%
      dplyr::filter(age_g == var) %>%
      left_join(., infection_data$infection_age,
        by = c("date", "age_g"),
        suffix = c("_contact", "_infection")
      )
  }

  # remove unknown municipalities
  df <- df %>%
    dplyr::filter(municipality != "Unknown")

  # sequence of dates for which to estimate the covid contact rate
  dates <- if(test) df$date[1:3] else df$date

  # our prior belief is that there is around 20% of patients out of those with covid-19 infection
  # the number of infected people will be determined based on RIVM data for Rotterdam-Rijnmond
  df <- df %>%
    mutate(prior_contact_rate = (rate_infection / 1000) * 0.2)


  # store estimates
  results <- data.frame(
    date = dates,
    p = rep(0, length(dates)),
    p_err = rep(0, length(dates)),
    p_upper = rep(0, length(dates)),
    p_lower = rep(0, length(dates)),
    sd_mun = rep(0, length(dates)),
    sd_mun_err = rep(0, length(dates)),
    sd_mun_upper = rep(0, length(dates)),
    sd_mun_lower = rep(0, length(dates)),
    sd_prak = rep(0, length(dates)),
    sd_prak_err = rep(0, length(dates)),
    sd_prak_upper = rep(0, length(dates)),
    sd_prak_lower = rep(0, length(dates))
  )

  # store fits
  results_fit <- vector(mode = "list")

  for (i in 1:length(dates)) {
    df_temp <- df %>%
      dplyr::filter(date == dates[i])

    # 20% of all covid positive people may need GP assistance: this is our prior
    # This is highly unlikely to exceed 2% in any case
    mean_contact_rate <- list(p = 0.5, x = df_temp$prior_contact_rate[1])
    max_contact_rate <- list(p = 0.99, x = 0.02)

    # Convert those numbers to beta first
    beta_par <- LearnBayes::beta.select(mean_contact_rate, max_contact_rate)

    # and then to log odds from which to simulate mean and sd for normal prior
    set.seed(12)
    p_sim <- rbeta(1000, beta_par[1], beta_par[2])
    theta_sim <- log(p_sim / (1 - p_sim))
    prior_mean <- mean(theta_sim)
    prior_sd <- sd(theta_sim)


    # fit model -----------------------------------------------------------------------------------

    # define prior parameters
    stanvars <- brms::stanvar(prior_mean, name = "prior_mean") +
      brms::stanvar(prior_sd, name = "prior_sd")
    covid_prior <- c(prior(normal(prior_mean, prior_sd), class = Intercept))

    fit <- brm(
      data = df_temp,
      family = binomial,
      covid_contacts | trials(gp_size) ~ 1 + (1 | municipality / practice),
      prior = covid_prior,
      refresh = 0, stanvars = stanvars, control = list(adapt_delta = 0.99)
    )

    coefs <- summary(fit)

    # save results
    p <- c(
      coefs$fixed[, c(1:4)],
      coefs$random$municipality[1:4],
      coefs$random$`municipality:practice`[1:4]
    )

    results[i, -1] <- p
    results_fit[[as.character(dates[i])]] <- fit
  }
  return(list(results, results_fit))
}




# test it first. If the function finishes without any error messages, then you are good to go
covid_contact_all_north <- get_covid_contact_rate(var = "all", database = "Groningen", test = TRUE)
covid_contact_19_north <- get_covid_contact_rate(var = "0-19", database = "Groningen", test = TRUE)

# this should return a number
covid_contact_all_north[[1]][2, 2]
# this should be the same number as above
summary(covid_contact_all_north[[2]][[paste(covid_contact_all_north[[1]][2, 1])]])$fixed[1, 1]




# run it for real.
# This will take a loooong time !! Better keep it running overnight
covid_contact_all_north <- get_covid_contact_rate(var = "all", database =  "Groningen")
covid_contact_19_north <- get_covid_contact_rate(var = "0-19", database =  "Groningen")
covid_contact_39_north <- get_covid_contact_rate(var = "20-39", database = "Groningen")
covid_contact_59_north <- get_covid_contact_rate(var = "40-59", database = "Groningen")
covid_contact_79_north <- get_covid_contact_rate(var = "60-79", database = "Groningen")
covid_contact_80_north <- get_covid_contact_rate(var = "80+", database =   "Groningen")


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
