# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# GRIP3: GP COVID-19 contacts vs infection rate ---------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------


#' This script takes aggregated GP covid contact data and estimate the GP contact rate
#' for covid-related complaints for each week for which the data are available
#'
# Contact Premysl Velek, p.velek@erasmusmc.nl for any questions!


# Libraries

# ' Installing packages
packages_needed <- c(
  "tidyverse", "here",
  "svDialogs", "tcltk", "colorspace", "grid", "lemon", "purr", "gridExtra"
)

# this will install all the necessary packages
packages_to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(packages_to_install) > 0) install.packages(packages_to_install)
rm(packages_to_install, packages_needed)


library(tidyverse)
library(colorspace)
library(grid)


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
contact_data <- select_read_data("covid contact")
consult_data <- select_read_data("covid consults")

# The second data is the file 'covid_infections_all_regions.RData' that you were sent. You can also
# download it from the Erasmus MC One Drive at:
# https://erasmusmc-my.sharepoint.com/:u:/g/personal/p_velek_erasmusmc_nl/EewDlNojvXRHrm-KbHpjt3sBPLlV31CqMmBOY9Pzz6PC7w?e=md3zht
infection_data <- select_read_data("infection")

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

infection_data  <- select_infection_data(region = "select your region or select all if STIZON")



# Linear regression - by age and at the province level -------------------------

df_contact <- contact_data$age
df_contact <- left_join(df_contact, infection_data$infection_age,
                by = c("date", "age_g"),
                suffix = c("_contact", "_infection")
)

df_consult <- consult_data$age
df_consult <- left_join(df_consult, infection_data$infection_age,
                by = c("date", "age_g"),
                suffix = c("_consult", "_infection")
)

# uncomment your region, if you're STIZON uncomment the last option

# region <- "North Netherlands (Groningen, Friesland, Drenthe, Overijssel)"
# region <- "Rotterdam-Rijnmond"
# region <- "Utrecht"
# region <- "Nijmegen"
# region <- "South Limburg"
# region <- "The Netherlands"

# Linear regression covid contacts v. infections with two regression lines: one for weeks prior September 2021, one for after
inf_v_contact <- df_contact %>%
  dplyr::mutate(period = if_else(date < ymd("2021-08-30"), "Second wave", "Third wave"),
                period = as.factor(period),
                age_g = paste("Age:", age_g)) %>%
  ggplot(aes(x = rate_infection, y = rate_contact, group = period, colour = period)) +
  geom_point(size = 1) +


  geom_smooth(method = lm, col = NA, se = TRUE, formula = y ~ x) +
  geom_smooth(aes(col = period), method = lm, se = FALSE) +


  facet_wrap(~ age_g, scales = "fixed") +
  scale_color_discrete_qualitative(name  = NULL) +
  scale_x_continuous(name = "Infection rate per 1000 person-weeks") +
  scale_y_continuous(name = "GP contact rate per 1000 person-weeks") +
  ggtitle(label = "COVID-19 infection rates vs GP contact rate for COVID-19",
          subtitle = region) +
  theme_minimal(base_size = 12) +
  theme(legend.key = element_rect(fill = NA, colour = NA),
        axis.title = element_text(size = 11),
        axis.text =  element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)))


# The same but with consults (including repeat consults)
inf_v_consult <- df_consult %>%
  dplyr::mutate(period = if_else(date < ymd("2021-08-30"), "Second wave", "Third wave"),
                period = as.factor(period),
                age_g = paste("Age:", age_g)) %>%
  ggplot(aes(x = rate_infection, y = rate_consult, group = period, colour = period)) +
  geom_point(size = 1) +


  geom_smooth(method = lm, col = NA, se = TRUE, formula = y ~ x) +
  geom_smooth(aes(col = period), method = lm, se = FALSE) +


  facet_wrap(~ age_g, scales = "fixed") +
  scale_color_discrete_qualitative(name  = NULL) +
  scale_x_continuous(name = "Infection rate per 1000 person-weeks") +
  scale_y_continuous(name = "GP consultation rate per 1000 person-weeks") +
  ggtitle(label = "COVID-19 infection rates vs GP contact rate for COVID-19",
          subtitle = region) +
  theme_minimal(base_size = 12) +
  theme(legend.key = element_rect(fill = NA, colour = NA),
        axis.title = element_text(size = 11),
        axis.text =  element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)))


# helper function to move the legend to the empty space in the bottom-right corner
shift_legend <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x, zeroGrob()))

  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )

  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

save_me_contact <- gridExtra::grid.arrange(shift_legend(inf_v_contact))
save_me_consult <- gridExtra::grid.arrange(shift_legend(inf_v_consult))

# save the resulting plot
ggsave(filename = here::here("fig1_north.png"), plot = save_me_contact, width = 2800, height = 1800,
       units = c("px"), scale = 0.8)
ggsave(filename = here::here("fig1_north.png"), plot = save_me_consult, width = 2800, height = 1800,
       units = c("px"), scale = 0.8)

# get the correlation coefficient by age
get_lin_fit <- function(age, data = df_contact){
  data %>%
    dplyr::filter(age_g == {{ age }}) %>%
    dplyr::mutate(period = if_else(date < ymd("2021-09-30"), "Second wave", "Third wave"),
                  period = as.factor(period)) %>%
    lm(rate_contact ~ rate_infection + rate_infection:period, data = .) -> lin_fit

  summary(lin_fit)
}


# fit regression model to the data ------------------------------------------------------------

# first with contact data (only first contact counts)

fit19_contact <- get_lin_fit("0-19")
fit39_contact <- get_lin_fit("20-39")
fit59_contact <- get_lin_fit("40-59")
fit79_contact <- get_lin_fit("60-79")
fit80_contact <- get_lin_fit("80+")


fit_coefs_contact <- list(fit19_contact = fit19$coefficients,
                          fit39_contact = fit39$coefficients,
                          fit59_contact = fit59$coefficients,
                          fit79_contact = fit79$coefficients,
                          fit80_contact = fit80$coefficients)


# then with all consultation, including repeat consultations
fit19_consults <- get_lin_fit("0-19" , data = df_consult)
fit39_consults <- get_lin_fit("20-39", data = df_consult)
fit59_consults <- get_lin_fit("40-59", data = df_consult)
fit79_consults <- get_lin_fit("60-79", data = df_consult)
fit80_consults <- get_lin_fit("80+"  , data = df_consult)


fit_coefs_consult <- list(fit19_consults = fit19$coefficients,
                          fit39_consults = fit39$coefficients,
                          fit59_consults = fit59$coefficients,
                          fit79_consults = fit79$coefficients,
                          fit80_consults = fit80$coefficients)



# Save ----------------------------------------------------------------------------------------

# save data
df_contact_share <- df_contact %>%
  dplyr::select(rate_contact, rate_infection, date, age_g)

df_consult_share <- df_consult %>%
  dplyr::select(rate_contact, rate_infection, date, age_g)

save(df_contact_share, df_consult_share, file = "inf_v_contact.RData")

# save correlation coefficients - this can be share with partners
save(fit_coefs_contact, fit_coefs_consult, file = here::here("fit_coefs.RData"))


# save the complete fits - this stays saved locally
save(fit19_contact,
     fit39_contact,
     fit59_contact,
     fit79_contact,
     fit80_contact,

     fit19_consults,
     fit39_consults,
     fit59_consults,
     fit79_consults,
     fit80_consults,

     file = here::here("fits.RData"))


# ----------------- END ------------------------

