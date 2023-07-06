library(brms)
library(tidyverse)


covid_contacts <- readRDS("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/manuscripts/covid_cohort_primary_care/clean_data/covid_contacts.RData")

load("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/manuscripts/covid_cohort_primary_care/rivm_data/covid_contacts.RData")

df <- n_contacts_mun_total %>%
  dplyr::filter(date == "2020-11-23")


rep(1:length(unique(n_contacts_mun_total$date)), each = length(unique(n_contacts_mun_total$municipality)))
rep(1:70, each = 12)

rep(1:length(unique(n_contacts_mun_tot$date)), 
                   each = length(levels(n_contacts_mun_tot$municipality)))

data_points <- n_contacts_mun_total %>%
  group_by(date) %>%
  count() %>%
  pull(n)
order <- rep(1:70, data_points)

dates <- unique(n_contacts_mun_total$date)

# add covid cases

n_contacts_mun_total <- left_join(n_contacts_mun_total, 
            cases_rotterdam_rijnmond %>% rename(date = week), 
            by = "date")

length(levels(n_contacts_mun_tot$municipality))

df <- n_contacts_mun_total %>% 
  mutate(municipality = factor(municipality)) %>%
  arrange(date) %>%
  add_column(order = order)

library(brms)
prior_int <- prior(normal(-4.95, 1.94), class = Intercept)

fit <- brms::brm(data = df,
           family = binomial,
           covid_contacts | trials(sample_size) ~ 1 + order + n + (1 + n | municipality),
           prior = prior_int,
           refresh = 0)


fit <- brms::brm(data = df,
                 family = binomial,
                 covid_contacts | trials(sample_size) ~ 1 + (1 | municipality),
                 prior = prior_int,
                 refresh = 0)

# prior specification
library(ProbBayes)

# our prior belief is that there is around 1% of patients per week, contacting GPs
mean_contact_rate <- list(x = 0.01, p = 0.5)

# this is highly unlikely to exceed 5%
max_contact_rate <- list(x = 0.05, p = 0.9)

beta_par <- beta.select(mean_contact_rate, max_contact_rate)
set.seed(12)
p_sim <- rbeta(1000, beta_par[1], beta_par[2])
theta_sim <- log(p_sim / (1 - p_sim))

mean(theta_sim)
# prior mean is -4.95
sd(theta_sim)
# prior sigma is 1.94

# convert logit to probability
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}

# store results
results <- data.frame(p = rep(0, length(dates)), 
                      p_upper = rep(0, length(dates)),
                      p_lower = rep(0, length(dates)),
                      
                      sd_mun = rep(0, length(dates)),
                      sd_upper = rep(0, length(dates)),
                      sd_lower = rep(0, length(dates)),
                      )


for (i in 3:length(dates)) {
  df <- n_contacts_mun_total %>%
    dplyr::filter(date == dates[i])

  fit <- brm(data = df,
             family = binomial,
             covid_contacts | trials(sample_size) ~ 1 + (1 | municipality),
             prior = c(prior(normal(-4.95, 1.94), class = Intercept)),
             refresh = 0)
  
  coefs <- summary(fit)

  p <- c(coefs$fixed[, c(1, 3, 4)], coefs$random[1, 3, 4])

  results[i, ] <- logit2prob(p)
}


post <- posterior_samples(fit)

post %>% mutate(p = (exp(b_Intercept) / (1 + exp(b_Intercept))) * 1000 ) -> post

post %>%
    mutate(across(everything(), ~ logit2prob(.x) * 1000)) -> post

ggplot(post, aes(p * 1000)) +
    geom_density()

p <- summary(fit)$fixed[, c(1, 3, 4)]


logit2prob(theta) * 1000




save(results, file = "covid_contact_results.RData")
