load("V:/Uitwissel/Premysl/covid_cohort_primary_care/R/covid_contact_results.RData")


library(tidyverse)
results$date <- seq.Date(ymd("2020-08-31"), by = "week", length.out = 70)

results <- results %>%
  mutate(p = p * 1000,
         lower = lower * 1000,
         upper = upper * 1000) %>%
  add_column(database = "Rotterdam")

# simulate other regions
results_stizon <- results %>%
  mutate(p = p + rnorm(70, 0.6, 0.2),
         lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1), 
         database = "STIZON") 

results_gron <- results %>%
  mutate(p = p + rnorm(70, 0.7, 0.2),
         lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1), 
         database = "Groningen")

results_utr <- results %>%
  mutate(p = p + rnorm(70, 0.3, 0.2),
         lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1), 
         database = "Utrecht") 

results_maa <- results %>%
  mutate(p = p + rnorm(70, 0.7, 0.2),
         lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1), 
         database = "Maastricht") 

results_nijm <- results %>%
  mutate(p = p + rnorm(70, 0.3, 0.2),
         lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1), 
         database = "Nijmegen") 

all <- bind_rows(list(results, results_stizon, results_gron, results_maa,
               results_utr, results_nijm)) 

summed <- all %>%
  group_by(date) %>%
  summarise(p = mean(p)) %>%
  mutate(lower = p - rnorm(70, 0.4, sd = 0.1),
         upper = p + rnorm(70, 0.4, sd = 0.1)) %>%
  add_column(database = "Summarised") 

all %>%
  ggplot(aes(x = date, y = p)) +
  geom_line(alpha = 0.5, aes(group = as.factor(database), 
                             colour = as.factor(database))) +
  geom_point(alpha = 0.5, aes(group = as.factor(database), 
                              colour = as.factor(database))) +
  geom_line(data = summed, aes(y = p), alpha = 1, colour = "black", linewidth = 1.1) +
  geom_ribbon(data = summed, aes(ymin = lower, ymax = upper), alpha = 0.2) +
  
  scale_y_continuous(name = "Number of contacts per 1000 person-weeks") +
  scale_x_date(name = NULL, breaks = "5 weeks") +
  scale_colour_discrete(name = "Database") +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 1)) 
  
