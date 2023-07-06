library(tidyverse)
library(lubridate)

path_cases <- "//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/
  manuscripts/covid_cohort_primary_care/rivm_data/
  COVID-19_aantallen_gemeente_per_dag.csv"

cases_rotterdam_rijnmond <- read_delim("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/manuscripts/covid_cohort_primary_care/rivm_data/COVID-19_aantallen_gemeente_per_dag.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
    filter(Security_region_name == "Rotterdam-Rijnmond") %>%
    mutate(Date_of_publication = as.Date(Date_of_publication, "%d/%m/%Y")) %>%
    mutate(week = floor_date(Date_of_publication, unit = "weeks", 
           week_start = getOption("lubridate.week.start", 1))) %>%
    group_by(week) %>%
    summarise(n = sum(Total_reported)) %>%
    # filter(week %in% covid_cons_rate$data$week) %>%
    identity()

cases_rotterdam_rijnmond <- cases_rotterdam_rijnmond %>%
    mutate(rate = (n / 651631) * 100000)




cases_rotterdam <- read_delim("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/COVID-19_aantallen_gemeente_per_dag.csv", delim = ";") %>%
    filter(Municipality_name == "Rotterdam") %>%
    mutate(Date_of_publication = as.Date(Date_of_publication, "%d/%m/%Y")) %>%
    mutate(week = floor_date(Date_of_publication, unit = "weeks", 
                             week_start = getOption("lubridate.week.start", 1))) %>%
    group_by(week) %>%
    summarise(n = sum(Total_reported)) %>%
    filter(week %in% covid_cons_rate$data$week) 

# standardise to 100,000 people, considering Rotterdam has 651,631 inhabitants 
# (https://www.cbs.nl/en-gb/figures/detail/37259eng?q=rotterdam)

cases_rotterdam <- cases_rotterdam %>%
    mutate(rate = (n / 651631) * 100000)



load("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/plots/covid_cons_rate.RData")

ggplot(d, aes(x = week, y = (rate / 10))) +
    geom_line(aes(colour = "GP consultation rate for acute COVID-19"), size = 3) +
    geom_point(size = 4, colour = "blue1") +
    
    geom_line(aes(y = cases_rotterdam$rate, colour = "Infection rate in Rotterdam"), size = 2) +
    geom_point(aes(y = cases_rotterdam$rate), size = 2.3, colour = "darkslategrey") +
    annotate("text", x = ymd("2021-05-01"), y = 700, label = "Over 50% of\npopulation fully\nvaccinated",
             hjust = 0, size = 6) +
    geom_vline(xintercept = ymd("2021-06-30"), colour = "black", linetype = "dotted", size = 1) +
    annotate("curve", x = ymd("2021-05-20"), xend = ymd("2021-06-29"), curvature = .4, size = 1,
             y = 650, yend = 600, arrow = arrow(ends = "last", angle = 45, length = unit(.2,"cm"))) +
    scale_color_manual(name = NULL, 
                       values = c("COVID-19 infection rate in Rotterdam" = "darkgrey", 
                                  "GP consultation rate for acute COVID-19" = "lightblue")) +
    scale_x_date(date_breaks = "6 weeks", date_labels = "%b %Y", name = NULL) +
    scale_y_continuous(name = "per 100,000 person-weeks") +
    ggtitle("GP consultation rate for acutre COVID-19 (2020-2021)") +
    theme_minimal(base_size = 23) +
    theme(legend.position = "bottom")
    



as.Date("12/07/2021", "%d/%m/%Y")
