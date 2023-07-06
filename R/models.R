# total consults in RG & add infection rates
consults <- covid_contacts$total %>%
    left_join(., infections$infections_RG_total, by = "date", suffix = c("_consults", "_infection"))


ggplot(consults, aes(x = date, y =  rate_consults)) +
    geom_line() +
    geom_point() +

    geom_line(aes(y = rate_infection), colour =  "red") +
    geom_point(aes(y = rate_infection), colour = "red") +
    scale_y_continuous(name = y_axis, limits = c(0, 20))




# consults by age and sex in RG & add infection rates
consults_age <- covid_contacts$age  %>%
    left_join(., infections$infections_age_RG, by = c("date", "age_g"),
              suffix = c("_consults", "_infection"))



# consults by age and sex in RG & add infection rates
consults_sex <- covid_contacts$sex  %>%
    left_join(., infections$infections_sex_RG %>% rename(sex = Sex), by = c("date", "sex"),
              suffix = c("_consults", "_infection"))



save(consults, consults_age, consults_sex, file = here::here("data", "consults.RData"))
load("V:/Uitwissel/Premysl/GRIP3/consults.RData")

library(strucchange)
library(tidyverse)
library(patchwork)
library(fable)
library(forecast)
library(fpp3)
library(cowplot)


age_groups <- c(
  "0-19",
  "20-39",
  "40-59",
  "60-79",
  "80+"
)



estim_contacts <- function(
        df = consults_age,
        age = c(
            "0-19" ,
            "20-39",
            "40-59",
            "60-79",
            "80+"),
            h = 20,
            cutoff_date = "2021-08-30"
            )
{

    y <- df$rate_consults[df$age_g == age]
    infection <- df$rate_infection[df$age_g == age]

    dat <- data.frame(date = unique(df$date), consults = y, infections = infection)
    names(dat) <- c("date", "consults", "infections")

    baseline <- dat %>%
        dplyr::filter(date <= lubridate::ymd(cutoff_date)) %>%
        as_tsibble()

    counterfactual <- dat %>%
        dplyr::filter(date > lubridate::ymd(cutoff_date)) %>%
        as_tsibble() %>%
        head(h)

    fit <- baseline |>
        model(ARIMA(consults ~ infections + pdq()))

    pred <- fabletools::forecast(fit, new_data = counterfactual) %>%
        hilo(level = c(80, 95)) %>%
        mutate(
            lower80 = `80%`$lower,
            upper80 = `80%`$upper,
            lower95 = `95%`$lower,
            upper95 = `95%`$upper
        ) %>%

        mutate(
            lower80 = if_else(lower80 < 0, 0, lower80),
            upper80 = if_else(upper80 < 0, 0, upper80),
            lower95 = if_else(lower95 < 0, 0, lower95),
            upper95 = if_else(upper95 < 0, 0, upper95),
            .mean   = if_else(.mean < 0, 0, .mean)
        )

    colours <- c("Observed" = "brown2",
                "Estimated" = "deepskyblue2")


    p <- df %>%
        dplyr::filter(age_g == age,
                      date < lubridate::ymd(cutoff_date) + lubridate::weeks(h)) %>%
        left_join(., pred, by = "date") %>%
        ggplot(aes(x =  date, y = rate_consults)) +
        geom_line(aes(colour =  "Observed")) +
        geom_point(aes(colour = "Observed"), size = 2) +
        # geom_line(aes(y = rate_infection), colour = "grey", linewidth = 1.5) +
        geom_ribbon(aes(ymin = lower80, ymax = upper80), fill = "deepskyblue2", alpha = 0.2) +
        geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "deepskyblue2", alpha = 0.2) +
        geom_line(aes(y = .mean, colour =  "Estimated")) +
        geom_point(aes(y = .mean, colour = "Estimated")) +
        labs(color = NULL) +
        scale_color_manual(values = colours) +
        scale_x_date(date_breaks = "15 weeks", date_labels = "%b %Y", name = NULL) +
        ggtitle(age) +
        theme_minimal(base_size = 20) +
        theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust = 0))

    pred %>%
        dplyr::select(lower80, upper80, lower95, upper95, .mean)

    list(p = p, pred = pred, fit = fit)

    }


estim_contacts2 <- function(
    df = consults_age,
    age = c(
      "0-19" ,
      "20-39",
      "40-59",
      "60-79",
      "80+"),
    h = 20,
    cutoff_date = "2021-08-30"
)
{
  
  y <- df$rate_consults[df$age_g == age]
  infection <- df$rate_infection[df$age_g == age]
  
  dat <- data.frame(date = unique(df$date), consults = y, infections = infection)
  names(dat) <- c("date", "consults", "infections")
  
  baseline <- dat %>%
    dplyr::filter(date <= lubridate::ymd(cutoff_date)) %>%
    as_tsibble()
  
  counterfactual <- dat %>%
    dplyr::filter(date > lubridate::ymd(cutoff_date)) %>%
    as_tsibble() %>%
    head(h)
  
  fit <- baseline |>
    model(ARIMA(consults ~ infections + pdq()))
  
  pred <- fabletools::forecast(fit, new_data = counterfactual) %>%
    hilo(level = c(80, 95)) %>%
    mutate(
      lower80 = `80%`$lower,
      upper80 = `80%`$upper,
      lower95 = `95%`$lower,
      upper95 = `95%`$upper
    ) %>%
    
    mutate(
      lower80 = if_else(lower80 < 0, 0, lower80),
      upper80 = if_else(upper80 < 0, 0, upper80),
      lower95 = if_else(lower95 < 0, 0, lower95),
      upper95 = if_else(upper95 < 0, 0, upper95),
      .mean   = if_else(.mean < 0, 0, .mean)
    )
  
  colours <- c("Observed" = "brown2",
               "Estimated" = "deepskyblue2")
  
  
  p <- df %>%
    dplyr::filter(age_g == age,
                  date < lubridate::ymd(cutoff_date) + lubridate::weeks(h)) %>%
    left_join(., pred, by = "date") %>%
    ggplot(aes(x =  date, y = rate_consults)) +
    geom_line(aes(colour =  "Observed")) +
    geom_point(aes(colour = "Observed"), size = 2) +
    # geom_line(aes(y = rate_infection), colour = "grey", linewidth = 1.5) +
    geom_point(aes(y = .mean, colour = "Estimated")) +
    geom_errorbar(aes(ymin = lower95, ymax = upper95), colour = "deepskyblue2") +
    labs(color = NULL) +
    scale_color_manual(values = colours) +
    scale_x_date(date_breaks = "15 weeks", date_labels = "%b %Y", name = NULL) +
    ggtitle(age) +
    theme_minimal(base_size = 20) +
    theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust = 0))
  
  pred %>%
    dplyr::select(lower80, upper80, lower95, upper95, .mean)
  
  list(p = p, pred = pred, fit = fit)
  
}

age80    <- estim_contacts(age = age_groups[5], h = 30)
age60_79 <- estim_contacts(age = age_groups[4], h = 30)
age40_59 <- estim_contacts(age = age_groups[3], h = 30)
age20_39 <- estim_contacts(age = age_groups[2], h = 30)
age0_19  <- estim_contacts(age = age_groups[1], h = 30)

age80    <- estim_contacts2(age = age_groups[5], h = 30)
age60_79 <- estim_contacts2(age = age_groups[4], h = 30)
age40_59 <- estim_contacts2(age = age_groups[3], h = 30)
age20_39 <- estim_contacts2(age = age_groups[2], h = 30)
age0_19  <- estim_contacts2(age = age_groups[1], h = 30)

legend <- cowplot::get_legend(age0_19$p)


plots <- plot_grid(age0_19$p + theme(legend.position = "none"),
  age20_39$p + theme(legend.position = "none"),
  age40_59$p + theme(legend.position = "none"),
  age60_79$p + theme(legend.position = "none"),
  age80$p + theme(legend.position = "none"),
  legend, nrow = 2)


title <- ggdraw() +
      draw_label(
        "GP contact rate for COVID-19 related complaints by age (2020-2022)",
        x = 0,
        hjust = 0,
        size = 22
      ) +

      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )

plot_grid(
      title, plots,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    )

y.grob <- grid::textGrob("Weekly contacts per 1,000 patients",
                   gp = grid::gpar(fontsize = 15), rot = 90)

grid::grid.arrange(arrangeGrob(plot_grid, left = y.grob))

report(age80$fit)
report(age60_79$fit)
report(age40_59$fit)
report(age20_39$fit)
report(age0_19$fit)

age80$fit %>%
    gg_tsresiduals()

age60_79$fit %>%
    gg_tsresiduals()

age40_59$fit %>%
    gg_tsresiduals()

age20_39$fit %>%
    gg_tsresiduals()

age0_19$fit %>%
    gg_tsresiduals()


d <- age40_59$p
age60_79$p[["data"]]

age40_59$p[["data"]] %>%
  mutate(ci_width = upper95 - lower95) %>%
  ggplot(aes(x = date, y = ci_width)) +
  geom_line() +
  geom_point()

age60_79$p[["data"]] %>%
  mutate(ci_width = upper95 - lower95) %>%
  ggplot(aes(x = date, y = ci_width)) +
  geom_line()


age80$p[["data"]] %>%
  mutate(ci_width = upper95 - lower95) %>%
  ggplot(aes(x = date, y = ci_width)) +
  geom_line()

age20_39$p[["data"]] %>%
  mutate(ci_width = upper95 - lower95) %>%
  ggplot(aes(x = date, y = ci_width)) +
  geom_line()

age0_19$p[["data"]] %>%
  mutate(ci_width = upper95 - lower95) %>%
  ggplot(aes(x = date, y = ci_width)) +
  geom_line()

#  80
i <- 1

df <- data.frame(date = consults$date, consults = y, infections = infection)
names(df) <- c("date", "consults", "infections")
baseline <- df %>%
    dplyr::filter(date <= lubridate::ymd("2021-08-30")) %>%
    # mutate(consults = if_else(consults == 0.0, consults + 0.01, consults)) %>%
    as_tsibble()


counterfactual <- df %>%
  dplyr::filter(date > lubridate::ymd("2021-08-30")) %>%
  as_tsibble() %>%
  head(20)

pred <- baseline |>
    model(ARIMA(consults ~ infections + pdq())) %>%
    fabletools::forecast(new_data = counterfactual) %>%
    hilo(level = c(80, 95)) %>%
    mutate(
        lower80 = `80%`$lower,
        upper80 = `80%`$upper,
        lower95 = `95%`$lower,
        upper95 = `95%`$upper
  )

consults_age %>%
    dplyr::filter(age_g == "80+",
                  date < lubridate::ymd("2022-01-30")) %>%
    left_join(., pred, by = "date") %>%
    ggplot(aes(x =  date, y = rate_consults)) +
    geom_line(colour = "brown2") +
    geom_point(colour = "brown2", size = 2) +
    geom_line(aes(y = rate_infection), colour = "grey", linewidth = 1.5) +
    geom_line(aes(y = .mean), colour =  "deepskyblue2") +
    geom_point(aes(y = .mean), colour = "deepskyblue2") +
    geom_ribbon(aes(ymin = lower80, ymax = upper80), fill = "blue", alpha = 0.2) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "blue", alpha = 0.2) +
    # scale_y_continuous(limits = c(0, 20)) +
    scale_x_date(date_breaks = "15 weeks", date_labels = "%b %Y", name = NULL) +
    ggtitle("GP consultation rate for acute COVID-19 by age") +
    theme_minimal(base_size = 20) +
    theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust = 0))


#  60-79
i <- 4
y <- consults_age$rate_consults[consults_age$age_g == age_groups[i]]
infection <- consults_age$rate_infection[consults_age$age_g == age_groups[i]]

df <- data.frame(date = consults$date, consults = y, infections = infection)
names(df) <- c("date", "consults", "infections")
df <- as_tsibble(df)

fit <- df |>
    model(ARIMA(consults ~ infections + trend() + pdq()))

report(fit)

# 40-59
i <- 3
y <- consults_age$rate_consults[consults_age$age_g == age_groups[i]]
infection <- consults_age$rate_infection[consults_age$age_g == age_groups[i]]

df <- data.frame(date = consults$date, consults = y, infections = infection)
names(df) <- c("date", "consults", "infections")
df <- as_tsibble(df)

fit <- df |>
    model(ARIMA(consults ~ infections + trend() + pdq()))

report(fit)

#  20-39
i <- 2
y <- consults_age$rate_consults[consults_age$age_g == age_groups[i]]
infection <- consults_age$rate_infection[consults_age$age_g == age_groups[i]]

df <- data.frame(date = consults$date, consults = y, infections = infection)
names(df) <- c("date", "consults", "infections")
df <- as_tsibble(df)

fit <- df |>
    model(ARIMA(consults ~ infections + trend() + pdq()))

report(fit)


#  0-18
i <- 1
y <- consults_age$rate_consults[consults_age$age_g == age_groups[i]]
infection <- consults_age$rate_infection[consults_age$age_g == age_groups[i]]

df <- data.frame(date = consults$date, consults = y, infections = infection)
names(df) <- c("date", "consults", "infections")
df <- as_tsibble(df)

fit <- df |>
    model(ARIMA(consults ~ infections + trend() + pdq()))

report(fit)



tseries::kpss.test(y, null = "T")
tseries::kpss.test(y, null = "L")
auto.arima(y, xreg = infections)
auto.arima(diff(y), xreg = diff(infections))

Box.test(y, type = "Ljung-Box")




for(i in 1:length(age_groups)){
    y = ts(consults_age$rate_consults[consults_age$age_g == age_groups[i]],
           start = (2020 + (5 * 31) + (2 * 30) + 28), freq = 365.25 / 7)
    infections = ts(consults_age$rate_infection[consults_age$age_g == age_groups[i]],
                    start = (2020 + (5 * 31) + (2 * 30) + 28), freq = 365.25 / 7)

    consults_ts <- ts.intersect(
        rate_consults = y,
        lag1 = stats::lag(y, -1),
        lag2 = stats::lag(y, -2),
        lag3 = stats::lag(y, -3),
        lag4 = stats::lag(y, -4),
        rate_infections = infections
    )


    bp <- breakpoints(rate_consults ~ lag1 + lag2 + rate_infections,
                      data = consults_ts, breaks = 3, format.times = TRUE)


    bp_location <- c(0, bp$breakpoints, nrow(consults_ts))


    if(is.na(bp$breakpoints[1])) {
      centred <- as.integer(nrow(consults_ts) / 2)
      ci <- 0
    } else {
      ci <- as.vector(confint(bp)[[1]][, c(1, 3)])
      centred <- as.integer((diff(bp_location) / 2) + bp_location[-length(bp_location)])
    }

    res[[i]] <- as.data.frame(consults_ts) %>%
        add_column(date = consults$date[-c(1:4)]) %>%
        add_column(fitted = fitted(bp)) %>%

        ggplot(aes(x = date, y = rate_consults)) +
        geom_line(colour = "brown2") +
        geom_point(colour = "brown2", size = 2) +
        geom_line(aes(y = fitted), colour = "blue") +
        geom_vline(xintercept = consults$date[-c(1:4)][bp$breakpoints]) +
        geom_vline(xintercept = consults$date[-c(1:4)][ci], linetype = "dotted") +
        ggtitle(age_groups[i]) +
        geom_vline(xintercept = ymd("2021-06-14"), colour = "black", linetype = "dotted", size = 1) +
        theme_minimal() +
        annotate("text", x = consults$date[-c(1:4)][centred], y = 4,
               label = round(coef(bp)[, 4], 2)) +
        scale_x_date(breaks = NULL, name = NULL) +
        scale_y_continuous(name = "Contact rate") +
        theme_minimal(base_size = 15) +
        theme(axis.title.x = element_blank())

    betas[[i]] <- round(coef(bp), 2)
}

res[[1]] + res[[2]] + res[[3]] + res[[4]] +
    (res[[5]] + scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y", name = NULL)) +
    plot_layout(nrow = 5, byrow = FALSE)

    betasAR_3 <- betas
betasAR_2 <- betas


ggplot(consults_age, aes(x =  date, y = rate_consults)) +
    geom_line(colour = "brown2") +
    geom_point(colour = "brown2", size = 2) +
    geom_line(aes(y = rate_infection), colour = "grey", linewidth = 1.5) +
    facet_wrap(~ age_g) +
    scale_y_continuous(limits = c(0, 20)) +
    scale_x_date(date_breaks = "15 weeks", date_labels = "%b %Y", name = NULL) +
    ggtitle("GP consultation rate for acute COVID-19 by age") +
    theme_minimal(base_size = 20) +
    theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust = 0))

