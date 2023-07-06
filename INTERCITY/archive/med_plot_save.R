med_plot <- med_tree %>%
  group_by(medication) %>%
  count() %>%
  # mutate(medication = if_else((n < 300 | medication == "OTHER"), "OTHER", medication)) %>%
  summarise(n = sum(n)) %>%
  mutate(medication = if_else(medication == "AMOXICILLIN AND BETA-LACTAMASE INHIBITOR",
                              "AMOXICILLIN AND \nBETA-LACTAMASE \nINHIBITOR",
                              medication)) %>%
  ggplot(aes(area = n, fill = medication,
             label = paste(medication, n, sep = "\n"))) +
  plot_tree

save(med_plot, file = "med_plot.RData")
