library(tidyverse)
library(gridExtra)
library(grid)

load("//storage/v/vcl13/HUGE/Data/Onderzoek_Projecten/Zorgmijding_COVID/GRIP3/med.RData")

temp <- cc_med %>%
  group_by(med_class, med_class_3) %>%
  count() %>%
  arrange(med_class, desc(n)) %>%
  filter(n > 20)



one <- temp %>%
  filter(!(is.na(med_class)),
         !(med_class %in% c("V", "Y"))) %>%
  ungroup() %>%
  slice(1:11) %>%
  ggplot(aes(x = med_class_3, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ med_class, scales = "free") +
  theme_minimal(base_size = 15) +
  scale_x_discrete(name = NULL)

two <- temp %>%
  filter(!(is.na(med_class)),
         !(med_class %in% c("V", "Y"))) %>%
  ungroup() %>%
  slice(12:22) %>%
  ggplot(aes(x = med_class_3, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ med_class, scales = "free") +
  theme_minimal(base_size = 15) +
  scale_x_discrete(name = "Drug type (2nd level ATC code)")
  

three <- temp %>%
  filter(!(is.na(med_class)),
         !(med_class %in% c("V", "Y"))) %>%
  ungroup() %>%
  slice(24:35) %>%
  ggplot(aes(x = med_class_3, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(~ med_class, scales = "free") +
  theme_minimal(base_size = 15) +
  scale_x_discrete(name = NULL) +
  theme(plot.background = element_rect(fill = "grey", colour = "grey"))

tit <- textGrob("Medication prescribed to COVID-19 positive people in primary care",
                gp = gpar(fontsize = 20, hjust = 1))

grid.arrange(one, three, two, top = tit)
pdf("medication.pdf", height = 15, width = 10)
grid.table(temp %>% filter(!(is.na(med_class))))
dev.off()           
