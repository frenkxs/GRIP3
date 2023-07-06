# save data
df_contact_share <- df_contact %>%
  dplyr::select(rate_contact, rate_infection, date, age_g)

df_consult_share <- df_consult %>%
  dplyr::select(rate_contact, rate_infection, date, age_g)

save(df_contact_share, df_consult, file = "inf_v_contact.RData")
