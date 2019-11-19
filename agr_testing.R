library(tidyverse)

agr_data <- read_csv("data/agr_samples.csv") %>%
  mutate(Jonathan=str_to_lower(Jonathan), Michael=str_to_lower(Michael),
         Jonathan=if_else(Jonathan %in% c("mistag", "passive"), "bad_data", Jonathan),
         Michael=if_else(Michael %in% c("mistag", "passive"), "bad_data", Michael),
         agree=if_else(Jonathan == Michael, T, F))

agr_table <- xtabs(~Jonathan + Michael, data=agr_data)

exp_agr <- (nrow(filter(agr_data, Jonathan=="ambiguous"))/200) * (nrow(filter(agr_data, Michael=="ambiguous"))/200) +
  (nrow(filter(agr_data, Jonathan=="bad_data"))/200) * (nrow(filter(agr_data, Michael=="bad_data"))/200) +
  (nrow(filter(agr_data, Jonathan=="continuative"))/200) * (nrow(filter(agr_data, Michael=="continuative"))/200) +
  (nrow(filter(agr_data, Jonathan=="iterative"))/200) * (nrow(filter(agr_data, Michael=="iterative"))/200)
act_agr <- nrow(filter(agr_data, agree))/200
kappa <- (act_agr - exp_agr) / (1 - exp_agr)