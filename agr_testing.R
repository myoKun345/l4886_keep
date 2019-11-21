library(tidyverse)

agr_data <- read_csv("data/agr_samples.csv") %>%
  mutate(Jonathan=str_to_lower(Jonathan), Michael=str_to_lower(Michael),
         Jonathan=if_else(Jonathan %in% c("mistag", "passive"), "bad_data", Jonathan),
         Michael=if_else(Michael %in% c("mistag", "passive"), "bad_data", Michael),
         agree=if_else(Jonathan == Michael, 1,
                       if_else((Jonathan %in% c("continuative", "iterative") & Michael=="ambiguous") | (Michael %in% c("continuative", "iterative") & Jonathan=="ambiguous"),
                               0.5, 0)))

agr_table <- xtabs(~Jonathan + Michael, data=agr_data)

exp_agr <- (nrow(filter(agr_data, Jonathan=="ambiguous"))/200) * (nrow(filter(agr_data, Michael=="ambiguous"))/200) +
  (nrow(filter(agr_data, Jonathan=="bad_data"))/200) * (nrow(filter(agr_data, Michael=="bad_data"))/200) +
  (nrow(filter(agr_data, Jonathan=="continuative"))/200) * (nrow(filter(agr_data, Michael=="continuative"))/200) +
  (nrow(filter(agr_data, Jonathan=="iterative"))/200) * (nrow(filter(agr_data, Michael=="iterative"))/200) +
  (nrow(filter(agr_data, Jonathan %in% c("continuative", "iterative")))/200) * (nrow(filter(agr_data, Michael=="ambiguous"))/200) * 0.5 +
  (nrow(filter(agr_data, Michael %in% c("continuative", "iterative")))/200) * (nrow(filter(agr_data, Jonathan=="ambiguous"))/200) * 0.5
act_agr <- sum(agr_data$agree)/200
kappa <- (act_agr - exp_agr) / (1 - exp_agr)

error = sqrt(act_agr * (1 - act_agr) / 200) * 1.96
ci.low = act_agr - error
ci.high = act_agr + error

kappa.low <- (ci.low - exp_agr) / (1 - exp_agr)
kappa.high <- (ci.high - exp_agr) / (1 - exp_agr)
