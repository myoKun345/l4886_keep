library(tidyverse)

bnc <- read_csv("data/bnc3k_finished.csv") %>%
  set_names(c("text", "tense", "aspect", "subjp", "ving")) %>%
  mutate(tense=str_to_lower(tense),
         aspect=str_to_lower(aspect),
         subjp=str_to_lower(subjp),
         ving=str_to_lower(ving))
bnc_valid <- bnc %>% filter(aspect %in% c("iterative", "continuative", "ambiguous")) %>%
  mutate(subjp=as_factor(str_extract(subjp, ".{3}")))

bnc_ttab <- xtabs(~aspect + tense, data=bnc_valid); bnc_ttab
chisq.test(bnc_ttab)

bnc_stab <- xtabs(~aspect + subjp, data=bnc_valid); bnc_stab
chisq.test(bnc_stab)

bnc_wtab <- xtabs(~aspect + ving, data=bnc_valid); bnc_wtab
chisq.test(bnc_wtab)


