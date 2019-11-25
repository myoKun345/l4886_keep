library(tidyverse)

bnc <- read_csv("data/bnc3k_finished.csv") %>%
  set_names(c("text", "tense", "aspect", "subjp", "ving")) %>%
  mutate(tense=str_to_lower(tense),
         aspect=str_to_lower(aspect),
         subjp=str_to_lower(subjp),
         ving=str_to_lower(ving),
         dialect="B")
bnc_valid <- bnc %>% filter(aspect %in% c("iterative", "continuative", "ambiguous")) %>%
  mutate(subjp=as_factor(str_extract(subjp, ".{3}")))

bnc_ttab <- xtabs(~aspect + tense, data=bnc_valid); bnc_ttab
chisq.test(bnc_ttab)

bnc_stab <- xtabs(~aspect + subjp, data=bnc_valid); bnc_stab
chisq.test(bnc_stab)

bnc_wtab <- xtabs(~aspect + ving, data=bnc_valid); bnc_wtab
chisq.test(bnc_wtab)

coca <- read_csv("data/coca3k_finished.csv") %>%
  set_names(c("text", "tense", "aspect", "subjp", "ving")) %>%
  mutate(tense=str_to_lower(tense),
         aspect=str_to_lower(aspect),
         subjp=str_to_lower(subjp),
         ving=str_to_lower(ving),
         dialect="A")
coca_valid <- coca %>% filter(aspect %in% c("iterative", "continuative", "ambiguous")) %>%
  mutate(subjp=as_factor(str_extract(subjp, ".{3}")))

coca_ttab <- xtabs(~aspect + tense, data=coca_valid); coca_ttab
chisq.test(coca_ttab)

coca_stab <- xtabs(~aspect + subjp, data=coca_valid); coca_stab
chisq.test(coca_stab)

coca_wtab <- xtabs(~aspect + ving, data=coca_valid); coca_wtab
chisq.test(coca_wtab)

both <- rbind(bnc_valid, coca_valid) %>% rowwise() %>%
  mutate(has_particle = (lengths(str_split(ving, " ")) > 1),
         particle = if_else(has_particle, unlist(str_split(ving, " "))[2], "N/A"),
         particle = if_else(str_detect(particle, "(^a$|adj|born|cut|hands|lost|thrown|care|sick|sure|sex|their|told|water|well|wrong|ed$|en$)"), "N/A", particle),
         has_particle = !(particle == "N/A"))
both_dtab <- xtabs(~aspect + dialect, data=both); both_dtab
chisq.test(both_dtab)

both_hptab <- xtabs(~aspect + has_particle, data=both); both_hptab
chisq.test(both_hptab)

both_ptab <- xtabs(~aspect + particle, data=both %>% filter(has_particle)); both_ptab
chisq.test(both_ptab)

library(lme4)
library(emmeans)
library(MuMIn)
both_lm <- both %>% mutate(iterative = aspect == "iterative", british = dialect == "B",
                           second = subjp == "2nd", present = tense == "present")
both_model <- lmer(iterative ~ british + has_particle + second + present + 1|ving, data=both_lm)
summary(both_model)
r.squaredGLMM(both_model)
lm_means <- emmeans(both_model, specs=~1)
lm_r <- resid(both_model)
