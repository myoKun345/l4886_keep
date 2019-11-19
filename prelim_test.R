library(tidyverse)

bnc <- read_csv("data/bnc50.csv") %>% mutate(
  CompoundAA=if_else(str_detect(CompoundAA, "uative/iterat"), "A",
                     if_else(str_detect(CompoundAA, "terative"), "I", "C")),
  Nation="B"
)
coca <- read_csv("data/coca50.csv") %>% mutate(
  CompoundAA=if_else(str_detect(CompoundAA, "uative/iterat"), "A",
                     if_else(str_detect(CompoundAA, "terative"), "I", "C")),
  Nation="A"
)

both <- rbind(bnc, coca)

cont <- xtabs(~ CompoundAA + Nation, data = both)
chisq.test(cont)

summary(as.factor(bnc$CompoundAA))
summary(as.factor(coca$CompoundAA))
