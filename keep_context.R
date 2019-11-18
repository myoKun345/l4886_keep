library(tidyverse)

bnc <- read_table("data/keep_bnc_3k_context.txt")
coca <- read_table("data/keep_coca_3k_context.txt")

write_csv(bnc, "data/bnc_context.csv")
write_csv(coca, "data/coca_context.csv")

