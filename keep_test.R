library(tidyverse)

corpus = "bnc"

keep_all <- read_table2(paste("data/keep_", corpus, "_all.txt", sep="")) %>% select(-c(TRASH, TRASH2))
all_summary <- keep_all %>% group_by(VING) %>% summarize(freq=n()/nrow(keep_all))

keep_3k <- read_table2(paste("data/keep_", corpus, "_3k_test.txt", sep="")) %>% select(-c(TRASH, TRASH2))
samp_summary <- keep_3k %>% group_by(VING) %>% summarize(sfreq=n()/nrow(keep_3k))

keep_test <- all_summary %>% left_join(samp_summary, "VING") %>% filter(!is.na(sfreq)) %>% top_n(100, freq)

wilcox.test(keep_test$freq, keep_test$sfreq, paired=T)

keep_hundo <- read_table2(paste("data/keep_", corpus, "_100_test.txt", sep="")) %>% select(-c(TRASH, TRASH2))
hundo_summary <- keep_hundo %>% group_by(VING) %>% summarize(sfreq=n()/nrow(keep_hundo))

keep_test <- all_summary %>% left_join(hundo_summary, "VING") %>% filter(!is.na(sfreq)) %>% top_n(20, freq)

wilcox.test(keep_test$freq, keep_test$sfreq, paired=T)
