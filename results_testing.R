library(tidyverse)
library(ggrepel)

load_and_validate <- function(df, dialect) {
  df %>% set_names(c("text", "tense", "aspect", "subjp", "ving")) %>%
    mutate(tense=str_to_lower(tense),
           aspect=str_to_lower(aspect),
           subjp=str_extract(str_to_lower(subjp), ".{3}"),
           ving=str_to_lower(ving),
           dialect=dialect) %>%
    filter(aspect %in% c("iterative", "continuative", "ambiguous")) %>% rowwise() %>%
    mutate(has_particle = (lengths(str_split(ving, " ")) > 1),
           particle = if_else(has_particle, unlist(str_split(ving, " "))[2], "N/A"),
           particle = if_else(str_detect(particle, "(^a$|adj|born|cut|hands|hurt|lost|thrown|care|sick|sure|sex|their|told|water|well|wrong|ed$|en$)"), "N/A", particle),
           has_particle = !(particle == "N/A")) %>% ungroup()
}

top_words_and_assoc <- function(df) {
  df %>% group_by(ving, aspect) %>% summarize(count=n()) %>%
    spread(aspect, count, fill=0) %>%
    mutate(total=ambiguous+continuative+iterative,
           amb_aso=(ambiguous - (iterative+continuative))/total,
           cont_aso=(continuative - (iterative+ambiguous))/total,
           iter_aso=(iterative - (continuative+ambiguous))/total) %>% ungroup() %>%
    mutate(amb_rank=dense_rank(desc(ambiguous)),
           cont_rank=dense_rank(desc(continuative)),
           iter_rank=dense_rank(desc(iterative)))
}

top_particles_and_assoc <- function(df) {
  df %>% filter(has_particle) %>% group_by(particle, aspect) %>% summarize(count=n()) %>%
    spread(aspect, count, fill=0) %>%
    mutate(total=ambiguous+continuative+iterative,
           amb_aso=(ambiguous - (iterative+continuative))/total,
           cont_aso=(continuative - (iterative+ambiguous))/total,
           iter_aso=(iterative - (continuative+ambiguous))/total) %>% ungroup() %>%
    mutate(amb_rank=dense_rank(desc(ambiguous)),
           cont_rank=dense_rank(desc(continuative)),
           iter_rank=dense_rank(desc(iterative)))
}

plot_cont_rank_correlation <- function(df) {
  ggplot(df, aes(x=cont_rank, y=iter_rank, color=cont_aso)) + geom_point() +
    geom_text_repel(aes(label=ving), hjust=0.5, vjust=1) + geom_abline(slope = 1, intercept = 0) +
    scale_x_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) + scale_y_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, limits=c(-1,1), name="Associated with Continuative") +
    xlab("Rank (Continuative Attestations)") + ylab("Rank (Iterative Attestations)")
}

plot_iter_rank_correlation <- function(df) {
  ggplot(df, aes(x=iter_rank, y=cont_rank, color=iter_aso)) + geom_point() +
    geom_text_repel(aes(label=ving), hjust=0.5, vjust=1) + geom_abline(slope = 1, intercept = 0) +
    scale_x_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) + scale_y_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, limits=c(-1,1), name="Associated with Iterative") +
    xlab("Rank (Iterative Attestations)") + ylab("Rank (Continuative Attestations)")
}

plot_cont_rank_correlation_prt <- function(df) {
  ggplot(df, aes(x=cont_rank, y=iter_rank, color=cont_aso)) + geom_point() +
    geom_text_repel(aes(label=particle), hjust=0.5, vjust=1) + geom_abline(slope = 1, intercept = 0) +
    scale_x_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) + scale_y_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, limits=c(-1,1), name="Associated with Continuative") +
    xlab("Rank (Continuative Attestations)") + ylab("Rank (Iterative Attestations)")
}

plot_iter_rank_correlation_prt <- function(df) {
  ggplot(df, aes(x=iter_rank, y=cont_rank, color=iter_aso)) + geom_point() +
    geom_text_repel(aes(label=particle), hjust=0.5, vjust=1) + geom_abline(slope = 1, intercept = 0) +
    scale_x_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) + scale_y_reverse(minor_breaks=NULL, breaks=seq(1,50,2)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, limits=c(-1,1), name="Associated with Iterative") +
    xlab("Rank (Iterative Attestations)") + ylab("Rank (Continuative Attestations)")
}

bnc_valid <- read_csv("data/bnc3k_finished.csv") %>% load_and_validate("B")

bnc_ttab <- xtabs(~aspect + tense, data=bnc_valid); bnc_ttab
chisq.test(bnc_ttab)

bnc_stab <- xtabs(~aspect + subjp, data=bnc_valid); bnc_stab
chisq.test(bnc_stab)

bnc_wtab <- xtabs(~aspect + ving, data=bnc_valid); bnc_wtab
chisq.test(bnc_wtab)

bnc_hptab <- xtabs(~aspect + has_particle, data=bnc_valid); bnc_hptab
chisq.test(bnc_hptab)

bnc_ptab <- xtabs(~aspect + particle, data=bnc_valid %>% filter(has_particle)); bnc_ptab
chisq.test(bnc_ptab)

bnc_words <- bnc_valid %>% top_words_and_assoc()
hapaxes <- tibble("corpus", "c", "i", "a", .rows=2) %>% setNames(c("corpus", "continuative", "iterative", "ambiguous"))
hapaxes[1,1] <- "BNC"
hapaxes[1,2] <- nrow(bnc_words %>% filter(continuative == 1 & total == 1))
hapaxes[1,3] <- nrow(bnc_words %>% filter(iterative == 1 & total == 1))
hapaxes[1,4] <- nrow(bnc_words %>% filter(ambiguous == 1 & total == 1))

plot_cont_rank_correlation(bnc_words %>% top_n(20, continuative))
plot_iter_rank_correlation(bnc_words %>% top_n(20, iterative))

bnc_prts <- bnc_valid %>% top_particles_and_assoc()

coca_valid <- read_csv("data/coca3k_finished.csv") %>% load_and_validate("A")

coca_ttab <- xtabs(~aspect + tense, data=coca_valid); coca_ttab
chisq.test(coca_ttab)

coca_stab <- xtabs(~aspect + subjp, data=coca_valid); coca_stab
chisq.test(coca_stab)

coca_wtab <- xtabs(~aspect + ving, data=coca_valid); coca_wtab
chisq.test(coca_wtab)

coca_hptab <- xtabs(~aspect + has_particle, data=coca_valid); coca_hptab
chisq.test(coca_hptab)

coca_ptab <- xtabs(~aspect + particle, data=coca_valid %>% filter(has_particle)); coca_ptab
chisq.test(coca_ptab)

coca_words <- coca_valid %>% top_words_and_assoc()
hapaxes[2,1] <- "COCA"
hapaxes[2,2] <- nrow(coca_words %>% filter(continuative == 1 & total == 1))
hapaxes[2,3] <- nrow(coca_words %>% filter(iterative == 1 & total == 1))
hapaxes[2,4] <- nrow(coca_words %>% filter(ambiguous == 1 & total == 1))

plot_cont_rank_correlation(coca_words %>% top_n(20, continuative))
plot_iter_rank_correlation(coca_words %>% top_n(20, iterative))

coca_prts <- coca_valid %>% top_particles_and_assoc()

both <- rbind(bnc_valid, coca_valid)
both_dtab <- xtabs(~aspect + dialect, data=both); both_dtab
chisq.test(both_dtab)

both_ttab <- xtabs(~aspect + tense, data=both); both_ttab
chisq.test(both_ttab)

both_stab <- xtabs(~aspect + subjp, data=both); both_stab
chisq.test(both_stab)

both_wtab <- xtabs(~aspect + ving, data=both); both_wtab
chisq.test(both_wtab)

both_hptab <- xtabs(~aspect + has_particle, data=both); both_hptab
chisq.test(both_hptab)

both_ptab <- xtabs(~aspect + particle, data=both %>% filter(has_particle)); both_ptab
chisq.test(both_ptab)

both_words <- both %>% top_words_and_assoc()

plot_cont_rank_correlation(both_words %>% top_n(20, continuative))
plot_iter_rank_correlation(both_words %>% top_n(20, iterative))

both_prts <- both %>% top_particles_and_assoc()

all_top20_cont <- rbind(bnc_words %>% top_n(20, continuative) %>% mutate(corpus="BNC"),
                        coca_words %>% top_n(20, continuative) %>% mutate(corpus="COCA"),
                        both_words %>% top_n(20, continuative) %>% mutate(corpus="Both"))
plot_cont_rank_correlation(all_top20_cont) + facet_wrap(~corpus, scales="free")

all_top20_iter <- rbind(bnc_words %>% top_n(20, iterative) %>% mutate(corpus="BNC"),
                        coca_words %>% top_n(20, iterative) %>% mutate(corpus="COCA"),
                        both_words %>% top_n(20, iterative) %>% mutate(corpus="Both"))
plot_iter_rank_correlation(all_top20_iter) + facet_wrap(~corpus, scales="free")

prt_top20_cont <- rbind(bnc_prts %>% top_n(20, continuative) %>% mutate(corpus="BNC"),
                        coca_prts %>% top_n(20, continuative) %>% mutate(corpus="COCA"),
                        both_prts %>% top_n(20, continuative) %>% mutate(corpus="Both"))
plot_cont_rank_correlation_prt(prt_top20_cont) + facet_wrap(~corpus, scales="free")

prt_top20_iter <- rbind(bnc_prts %>% top_n(20, iterative) %>% mutate(corpus="BNC"),
                        coca_prts %>% top_n(20, iterative) %>% mutate(corpus="COCA"),
                        both_prts %>% top_n(20, iterative) %>% mutate(corpus="Both"))
plot_iter_rank_correlation_prt(prt_top20_iter) + facet_wrap(~corpus, scales="free")

library(lme4)
library(emmeans)
library(MuMIn)
library(broom)
both_lm <- both %>% mutate(aspect = set_names(c(0, 0.5, 1),
                                              c("continuative", "ambiguous", "iterative"))[aspect],
                           dialect = as.numeric(dialect == "B"),
                           subjp = set_names(c(0, 1, 2, 3),
                                             c("N/A", "1st", "2nd", "3rd"))[subjp],
                           tense = set_names(c(-1, 0, 1),
                                             c("past", "present", "future"))[tense])
both_model <- lmer(aspect ~ dialect + has_particle + subjp + tense + 1|ving, data=both_lm)
summary(both_model)
r.squaredGLMM(both_model)
lm_means <- emmeans(both_model, specs=~1)
lmer_aug <- augment(both_model)

ggplot(lmer_aug, aes(x=.resid)) + geom_histogram()
