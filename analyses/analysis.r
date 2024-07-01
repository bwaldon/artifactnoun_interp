setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# for centering predictors in models with interaction terms 
source("helpers.R")
# for data preprocessing and visualization
library(tidyverse)
library(readxl)
# for computing confidence intervals
library(Hmisc)
# for frequentist modeling
library(lme4)
# for bayesian modeling 
library(brms)
# for colorblind-friendly plot themes
library(viridis)

# 1. import data

# # to analyze experiment 2 data, change variable value to "experiment2"
study <- "experiment1"

d <- read_excel(sprintf("../data/%s.xlsx", study), col_names = TRUE) %>%
  filter(Finished == "1.0") %>%
  drop_na(Question)

# 2. exclusions 

# # who failed the attention check? 
fail_attn <- (d %>% filter(!(`Check 1: Color` == "6.0")))$ResponseId

# # who failed the comprehension check? 
fail_comp <- (d %>% filter(!(`Comprehension check` == "1.0")))$ResponseId

# # who failed the transitivity problem? 
fail_trans <- (d %>% filter(!(`Check 2: Transit` == "3.0")))$ResponseId

# # who self-reports non-US? 
non_US <- (d %>% filter(!(Country == "2.0")))$ResponseId

# # who self-reports non-native English? 
non_english <- (d %>% filter((Fluency == "3.0")))$ResponseId

# # not excluding non_english, because it wasn't a preregistered exclusion criterion.
excluded_participants <- unique(c(fail_attn,fail_comp,fail_trans,non_US))
percent_excluded = length(excluded_participants) / length(d$ResponseId)

d_withExclusions <- d %>%
  filter(!(ResponseId %in% excluded_participants))

# 2.5 demographic summaries for "firearm" participants

table((d_withExclusions %>% filter(Noun == "firearm"))$gender)
table(is.na((d_withExclusions %>% filter(Noun == "firearm"))$gender))

table((d_withExclusions %>% filter(Noun == "firearm"))$`Party...38`)
table(is.na((d_withExclusions %>% filter(Noun == "firearm"))$`Party...38`))

# 3. data summaries

if (study == "experiment2") {
  d_withExclusions <- d_withExclusions %>%
    mutate(Context = recode(Context, "actual" = "restricted",
                            "functional" = "potential",
                            "intentional" = "design"))
}

d_withExclusions_responseProps <- d_withExclusions %>%
  group_by(Noun,Domain,Context) %>%
  summarise(nYes = sum(Question == "1.0"),
            nTotal = n(),
            binom_conf = binconf(nYes, nTotal, return.df = TRUE)) %>%
  ungroup() %>%
  unnest(binom_conf) %>%
  mutate(Context = relevel(factor(Context), ref = "restricted")) %>%
  mutate(Noun = relevel(factor(Noun), ref = "firearm"))

write_csv(d_withExclusions_responseProps,
          file = sprintf("%s_results_byContextDomainNoun.csv",study))

ggplot(d_withExclusions_responseProps, aes(x = Context, y = PointEst, fill = Domain)) +
  facet_wrap(~Noun) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                size = 0.5,
                width = 0.2,
                position = position_dodge(0.9)) +
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") +
  ylab("Proportion of 'yes' response") +
  xlab("Definition") +
  scale_fill_viridis(discrete = TRUE, begin = 0.5) 

ggsave(sprintf("../viz/%s/response_proportions.pdf", study), width = 4, height = 3, units = "in")
ggsave(sprintf("../viz/%s/response_proportions.png", study), width = 4, height = 3, units = "in")

d_withExclusions_responseProps_byContextandNoun <- d_withExclusions %>%
  group_by(Context,Noun) %>%
  summarise(nYes = sum(Question == "1.0"),
            nTotal = n(),
            binom_conf = binconf(nYes, nTotal, return.df = TRUE)) %>%
  ungroup() %>%
  unnest(binom_conf) %>%
  mutate(Context = relevel(factor(Context), ref = "restricted")) %>%
  mutate(Noun = relevel(factor(Noun), ref = "firearm"))

ggplot(d_withExclusions_responseProps_byContextandNoun, aes(x = Context, y = PointEst, fill = Context)) +
  facet_wrap(~Noun) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                size = 0.5,
                width = 0.2,
                position = position_dodge(0.9)) +
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none") +
  ylab("Proportion of 'yes' response") +
  xlab("Definition") +
  scale_fill_viridis(discrete = TRUE, begin = 0.5) 

write_csv(d_withExclusions_responseProps_byContextandNoun,
           file = sprintf("%s_results_byContextNoun.csv",study))

ggsave(sprintf("../viz/%s/response_proportions_byContextandNoun.pdf", study), width = 4, height = 3, units = "in")
ggsave(sprintf("../viz/%s/response_proportions_byContextandNoun.png", study), width = 4, height = 3, units = "in")

d_withExclusions_responseProps_byNoun <- d_withExclusions %>%
  group_by(Noun) %>%
  summarise(nYes = sum(Question == "1.0"),
            nTotal = n(),
            binom_conf = binconf(nYes, nTotal, return.df = TRUE)) %>%
  ungroup() %>%
  unnest(binom_conf) %>%
  mutate(Noun = relevel(factor(Noun), ref = "firearm")) 

ggplot(d_withExclusions_responseProps_byNoun, aes(x = Noun, y = PointEst)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                size = 0.5,
                width = 0.2,
                position = position_dodge(0.9)) +
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") +
  ylab("Proportion of 'yes' response") +
  xlab("Definition") +
  scale_fill_viridis(discrete = TRUE, begin = 0.5) 

write_csv(d_withExclusions_responseProps_byNoun,
          file = sprintf("%s_results_byNoun.csv",study))

ggsave(sprintf("../viz/%s/response_proportions_byNoun.pdf", study), width = 4, height = 3, units = "in")
ggsave(sprintf("../viz/%s/response_proportions_byNoun.png", study), width = 4, height = 3, units = "in")

# 6. logistic regression analyses

d_withExclusions_regressionTransform <- d_withExclusions %>%
  mutate(Question = relevel(as.factor(as.numeric(Question)), ref = 2)) %>%
  mutate(Context = relevel(as.factor(Context), ref = "restricted")) %>%
  mutate(Noun = relevel(as.factor(Noun), ref = "firearm")) %>%
  mutate(Domain = as.factor(Domain)) 

d_withExclusions_regressionTransform_center <- d_withExclusions_regressionTransform %>%
  mutate(Context = myCenter(Context)) %>%
  mutate(Domain = myCenter(Domain)) 

model_lme4 <- glm(Question ~ Context*Domain*Noun, 
                  data = d_withExclusions_regressionTransform_center,
                    family = "binomial")

model_lme4_summary <- summary(model_lme4)
model_lme4_summary

model_lme4_final <- glm(Question ~ Context+Domain+Noun, data = d_withExclusions_regressionTransform_center,
                        family = "binomial")

model_lme4_final_summary <- summary(model_lme4_final)
model_lme4_final_summary

# # auxiliary analyses

# # # prune 3-way interactions

model_lme4_prune3way <- glm(Question ~ Context+Domain+Noun+Context:Domain+Context:Noun+Domain:Noun, data = d_withExclusions_regressionTransform_center,
                          family = "binomial")

summary(model_lme4_prune3way)

# # # additionally prune domain/noun 2-way interaction

model_lme4_pruneDomainNoun <- glm(Question ~ Context+Domain+Noun+Context:Domain, data = d_withExclusions_regressionTransform_center,
                           family = "binomial")

summary(model_lme4_pruneDomainNoun)

# # # post-hoc random effects model

model_ranef <- glmer(Question ~ Context+Domain + (1 + Context|Noun), data = d_withExclusions_regressionTransform_center,
                     family = "binomial")
summary(model_ranef)

# # # bayesian regression

model_brm_final <- brm(Question ~ Context+Domain+Noun, data = d_withExclusions_regressionTransform_center,
                 family = bernoulli(link = "logit"))

model_brm_final_summary <- summary(model_brm_final)
model_brm_final_summary