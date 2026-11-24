library(tidyverse)
library(lmerTest)
library(emmeans)

# read in data
data <- read_csv("exp2a_data.csv")

# sample size
data %>% 
  group_by(condition) %>%
  summarise(n = n_distinct(id))

# summary
data_summary <- data  %>% 
  group_by(condition) %>%
  summarise(homophone = mean(homo), superordinate = mean(general_cat), other = mean(others)) %>%
  pivot_longer(homophone:other, names_to = "Meaning", values_to = "percent") %>%
  ungroup()

#visualization
data_summary$Meaning <- factor(data_summary$Meaning, levels = c("homophone", "superordinate", "other"))

data_summary %>%
  ggplot(aes(fill = Meaning, y = percent, x = condition)) + 
  geom_bar(position="fill", stat = "identity") +
  theme_bw() +
  ylab("") +
  xlab("Condition") +
  ggtitle("Experiment 2a") +
  ylab("Proportion of Responses")

# make factors
data <- data %>%
  mutate(
    condition = factor(condition, 
                       levels = c("Control", "Higher-Level", "Lower-Level")),
    word = factor(word),
    id = factor(id)
  )

# simple coding
c3 <- contr.treatment(3)
coding3 <- matrix(rep(1/3, 6), ncol = 2)
simple3 <- c3-coding3
contrasts(data$condition) <- simple3

# modeling: homophone responses
library(blme)

homophone_condition_pen <- bglmer(
  homo ~ condition + (1 | word) + (1 | id),
  data = data,
  family = binomial,
  fixef.prior = normal(sd = 2.5),
  cov.prior = NULL
)

summary(homophone_condition_pen)

emm_homophone = emmeans(homophone_condition_pen, specs =  pairwise ~ condition, type = "response")
emm_homophone

model_sd1.5 <- update(
  homophone_condition_pen,
  fixef.prior = normal(sd = 1.5)
)

summary(model_sd1.5)

emmeans(model_sd1.5, specs =  pairwise ~ condition, type = "response")

model_sd5 <- update(
  homophone_condition_pen,
  fixef.prior = normal(sd = 5)
)

summary(model_sd5)

emmeans(model_sd5, specs =  pairwise ~ condition, type = "response")

# compare to chance level
homo_chance <- data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(homo)),
    homo_n = sum(homo, na.rm = TRUE),
    homo_prop = mean(homo, na.rm = TRUE),
    p_value = binom.test(
      homo_n,
      n,
      p = 1/64,
      alternative = "two.sided"
    )$p.value
  )

homo_chance

# modeling: superordinate responses
super_null <- glmer(general_cat ~ 1 + (1|word) + (1|id),
                    data = data, 
                    family="binomial") 

super_condition <- update(super_null, .~. + condition)

anova(super_null, super_condition)

summary(super_condition)

emm_superordinate = emmeans(super_condition, specs =  pairwise ~ condition, type = "response")
emm_superordinate

# compare to chance level
general_chance <- data %>%
  group_by(condition) %>%
  summarise(
    n = sum(!is.na(general_cat)),
    general_n = sum(general_cat, na.rm = TRUE),
    general_prop = mean(general_cat, na.rm = TRUE),
    p_value = binom.test(
      general_n,
      n,
      p = 1/64,
      alternative = "two.sided"
    )$p.value
  )

general_chance
