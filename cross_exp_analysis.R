library(tidyverse)
library(lmerTest)
library(emmeans)

data1a <- read_csv("exp1a_data.csv")
data1a$exp <- "1a"

data1b <- read_csv("exp1b_data.csv")
data1b$exp <- "1b"

data2a <- read_csv("exp2a_data.csv")
data2a$exp <- "2a"

data2b <- read_csv("exp2b_data.csv")
data2b$exp <- "2b"

data <- rbind(
  data1a,
  data1b,
  data2a,
  data2b
)

data <- data %>%
  mutate(
    condition = factor(condition, 
                       levels = c("Control", "Higher-Level", "Lower-Level")),
    word = factor(word),
    id = factor(id)
  )

# simple coding for condition
c3 <- contr.treatment(3)
coding3 <- matrix(rep(1/3, 6), ncol = 2)
simple3 <- c3-coding3
contrasts(data$condition) <- simple3

# compare exp1a and 1b: homophone responeses
data_exp1 <- data %>%
  filter(exp %in% c("1a", "1b")) %>%
  mutate(exp = factor(exp, levels = c("1a", "1b")))

c2 <- contr.treatment(2)
coding2 <- matrix(rep(1/2, 2), ncol = 1)
simple2 <- c2 - coding2

contrasts(data_exp1$exp) <- simple2

homo_1 <- glmer(
  homo ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data_exp1,
  family = binomial
)

summary(homo_1)

emm_homo_1 <- emmeans(
  homo_1,
  ~ exp | condition
)

pairs(emm_homo_1, adjust = "tukey")

# compare exp1a and 1b: superordinate responeses
super_1 <- glmer(
  general_cat ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data_exp1,
  family = binomial
)

summary(super_1)

emm_super_1 <- emmeans(
  super_1,
  ~ exp | condition
)

pairs(emm_super_1, adjust = "tukey")

# compare exp2a and 2b: homophone responses
data_exp2 <- data %>%
  filter(exp %in% c("2a", "2b")) %>%
  mutate(exp = factor(exp, levels = c("2a", "2b")))

contrasts(data_exp2$exp) <- simple2

homo_2 <- glmer(
  homo ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data_exp2,
  family = binomial
)

summary(homo_2)

emm_homo_2 <- emmeans(
  homo_2,
  ~ exp | condition
)

pairs(emm_homo_2, adjust = "tukey")

# compare exp2a and 2b: superordinate responses
super_2 <- glmer(
  general_cat ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data_exp2,
  family = binomial
)

summary(super_2)

emm_super_2 <- emmeans(
  super_2,
  ~ exp | condition
)

pairs(emm_super_2, adjust = "tukey")

# compare all experiments: homophone responses
c4 <- contr.treatment(4)
coding4 <- matrix(rep(1/4, 12), ncol = 3)
simple4 <- c4 - coding4
data$exp <- factor(data$exp, levels = c("1a", "1b", "2a", "2b"))
contrasts(data$exp) <- simple4

homo_cross <- glmer(
  homo ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

summary(homo_cross)

emm_homo_cross <- emmeans(
  homo_cross,
  ~ exp | condition
)

pairs(emm_homo_cross, adjust = "tukey")

# compare all experiments: superordinate responses
super_cross <- glmer(
  general_cat ~ condition * exp +
    (1 | word) +
    (1 | id),
  data = data,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

summary(super_cross)

emm_super_cross <- emmeans(
  super_cross,
  ~ exp | condition
)

pairs(emm_super_cross, adjust = "tukey")
