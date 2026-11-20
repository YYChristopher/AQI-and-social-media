library(readxl)
library(openxlsx)
library(dplyr)
library(psych)
library(mediation)
library(lmtest)
library(fixest)
library(lfe)
library(ggplot2)
library(scales)
library(car)

#Work Space of the data
work_dir = 'F:/24561/Documents/ScienceResearch/WeiboData/Mediation'
#Set the Work Space of the data
setwd(work_dir)

############# Data & Preprocess ##############
exp.file <- read_excel('exp-data.xlsx')
used.name <- c('PM25', 'aqi_level', 'TPW', 'TNW', 'PWSO', 'PWOO', 'NWSO', 'NWOO', 'depression', 
               'helplessness', 'sadness', 'disappointed', 'upset', 'anger', 'annoyance', 'resentment',
               'irritable', 'hostile', 'pride', 'proud', 'active', 'peaceful', 'contented', 'warmth',
               'considerate', 'intimacy', 'attentive', 'gratitude', 'gender', 'age')
exp.org.data <- exp.file[used.name]
PWSO.word <- c('pride', 'peaceful', 'attentive', 'active', 'proud')
PWOO.word <- c('warmth', 'intimacy', 'gratitude', 'considerate', 'contented')
NWSO.word <- c('depression', 'helplessness', 'sadness', 'disappointed', 'upset')
NWOO.word <- c('anger', 'annoyance', 'resentment', 'irritable', 'hostile')
PWSO.data <- exp.org.data[ ,PWSO.word]
PWOO.data <- exp.org.data[ ,PWOO.word]
NWSO.data <- exp.org.data[ ,NWSO.word]
NWOO.data <- exp.org.data[ ,NWOO.word]

PWSO.col <- rowMeans(PWSO.data)
PWOO.col <- rowMeans(PWOO.data)
NWSO.col <- rowMeans(NWSO.data)
NWOO.col <- rowMeans(NWOO.data)
exp.data <- cbind(exp.org.data[c('PM25', 'aqi_level', 'TPW', 'TNW', 'PWSO', 'PWOO', 'NWSO', 'NWOO', 'gender', 'age')],
                  PWSO.col, PWOO.col, NWSO.col, NWOO.col)
# View(exp.data)
describe(exp.data)[c('min', 'max', 'mean', 'sd')]
rv.names <- c('TPW', 'TNW', 'PWSO', 'PWOO', 'NWSO', 'NWOO')
rv <- exp.data[, rv.names]
rv.emo.names <- c('PWSO.col', 'PWOO.col', 'NWSO.col', 'NWOO.col')
rv.emo <- exp.data[, rv.emo.names]
exp.data <- cbind(exp.org.data[c('PM25', 'aqi_level', 'gender', 'age')], rv, rv.emo)
names(exp.data) <- c("PM25", "aqi_level", "gender", "age", "TPW", "TNW", "PWSO", "PWOO", "NWSO", "NWOO", 
                     "PWSO.emo", "PWOO.emo", "NWSO.emo", "NWOO.emo")


# # Save Experimental Data
# wb <- createWorkbook()
# addWorksheet(wb, "Sheet1")
# writeData(wb, sheet = 1, x = exp.data)
# saveWorkbook(wb, "process-data.xlsx")

############# Linear Regression #################
lm.PWSO.aqi <- PWSO~PM25 + age + gender
lm.PWSO <- lm(lm.PWSO.aqi, data = exp.data)
summary(lm.PWSO)

lm.PWOO.aqi <- PWOO~PM25 + age + gender
lm.PWOO <- lm(lm.PWOO.aqi, data = exp.data)
summary(lm.PWOO)

lm.NWSO.aqi <- NWSO~PM25 + age + gender
lm.NWSO <- lm(lm.NWSO.aqi, data = exp.data)
summary(lm.NWSO)

lm.NWOO.aqi <- NWOO~PM25 + age + gender
lm.NWOO <- lm(lm.NWOO.aqi, data = exp.data)
summary(lm.NWOO)

###### Emotional
lm.PWSO.emo.aqi <- PWSO.emo~PM25 + age + gender
lm.PWSO.emo <- lm(lm.PWSO.emo.aqi, data = exp.data)
summary(lm.PWSO.emo)

lm.PWOO.emo.aqi <- PWOO.emo~PM25 + age + gender
lm.PWOO.emo <- lm(lm.PWOO.emo.aqi, data = exp.data)
summary(lm.PWOO.emo)

lm.NWSO.emo.aqi <- NWSO.emo~PM25 + age + gender
lm.NWSO.emo <- lm(lm.NWSO.emo.aqi, data = exp.data)
summary(lm.NWSO.emo)

lm.NWOO.emo.aqi <- NWOO.emo~PM25 + age + gender
lm.NWOO.emo <- lm(lm.NWOO.emo.aqi, data = exp.data)
summary(lm.NWOO.emo)

############# Mediation Analysis #################

### New Model
lm.PWSO.new <- PWSO~PM25 + PWSO.emo + age + gender
lm.PWSO.new <- lm(lm.PWSO.new, data = exp.data)
summary(lm.PWSO.new)

lm.PWOO.new <- PWOO~PM25 + PWOO.emo + age + gender
lm.PWOO.new <- lm(lm.PWOO.new, data = exp.data)
summary(lm.PWOO.new)

lm.NWSO.new <- NWSO~PM25 + NWSO.emo + age + gender
lm.NWSO.new <- lm(lm.NWSO.new, data = exp.data)
summary(lm.NWSO.new)

lm.NWOO.new <- NWOO~PM25 + NWOO.emo + age + gender
lm.NWOO.new <- lm(lm.NWOO.new, data = exp.data)
summary(lm.NWOO.new)

### Mediation Test
summary(lm.PWSO)
summary(lm.PWSO.emo)
summary(lm.PWSO.new)
med.PWSO.out <- mediate(model.m = lm.PWSO.emo, model.y = lm.PWSO.new, treat = "PM25", mediator = "PWSO.emo", 
                        boot = TRUE, sims = 5000)
summary(med.PWSO.out)
plot(med.PWSO.out)

summary(lm.PWOO)
summary(lm.PWOO.emo)
summary(lm.PWOO.new)
med.PWOO.out <- mediate(model.m = lm.PWOO.emo, model.y = lm.PWOO.new, treat = "PM25", mediator = "PWOO.emo", 
                        boot = TRUE, sims = 5000)
summary(med.PWOO.out)
plot(med.PWOO.out)

summary(lm.NWSO)
summary(lm.NWSO.emo)
summary(lm.NWSO.new)
med.NWSO.out <- mediate(model.m = lm.NWSO.emo, model.y = lm.NWSO.new, treat = "PM25", mediator = "NWSO.emo", 
                        boot = TRUE, sims = 5000)
summary(med.NWSO.out)
plot(med.NWSO.out)

summary(lm.NWOO)
summary(lm.NWOO.emo)
summary(lm.NWOO.new)
med.NWOO.out <- mediate(model.m = lm.NWOO.emo, model.y = lm.NWOO.new, treat = "PM25", mediator = "NWOO.emo", 
                        boot = TRUE, sims = 5000)
summary(med.NWOO.out)
plot(med.NWOO.out)



# ANOVA & Other Tests
# For Age
exp.data$PM25 <- factor(exp.data$PM25)
exp.data$age <- as.numeric(exp.data$age)
table(exp.data$PM25)

fit <- aov(age ~ PM25, data = exp.data)
summary(fit)
TukeyHSD(fit) # Mainly use Tukey Modification
leveneTest(age ~ PM25, data = exp.data)
kruskal.test(age ~ PM25, data = exp.data)

# par(mfrow = c(2,2))
# plot(fit)
# par(mfrow = c(1,1))

boxplot(age ~ PM25, data = exp.data, xlab = expression(paste(PM[2.5], ' group')), ylab = "Age")

prop_df <- exp.data %>%
  group_by(PM25) %>%
  summarise(
    prop_gender0 = mean(gender == 0, na.rm = TRUE),
    n = sum(!is.na(gender)),
    .groups = "drop"
  ) %>%
  mutate(
    PM25 = factor(PM25, levels = c(60, 120, 160, 220, 320))  # 保证顺序
  )

ggplot(prop_df, aes(x = PM25, y = prop_gender0)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(prop_gender0, accuracy = 0.1)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = expression(PM[2.5]), y = "Female proportion") +
  theme_minimal()

ggplot(prop_df, aes(x = PM25, y = prop_gender0)) +
  geom_col(
    width = 0.7,
    fill  = "grey85",     # 浅灰，接近箱线图
    color = "black"       # 柱子边框
  ) +
  # geom_text(aes(label = percent(prop_gender0, accuracy = 0.1)),
  #           vjust = -0.4, size = 4) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = expression(paste(PM[2.5], ' group')), y = "Proportion of males") +
  theme_classic() +
  theme(
    panel.border = element_rect(
      color = "black",
      fill  = NA
    ),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# For gender

exp.data$PM25 <- factor(exp.data$PM25)
exp.data$gender <- factor(exp.data$gender)
tab <- table(exp.data$PM25, exp.data$gender)
tab

chisq.test(tab)

pm_levels <- levels(exp.data$PM25)
pairs <- combn(pm_levels, 2, simplify = FALSE)

# pair_p <- sapply(pairs, function(p) {
#   sub <- exp.data[exp.data$PM25 %in% p, ]
#   chisq.test(table(sub$PM25, sub$gender))$p.value
# })
# 
# p.adjust(pair_p, method = "holm")

pair_test <- lapply(pairs, function(p) {
  sub <- exp.data[exp.data$PM25 %in% p, ]
  tab <- table(sub$PM25, sub$gender)
  
  # Expectation Count
  exp_min <- min(chisq.test(tab)$expected)
  
  # Rule: If the expected frequency is too small, use Fisher's exact test; otherwise, use the Chi-square test.
  if (exp_min < 5) {
    test <- fisher.test(tab)
    method <- "Fisher"
  } else {
    test <- chisq.test(tab, correct = FALSE)  # Do not apply Yates' correction.
    method <- "Chi-square"
  }
  
  data.frame(
    group1 = p[1],
    group2 = p[2],
    method = method,
    p_value = test$p.value,
    stringsAsFactors = FALSE
  )
})

res <- do.call(rbind, pair_test)

# Multiple comparison correction (Holm or BH methods are recommended)
res$p_holm <- p.adjust(res$p_value, method = "holm")
res$p_bh <- p.adjust(res$p_value, method = "BH")

res[order(res$p_value), ]
