library(readxl)
library(dplyr)
library(psych)
library(mediation)
library(lmtest)
library(lfe)
library(ggplot2)

#Work Space of the data
work_dir = 'F:/24561/Documents/ResearchWrite/Weibo&Weather/code in Github/Mediation'
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

############# Linear Regression #################
lm.PWSO.aqi <- PWSO~PM25
lm.PWSO <- lm(lm.PWSO.aqi, data = exp.data)
summary(lm.PWSO)

lm.PWOO.aqi <- PWOO~PM25
lm.PWOO <- lm(lm.PWOO.aqi, data = exp.data)
summary(lm.PWOO)

lm.NWSO.aqi <- NWSO~PM25
lm.NWSO <- lm(lm.NWSO.aqi, data = exp.data)
summary(lm.NWSO)

lm.NWOO.aqi <- NWOO~PM25
lm.NWOO <- lm(lm.NWOO.aqi, data = exp.data)
summary(lm.NWOO)

###### Emotional
lm.PWSO.emo.aqi <- PWSO.emo~PM25
lm.PWSO.emo <- lm(lm.PWSO.emo.aqi, data = exp.data)
summary(lm.PWSO.emo)

lm.PWOO.emo.aqi <- PWOO.emo~PM25
lm.PWOO.emo <- lm(lm.PWOO.emo.aqi, data = exp.data)
summary(lm.PWOO.emo)

lm.NWSO.emo.aqi <- NWSO.emo~PM25
lm.NWSO.emo <- lm(lm.NWSO.emo.aqi, data = exp.data)
summary(lm.NWSO.emo)

lm.NWOO.emo.aqi <- NWOO.emo~PM25
lm.NWOO.emo <- lm(lm.NWOO.emo.aqi, data = exp.data)
summary(lm.NWOO.emo)

############# Mediation Analysis #################

### New Model
lm.PWSO.new <- PWSO~PM25 + PWSO.emo
lm.PWSO.new <- lm(lm.PWSO.new, data = exp.data)
summary(lm.PWSO.new)

lm.PWOO.new <- PWOO~PM25 + PWOO.emo
lm.PWOO.new <- lm(lm.PWOO.new, data = exp.data)
summary(lm.PWOO.new)

lm.NWSO.new <- NWSO~PM25 + NWSO.emo
lm.NWSO.new <- lm(lm.NWSO.new, data = exp.data)
summary(lm.NWSO.new)

lm.NWOO.new <- NWOO~PM25 + NWOO.emo
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


#### Boxplot of different AQI level
# Prepare Data
exp.data.1 <- exp.data[which(exp.data[, 'PM25'] == 60), ]
exp.data.2 <- exp.data[which(exp.data[, 'PM25'] == 120), ]
exp.data.3 <- exp.data[which(exp.data[, 'PM25'] == 160), ]
exp.data.4 <- exp.data[which(exp.data[, 'PM25'] == 220), ]
exp.data.5 <- exp.data[which(exp.data[, 'PM25'] == 320), ]

## PWSO
PWSO.df <- data.frame(
  value = c(exp.data.1[,'PWSO'], exp.data.2[,'PWSO'], exp.data.3[,'PWSO'], exp.data.4[,'PWSO'], 
            exp.data.5[,'PWSO']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
PWSO.df <- PWSO.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

PWSO.plot <- PWSO.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 5.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("Levels"))) +
  ylab("Positive Words Self-Oriented")
# show(PWSO.plot)
# ggsave("./Boxplot/PWSO.png", plot = PWSO.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## PWOO
PWOO.df <- data.frame(
  value = c(exp.data.1[,'PWOO'], exp.data.2[,'PWOO'], exp.data.3[,'PWOO'], exp.data.4[,'PWOO'], 
            exp.data.5[,'PWOO']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
PWOO.df <- PWOO.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

PWOO.plot <- PWOO.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Positive Words Other-Oriented")
# show(PWOO.plot)
# ggsave("./Boxplot/PWOO.png", plot = PWOO.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## NWSO
NWSO.df <- data.frame(
  value = c(exp.data.1[,'NWSO'], exp.data.2[,'NWSO'], exp.data.3[,'NWSO'], exp.data.4[,'NWSO'], 
            exp.data.5[,'NWSO']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
NWSO.df <- NWSO.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

NWSO.plot <- NWSO.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 5.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Words Self-Oriented")
# show(NWSO.plot)
# ggsave("./Boxplot/NWSO.png", plot = NWSO.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## NWOO
NWOO.df <- data.frame(
  value = c(exp.data.1[,'NWOO'], exp.data.2[,'NWOO'], exp.data.3[,'NWOO'], exp.data.4[,'NWOO'], 
            exp.data.5[,'NWOO']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
NWOO.df <- NWOO.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

NWOO.plot <- NWOO.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Words Other-Oriented")
# show(NWOO.plot)
# ggsave("./Boxplot/NWOO.png", plot = NWOO.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## Emotional
## PWSO
PWSO.emo.df <- data.frame(
  value = c(exp.data.1[,'PWSO.emo'], exp.data.2[,'PWSO.emo'], exp.data.3[,'PWSO.emo'], 
            exp.data.4[,'PWSO.emo'], exp.data.5[,'PWSO.emo']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
PWSO.emo.df <- PWSO.emo.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

PWSO.emo.plot <- PWSO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(1, 5.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Positive Emotion Self-Oriented")
# show(PWSO.emo.plot)
# ggsave("./Boxplot/PWSO_emo.png", plot = PWSO.emo.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## PWOO
PWOO.emo.df <- data.frame(
  value = c(exp.data.1[,'PWOO.emo'], exp.data.2[,'PWOO.emo'], exp.data.3[,'PWOO.emo'], 
            exp.data.4[,'PWOO.emo'], exp.data.5[,'PWOO.emo']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
PWOO.emo.df <- PWOO.emo.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

PWOO.emo.plot <- PWOO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Positive Emotion Other-Oriented")
# show(PWOO.emo.plot)
# ggsave("./Boxplot/PWOO_emo.png", plot = PWOO.emo.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## NWSO
NWSO.emo.df <- data.frame(
  value = c(exp.data.1[,'NWSO.emo'], exp.data.2[,'NWSO.emo'], exp.data.3[,'NWSO.emo'], 
            exp.data.4[,'NWSO.emo'], exp.data.5[,'NWSO.emo']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
NWSO.emo.df <- NWSO.emo.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

NWSO.emo.plot <- NWSO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  coord_cartesian(ylim = c(1, 5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Emotion Self-Oriented")
# show(NWSO.emo.plot)
# ggsave("./Boxplot/NWSO_emo.png", plot = NWSO.emo.plot, width = 8, height = 4.5, units = "in", dpi = 600)

## NWOO
NWOO.emo.df <- data.frame(
  value = c(exp.data.1[,'NWOO.emo'], exp.data.2[,'NWOO.emo'], exp.data.3[,'NWOO.emo'], 
            exp.data.4[,'NWOO.emo'], exp.data.5[,'NWOO.emo']),
  level = c(rep('60', nrow(exp.data.1)), rep('120', nrow(exp.data.2)), rep('160', nrow(exp.data.3)), 
            rep('220', nrow(exp.data.4)), rep('320', nrow(exp.data.5)))
)
NWOO.emo.df <- NWOO.emo.df %>%
  mutate(level = factor(level, levels = c("60", "120", "160", "220", "320")))

NWOO.emo.plot <- NWOO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  stat_boxplot(geom = "errorbar", width = 0.35) +  
  geom_boxplot() + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_boxplot(fill = "grey", color = "black") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Emotion Other-Oriented")
# show(NWOO.emo.plot)
# ggsave("./Boxplot/NWOO_emo.png", plot = NWOO.emo.plot, width = 8, height = 4.5, units = "in", dpi = 600)

#### violin plot
PWSO.emo.violin <- PWSO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_violin(fill = "grey", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.25, fill = "white") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Positive Emotion Self-Oriented")
ggsave("./Boxplot/PWSO_violin.png", plot = PWSO.emo.violin, width = 8, height = 4.5, units = "in", dpi = 600)

PWOO.emo.violin <- PWOO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_violin(fill = "grey", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.25, fill = "white") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Positive Emotion Other-Oriented")
ggsave("./Boxplot/PWOO_violin.png", plot = PWOO.emo.violin, width = 8, height = 4.5, units = "in", dpi = 600)

NWSO.emo.violin <- NWSO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_violin(fill = "grey", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.25, fill = "white") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Emotion Self-Oriented")
ggsave("./Boxplot/NWSO_violin.png", plot = NWSO.emo.violin, width = 8, height = 4.5, units = "in", dpi = 600)

NWOO.emo.violin <- NWOO.emo.df %>%  
  ggplot(aes(x = level, y = value, fill = level)) + 
  # coord_cartesian(ylim = c(0, 7.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  geom_violin(fill = "grey", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.25, fill = "white") +
  xlab(expression(paste("AQI Levels"))) +
  ylab("Negative Emotion Other-Oriented")
ggsave("./Boxplot/NWOO_violin.png", plot = NWOO.emo.violin, width = 8, height = 4.5, units = "in", dpi = 600)
