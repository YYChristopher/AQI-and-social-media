library(readxl)
library(dplyr)
library(sandwich)
library(lmtest)
library(fixest)
library(plm)
library(lfe)
library(broom)

#Work Space of the data
work_dir = 'F:/24561/Documents/ResearchWrite/Weibo&Weather/code in Github/Code'
#Set the Work Space of the data
setwd(work_dir)

############# Data ##############
wewe.file <- read_excel('panel.xlsx')
wewe <- data.frame(wewe.file)
wewe <- subset(wewe, select = -c(posts_with_hatewords, total_hatewords, hatewords_other_oriented, 
                                 hatewords_self_oriented, arousal_score_mean))
############# hist ##############
hist(log(wewe$AQI+1), breaks = 20, main = "hist of log", xlab = "log AQI value", 
     ylab = "frequency", col = "skyblue", border = "black")
############# QQplot ##############
# qqnorm(wewe$AQI, main = "QQ plot")
# qqline(wewe$AQI, col = "red", lwd = 2)  
# qqnorm(log(wewe$AQI+1), main = "QQ plot")
# qqline(log(wewe$AQI+1), col = "red", lwd = 2) 

############# Variables ###############
postnum <- wewe[, "total_posts"]
rv.names <- c("posts_with_positive_words", "total_positive_words", "posts_with_negative_words", 
              "total_negative_words", "positive_words_self_oriented", "positive_words_other_oriented",
              "negative_words_self_oriented", "negative_words_other_oriented")
rv <- wewe[, rv.names] / postnum * 100
Holidays <- wewe[ ,'Holidays']

names(rv) <- c('PWPW', 'TPW', 'PWNW', 'TNW', 'PWSO', 'PWOO', 'NWSO', 'NWOO')
AQI <- wewe$AQI
D.names <- c("temp_max", "temp_min", "day_rain", "night_rain", "cloud_day", "cloud_night",
             "day_wind_speed", "night_wind_speed")
D <- wewe[, D.names] # control variables
D_sq <- D^2 # Consider Square for control variables
names(D_sq) <- c("temp_max_Sq", "temp_min_Sq", "day_rain_Sq", "night_rain_Sq", "cloud_day_Sq", 
                 "cloud_night_Sq", "day_wind_speed_Sq", "night_wind_speed_Sq")
ind.names <- c("city", "month", "day_of_week")
ind.names1 <- c("city", "month")
ind.names2 <- c("city", "day_of_week")
ind <- wewe[ , ind.names]
ind1 <- wewe[ , ind.names1] # Indicator variables, to consider interaction effect
ind2 <- wewe[ , ind.names2] 
inter_citymonth <- interaction(wewe$city, wewe$month)
inter_cityweek <- interaction(wewe$city, wewe$day_of_week)

rv.log <- log(1 + rv) # log process on response variables
AQI.log <- log(1 + AQI) # maybe AQI also need log process

# For mission 1
wewe.PWPW <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, D, D_sq))
form.PWPW <- PWPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.PWPW <- feols(form.PWPW, data = wewe.PWPW, vcov = ~city)
summary(fe.PWPW)

wewe.TPW <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, D, D_sq))
form.TPW <- TPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.TPW <- feols(form.TPW, data = wewe.TPW, vcov = ~city)
summary(fe.TPW)

wewe.PWNW <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
form.PWNW <- PWNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.PWNW <- feols(form.PWNW, data = wewe.PWNW, vcov = ~city)
summary(fe.PWNW)

wewe.TNW <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
form.TNW <- TNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.TNW <- feols(form.TNW, data = wewe.TNW, vcov = ~city)
summary(fe.TNW)

# For mission 2
wewe.PWSO <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, D, D_sq))
form.PWSO <- PWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.PWSO <- feols(form.PWSO, data = wewe.PWSO, vcov = ~city)
summary(fe.PWSO)

wewe.PWOO <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, D, D_sq))
form.PWOO <- PWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.PWOO <- feols(form.PWOO, data = wewe.PWOO, vcov = ~city)
summary(fe.PWOO)

wewe.NWSO <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
form.NWSO <- NWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.NWSO <- feols(form.NWSO, data = wewe.NWSO, vcov = ~city)
summary(fe.NWSO)

wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO <- NWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
fe.NWOO <- feols(form.NWOO, data = wewe.NWOO, vcov = ~city)
summary(fe.NWOO)

# For mission 3(with day)
wewe.PWPW.day <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, D, D_sq))
form.PWPW.day <- PWPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.PWPW.day <- feols(form.PWPW.day, data = wewe.PWPW.day, vcov = ~city)
summary(fe.PWPW.day)

wewe.TPW.day <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, D, D_sq))
form.TPW.day <- TPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.TPW.day <- feols(form.TPW.day, data = wewe.TPW.day, vcov = ~city)
summary(fe.TPW.day)

wewe.PWNW.day <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
form.PWNW.day <- PWNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.PWNW.day <- feols(form.PWNW.day, data = wewe.PWNW.day, vcov = ~city)
summary(fe.PWNW.day)

wewe.TNW.day <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
form.TNW.day <- TNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.TNW.day <- feols(form.TNW.day, data = wewe.TNW.day, vcov = ~city)
summary(fe.TNW.day)

wewe.PWSO.day <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, D, D_sq))
form.PWSO.day <- PWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.PWSO.day <- feols(form.PWSO.day, data = wewe.PWSO.day, vcov = ~city)
summary(fe.PWSO.day)

wewe.PWOO.day <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, D, D_sq))
form.PWOO.day <- PWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.PWOO.day <- feols(form.PWOO.day, data = wewe.PWOO.day, vcov = ~city)
summary(fe.PWOO.day)

wewe.NWSO.day <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
form.NWSO.day <- NWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.NWSO.day <- feols(form.NWSO.day, data = wewe.NWSO.day, vcov = ~city)
summary(fe.NWSO.day)

wewe.NWOO.day <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO.day <- NWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + day_of_week
fe.NWOO.day <- feols(form.NWOO.day, data = wewe.NWOO.day, vcov = ~city)
summary(fe.NWOO.day)

# For mission 3(without day)
wewe.PWPW.noday <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, D, D_sq))
form.PWPW.noday <- PWPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.noday <- feols(form.PWPW.noday, data = wewe.PWPW.noday, vcov = ~city)
summary(fe.PWPW.noday)

wewe.TPW.noday <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, D, D_sq))
form.TPW.noday <- TPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.noday <- feols(form.TPW.noday, data = wewe.TPW.noday, vcov = ~city)
summary(fe.TPW.noday)

wewe.PWNW.noday <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
form.PWNW.noday <- PWNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.noday <- feols(form.PWNW.noday, data = wewe.PWNW.noday, vcov = ~city)
summary(fe.PWNW.noday)

wewe.TNW.noday <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
form.TNW.noday <- TNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.noday <- feols(form.TNW.noday, data = wewe.TNW.noday, vcov = ~city)
summary(fe.TNW.noday)

wewe.PWSO.noday <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, D, D_sq))
form.PWSO.noday <- PWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.noday <- feols(form.PWSO.noday, data = wewe.PWSO.noday, vcov = ~city)
summary(fe.PWSO.noday)

wewe.PWOO.noday <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, D, D_sq))
form.PWOO.noday <- PWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.noday <- feols(form.PWOO.noday, data = wewe.PWOO.noday, vcov = ~city)
summary(fe.PWOO.noday)

wewe.NWSO.noday <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
form.NWSO.noday <- NWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.noday <- feols(form.NWSO.noday, data = wewe.NWSO.noday, vcov = ~city)
summary(fe.NWSO.noday)

wewe.NWOO.noday <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO.noday <- NWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.noday <- feols(form.NWOO.noday, data = wewe.NWOO.noday, vcov = ~city)
summary(fe.NWOO.noday)

# For mission 3(Holidays)
wewe.PWPW.holi <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, D, D_sq, Holidays))
form.PWPW.holi <- PWPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.PWPW.holi <- feols(form.PWPW.holi, data = wewe.PWPW.holi, vcov = ~city)
summary(fe.PWPW.holi)

wewe.TPW.holi <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, D, D_sq, Holidays))
form.TPW.holi <- TPW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.TPW.holi <- feols(form.TPW.holi, data = wewe.TPW.holi, vcov = ~city)
summary(fe.TPW.holi)

wewe.PWNW.holi <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq, Holidays))
form.PWNW.holi <- PWNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.PWNW.holi <- feols(form.PWNW.holi, data = wewe.PWNW.holi, vcov = ~city)
summary(fe.PWNW.holi)

wewe.TNW.holi <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq, Holidays))
form.TNW.holi <- TNW ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.TNW.holi <- feols(form.TNW.holi, data = wewe.TNW.holi, vcov = ~city)
summary(fe.TNW.holi)

wewe.PWSO.holi <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, D, D_sq, Holidays))
form.PWSO.holi <- PWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.PWSO.holi <- feols(form.PWSO.holi, data = wewe.PWSO.holi, vcov = ~city)
summary(fe.PWSO.holi)

wewe.PWOO.holi <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, D, D_sq, Holidays))
form.PWOO.holi <- PWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.PWOO.holi <- feols(form.PWOO.holi, data = wewe.PWOO.holi, vcov = ~city)
summary(fe.PWOO.holi)

wewe.NWSO.holi <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq, Holidays))
form.NWSO.holi <- NWSO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.NWSO.holi <- feols(form.NWSO.holi, data = wewe.NWSO.holi, vcov = ~city)
summary(fe.NWSO.holi)

wewe.NWOO.holi <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq, Holidays))
form.NWOO.holi <- NWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + Holidays
fe.NWOO.holi <- feols(form.NWOO.holi, data = wewe.NWOO.holi, vcov = ~city)
summary(fe.NWOO.holi)

# For mission 4(Nonlinear Analysis)
wewe.PWPW.seg <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, D, D_sq))
# Create AQI Segemented Dummy Variables
wewe.PWPW.seg$AQI_0_50   <- ifelse(wewe.PWPW.seg$AQI >= 0  & wewe.PWPW.seg$AQI < 50, 1, 0)
wewe.PWPW.seg$AQI_50_100   <- ifelse(wewe.PWPW.seg$AQI >= 50  & wewe.PWPW.seg$AQI < 100, 1, 0)
wewe.PWPW.seg$AQI_100_150  <- ifelse(wewe.PWPW.seg$AQI >= 100 & wewe.PWPW.seg$AQI < 150, 1, 0)
wewe.PWPW.seg$AQI_150_200  <- ifelse(wewe.PWPW.seg$AQI >= 150 & wewe.PWPW.seg$AQI < 200, 1, 0)
wewe.PWPW.seg$AQI_200_300  <- ifelse(wewe.PWPW.seg$AQI >= 200 & wewe.PWPW.seg$AQI < 300, 1, 0)
wewe.PWPW.seg$AQI_300_plus <- ifelse(wewe.PWPW.seg$AQI >= 300, 1, 0)
form.PWPW.seg <- PWPW ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.seg <- feols(form.PWPW.seg, data = wewe.PWPW.seg, vcov = ~city)
summary(fe.PWPW.seg)

wewe.TPW.seg <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, D, D_sq))
wewe.TPW.seg$AQI_0_50   <- ifelse(wewe.TPW.seg$AQI >= 0  & wewe.TPW.seg$AQI < 50, 1, 0)
wewe.TPW.seg$AQI_50_100   <- ifelse(wewe.TPW.seg$AQI >= 50  & wewe.TPW.seg$AQI < 100, 1, 0)
wewe.TPW.seg$AQI_100_150  <- ifelse(wewe.TPW.seg$AQI >= 100 & wewe.TPW.seg$AQI < 150, 1, 0)
wewe.TPW.seg$AQI_150_200  <- ifelse(wewe.TPW.seg$AQI >= 150 & wewe.TPW.seg$AQI < 200, 1, 0)
wewe.TPW.seg$AQI_200_300  <- ifelse(wewe.TPW.seg$AQI >= 200 & wewe.TPW.seg$AQI < 300, 1, 0)
wewe.TPW.seg$AQI_300_plus <- ifelse(wewe.TPW.seg$AQI >= 300, 1, 0)
form.TPW.seg <- TPW ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.seg <- feols(form.TPW.seg, data = wewe.TPW.seg, vcov = ~city)
summary(fe.TPW.seg)

wewe.PWNW.seg <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
wewe.PWNW.seg$AQI_0_50   <- ifelse(wewe.PWNW.seg$AQI >= 0  & wewe.PWNW.seg$AQI < 50, 1, 0)
wewe.PWNW.seg$AQI_50_100   <- ifelse(wewe.PWNW.seg$AQI >= 50  & wewe.PWNW.seg$AQI < 100, 1, 0)
wewe.PWNW.seg$AQI_100_150  <- ifelse(wewe.PWNW.seg$AQI >= 100 & wewe.PWNW.seg$AQI < 150, 1, 0)
wewe.PWNW.seg$AQI_150_200  <- ifelse(wewe.PWNW.seg$AQI >= 150 & wewe.PWNW.seg$AQI < 200, 1, 0)
wewe.PWNW.seg$AQI_200_300  <- ifelse(wewe.PWNW.seg$AQI >= 200 & wewe.PWNW.seg$AQI < 300, 1, 0)
wewe.PWNW.seg$AQI_300_plus <- ifelse(wewe.PWNW.seg$AQI >= 300, 1, 0)
form.PWNW.seg <- PWNW ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.seg <- feols(form.PWNW.seg, data = wewe.PWNW.seg, vcov = ~city)
summary(fe.PWNW.seg)

wewe.TNW.seg <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
wewe.TNW.seg$AQI_0_50   <- ifelse(wewe.TNW.seg$AQI >= 0  & wewe.TNW.seg$AQI < 50, 1, 0)
wewe.TNW.seg$AQI_50_100   <- ifelse(wewe.TNW.seg$AQI >= 50  & wewe.TNW.seg$AQI < 100, 1, 0)
wewe.TNW.seg$AQI_100_150  <- ifelse(wewe.TNW.seg$AQI >= 100 & wewe.TNW.seg$AQI < 150, 1, 0)
wewe.TNW.seg$AQI_150_200  <- ifelse(wewe.TNW.seg$AQI >= 150 & wewe.TNW.seg$AQI < 200, 1, 0)
wewe.TNW.seg$AQI_200_300  <- ifelse(wewe.TNW.seg$AQI >= 200 & wewe.TNW.seg$AQI < 300, 1, 0)
wewe.TNW.seg$AQI_300_plus <- ifelse(wewe.TNW.seg$AQI >= 300, 1, 0)
form.TNW.seg <- TNW ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.seg <- feols(form.TNW.seg, data = wewe.TNW.seg, vcov = ~city)
summary(fe.TNW.seg)

wewe.PWSO.seg <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, D, D_sq))
wewe.PWSO.seg$AQI_0_50   <- ifelse(wewe.PWSO.seg$AQI >= 0  & wewe.PWSO.seg$AQI < 50, 1, 0)
wewe.PWSO.seg$AQI_50_100   <- ifelse(wewe.PWSO.seg$AQI >= 50  & wewe.PWSO.seg$AQI < 100, 1, 0)
wewe.PWSO.seg$AQI_100_150  <- ifelse(wewe.PWSO.seg$AQI >= 100 & wewe.PWSO.seg$AQI < 150, 1, 0)
wewe.PWSO.seg$AQI_150_200  <- ifelse(wewe.PWSO.seg$AQI >= 150 & wewe.PWSO.seg$AQI < 200, 1, 0)
wewe.PWSO.seg$AQI_200_300  <- ifelse(wewe.PWSO.seg$AQI >= 200 & wewe.PWSO.seg$AQI < 300, 1, 0)
wewe.PWSO.seg$AQI_300_plus <- ifelse(wewe.PWSO.seg$AQI >= 300, 1, 0)
form.PWSO.seg <- PWSO ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.seg <- feols(form.PWSO.seg, data = wewe.PWSO.seg, vcov = ~city)
summary(fe.PWSO.seg)

wewe.PWOO.seg <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, D, D_sq))
wewe.PWOO.seg$AQI_0_50   <- ifelse(wewe.PWOO.seg$AQI >= 0  & wewe.PWOO.seg$AQI < 50, 1, 0)
wewe.PWOO.seg$AQI_50_100   <- ifelse(wewe.PWOO.seg$AQI >= 50  & wewe.PWOO.seg$AQI < 100, 1, 0)
wewe.PWOO.seg$AQI_100_150  <- ifelse(wewe.PWOO.seg$AQI >= 100 & wewe.PWOO.seg$AQI < 150, 1, 0)
wewe.PWOO.seg$AQI_150_200  <- ifelse(wewe.PWOO.seg$AQI >= 150 & wewe.PWOO.seg$AQI < 200, 1, 0)
wewe.PWOO.seg$AQI_200_300  <- ifelse(wewe.PWOO.seg$AQI >= 200 & wewe.PWOO.seg$AQI < 300, 1, 0)
wewe.PWOO.seg$AQI_300_plus <- ifelse(wewe.PWOO.seg$AQI >= 300, 1, 0)
form.PWOO.seg <- PWOO ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.seg <- feols(form.PWOO.seg, data = wewe.PWOO.seg, vcov = ~city)
summary(fe.PWOO.seg)

wewe.NWSO.seg <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
wewe.NWSO.seg$AQI_0_50   <- ifelse(wewe.NWSO.seg$AQI >= 0  & wewe.NWSO.seg$AQI < 50, 1, 0)
wewe.NWSO.seg$AQI_50_100   <- ifelse(wewe.NWSO.seg$AQI >= 50  & wewe.NWSO.seg$AQI < 100, 1, 0)
wewe.NWSO.seg$AQI_100_150  <- ifelse(wewe.NWSO.seg$AQI >= 100 & wewe.NWSO.seg$AQI < 150, 1, 0)
wewe.NWSO.seg$AQI_150_200  <- ifelse(wewe.NWSO.seg$AQI >= 150 & wewe.NWSO.seg$AQI < 200, 1, 0)
wewe.NWSO.seg$AQI_200_300  <- ifelse(wewe.NWSO.seg$AQI >= 200 & wewe.NWSO.seg$AQI < 300, 1, 0)
wewe.NWSO.seg$AQI_300_plus <- ifelse(wewe.NWSO.seg$AQI >= 300, 1, 0)
form.NWSO.seg <- NWSO ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.seg <- feols(form.NWSO.seg, data = wewe.NWSO.seg, vcov = ~city)
summary(fe.NWSO.seg)

wewe.NWOO.seg <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
wewe.NWOO.seg$AQI_0_50   <- ifelse(wewe.NWOO.seg$AQI >= 0  & wewe.NWOO.seg$AQI < 50, 1, 0)
wewe.NWOO.seg$AQI_50_100   <- ifelse(wewe.NWOO.seg$AQI >= 50  & wewe.NWOO.seg$AQI < 100, 1, 0)
wewe.NWOO.seg$AQI_100_150  <- ifelse(wewe.NWOO.seg$AQI >= 100 & wewe.NWOO.seg$AQI < 150, 1, 0)
wewe.NWOO.seg$AQI_150_200  <- ifelse(wewe.NWOO.seg$AQI >= 150 & wewe.NWOO.seg$AQI < 200, 1, 0)
wewe.NWOO.seg$AQI_200_300  <- ifelse(wewe.NWOO.seg$AQI >= 200 & wewe.NWOO.seg$AQI < 300, 1, 0)
wewe.NWOO.seg$AQI_300_plus <- ifelse(wewe.NWOO.seg$AQI >= 300, 1, 0)
form.NWOO.seg <- NWOO ~ AQI_0_50 + AQI_50_100 + AQI_100_150 + AQI_150_200 + AQI_200_300 + AQI_300_plus +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.seg <- feols(form.NWOO.seg, data = wewe.NWOO.seg, vcov = ~city)
summary(fe.NWOO.seg)

### For different AQI level
wewe <- wewe %>%
  mutate(AQI_level = case_when(
    AQI >= 0 & AQI <= 50 ~ 1,
    AQI > 50 & AQI <= 100 ~ 2,
    AQI > 100 & AQI <= 150 ~ 3,
    AQI > 150 & AQI <= 200 ~ 4,
    AQI > 200 & AQI <= 300 ~ 5,
    AQI > 300 ~ 6
  ))
levels <- wewe$AQI_level

wewe.PWPW.level <- as.data.frame(cbind(ind, rv.log['PWPW'], AQI, levels, D, D_sq))
form.PWPW.level <- PWPW ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWPW.level <- feols(form.PWPW.level, data = wewe.PWPW.level, vcov = ~city)
summary(fe.PWPW.level)

wewe.TPW.level <- as.data.frame(cbind(ind, rv.log['TPW'], AQI, levels, D, D_sq))
form.TPW.level <- TPW ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TPW.level <- feols(form.TPW.level, data = wewe.TPW.level, vcov = ~city)
summary(fe.TPW.level)

wewe.PWNW.level <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, levels, D, D_sq))
form.PWNW.level <- PWNW ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.level <- feols(form.PWNW.level, data = wewe.PWNW.level, vcov = ~city)
summary(fe.PWNW.level)

wewe.TNW.level <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, levels, D, D_sq))
form.TNW.level <- TNW ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.level <- feols(form.TNW.level, data = wewe.TNW.level, vcov = ~city)
summary(fe.TNW.level)

wewe.PWSO.level <- as.data.frame(cbind(ind, rv.log['PWSO'], AQI, levels, D, D_sq))
form.PWSO.level <- PWSO ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWSO.level <- feols(form.PWSO.level, data = wewe.PWSO.level, vcov = ~city)
summary(fe.PWSO.level)

wewe.PWOO.level <- as.data.frame(cbind(ind, rv.log['PWOO'], AQI, levels, D, D_sq))
form.PWOO.level <- PWOO ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWOO.level <- feols(form.PWOO.level, data = wewe.PWOO.level, vcov = ~city)
summary(fe.PWOO.level)

wewe.NWSO.level <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, levels, D, D_sq))
form.NWSO.level <- NWSO ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.level <- feols(form.NWSO.level, data = wewe.NWSO.level, vcov = ~city)
summary(fe.NWSO.level)

wewe.NWOO.level <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, levels, D, D_sq))
form.NWOO.level <- NWOO ~ i(levels, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.level <- feols(form.NWOO.level, data = wewe.NWOO.level, vcov = ~city)
summary(fe.NWOO.level)

# For mission 5(Robust Test)
# PM2.5
PM2_5 <- wewe$PM2_5
wewe.PWPW.PM2 <- as.data.frame(cbind(ind, rv.log['PWPW'], PM2_5, D, D_sq))
form.PWPW.PM2 <- PWPW ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.PM2 <- feols(form.PWPW.PM2, data = wewe.PWPW.PM2, vcov = ~city)
summary(fe.PWPW.PM2)

wewe.TPW.PM2 <- as.data.frame(cbind(ind, rv.log['TPW'], PM2_5, D, D_sq))
form.TPW.PM2 <- TPW ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.PM2 <- feols(form.TPW.PM2, data = wewe.TPW.PM2, vcov = ~city)
summary(fe.TPW.PM2)

wewe.PWNW.PM2 <- as.data.frame(cbind(ind, rv.log['PWNW'], PM2_5, D, D_sq))
form.PWNW.PM2 <- PWNW ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.PM2 <- feols(form.PWNW.PM2, data = wewe.PWNW.PM2, vcov = ~city)
summary(fe.PWNW.PM2)

wewe.TNW.PM2 <- as.data.frame(cbind(ind, rv.log['TNW'], PM2_5, D, D_sq))
form.TNW.PM2 <- TNW ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.PM2 <- feols(form.TNW.PM2, data = wewe.TNW.PM2, vcov = ~city)
summary(fe.TNW.PM2)
 
wewe.PWSO.PM2 <- as.data.frame(cbind(ind, rv.log['PWSO'], PM2_5, D, D_sq))
form.PWSO.PM2 <- PWSO ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.PM2 <- feols(form.PWSO.PM2, data = wewe.PWSO.PM2, vcov = ~city)
summary(fe.PWSO.PM2)

wewe.PWOO.PM2 <- as.data.frame(cbind(ind, rv.log['PWOO'], PM2_5, D, D_sq))
form.PWOO.PM2 <- PWOO ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.PM2 <- feols(form.PWOO.PM2, data = wewe.PWOO.PM2, vcov = ~city)
summary(fe.PWOO.PM2)

wewe.NWSO.PM2 <- as.data.frame(cbind(ind, rv.log['NWSO'], PM2_5, D, D_sq))
form.NWSO.PM2 <- NWSO ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.PM2 <- feols(form.NWSO.PM2, data = wewe.NWSO.PM2, vcov = ~city)
summary(fe.NWSO.PM2)

wewe.NWOO.PM2 <- as.data.frame(cbind(ind, rv.log['NWOO'], PM2_5, D, D_sq))
form.NWOO.PM2 <- NWOO ~ PM2_5 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.PM2 <- feols(form.NWOO.PM2, data = wewe.NWOO.PM2, vcov = ~city)
summary(fe.NWOO.PM2)

# PM10
PM10 <- wewe$PM10
wewe.PWPW.PM10 <- as.data.frame(cbind(ind, rv.log['PWPW'], PM10, D, D_sq))
form.PWPW.PM10 <- PWPW ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.PM10 <- feols(form.PWPW.PM10, data = wewe.PWPW.PM10, vcov = ~city)
summary(fe.PWPW.PM10)

wewe.TPW.PM10 <- as.data.frame(cbind(ind, rv.log['TPW'], PM10, D, D_sq))
form.TPW.PM10 <- TPW ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.PM10 <- feols(form.TPW.PM10, data = wewe.TPW.PM10, vcov = ~city)
summary(fe.TPW.PM10)

wewe.PWNW.PM10 <- as.data.frame(cbind(ind, rv.log['PWNW'], PM10, D, D_sq))
form.PWNW.PM10 <- PWNW ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.PM10 <- feols(form.PWNW.PM10, data = wewe.PWNW.PM10, vcov = ~city)
summary(fe.PWNW.PM10)

wewe.TNW.PM10 <- as.data.frame(cbind(ind, rv.log['TNW'], PM10, D, D_sq))
form.TNW.PM10 <- TNW ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.PM10 <- feols(form.TNW.PM10, data = wewe.TNW.PM10, vcov = ~city)
summary(fe.TNW.PM10)

wewe.PWSO.PM10 <- as.data.frame(cbind(ind, rv.log['PWSO'], PM10, D, D_sq))
form.PWSO.PM10 <- PWSO ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.PM10 <- feols(form.PWSO.PM10, data = wewe.PWSO.PM10, vcov = ~city)
summary(fe.PWSO.PM10)

wewe.PWOO.PM10 <- as.data.frame(cbind(ind, rv.log['PWOO'], PM10, D, D_sq))
form.PWOO.PM10 <- PWOO ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.PM10 <- feols(form.PWOO.PM10, data = wewe.PWOO.PM10, vcov = ~city)
summary(fe.PWOO.PM10)

wewe.NWSO.PM10 <- as.data.frame(cbind(ind, rv.log['NWSO'], PM10, D, D_sq))
form.NWSO.PM10 <- NWSO ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.PM10 <- feols(form.NWSO.PM10, data = wewe.NWSO.PM10, vcov = ~city)
summary(fe.NWSO.PM10)

wewe.NWOO.PM10 <- as.data.frame(cbind(ind, rv.log['NWOO'], PM10, D, D_sq))
form.NWOO.PM10 <- NWOO ~ PM10 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.PM10 <- feols(form.NWOO.PM10, data = wewe.NWOO.PM10, vcov = ~city)
summary(fe.NWOO.PM10)

# NO2
NO2 <- wewe$NO2
wewe.PWPW.NO2 <- as.data.frame(cbind(ind, rv.log['PWPW'], NO2, D, D_sq))
form.PWPW.NO2 <- PWPW ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.NO2 <- feols(form.PWPW.NO2, data = wewe.PWPW.NO2, vcov = ~city)
summary(fe.PWPW.NO2)

wewe.TPW.NO2 <- as.data.frame(cbind(ind, rv.log['TPW'], NO2, D, D_sq))
form.TPW.NO2 <- TPW ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.NO2 <- feols(form.TPW.NO2, data = wewe.TPW.NO2, vcov = ~city)
summary(fe.TPW.NO2)

wewe.PWNW.NO2 <- as.data.frame(cbind(ind, rv.log['PWNW'], NO2, D, D_sq))
form.PWNW.NO2 <- PWNW ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.NO2 <- feols(form.PWNW.NO2, data = wewe.PWNW.NO2, vcov = ~city)
summary(fe.PWNW.NO2)

wewe.TNW.NO2 <- as.data.frame(cbind(ind, rv.log['TNW'], NO2, D, D_sq))
form.TNW.NO2 <- TNW ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.NO2 <- feols(form.TNW.NO2, data = wewe.TNW.NO2, vcov = ~city)
summary(fe.TNW.NO2)

wewe.PWSO.NO2 <- as.data.frame(cbind(ind, rv.log['PWSO'], NO2, D, D_sq))
form.PWSO.NO2 <- PWSO ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.NO2 <- feols(form.PWSO.NO2, data = wewe.PWSO.NO2, vcov = ~city)
summary(fe.PWSO.NO2)

wewe.PWOO.NO2 <- as.data.frame(cbind(ind, rv.log['PWOO'], NO2, D, D_sq))
form.PWOO.NO2 <- PWOO ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.NO2 <- feols(form.PWOO.NO2, data = wewe.PWOO.NO2, vcov = ~city)
summary(fe.PWOO.NO2)

wewe.NWSO.NO2 <- as.data.frame(cbind(ind, rv.log['NWSO'], NO2, D, D_sq))
form.NWSO.NO2 <- NWSO ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.NO2 <- feols(form.NWSO.NO2, data = wewe.NWSO.NO2, vcov = ~city)
summary(fe.NWSO.NO2)

wewe.NWOO.NO2 <- as.data.frame(cbind(ind, rv.log['NWOO'], NO2, D, D_sq))
form.NWOO.NO2 <- NWOO ~ NO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.NO2 <- feols(form.NWOO.NO2, data = wewe.NWOO.NO2, vcov = ~city)
summary(fe.NWOO.NO2)

# SO2
SO2 <- wewe$SO2
wewe.PWPW.SO2 <- as.data.frame(cbind(ind, rv.log['PWPW'], SO2, D, D_sq))
form.PWPW.SO2 <- PWPW ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.SO2 <- feols(form.PWPW.SO2, data = wewe.PWPW.SO2, vcov = ~city)
summary(fe.PWPW.SO2)

wewe.TPW.SO2 <- as.data.frame(cbind(ind, rv.log['TPW'], SO2, D, D_sq))
form.TPW.SO2 <- TPW ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.SO2 <- feols(form.TPW.SO2, data = wewe.TPW.SO2, vcov = ~city)
summary(fe.TPW.SO2)

wewe.PWNW.SO2 <- as.data.frame(cbind(ind, rv.log['PWNW'], SO2, D, D_sq))
form.PWNW.SO2 <- PWNW ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.SO2 <- feols(form.PWNW.SO2, data = wewe.PWNW.SO2, vcov = ~city)
summary(fe.PWNW.SO2)

wewe.TNW.SO2 <- as.data.frame(cbind(ind, rv.log['TNW'], SO2, D, D_sq))
form.TNW.SO2 <- TNW ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.SO2 <- feols(form.TNW.SO2, data = wewe.TNW.SO2, vcov = ~city)
summary(fe.TNW.SO2)

wewe.PWSO.SO2 <- as.data.frame(cbind(ind, rv.log['PWSO'], SO2, D, D_sq))
form.PWSO.SO2 <- PWSO ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.SO2 <- feols(form.PWSO.SO2, data = wewe.PWSO.SO2, vcov = ~city)
summary(fe.PWSO.SO2)

wewe.PWOO.SO2 <- as.data.frame(cbind(ind, rv.log['PWOO'], SO2, D, D_sq))
form.PWOO.SO2 <- PWOO ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.SO2 <- feols(form.PWOO.SO2, data = wewe.PWOO.SO2, vcov = ~city)
summary(fe.PWOO.SO2)

wewe.NWSO.SO2 <- as.data.frame(cbind(ind, rv.log['NWSO'], SO2, D, D_sq))
form.NWSO.SO2 <- NWSO ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.SO2 <- feols(form.NWSO.SO2, data = wewe.NWSO.SO2, vcov = ~city)
summary(fe.NWSO.SO2)

wewe.NWOO.SO2 <- as.data.frame(cbind(ind, rv.log['NWOO'], SO2, D, D_sq))
form.NWOO.SO2 <- NWOO ~ SO2 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.SO2 <- feols(form.NWOO.SO2, data = wewe.NWOO.SO2, vcov = ~city)
summary(fe.NWOO.SO2)

# O3
O3 <- wewe$O3
wewe.PWPW.O3 <- as.data.frame(cbind(ind, rv.log['PWPW'], O3, D, D_sq))
form.PWPW.O3 <- PWPW ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.O3 <- feols(form.PWPW.O3, data = wewe.PWPW.O3, vcov = ~city)
summary(fe.PWPW.O3)

wewe.TPW.O3 <- as.data.frame(cbind(ind, rv.log['TPW'], O3, D, D_sq))
form.TPW.O3 <- TPW ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.O3 <- feols(form.TPW.O3, data = wewe.TPW.O3, vcov = ~city)
summary(fe.TPW.O3)

wewe.PWNW.O3 <- as.data.frame(cbind(ind, rv.log['PWNW'], O3, D, D_sq))
form.PWNW.O3 <- PWNW ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.O3 <- feols(form.PWNW.O3, data = wewe.PWNW.O3, vcov = ~city)
summary(fe.PWNW.O3)

wewe.TNW.O3 <- as.data.frame(cbind(ind, rv.log['TNW'], O3, D, D_sq))
form.TNW.O3 <- TNW ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.O3 <- feols(form.TNW.O3, data = wewe.TNW.O3, vcov = ~city)
summary(fe.TNW.O3)

wewe.PWSO.O3 <- as.data.frame(cbind(ind, rv.log['PWSO'], O3, D, D_sq))
form.PWSO.O3 <- PWSO ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.O3 <- feols(form.PWSO.O3, data = wewe.PWSO.O3, vcov = ~city)
summary(fe.PWSO.O3)

wewe.PWOO.O3 <- as.data.frame(cbind(ind, rv.log['PWOO'], O3, D, D_sq))
form.PWOO.O3 <- PWOO ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.O3 <- feols(form.PWOO.O3, data = wewe.PWOO.O3, vcov = ~city)
summary(fe.PWOO.O3)

wewe.NWSO.O3 <- as.data.frame(cbind(ind, rv.log['NWSO'], O3, D, D_sq))
form.NWSO.O3 <- NWSO ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.O3 <- feols(form.NWSO.O3, data = wewe.NWSO.O3, vcov = ~city)
summary(fe.NWSO.O3)

wewe.NWOO.O3 <- as.data.frame(cbind(ind, rv.log['NWOO'], O3, D, D_sq))
form.NWOO.O3 <- NWOO ~ O3 + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.O3 <- feols(form.NWOO.O3, data = wewe.NWOO.O3, vcov = ~city)
summary(fe.NWOO.O3)

# CO
CO <- wewe$CO
wewe.PWPW.CO <- as.data.frame(cbind(ind, rv.log['PWPW'], CO, D, D_sq))
form.PWPW.CO <- PWPW ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWPW.CO <- feols(form.PWPW.CO, data = wewe.PWPW.CO, vcov = ~city)
summary(fe.PWPW.CO)

wewe.TPW.CO <- as.data.frame(cbind(ind, rv.log['TPW'], CO, D, D_sq))
form.TPW.CO <- TPW ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TPW.CO <- feols(form.TPW.CO, data = wewe.TPW.CO, vcov = ~city)
summary(fe.TPW.CO)

wewe.PWNW.CO <- as.data.frame(cbind(ind, rv.log['PWNW'], CO, D, D_sq))
form.PWNW.CO <- PWNW ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWNW.CO <- feols(form.PWNW.CO, data = wewe.PWNW.CO, vcov = ~city)
summary(fe.PWNW.CO)

wewe.TNW.CO <- as.data.frame(cbind(ind, rv.log['TNW'], CO, D, D_sq))
form.TNW.CO <- TNW ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.TNW.CO <- feols(form.TNW.CO, data = wewe.TNW.CO, vcov = ~city)
summary(fe.TNW.CO)

wewe.PWSO.CO <- as.data.frame(cbind(ind, rv.log['PWSO'], CO, D, D_sq))
form.PWSO.CO <- PWSO ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWSO.CO <- feols(form.PWSO.CO, data = wewe.PWSO.CO, vcov = ~city)
summary(fe.PWSO.CO)

wewe.PWOO.CO <- as.data.frame(cbind(ind, rv.log['PWOO'], CO, D, D_sq))
form.PWOO.CO <- PWOO ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.PWOO.CO <- feols(form.PWOO.CO, data = wewe.PWOO.CO, vcov = ~city)
summary(fe.PWOO.CO)

wewe.NWSO.CO <- as.data.frame(cbind(ind, rv.log['NWSO'], CO, D, D_sq))
form.NWSO.CO <- NWSO ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWSO.CO <- feols(form.NWSO.CO, data = wewe.NWSO.CO, vcov = ~city)
summary(fe.NWSO.CO)

wewe.NWOO.CO <- as.data.frame(cbind(ind, rv.log['NWOO'], CO, D, D_sq))
form.NWOO.CO <- NWOO ~ CO + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO.CO <- feols(form.NWOO.CO, data = wewe.NWOO.CO, vcov = ~city)
summary(fe.NWOO.CO)

