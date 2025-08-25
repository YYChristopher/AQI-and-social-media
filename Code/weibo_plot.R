library(readxl)
library(lmtest)
library(lfe)
library(ggplot2)
library(ggpubr)      

#Work Space of the data
work_dir = 'F:/24561/Documents/ResearchWrite/Weibo&Weather/code in Github/Code'
#Set the Work Space of the data
setwd(work_dir)

############# Data ##############
wewe.file <- read_excel('panel.xlsx')
wewe <- data.frame(wewe.file)
wewe <- subset(wewe, select = -c(posts_with_hatewords, total_hatewords, hatewords_other_oriented, 
                                 hatewords_self_oriented, arousal_score_mean))
wewe$rain <- wewe$day_rain + wewe$night_rain
wewe$cloud <- wewe$cloud_day + wewe$cloud_night

############# Variables ###############
postnum <- wewe[, "total_posts"]
rv.names <- c("posts_with_positive_words", "total_positive_words", "posts_with_negative_words", 
              "total_negative_words", "positive_words_self_oriented", "positive_words_other_oriented",
              "negative_words_self_oriented", "negative_words_other_oriented")
rv <- wewe[, rv.names] / postnum * 100

names(rv) <- c('PWPW', 'TPW', 'PWNW', 'TNW', 'PWSO', 'PWOO', 'NWSO', 'NWOO')
AQI <- wewe$AQI
D.names <- c("temp_max", "temp_min", "day_rain", "night_rain", "rain", "cloud_day", "cloud_night", "cloud", 
             "day_wind_speed", "night_wind_speed")
D <- wewe[, D.names] # control variables
D_sq <- D^2 # Consider Square for control variables
names(D_sq) <- c("temp_max_Sq", "temp_min_Sq", "day_rain_Sq", "night_rain_Sq", "rain_Sq", "cloud_day_Sq", 
                 "cloud_night_Sq", "day_wind_speed_Sq", "night_wind_speed_Sq")
ind.names <- c("city", "month", "day_of_week", "Holidays")
ind.names1 <- c("city", "month")
ind.names2 <- c("city", "day_of_week")
ind <- wewe[ , ind.names]
ind1 <- wewe[ , ind.names1] # Indicator variables, to consider interaction effect
ind2 <- wewe[ , ind.names2] 
inter_citymonth <- interaction(wewe$city, wewe$month)
inter_cityweek <- interaction(wewe$city, wewe$day_of_week)

rv.log <- log(1 + rv) # log process on response variables
# rv.log <- rv
AQI.log <- log(1 + AQI) # maybe AQI also need log process

### Weekdays Heterogenity(Concentrate on Negative Words)
wewe.PWNW <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
form.PWNW.heter <- PWNW ~ i(day_of_week, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.heter <- feols(form.PWNW.heter, data = wewe.PWNW, vcov = ~city)
summary(fe.PWNW.heter)

wewe.TNW <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
form.TNW.heter <- TNW ~ i(day_of_week, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.heter <- feols(form.TNW.heter, data = wewe.TNW, vcov = ~city)
summary(fe.TNW.heter)

wewe.NWSO <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
form.NWSO.heter <- NWSO ~ i(day_of_week, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.heter <- feols(form.NWSO.heter, data = wewe.NWSO, vcov = ~city)
summary(fe.NWSO.heter)

wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO.heter <- NWOO ~ i(day_of_week, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.heter <- feols(form.NWOO.heter, data = wewe.NWOO, vcov = ~city)
summary(fe.NWOO.heter)

### Holidays Heterogenity(Concentrate on Negative Words)
wewe.PWNW <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
form.PWNW.heter2 <- PWNW ~ i(Holidays, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.heter2 <- feols(form.PWNW.heter2, data = wewe.PWNW, vcov = ~city)
summary(fe.PWNW.heter2)

wewe.TNW <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
form.TNW.heter2 <- TNW ~ i(Holidays, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.heter2 <- feols(form.TNW.heter2, data = wewe.TNW, vcov = ~city)
summary(fe.TNW.heter2)

wewe.NWSO <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
form.NWSO.heter2 <- NWSO ~ i(Holidays, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.heter2 <- feols(form.NWSO.heter2, data = wewe.NWSO, vcov = ~city)
summary(fe.NWSO.heter2)

wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO.heter2 <- NWOO ~ i(Holidays, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.heter2 <- feols(form.NWOO.heter2, data = wewe.NWOO, vcov = ~city)
summary(fe.NWOO.heter2)


################ Temprature Heterogenity ##################
### Rain
wewe.PWNW.rain <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
wewe.PWNW.rain$norain <- ifelse(wewe.PWNW.rain$rain >= 0  & wewe.PWNW.rain$rain < 0.1, 1, 0)
wewe.PWNW.rain$smallrain  <- ifelse(wewe.PWNW.rain$rain >= 0.1  & wewe.PWNW.rain$rain < 10, 1, 0)
wewe.PWNW.rain$midrain <- ifelse(wewe.PWNW.rain$rain >= 10 & wewe.PWNW.rain$rain < 25, 1, 0)
wewe.PWNW.rain$bigrain <- ifelse(wewe.PWNW.rain$rain >= 25 & wewe.PWNW.rain$rain < 50, 1, 0)
wewe.PWNW.rain$rainstorm <- ifelse(wewe.PWNW.rain$rain >= 50 & wewe.PWNW.rain$rain < 100, 1, 0)
wewe.PWNW.rain$rainlevel <- max.col(wewe.PWNW.rain[, c("norain", "smallrain", "midrain", "bigrain", "rainstorm")], ties.method = "first") - 1
wewe.PWNW.rain <- wewe.PWNW.rain[, !(names(wewe.PWNW.rain) %in% c("norain", "smallrain", "midrain", "bigrain", "rainstorm"))]
form.PWNW.rain <- PWNW ~ i(rainlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.rain <- feols(form.PWNW.rain, data = wewe.PWNW.rain, vcov = ~city)
summary(fe.PWNW.rain)

wewe.TNW.rain <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
wewe.TNW.rain$norain <- ifelse(wewe.TNW.rain$rain >= 0  & wewe.TNW.rain$rain < 0.1, 1, 0)
wewe.TNW.rain$smallrain  <- ifelse(wewe.TNW.rain$rain >= 0.1  & wewe.TNW.rain$rain < 10, 1, 0)
wewe.TNW.rain$midrain <- ifelse(wewe.TNW.rain$rain >= 10 & wewe.TNW.rain$rain < 25, 1, 0)
wewe.TNW.rain$bigrain <- ifelse(wewe.TNW.rain$rain >= 25 & wewe.TNW.rain$rain < 50, 1, 0)
wewe.TNW.rain$rainstorm <- ifelse(wewe.PWNW.rain$rain >= 50 & wewe.PWNW.rain$rain < 100, 1, 0)
wewe.TNW.rain$rainlevel <- max.col(wewe.TNW.rain[, c("norain", "smallrain", "midrain", "bigrain", "rainstorm")], ties.method = "first") - 1
wewe.TNW.rain <- wewe.TNW.rain[, !(names(wewe.TNW.rain) %in% c("norain", "smallrain", "midrain", "bigrain", "rainstorm"))]
form.TNW.rain <- TNW ~ i(rainlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.rain <- feols(form.TNW.rain, data = wewe.TNW.rain, vcov = ~city)
summary(fe.TNW.rain)

wewe.NWSO.rain <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
wewe.NWSO.rain$norain <- ifelse(wewe.NWSO.rain$rain >= 0  & wewe.NWSO.rain$rain < 0.1, 1, 0)
wewe.NWSO.rain$smallrain  <- ifelse(wewe.NWSO.rain$rain >= 0.1  & wewe.NWSO.rain$rain < 10, 1, 0)
wewe.NWSO.rain$midrain <- ifelse(wewe.NWSO.rain$rain >= 10 & wewe.NWSO.rain$rain < 25, 1, 0)
wewe.NWSO.rain$bigrain <- ifelse(wewe.NWSO.rain$rain >= 25 & wewe.NWSO.rain$rain < 50, 1, 0)
wewe.NWSO.rain$rainstorm <- ifelse(wewe.NWSO.rain$rain >= 50, 1, 0)
wewe.NWSO.rain$rainlevel <- max.col(wewe.NWSO.rain[, c("norain", "smallrain", "midrain", "bigrain", "rainstorm")], ties.method = "first") - 1
wewe.NWSO.rain <- wewe.NWSO.rain[, !(names(wewe.NWSO.rain) %in% c("norain", "smallrain", "midrain", "bigrain", "rainstorm"))]
form.NWSO.rain <- NWSO ~ i(rainlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.rain <- feols(form.NWSO.rain, data = wewe.NWSO.rain, vcov = ~city)
summary(fe.NWSO.rain)

wewe.NWOO.rain <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
wewe.NWOO.rain$norain <- ifelse(wewe.NWOO.rain$rain >= 0  & wewe.NWOO.rain$rain < 0.1, 1, 0)
wewe.NWOO.rain$smallrain  <- ifelse(wewe.NWOO.rain$rain >= 0.1  & wewe.NWOO.rain$rain < 10, 1, 0)
wewe.NWOO.rain$midrain <- ifelse(wewe.NWOO.rain$rain >= 10 & wewe.NWOO.rain$rain < 25, 1, 0)
wewe.NWOO.rain$bigrain <- ifelse(wewe.NWOO.rain$rain >= 25 & wewe.NWOO.rain$rain < 50, 1, 0)
wewe.NWOO.rain$rainstorm <- ifelse(wewe.NWOO.rain$rain >= 50, 1, 0)
wewe.NWOO.rain$rainlevel <- max.col(wewe.NWOO.rain[, c("norain", "smallrain", "midrain", "bigrain", "rainstorm")], ties.method = "first") - 1
wewe.NWOO.rain <- wewe.NWOO.rain[, !(names(wewe.NWOO.rain) %in% c("norain", "smallrain", "midrain", "bigrain", "rainstorm"))]
form.NWOO.rain <- NWOO ~ i(rainlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.rain <- feols(form.NWOO.rain, data = wewe.NWOO.rain, vcov = ~city)
summary(fe.NWOO.rain)

### Temperature
# Calculate 25% and 75% quantile
q_tempmax <- quantile(wewe$temp_max, probs = c(0.25, 0.75), na.rm = TRUE)
q_tempmin <- quantile(wewe$temp_min, probs = c(0.25, 0.75), na.rm = TRUE)
wewe$desire <- ifelse(
  wewe$temp_max >= q_tempmax[1] & wewe$temp_max <= q_tempmax[2] &
    wewe$temp_min >= q_tempmin[1] & wewe$temp_min <= q_tempmin[2],
  1, 0
)

wewe.PWNW.temp <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq, wewe$desire))
names(wewe.PWNW.temp)[length(names(wewe.PWNW.temp))] <- "desire"
form.PWNW.temp <- PWNW ~ i(desire, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.temp <- feols(form.PWNW.temp, data = wewe.PWNW.temp, vcov = ~city)
summary(fe.PWNW.temp)

wewe.TNW.temp <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq, wewe$desire))
names(wewe.TNW.temp)[length(names(wewe.TNW.temp))] <- "desire"
form.TNW.temp <- TNW ~ i(desire, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.temp <- feols(form.TNW.temp, data = wewe.TNW.temp, vcov = ~city)
summary(fe.TNW.temp)

wewe.NWSO.temp <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq, wewe$desire))
names(wewe.NWSO.temp)[length(names(wewe.NWSO.temp))] <- "desire"
form.NWSO.temp <- NWSO ~ i(desire, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.temp <- feols(form.NWSO.temp, data = wewe.NWSO.temp, vcov = ~city)
summary(fe.NWSO.temp)

wewe.NWOO.temp <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq, wewe$desire))
names(wewe.NWOO.temp)[length(names(wewe.NWOO.temp))] <- "desire"
form.NWOO.temp <- NWOO ~ i(desire, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.temp <- feols(form.NWOO.temp, data = wewe.NWOO.temp, vcov = ~city)
summary(fe.NWOO.temp)


### Cloud
cloud.low <- quantile(wewe$cloud, probs = 0.25, na.rm = TRUE)
cloud.mid <- quantile(wewe$cloud, probs = 0.5, na.rm = TRUE)
cloud.high <- quantile(wewe$cloud, probs = 0.75, na.rm = TRUE)

wewe.PWNW.cloud <- as.data.frame(cbind(ind, rv.log['PWNW'], AQI, D, D_sq))
wewe.PWNW.cloud$lowcloud <- ifelse(wewe.PWNW.cloud$cloud >= 0  & wewe.PWNW.cloud$cloud < cloud.low, 1, 0)
wewe.PWNW.cloud$midcloud  <- ifelse(wewe.PWNW.cloud$cloud >= cloud.low  & wewe.PWNW.cloud$cloud < cloud.mid, 1, 0)
wewe.PWNW.cloud$highcloud <- ifelse(wewe.PWNW.cloud$cloud >= cloud.mid & wewe.PWNW.cloud$cloud < cloud.high, 1, 0)
wewe.PWNW.cloud$fullcloud <- ifelse(wewe.PWNW.cloud$cloud >= cloud.high, 1, 0)
wewe.PWNW.cloud$cloudlevel <- max.col(wewe.PWNW.cloud[, c("lowcloud", "midcloud", "highcloud", "fullcloud")], ties.method = "first") - 1
wewe.PWNW.cloud <- wewe.PWNW.cloud[, !(names(wewe.PWNW.cloud) %in% c("lowcloud", "midcloud", "highcloud", "fullcloud"))]
form.PWNW.cloud <- PWNW ~ i(cloudlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.PWNW.cloud <- feols(form.PWNW.cloud, data = wewe.PWNW.cloud, vcov = ~city)
summary(fe.PWNW.cloud)

wewe.TNW.cloud <- as.data.frame(cbind(ind, rv.log['TNW'], AQI, D, D_sq))
wewe.TNW.cloud$lowcloud <- ifelse(wewe.TNW.cloud$cloud >= 0  & wewe.TNW.cloud$cloud < cloud.low, 1, 0)
wewe.TNW.cloud$midcloud  <- ifelse(wewe.TNW.cloud$cloud >= cloud.low  & wewe.TNW.cloud$cloud < cloud.mid, 1, 0)
wewe.TNW.cloud$highcloud <- ifelse(wewe.TNW.cloud$cloud >= cloud.mid & wewe.TNW.cloud$cloud < cloud.high, 1, 0)
wewe.TNW.cloud$fullcloud <- ifelse(wewe.TNW.cloud$cloud >= cloud.high, 1, 0)
wewe.TNW.cloud$cloudlevel <- max.col(wewe.TNW.cloud[, c("lowcloud", "midcloud", "highcloud", "fullcloud")], ties.method = "first") - 1
wewe.TNW.cloud <- wewe.TNW.cloud[, !(names(wewe.TNW.cloud) %in% c("lowcloud", "midcloud", "highcloud", "fullcloud"))]
form.TNW.cloud <- TNW ~ i(cloudlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.TNW.cloud <- feols(form.TNW.cloud, data = wewe.TNW.cloud, vcov = ~city)
summary(fe.TNW.cloud)

wewe.NWSO.cloud <- as.data.frame(cbind(ind, rv.log['NWSO'], AQI, D, D_sq))
wewe.NWSO.cloud$lowcloud <- ifelse(wewe.NWSO.cloud$cloud >= 0  & wewe.NWSO.cloud$cloud < cloud.low, 1, 0)
wewe.NWSO.cloud$midcloud  <- ifelse(wewe.NWSO.cloud$cloud >= cloud.low  & wewe.NWSO.cloud$cloud < cloud.mid, 1, 0)
wewe.NWSO.cloud$highcloud <- ifelse(wewe.NWSO.cloud$cloud >= cloud.mid & wewe.NWSO.cloud$cloud < cloud.high, 1, 0)
wewe.NWSO.cloud$fullcloud <- ifelse(wewe.NWSO.cloud$cloud >= cloud.high, 1, 0)
wewe.NWSO.cloud$cloudlevel <- max.col(wewe.NWSO.cloud[, c("lowcloud", "midcloud", "highcloud", "fullcloud")], ties.method = "first") - 1
wewe.NWSO.cloud <- wewe.NWSO.cloud[, !(names(wewe.NWSO.cloud) %in% c("lowcloud", "midcloud", "highcloud", "fullcloud"))]
form.NWSO.cloud <- NWSO ~ i(cloudlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWSO.cloud <- feols(form.NWSO.cloud, data = wewe.NWSO.cloud, vcov = ~city)
summary(fe.NWSO.cloud)

wewe.NWOO.cloud <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
wewe.NWOO.cloud$lowcloud <- ifelse(wewe.NWOO.cloud$cloud >= 0  & wewe.NWOO.cloud$cloud < cloud.low, 1, 0)
wewe.NWOO.cloud$midcloud  <- ifelse(wewe.NWOO.cloud$cloud >= cloud.low  & wewe.NWOO.cloud$cloud < cloud.mid, 1, 0)
wewe.NWOO.cloud$highcloud <- ifelse(wewe.NWOO.cloud$cloud >= cloud.mid & wewe.NWOO.cloud$cloud < cloud.high, 1, 0)
wewe.NWOO.cloud$fullcloud <- ifelse(wewe.NWOO.cloud$cloud >= cloud.high, 1, 0)
wewe.NWOO.cloud$cloudlevel <- max.col(wewe.NWOO.cloud[, c("lowcloud", "midcloud", "highcloud", "fullcloud")], ties.method = "first") - 1
wewe.NWOO.cloud <- wewe.NWOO.cloud[, !(names(wewe.NWOO.cloud) %in% c("lowcloud", "midcloud", "highcloud", "fullcloud"))]
form.NWOO.cloud <- NWOO ~ i(cloudlevel, AQI) +
  temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city + month
fe.NWOO.cloud <- feols(form.NWOO.cloud, data = wewe.NWOO.cloud, vcov = ~city)
summary(fe.NWOO.cloud)

############################# Plot #############################
#### Prepare Results

########################### Holidays ##########################
# PWNW
PWNW.day.data <- data.frame(
  Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Workday", "Holiday"),
  Group = c(rep("Week", 7), rep("Holiday", 2)),
  Coef = c(fe.PWNW.heter$coeftable[1:7,1], fe.PWNW.heter2$coeftable[1:2,1]),
  SE = c(fe.PWNW.heter$se[1:7], fe.PWNW.heter$se[1:2]),
  Pvalue = c(fe.PWNW.heter$coeftable[1:7,4], fe.PWNW.heter2$coeftable[1:2,4] - 0.01)
)

# 添加显著性星号
PWNW.day.data <- PWNW.day.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))

PWNW.day.data$Day <- factor(PWNW.day.data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                          "Friday", "Saturday", "Sunday", "Workday", "Holiday"))

ggplot(PWNW.day.data, aes(x = Day, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Day) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Week" = "#F4A582", "Holiday" = "#92C5DE")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.002))


# TNW
TNW.day.data <- data.frame(
  Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Workday", "Holiday"),
  Group = c(rep("Week", 7), rep("Holiday", 2)),
  Coef = c(fe.TNW.heter$coeftable[1:7,1], fe.TNW.heter2$coeftable[1:2,1]),
  SE = c(fe.TNW.heter$se[1:7], fe.TNW.heter$se[1:2]),
  Pvalue = c(fe.TNW.heter$coeftable[1:7,4], fe.TNW.heter2$coeftable[1:2,4] - 0.01)
)

# 添加显著性星号
TNW.day.data <- TNW.day.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))

TNW.day.data$Day <- factor(TNW.day.data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                          "Friday", "Saturday", "Sunday", "Workday", "Holiday"))

TNW.day.plot <- ggplot(TNW.day.data, aes(x = Day, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Day) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Week" = "#F4A582", "Holiday" = "#92C5DE")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.0025))
ggsave("./Plots/bar/TNW_day.png", plot = TNW.day.plot, width = 8, height = 4.5, dpi = 600)


# NWSO
NWSO.day.data <- data.frame(
  Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Workday", "Holiday"),
  Group = c(rep("Week", 7), rep("Holiday", 2)),
  Coef = c(fe.NWSO.heter$coeftable[1:7,1], fe.NWSO.heter2$coeftable[1:2,1]),
  SE = c(fe.NWSO.heter$se[1:7], fe.NWSO.heter$se[1:2]),
  Pvalue = c(fe.NWSO.heter$coeftable[1:7,4], fe.NWSO.heter2$coeftable[1:2,4])
)

# 添加显著性星号
NWSO.day.data <- NWSO.day.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))

NWSO.day.data$Day <- factor(NWSO.day.data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                          "Friday", "Saturday", "Sunday", "Workday", "Holiday"))

NWSO.day.plot <- ggplot(NWSO.day.data, aes(x = Day, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Day) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Week" = "#F4A582", "Holiday" = "#92C5DE")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.002))
# ggsave("./Plots/bar/NWSO_day.png", plot = NWSO.day.plot , width = 8, height = 4.5, dpi = 600)

# NWOO
NWOO.day.data <- data.frame(
  Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Workday", "Holiday"),
  Group = c(rep("Week", 7), rep("Holiday", 2)),
  Coef = c(fe.NWOO.heter$coeftable[1:7,1], fe.NWOO.heter2$coeftable[1:2,1]),
  SE = c(fe.NWOO.heter$se[1:7], fe.NWOO.heter$se[1:2]),
  Pvalue = c(fe.NWOO.heter$coeftable[1:7,4], fe.NWOO.heter2$coeftable[1:2,4])
)

# 添加显著性星号
NWOO.day.data <- NWOO.day.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))

NWOO.day.data$Day <- factor(NWOO.day.data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                        "Friday", "Saturday", "Sunday", "Workday", "Holiday"))

NWOO.day.plot <- ggplot(NWOO.day.data, aes(x = Day, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Day) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Week" = "#F4A582", "Holiday" = "#92C5DE")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.003))
ggsave("./Plots/bar/NWOO_day.png", plot = NWOO.day.plot , width = 8, height = 4.5, dpi = 600)

############################## Weather ##############################
# PWNW
PWNW.weather.data <- data.frame(
  Weather = c("Low", "Medium", "High", "Full", "Undesirable", "Desirable"),
  Group = c(rep("Cloud", 4), rep("Temp", 2)),
  Coef = c(fe.PWNW.cloud$coeftable[1:4,1], fe.PWNW.temp$coeftable[1:2,1]),
  SE = c(fe.PWNW.cloud$se[1:4], fe.PWNW.temp$se[1:2]),
  Pvalue = c(fe.PWNW.cloud$coeftable[1:4,4], fe.PWNW.temp$coeftable[1:2,4])
)

# 添加显著性星号
PWNW.weather.data <- PWNW.weather.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))
PWNW.weather.data$Weather <- factor(PWNW.weather.data$Weather, 
                                levels = c("Low", "Medium", "High", "Full", 
                                           "Undesirable", "Desirable"))

ggplot(PWNW.weather.data, aes(x = Weather, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Weather) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Cloud" = "#5DC3B5", "Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.002))

# TNW
TNW.weather.data <- data.frame(
  Weather = c("Low", "Medium", "High", "Full", "Undesirable", "Desirable"),
  Group = c(rep("Cloud", 4), rep("Temp", 2)),
  Coef = c(fe.TNW.cloud$coeftable[1:4,1], fe.TNW.temp$coeftable[1:2,1]),
  SE = c(fe.TNW.cloud$se[1:4], fe.TNW.temp$se[1:2]),
  Pvalue = c(fe.TNW.cloud$coeftable[1:4,4], fe.TNW.temp$coeftable[1:2,4])
)

# 添加显著性星号
TNW.weather.data <- TNW.weather.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))
TNW.weather.data$Weather <- factor(TNW.weather.data$Weather, 
                                levels = c("Low", "Medium", "High", "Full", 
                                           "Undesirable", "Desirable"))

ggplot(TNW.weather.data, aes(x = Weather, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Weather) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Cloud" = "#5DC3B5", "Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.0025))

# NWSO
NWSO.weather.data <- data.frame(
  Weather = c("Low", "Medium", "High", "Full", "Undesirable", "Desirable"),
  Group = c(rep("Cloud", 4), rep("Temp", 2)),
  Coef = c(fe.NWSO.cloud$coeftable[1:4,1], fe.NWSO.temp$coeftable[1:2,1]),
  SE = c(fe.NWSO.cloud$se[1:4], fe.NWSO.temp$se[1:2]),
  Pvalue = c(fe.NWSO.cloud$coeftable[1:4,4], fe.NWSO.temp$coeftable[1:2,4])
)

# 添加显著性星号
NWSO.weather.data <- NWSO.weather.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))
NWSO.weather.data$Weather <- factor(NWSO.weather.data$Weather, 
                                levels = c("Low", "Medium", "High", "Full", 
                                           "Undesirable", "Desirable"))

ggplot(NWSO.weather.data, aes(x = Weather, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Weather) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Cloud" = "#5DC3B5", "Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.002))

# NWOO
NWOO.weather.data <- data.frame(
  Weather = c("Low", "Medium", "High", "Full", "Undesirable", "Desirable"),
  Group = c(rep("Cloud", 4), rep("Temp", 2)),
  Coef = c(fe.NWOO.cloud$coeftable[1:4,1], fe.NWOO.temp$coeftable[1:2,1]),
  SE = c(fe.NWOO.cloud$se[1:4], fe.NWOO.temp$se[1:2]),
  Pvalue = c(fe.NWOO.cloud$coeftable[1:4,4], fe.NWOO.temp$coeftable[1:2,4])
)

# 添加显著性星号
NWOO.weather.data <- NWOO.weather.data %>%
  mutate(sig = case_when(
    Pvalue < 0.01 ~ "***",
    Pvalue < 0.05 ~ "**",
    Pvalue < 0.1 ~ "*",
    TRUE ~ ""
  ))
NWOO.weather.data$Weather <- factor(NWOO.weather.data$Weather, 
                                levels = c("Low", "Medium", "High", "Full", 
                                           "Undesirable", "Desirable"))

ggplot(NWOO.weather.data, aes(x = Weather, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  geom_text(aes(x = as.numeric(Weather) + 0.2, 
                y = Coef + SE * 0.3, 
                label = sig), size = 5) +
  # geom_text(aes(label = sig, y = Coef + SE), size = 5) +  # 显著性星号
  scale_fill_manual(values = c("Cloud" = "#5DC3B5", "Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +  # 分隔线
  coord_cartesian(ylim = c(0, 0.004))

### Only Temperature
PWNW.temp.data <- PWNW.weather.data[5:6, ]
PWNW.temp.data$x <- c(0.2, 0.6)
PWNW.temp.bar <- ggplot(PWNW.temp.data, aes(x = x, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  # geom_text(aes(x = as.numeric(Wea) + 0.2, 
  #               y = Coef + SE * 0.3, 
  #               label = sig), size = 5) +
  geom_text(aes(label = sig, y = Coef + SE * 1.2), size = 5) +  # 显著性星号
  scale_x_continuous(limits = c(0, 0.8), breaks = c(0.2, 0.6), labels = c('Undesirable', 'Desirable')) +
  scale_fill_manual(values = c("Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.margin = margin(t = 10, r = 120, b = 10, l = 120),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 0.002))
# ggsave("./Plots/bar/PWNW_tempbar.png", plot = PWNW.temp.bar, width = 8, height = 4.5, units = "in", dpi = 600)

TNW.temp.data <- TNW.weather.data[5:6, ]
TNW.temp.data$x <- c(0.2, 0.6)
TNW.temp.bar <- ggplot(TNW.temp.data, aes(x = x, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  # geom_text(aes(x = as.numeric(Wea) + 0.2, 
  #               y = Coef + SE * 0.3, 
  #               label = sig), size = 5) +
  geom_text(aes(label = sig, y = Coef + SE * 1.2), size = 5) +  # 显著性星号
  scale_x_continuous(limits = c(0, 0.8), breaks = c(0.2, 0.6), labels = c('Undesirable', 'Desirable')) +
  scale_fill_manual(values = c("Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.margin = margin(t = 10, r = 120, b = 10, l = 120),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 0.0025))
# ggsave("./Plots/bar/TNW_tempbar.png", plot = TNW.temp.bar, width = 8, height = 4.5, units = "in", dpi = 600)

NWSO.temp.data <- NWSO.weather.data[5:6, ]
NWSO.temp.data$x <- c(0.2, 0.6)
NWSO.temp.bar <- ggplot(NWSO.temp.data, aes(x = x, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  # geom_text(aes(x = as.numeric(Wea) + 0.2, 
  #               y = Coef + SE * 0.3, 
  #               label = sig), size = 5) +
  geom_text(aes(label = sig, y = Coef + SE * 1.2), size = 5) +  # 显著性星号
  scale_x_continuous(limits = c(0, 0.8), breaks = c(0.2, 0.6), labels = c('Undesirable', 'Desirable')) +
  scale_fill_manual(values = c("Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.margin = margin(t = 10, r = 120, b = 10, l = 120),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 0.002))
# ggsave("./Plots/bar/NWSO_tempbar.png", plot = NWSO.temp.bar, width = 8, height = 4.5, units = "in", dpi = 600)

NWOO.temp.data <- NWOO.weather.data[5:6, ]
NWOO.temp.data$x <- c(0.2, 0.6)
NWOO.temp.bar <- ggplot(NWOO.temp.data, aes(x = x, y = Coef, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), width = 0.2) +
  # geom_text(aes(x = as.numeric(Wea) + 0.2, 
  #               y = Coef + SE * 0.3, 
  #               label = sig), size = 5) +
  geom_text(aes(label = sig, y = Coef + SE * 1.2), size = 5) +  # 显著性星号
  scale_x_continuous(limits = c(0, 0.8), breaks = c(0.2, 0.6), labels = c('Undesirable', 'Desirable')) +
  scale_fill_manual(values = c("Temp" = "#4F76A3")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.margin = margin(t = 10, r = 120, b = 10, l = 120),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 0.004))
# ggsave("./Plots/bar/NWOO_tempbar.png", plot = NWOO.temp.bar, width = 8, height = 4.5, units = "in", dpi = 600)

###### Plot the 4 figures together
temp.data1 <- bind_rows(PWNW.temp.data, TNW.temp.data, NWSO.temp.data, NWOO.temp.data)
names_temp <- c('PWNW', 'PWNW', 'TNW', 'TNW', 'NWSO', 'NWSO', 'NWOO', 'NWOO')
temp.data.total <- cbind(temp.data1, names_temp)
temp.data.total$names_temp <- factor(temp.data.total$names_temp, levels = c("PWNW", "TNW", "NWSO", "NWOO"),
                   labels = c("PostsWithNegativeWords", "TotalNegativeWords", 
                              "NegativeWordsSelfOriented", "NegativeWordsOtherOriented"))

temp.bar <- ggplot(temp.data.total, aes(x = names_temp, y = Coef, fill = Weather)) +
            geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
            geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), position = position_dodge(0.7), 
                          width = 0.4) +
            geom_text(aes(label = sig, y = Coef + SE * 1.2), position = position_dodge(0.7), size = 5) +
            scale_fill_manual(values = c("Undesirable" = "#d73027", "Desirable" = "#088247")) +
            labs(y = expression("Effect of AQI"), x = NULL) +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, face = 'bold'), 
                  panel.grid = element_blank()) +
            theme(
              legend.position = c(0.15, 0.8),               # 图内位置
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 9),
              legend.key.size = unit(0.4, "cm"),
              legend.background = element_rect(fill = "white", color = "black")  # 给图例加边框
            )
show(temp.bar)
# ggsave("./Plots/bar/tempbar.png", plot = temp.bar, width = 8, height = 4.5, units = "in", dpi = 600)

###### Plot the latter 3 figures together
temp.data2 <- bind_rows(TNW.temp.data, NWSO.temp.data, NWOO.temp.data)
colnames(temp.data2) <- c("Weather", "Group", "Coef", "SE", "Pvalue", "sig", "x")
names_temp2 <- c('TNW', 'TNW', 'NWSO', 'NWSO', 'NWOO', 'NWOO')
temp.data2.total <- cbind(temp.data2, names_temp2)
temp.data2.total$names_temp2 <- factor(temp.data2.total$names_temp2, levels = c("TNW", "NWSO", "NWOO"),
                                     labels = c("Total Negative Words", 
                                                "Self-Oriented Negative Words", "Other-Oriented Negative Words"))

temp.bar2 <- ggplot(temp.data2.total, aes(x = names_temp2, y = Coef, fill = Weather)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Coef - SE, ymax = Coef + SE), position = position_dodge(0.7), 
                width = 0.4) +
  geom_text(aes(label = sig, y = Coef + SE * 1.2), position = position_dodge(0.7), size = 5) +
  scale_fill_manual(values = c("Undesirable" = "#d73027", "Desirable" = "#088247")) +
  labs(y = expression("Effect of AQI"), x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, face = 'bold'), 
        panel.grid = element_blank()) +
  theme(
    legend.position = c(0.15, 0.8),               # 图内位置
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.background = element_rect(fill = "white", color = "black")  # 给图例加边框
  )
show(temp.bar2)
ggsave("./Plots/bar/tempbar2.png", plot = temp.bar2, width = 8, height = 4.5, units = "in", dpi = 600)


##### Data Description Analysis and Plot

# Scatter Plots
wewe$day_of_year <- yday(wewe$date)
wewe_avg <- wewe %>%
  group_by(city) %>%
  summarise(mean_AQI = mean(AQI, na.rm = TRUE))

wewe.sh <- wewe %>% filter(city == "上海")
wewe.bj <- wewe %>% filter(city == "北京")
wewe.tj <- wewe %>% filter(city == "天津")
wewe.cq <- wewe %>% filter(city == "重庆")

ggplot(wewe.sh, aes(x = day_of_year, y = AQI)) +
  geom_point(size = 1.5, alpha = 0.7, color = "#003366") +  # 点图
  labs(x = "Day of year", y = "AQI") +
  scale_color_brewer(palette = "Set1") +
  theme_classic(base_size = 14) 

ggplot(wewe.bj, aes(x = day_of_year, y = AQI)) +
  geom_point(size = 1.5, alpha = 0.7, color = "#003366") +  # 点图
  labs(x = "Day of year", y = "AQI") +
  scale_color_brewer(palette = "Set1") +
  theme_classic(base_size = 14) 

ggplot(wewe.tj, aes(x = day_of_year, y = AQI)) +
  geom_point(size = 1.5, alpha = 0.7, color = "#003366") +  # 点图
  labs(x = "Day of year", y = "AQI") +
  scale_color_brewer(palette = "Set1") +
  theme_classic(base_size = 14) 

ggplot(wewe.cq, aes(x = day_of_year, y = AQI)) +
  geom_point(size = 1.5, alpha = 0.7, color = "#003366") +  # 点图
  labs(x = "Day of year", y = "AQI") +
  scale_color_brewer(palette = "Set1") +
  theme_classic(base_size = 14) 

scatter.all <- ggplot(wewe, aes(x = day_of_year, y = AQI, color = city)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(x = "Day of Year", y = "AQI", color = "City") +
  theme_classic(base_size = 14) +
  scale_color_manual(
    name = "City",
    values = c("上海" = "blue", "北京" = "red", "天津" = "green", "重庆" = "purple"),
    labels = c("Shanghai", "Beijing", "Tianjin", "Chongqing")
  )
ggsave("./Plots/scatter/scatter_All.png", plot = scatter.all, width = 8, height = 4.5, units = "in", dpi = 600)


#### Scatter for residuals
wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
form.NWOO <- NWOO ~ AQI + temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
  day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
  night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month
fe.NWOO <- feols(form.NWOO, data = wewe.NWOO, vcov = ~city^month)
summary(fe.NWOO)

residual.df <- data.frame(
  resi <- residuals(fe.NWOO), 
  day_of_year <- yday(wewe$date),
  city <- wewe$city
)

scatter.resi <- ggplot(wewe, aes(x = day_of_year, y = resi, color = city)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(x = "Day of Year", y = "Residual AQI(control for city*month FE)", color = "City") +
  theme_classic(base_size = 14) +
  scale_color_manual(
    name = "City",
    values = c("上海" = "blue", "北京" = "red", "天津" = "green", "重庆" = "purple"),
    labels = c("Shanghai", "Beijing", "Tianjin", "Chongqing")
  )
ggsave("./Plots/scatter/residual.png", plot = scatter.resi, width = 8, height = 4.5, units = "in", dpi = 600)

### Plot for Residuals
# Holidays

# # First regression without AQI as an explanatory variable
# wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
# form.NWOO.noAQI <- NWOO ~ temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
#   day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
#   night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
# fe.NWOO.noAQI <- feols(form.NWOO.noAQI, data = wewe.NWOO, vcov = ~city)
# summary(fe.NWOO.noAQI)
# res.noAQI <- residuals(fe.NWOO.noAQI)
# 
# AQI.holi <- wewe[which(wewe['Holidays'] == 1), 'AQI']
# AQI.noholi <- wewe[which(wewe['Holidays'] == 0), 'AQI']
# res.holi <- res.noAQI[which(wewe['Holidays'] == 1)]
# res.noholi <- res.noAQI[which(wewe['Holidays'] == 0)]
# data.holi <- as.data.frame(cbind(AQI.holi, res.holi))
# data.noholi <- as.data.frame(cbind(AQI.noholi, res.noholi))
# 
# holi.model <- lm(res.holi~AQI.holi, data = data.holi)
# summary(holi.model)
# noholi.model <- lm(res.noholi~AQI.noholi, data = data.noholi)
# summary(noholi.model)
# 
# # Temperature
# # Regression for all data
# wewe.NWOO <- as.data.frame(cbind(ind, rv.log['NWOO'], AQI, D, D_sq))
# form.NWOO.noAQI <- NWOO ~ temp_max + temp_min + day_rain + night_rain + cloud_day + cloud_night +
#   day_wind_speed + night_wind_speed + temp_max_Sq + temp_min_Sq + day_rain_Sq +
#   night_rain_Sq + cloud_day_Sq + cloud_night_Sq + day_wind_speed_Sq + night_wind_speed_Sq|city^month + day_of_week
# fe.NWOO.noAQI <- feols(form.NWOO.noAQI, data = wewe.NWOO, vcov = ~city)
# summary(fe.NWOO.noAQI)
# res.noAQI.NWOO <- residuals(fe.NWOO.noAQI)
# 
# AQI.desire <- AQI[which(wewe['desire'] == 1)]
# AQI.nodesire <- AQI[which(wewe['desire'] == 0)]
# res.desire <- res.noAQI.NWOO[which(wewe['desire'] == 1)]
# res.nodesire <- res.noAQI.NWOO[which(wewe['desire'] == 0)]
# data.desire <- as.data.frame(cbind(AQI.desire, res.desire))
# data.nodesire <- as.data.frame(cbind(AQI.nodesire, res.nodesire))
# 
# desire.model <- lm(res.desire~AQI.desire, data = data.desire)
# summary(desire.model)
# nodesire.model <- lm(res.nodesire~AQI.nodesire, data = data.nodesire)
# summary(nodesire.model)
# 
# # Regression for some quantiles
# probs <- seq(0.02, 1, by = 0.02)
# AQI.desire.quan <- quantile(AQI.desire, probs = probs)
# AQI.nodesire.quan <- quantile(AQI.nodesire, probs = probs)
# res.desire.quan <- quantile(res.desire, probs = probs)
# res.nodesire.quan <- quantile(res.nodesire, probs = probs)
# data.desire.quan <- as.data.frame(cbind(AQI.desire.quan, res.desire.quan))
# data.nodesire.quan <- as.data.frame(cbind(AQI.nodesire.quan, res.nodesire.quan))
# 
# desire.quan.model <- lm(res.desire.quan~AQI.desire.quan, data = data.desire.quan)
# summary(desire.quan.model)
# nodesire.quan.model <- lm(res.nodesire.quan~AQI.nodesire.quan, data = data.nodesire.quan)
# summary(nodesire.quan.model)
# 
# scaled_AQI <- (AQI - min(AQI)) / (max(AQI) - min(AQI)) * (100 - 0) + 0
# AQI.desire <- scaled_AQI[which(wewe['desire'] == 1)]
# AQI.nodesire <- scaled_AQI[which(wewe['desire'] == 0)]
# res.desire <- res.noAQI.NWOO[which(wewe['desire'] == 1)]
# res.nodesire <- res.noAQI.NWOO[which(wewe['desire'] == 0)]
# data.desire <- as.data.frame(cbind(AQI.desire, res.desire))
# data.nodesire <- as.data.frame(cbind(AQI.nodesire, res.nodesire))
# 
# AQI.desire.quan <- quantile(AQI.desire, probs = probs)
# AQI.nodesire.quan <- quantile(AQI.nodesire, probs = probs)
# res.desire.quan <- quantile(res.desire, probs = probs)
# res.nodesire.quan <- quantile(res.nodesire, probs = probs)
# data.desire.quan <- as.data.frame(cbind(AQI.desire.quan, res.desire.quan))
# data.nodesire.quan <- as.data.frame(cbind(AQI.nodesire.quan, res.nodesire.quan))
# 
# desire.quan.model <- lm(res.desire.quan~AQI.desire.quan, data = data.desire.quan)
# summary(desire.quan.model)
# nodesire.quan.model <- lm(res.nodesire.quan~AQI.nodesire.quan, data = data.nodesire.quan)
# summary(nodesire.quan.model)


# ## Main Plot
# desire.df <- data.frame(
#   AQI_quantile = c(AQI.desire.quan, AQI.nodesire.quan), 
#   res_quantile = c(res.desire.quan, res.nodesire.quan),
#   temp = c(rep('desire', 50), rep('nodesire', 50))
# )
# 
# ggplot(desire.df, aes(x = AQI_quantile, y = res_quantile, color = temp, fill = temp)) +
#   geom_point(alpha = 0.7, size = 2) +
#   geom_smooth(method = "lm", se = TRUE, size = 1.2) +
#   scale_color_manual(values = c("desire" = "#e74c3c", "nodesire" = "#3498db")) +
#   scale_fill_manual(values = c("desire" = "#e74c3c", "nodesire" = "#3498db")) +
#   labs(x = expression("AQI quantile"), y = "Residuals") +
#   theme_classic(base_size = 14)


