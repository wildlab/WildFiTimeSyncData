require(gridExtra)
library("ggplot2")
library(plotly)
library(RColorBrewer)
library(lubridate)
library(dplyr)

### ------------ SETTINGS ------------

# plot settings
textSize <- 20

# colors
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colorForProximity <- "#5c9090"
colorForTemperature <- "#ff635b"
colorForProxResync <- "#797878"

### ------------ WIFI ------------

# read data
dataWiFi <- read.csv(file = 'stationaryData_WiFi.csv')

# convert time difference to ground truth to number
dataWiFi$timeDifferenceTo1PPSms <- as.numeric(as.character(dataWiFi$timeDifferenceTo1PPSms))

# remove failed synchronization attempts (e.g., WiFi connection timeouts)
dataWiFi <- dataWiFi[!(dataWiFi$timeDifferenceTo1PPSms %in% c(NA)), ]

# split data set based on internet access
dataBroadband <- dataWiFi[(dataWiFi$networkClass %in% c("Broadband")), ]
dataLTE <- dataWiFi[(dataWiFi$networkClass %in% c("Mobile LTE")), ]
data2G <- dataWiFi[(dataWiFi$networkClass %in% c("Mobile 2G")), ]

# results broadband
print("Broadband")
length(dataBroadband$timeDifferenceTo1PPSms)
min(dataBroadband$timeDifferenceTo1PPSms)
max(dataBroadband$timeDifferenceTo1PPSms)
mean(dataBroadband$timeDifferenceTo1PPSms)
sd(dataBroadband$timeDifferenceTo1PPSms)
median(dataBroadband$timeDifferenceTo1PPSms)
mad(dataBroadband$timeDifferenceTo1PPSms,constant=1)

# results LTE
print("Mobile LTE")
length(dataLTE$timeDifferenceTo1PPSms)
min(dataLTE$timeDifferenceTo1PPSms)
max(dataLTE$timeDifferenceTo1PPSms)
mean(dataLTE$timeDifferenceTo1PPSms)
sd(dataLTE$timeDifferenceTo1PPSms)
median(dataLTE$timeDifferenceTo1PPSms)
mad(dataLTE$timeDifferenceTo1PPSms,constant=1)

# results 2G
print("Mobile 2G")
length(data2G$timeDifferenceTo1PPSms)
min(data2G$timeDifferenceTo1PPSms)
max(data2G$timeDifferenceTo1PPSms)
mean(data2G$timeDifferenceTo1PPSms)
sd(data2G$timeDifferenceTo1PPSms)
median(data2G$timeDifferenceTo1PPSms)
mad(data2G$timeDifferenceTo1PPSms,constant=1)

# results complete WiFi
print("Complete")
length(dataWiFi$timeDifferenceTo1PPSms)
min(dataWiFi$timeDifferenceTo1PPSms)
max(dataWiFi$timeDifferenceTo1PPSms)
mean(dataWiFi$timeDifferenceTo1PPSms)
sd(dataWiFi$timeDifferenceTo1PPSms)
median(dataWiFi$timeDifferenceTo1PPSms)
mad(dataWiFi$timeDifferenceTo1PPSms,constant=1)

plotWiFi <- ggplot(data=dataWiFi, aes(x=factor(networkClass, level=c('Broadband', 'Mobile LTE', 'Mobile 2G')), y=timeDifferenceTo1PPSms, color=networkClass, fill=networkClass)) +
  geom_violin(data=dataWiFi, aes(y=timeDifferenceTo1PPSms), width = 1.6, size = 3, fill = NA) +
  geom_boxplot(data=dataWiFi, aes(y=timeDifferenceTo1PPSms), width = 0.025, size = 2) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white") +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Time offset error Toff;WiFi (ms), displayed logarithmically") +
  scale_color_manual(values = c("Broadband"=col_vector[1], "Mobile LTE"=col_vector[3], "Mobile 2G"=col_vector[5])) + 
  scale_fill_manual(values = c("Broadband"=col_vector[1], "Mobile LTE"=col_vector[3], "Mobile 2G"=col_vector[5]))

### ------------ GPS ------------

# read data
dataGPS <- read.csv(file = 'stationaryData_GPS.csv')

# convert time difference to number and convert us to ms
dataGPS$timeDifferenceTo1PPSus <- as.numeric(as.character(dataGPS$timeDifferenceTo1PPSus)) / 1000.0

# reduce data set, removing GPS timeouts
dataGPS <- dataGPS[!(dataGPS$timeDifferenceTo1PPSus %in% c(NA)), ]

# substract GPS UART delay compensation for Quectel L70
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites<4] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites<4] - 73.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==4] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==4] - 73.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==5] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==5] - 77.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==6] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==6] - 142.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==7] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==7] - 155.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==8] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==8] - 167.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==9] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==9] - 179.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==10] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==10] - 194.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==11] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites==11] - 225.0
dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites>11] <- dataGPS$timeDifferenceTo1PPSus[dataGPS$GPSNumberOfSatellites>11] - 225.0

# output complete dataset
print("GPS with compensation")
length(dataGPS$timeDifferenceTo1PPSus)
min(dataGPS$timeDifferenceTo1PPSus)
max(dataGPS$timeDifferenceTo1PPSus)
mean(dataGPS$timeDifferenceTo1PPSus)
sd(dataGPS$timeDifferenceTo1PPSus)
median(dataGPS$timeDifferenceTo1PPSus)
mad(dataGPS$timeDifferenceTo1PPSus,constant=1)

plotGPS <- ggplot(dataGPS, aes(x='Delay-compensated GPS', y=timeDifferenceTo1PPSus)) +
  geom_violin(width = 1.2, size = 3, fill = NA, color=col_vector[7]) +
  geom_boxplot(width = 0.025, size = 2, fill=col_vector[7], color=col_vector[7]) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white", color=col_vector[7]) +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Time offset error Toff;GPS (ms), displayed logarithmically")


### ------------ PROXIMITY RESYNC ------------

dataProximityResync <- read.csv(file = 'stationaryData_Proximity.csv', dec = "." ,sep=",")

dataProximityResync$timeDiffBMinusAMs <- -(dataProximityResync$timeDiffBMinusAMs * 1000)

# output complete dataset
print("Proximity Resync")
length(dataProximityResync$timeDiffBMinusAMs)
min(dataProximityResync$timeDiffBMinusAMs)
max(dataProximityResync$timeDiffBMinusAMs)
mean(dataProximityResync$timeDiffBMinusAMs)
sd(dataProximityResync$timeDiffBMinusAMs)
median(dataProximityResync$timeDiffBMinusAMs)
mad(dataProximityResync$timeDiffBMinusAMs,constant=1)

plotProximityResync <- ggplot(dataProximityResync, aes(x='Proximity', y=timeDiffBMinusAMs)) +
  geom_violin(width = 1.2, size = 3, fill = NA, color=colorForProxResync) +
  geom_boxplot(width = 0.025, size = 2, fill=proxResyncColor, color=proxResyncColor) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white", color=proxResyncColor) +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Relative time error Terror;relative (ms), displayed logarithmically")

### ------------ PROXIMITY 16 DAYS EXPERIMENT ------------

# read data
dataProximity <- read.csv(file = 'stationaryData_Proximity_16d_Experiment.csv', dec = "." ,sep=",")
dataTagOutside <- read.csv(file = 'stationaryData_Proximity_16d_Experiment_Tag_6B14.csv', dec = "." ,sep=",")
dataTagInside <- read.csv(file = 'stationaryData_Proximity_16d_Experiment_Tag_6280.csv', dec = "." ,sep=",")

# convert timestamps of tags
dataProximity$utcDate <- as.POSIXct(as.numeric(dataProximity$mostFrequentTimestampInGroup), origin = '1970-01-01', tz = 'GMT')
dataTagOutside$utcDate <- as.POSIXct(as.numeric(dataTagOutside$utcTimestamp), origin = '1970-01-01', tz = 'GMT')
dataTagInside$utcDate <- as.POSIXct(as.numeric(dataTagInside$utcTimestamp), origin = '1970-01-01', tz = 'GMT')

# crop data at end of experiment
dataProximity <- dataProximity %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))
dataTagOutside <- dataTagOutside %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))
dataTagInside <- dataTagInside %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))

# final data evaluation (not grouped, statistic from overall time differences between tags)
length(dataProximity$timeDiffMs)
median(dataProximity$timeDiffMs)
mad(dataProximity$timeDiffMs,constant=1)
min(dataProximity$timeDiffMs)
max(dataProximity$timeDiffMs)
mean(dataProximity$timeDiffMs)
sd(dataProximity$timeDiffMs)
tail(dataProximity, n=1)

min(dataTagInside$temperatureInDegCel)
max(dataTagInside$temperatureInDegCel)
min(dataTagOutside$temperatureInDegCel)
max(dataTagOutside$temperatureInDegCel)

plotProximity <- ggplot() +
  geom_area(data=dataProximity, aes(utcDate, timeDiffMs*1000), fill="#a1a1a1") +
  
  geom_line(data=dataTagInside, aes(utcDate, temperatureInDegCel), size=4, color=colorForTemperature, alpha=1) +
  geom_line(data=dataTagOutside, aes(utcDate, temperatureInDegCel), size=4, color=colorForTemperature, alpha=1) +
  
  scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d") + #%Y-%m-%d %H:%M
  scale_y_continuous(limits=c(-10, 60), breaks=c(0,20,40,60),
    sec.axis = sec_axis(~ ., name = "Temperature (deg C)", breaks=c(-10,0, 10, 20, 30))) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0)) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x = element_line(color = "grey", size = 1.5, linetype = 1)) +
  theme(panel.grid.major.y = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 50))) +
  theme(axis.text.y.right = element_text(color = colorForTemperature), axis.title.y.right = element_text(color=colorForTemperature)) +
  xlab("") +
  ylab("Relative time error Terror;relative (ms)") +
  labs(size='#Tags(t)')

### ------------ COMBINED PLOT: WIFI, GPS, PROXIMITY ------------

grid.arrange(
  arrangeGrob(
    plotGPS,plotWiFi,plotProximityResync,
    ncol = 3,
    widths=c(0.25, 0.5, 0.25)
  ),
  plotProximity,
  ncol = 1,
  heights = c(0.6, 0.4)
)

### ------------ QUANTILE CALCULATIONS ------------

# 95% quantile of GPS data
quant <- quantile(abs(dataGPS$timeDifferenceTo1PPSus), 0.95)
print(quant)

# 95% quantile of WiFi data
quant <- quantile(abs(dataWiFi$timeDifferenceTo1PPSms), 0.95)
print(quant)

# quantiles of both methods combined
dataGPSandWiFi <- c(dataGPS$timeDifferenceTo1PPSus, dataWiFi$timeDifferenceTo1PPSms)
quant <- quantile(abs(dataGPSandWiFi), c(0.75, 0.85, 0.95))
print(quant)

# quantiles for RTC between 0 and 50deg C
print("Quantiles between 0 and 50deg C:")
rtcSpecPerDay <- 130 # max. drift in ms

maxRTCDriftInTenMin <- ((rtcSpecPerDay/24)/6)
quantOnceEveryTenMin <- quant + maxRTCDriftInTenMin
print(quantOnceEveryTenMin)

maxRTCDriftInOneHour <- (rtcSpecPerDay/24)
quantOnceAnHour <- quant + maxRTCDriftInOneHour
print(quantOnceAnHour)

maxRTCDriftInOneDay <- rtcSpecPerDay
quantOnceADay <- quant + maxRTCDriftInOneDay
print(quantOnceADay)

# quantiles for RTC between -40 and 85deg C
print("Quantiles between -40 and 85deg C:")
rtcSpecPerDay <- 260 # max. drift in ms

maxRTCDriftInTenMin <- ((rtcSpecPerDay/24)/6)
quantOnceEveryTenMin <- quant + maxRTCDriftInTenMin
print(quantOnceEveryTenMin)

maxRTCDriftInOneHour <- (rtcSpecPerDay/24)
quantOnceAnHour <- quant + maxRTCDriftInOneHour
print(quantOnceAnHour)

maxRTCDriftInOneDay <- rtcSpecPerDay
quantOnceADay <- quant + maxRTCDriftInOneDay
print(quantOnceADay)
