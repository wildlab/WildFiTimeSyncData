require(gridExtra)
library("ggplot2")
library(plotly)
library(RColorBrewer)
library(lubridate)
library(dplyr)

dataWiFi <- read.csv(file = 'stationaryData_WiFi.csv')
dataGPS <- read.csv(file = 'stationaryData_GPS.csv')

#IMPORTANT: use mad() with constant=1

### settings
textSize <- 45

### calculate colors
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

### ------------ WiFi ------------

# convert time diff to number
dataWiFi$NTPminusSYS <- as.numeric(as.character(dataWiFi$NTPminusSYS))

# remove failed synchronization attempts (e.g., timeouts)
dataWiFi <- dataWiFi[!(dataWiFi$NTPminusSYS %in% c(NA)), ]

# reduce data set
dataBroadband <- dataWiFi[(dataWiFi$NetworkClass %in% c("Broadband")), ]
dataLTE <- dataWiFi[(dataWiFi$NetworkClass %in% c("Mobile LTE")), ]
data2G <- dataWiFi[(dataWiFi$NetworkClass %in% c("Mobile 2G")), ]

# results broadband
print("Broadband")
length(dataBroadband$NTPminusSYS)
min(dataBroadband$NTPminusSYS)
max(dataBroadband$NTPminusSYS)
mean(dataBroadband$NTPminusSYS)
sd(dataBroadband$NTPminusSYS)
median(dataBroadband$NTPminusSYS)
mad(dataBroadband$NTPminusSYS,constant=1)

# results LTE
print("Mobile LTE")
length(dataLTE$NTPminusSYS)
min(dataLTE$NTPminusSYS)
max(dataLTE$NTPminusSYS)
mean(dataLTE$NTPminusSYS)
sd(dataLTE$NTPminusSYS)
median(dataLTE$NTPminusSYS)
mad(dataLTE$NTPminusSYS,constant=1)

# results 2G
print("Mobile 2G")
length(data2G$NTPminusSYS)
min(data2G$NTPminusSYS)
max(data2G$NTPminusSYS)
mean(data2G$NTPminusSYS)
sd(data2G$NTPminusSYS)
median(data2G$NTPminusSYS)
mad(data2G$NTPminusSYS,constant=1)

# results complete WiFi
print("Complete")
length(dataWiFi$NTPminusSYS)
min(dataWiFi$NTPminusSYS)
max(dataWiFi$NTPminusSYS)
mean(dataWiFi$NTPminusSYS)
sd(dataWiFi$NTPminusSYS)
median(dataWiFi$NTPminusSYS)
mad(dataWiFi$NTPminusSYS,constant=1)

plotWiFi <- ggplot(data=dataWiFi, aes(x=factor(NetworkClass, level=c('Broadband', 'Mobile LTE', 'Mobile 2G')), y=NTPminusSYS, color=NetworkClass, fill=NetworkClass)) +
  geom_violin(data=dataWiFi, aes(y=NTPminusSYS), width = 1.6, size = 3, fill = NA) +
  geom_boxplot(data=dataWiFi, aes(y=NTPminusSYS), width = 0.025, size = 2) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white") +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Time offset error Toff;WiFi (ms), displayed logarithmically") +
  scale_color_manual(values = c("Broadband"=col_vector[1], "Mobile LTE"=col_vector[3], "Mobile 2G"=col_vector[5])) + 
  scale_fill_manual(values = c("Broadband"=col_vector[1], "Mobile LTE"=col_vector[3], "Mobile 2G"=col_vector[5]))

### ------------ GPS ------------

# convert time diff to number and convert us to ms
dataGPS$PPStoUART <- as.numeric(as.character(dataGPS$PPStoUART)) / 1000.0

# reduce data set
dataGPS <- dataGPS[!(dataGPS$PPStoUART %in% c(NA)), ]

# substract GPS UART compensation from Georg
dataGPS$PPStoUART[dataGPS$Sats<4] <- dataGPS$PPStoUART[dataGPS$Sats<4] - 73.0
dataGPS$PPStoUART[dataGPS$Sats==4] <- dataGPS$PPStoUART[dataGPS$Sats==4] - 73.0
dataGPS$PPStoUART[dataGPS$Sats==5] <- dataGPS$PPStoUART[dataGPS$Sats==5] - 77.0
dataGPS$PPStoUART[dataGPS$Sats==6] <- dataGPS$PPStoUART[dataGPS$Sats==6] - 142.0
dataGPS$PPStoUART[dataGPS$Sats==7] <- dataGPS$PPStoUART[dataGPS$Sats==7] - 155.0
dataGPS$PPStoUART[dataGPS$Sats==8] <- dataGPS$PPStoUART[dataGPS$Sats==8] - 167.0
dataGPS$PPStoUART[dataGPS$Sats==9] <- dataGPS$PPStoUART[dataGPS$Sats==9] - 179.0
dataGPS$PPStoUART[dataGPS$Sats==10] <- dataGPS$PPStoUART[dataGPS$Sats==10] - 194.0
dataGPS$PPStoUART[dataGPS$Sats==11] <- dataGPS$PPStoUART[dataGPS$Sats==11] - 225.0
dataGPS$PPStoUART[dataGPS$Sats>11] <- dataGPS$PPStoUART[dataGPS$Sats>11] - 225.0

# output complete dataset
print("GPS with compensation")
length(dataGPS$PPStoUART)
min(dataGPS$PPStoUART)
max(dataGPS$PPStoUART)
mean(dataGPS$PPStoUART)
sd(dataGPS$PPStoUART)
median(dataGPS$PPStoUART)
mad(dataGPS$PPStoUART,constant=1)

plotGPS <- ggplot(dataGPS, aes(x='Delay-compensated GPS', y=PPStoUART)) +
  geom_violin(width = 1.2, size = 3, fill = NA, color=col_vector[7]) +
  geom_boxplot(width = 0.025, size = 2, fill=col_vector[7], color=col_vector[7]) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white", color=col_vector[7]) +
  #scale_y_continuous(limits = c(-70, 250),breaks=c(-50,0,50,100,150,200)) +
  #scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-100,250), breaks=c(-100,-10,-1,0,1,10,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Time offset error Toff;GPS (ms), displayed logarithmically")


### ------------ PROXIMITY RESYNC ------------

dataProximityResync <- read.csv(file = 'DataProximityResync/proxTimeDiffsGWTime_cropped.csv', dec = "." ,sep=",")

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

proxResyncColor <- "#797878"
plotProximityResync <- ggplot(dataProximityResync, aes(x='Proximity', y=timeDiffBMinusAMs)) +
  geom_violin(width = 1.2, size = 3, fill = NA, color=proxResyncColor) +
  geom_boxplot(width = 0.025, size = 2, fill=proxResyncColor, color=proxResyncColor) +
  stat_summary(fun.y=median, geom="point", shape=23, size=10, stroke=3, fill="white", color=proxResyncColor) +
  #scale_y_continuous(limits = c(-70, 250),breaks=c(-50,0,50,100,150,200)) +
  #scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-100,250), breaks=c(-100,-10,-1,0,1,10,100)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(), limits=c(-7000, 7000), breaks=c(-7000,-1000,-100,-10,-1,0,1,10,100,1000,7000)) +
  theme_minimal() +
  theme(text = element_text(size=textSize)) +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0)) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "grey", size = 1.5, linetype = 3)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))) +
  xlab("") +
  ylab("Relative time error Terror;relative (ms), displayed logarithmically")

### ------------ PROXIMITY 16 DAYS ------------

# NEW PLOT
dataProximity <- read.csv(file = 'DataProximity16Days/proxTimeDiffsGWTime.csv', dec = "." ,sep=",")
dataTagOutside <- read.csv(file = 'DataProximity16Days/mergedsubs_4C_75_25_94_6B_14_burstFormat_123464.csv', dec = "." ,sep=",")
dataTagInside <- read.csv(file = 'DataProximity16Days/mergedsubs_4C_75_25_94_62_80_burstFormat_123464.csv', dec = "." ,sep=",")

# CONVERT TIMESTAMPS OF TAGS
dataProximity$utcDate <- as.POSIXct(as.numeric(dataProximity$mostFrequentTimestampInGroup), origin = '1970-01-01', tz = 'GMT')
dataTagOutside$utcDate <- as.POSIXct(as.numeric(dataTagOutside$utcTimestamp), origin = '1970-01-01', tz = 'GMT')
dataTagInside$utcDate <- as.POSIXct(as.numeric(dataTagInside$utcTimestamp), origin = '1970-01-01', tz = 'GMT')

# (TEMPORARY) REDUCE TIME
#dataProximity <- dataProximity %>% filter(utcDate >= ymd_hms("2023-01-28 01:00:00"))
dataProximity <- dataProximity %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))
dataTagOutside <- dataTagOutside %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))
dataTagInside <- dataTagInside %>% filter(utcDate <= ymd_hms("2024-01-23 18:00:00"))

# FINAL DATA EVALUATION (NOT GROUPED, STATISTIC FROM OVERALL TIME DIFFERENCES BETWEEN TAGS)
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

colorForProximity <- "#5c9090"
colorForTemperature <- "#ff635b"

plotProximity <- ggplot() +
  #geom_point(data=dataProximity, aes(utcDate, timeDiffMs*1000), size=4.5, color=colorForProximity, shape=18, alpha=1) + 
  #geom_line(data=dataProximity, aes(utcDate, timeDiffMs*1000), size=2, color=colorForProximity, alpha=1) +
  geom_area(data=dataProximity, aes(utcDate, timeDiffMs*1000), fill="#a1a1a1") +
  
  geom_line(data=dataTagInside, aes(utcDate, temperatureInDegCel), size=4, color=colorForTemperature, alpha=1) +
  geom_line(data=dataTagOutside, aes(utcDate, temperatureInDegCel), size=4, color=colorForTemperature, alpha=1) +
  
  scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d") + #%Y-%m-%d %H:%M
  scale_y_continuous(limits=c(-10, 60), breaks=c(0,20,40,60),
    sec.axis = sec_axis(~ ., name = "Temperature (?C)", breaks=c(-10,0, 10, 20, 30))) +
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

### ------------ COMBINED PLOT ------------

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

### ------------ QUANTILES ------------

testQuantiles <- c(10,11,12)

print(quantile(testQuantiles))

quant <- quantile(abs(dataGPS$PPStoUART), 0.95)
print(quant)

quant <- quantile(abs(dataWiFi$NTPminusSYS), 0.95)
print(quant)

dataGPSandWiFi <- c(dataGPS$PPStoUART, dataWiFi$NTPminusSYS)
quant <- quantile(abs(dataGPSandWiFi), c(0.75, 0.85, 0.95))
print(quant)

rtcSpecPerDay <- 130*2 # 130 or 260

maxRTCDriftInTenMin <- ((rtcSpecPerDay/24)/6) # max drift of used RTC between 0 and 50?C
quantOnceEveryTenMin <- quant + maxRTCDriftInTenMin
print(quantOnceEveryTenMin)

maxRTCDriftInOneHour <- (rtcSpecPerDay/24) # max drift of used RTC between 0 and 50?C
quantOnceAnHour <- quant + maxRTCDriftInOneHour
print(quantOnceAnHour)

maxRTCDriftInOneDay <- rtcSpecPerDay # max drift of used RTC between 0 and 50?C
quantOnceADay <- quant + maxRTCDriftInOneDay
print(quantOnceADay)

count_entries <- sum(abs(dataGPSandWiFi) <= 55.0272)
print(count_entries)
length(dataGPSandWiFi)

