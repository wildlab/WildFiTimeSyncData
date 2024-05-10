require(gridExtra)
library("ggplot2")
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(ggExtra)

### ------------ DATA EVALUATION ------------

# read data
dataTimeDiffA <- read.csv(file = 'rousettusGatewayData_E4C0.csv', dec = "." ,sep=",")
dataTimeDiffA$gateway = "A"
dataTimeDiffB <- read.csv(file = 'rousettusGatewayData_E39C.csv', dec = "." ,sep=",")
dataTimeDiffB$gateway = "B"
dataTimeDiffC <- read.csv(file = 'rousettusGatewayData_F6A8.csv', dec = "." ,sep=",")
dataTimeDiffC$gateway = "C"
dataTimeDiffD <- read.csv(file = 'rousettusGatewayData_F630.csv', dec = "." ,sep=",")
dataTimeDiffD$gateway = "D"
dataTimeDiff <- rbind(dataTimeDiffA, dataTimeDiffB, dataTimeDiffC, dataTimeDiffD)

# convert timestamps of tags
dataTimeDiff$utcDate <- as.POSIXct(as.numeric(dataTimeDiff$mostFrequentTimestampInGroup), origin = '1970-01-01', tz = 'GMT')

# reduce to all deployed tags
ids <- c("0010", "0018", "0038", "0054", "0080", "F7FC", "F80C", "F810", "F838", "F850", "F858", "F85C", "F860", "F868", "F884", "F888", "F8C0", "F918", "F928", "F940", "F944", "F954", "F95C", "F978", "F980", "F9B4", "F9C0", "F9D8", "F9E8", "F9F4", "F9F8", "FA08", "FAA8", "FAAC", "FAF0", "FB0C", "FB70", "FB98", "FE60", "FE70", "FE74", "FE80", "FEAC", "FECC", "FEFC", "FF1C", "FF8C", "FF98", "FFCC", "FFD4", "FFD8", "FFFF", "0008", "0024", "0028", "0030", "003C", "007C", "F808", "F834", "F840", "F848", "F870", "F88C", "F8A0", "F8AC", "F8EC", "F900", "F93C", "F948", "F974", "F98C", "F998", "F9B8", "F9CC", "F9F0", "FA18", "FA6C", "FA78", "FA8C", "FA9C", "FAE8", "FB18", "FB1C", "FB28", "FB78", "FE9C", "FF2C", "FFBC", "FFC8", "FF60", "F86C", "F994", "FAD8", "FE8C", "FFF4", "FEBC", "FED0")
length(ids)
dataTimeDiff <- dataTimeDiff[dataTimeDiff$tagA %in% ids, ]
dataTimeDiff <- dataTimeDiff[dataTimeDiff$tagB %in% ids, ]

# initial data evaluation, not grouped, statistic from overall time differences, but from all four receiving gateway (received messages at same time)
length(dataTimeDiff$timeDiffSeconds)
median(dataTimeDiff$timeDiffSeconds)
mad(dataTimeDiff$timeDiffSeconds,constant=1)
min(dataTimeDiff$timeDiffSeconds)
max(dataTimeDiff$timeDiffSeconds)
mean(dataTimeDiff$timeDiffSeconds)
sd(dataTimeDiff$timeDiffSeconds)

# compare data evaluation with per gateway A, B, C and D
median(dataTimeDiff[dataTimeDiff$gateway=="A",]$timeDiffSeconds)
mad(dataTimeDiff[dataTimeDiff$gateway=="A",]$timeDiffSeconds,constant=1)
median(dataTimeDiff[dataTimeDiff$gateway=="B",]$timeDiffSeconds)
mad(dataTimeDiff[dataTimeDiff$gateway=="B",]$timeDiffSeconds,constant=1)
median(dataTimeDiff[dataTimeDiff$gateway=="C",]$timeDiffSeconds)
mad(dataTimeDiff[dataTimeDiff$gateway=="C",]$timeDiffSeconds,constant=1)
median(dataTimeDiff[dataTimeDiff$gateway=="D",]$timeDiffSeconds)
mad(dataTimeDiff[dataTimeDiff$gateway=="D",]$timeDiffSeconds,constant=1)

# order tag ids to be able to group them (smaller ID comes first)
dataTimeDiff <- dataTimeDiff %>%
  mutate(
    should_switch = tagA > tagB,
    tagASorted = ifelse(should_switch, tagB, tagA),
    tagBSorted = ifelse(should_switch, tagA, tagB),
    timeDiffBMinusAMsSorted = ifelse(should_switch, -timeDiffBMinusAMs, timeDiffBMinusAMs)
  ) %>%
  select(-should_switch)

# now in case multiple gateways measured time differences at the same time: take the lowest time delay, as message collisions can cause delays, so the minimum will be most accurate
dataTimeDiffMinOfGWs <- dataTimeDiff %>%
  group_by(mostFrequentTimestampInGroup, tagASorted, tagBSorted) %>%
  slice(which.min(timeDiffSeconds))
nrow(dataTimeDiff)
nrow(dataTimeDiffMinOfGWs)

# example test if actual minimum is taken (so slicing worked)
testRow <- dataTimeDiff %>% filter(mostFrequentTimestampInGroup == 1674786900, tagASorted == "FE70", tagBSorted == "FE80")
print(testRow)
testRow2 <- dataTimeDiffMinOfGWs %>% filter(mostFrequentTimestampInGroup == 1674786900, tagASorted == "FE70", tagBSorted == "FE80")
print(testRow2)
testRow <- dataTimeDiff %>% filter(mostFrequentTimestampInGroup == 1674787500, tagASorted == "FE80", tagBSorted == "FEAC")
print(testRow)
testRow2 <- dataTimeDiffMinOfGWs %>% filter(mostFrequentTimestampInGroup == 1674787500, tagASorted == "FE80", tagBSorted == "FEAC")
print(testRow2)

# final data evaluation (grouped, statistic from overall time differences between tags)
length(dataTimeDiffMinOfGWs$timeDiffSeconds)
median(dataTimeDiffMinOfGWs$timeDiffSeconds)
mad(dataTimeDiffMinOfGWs$timeDiffSeconds,constant=1)
min(dataTimeDiffMinOfGWs$timeDiffSeconds)
max(dataTimeDiffMinOfGWs$timeDiffSeconds)
mean(dataTimeDiffMinOfGWs$timeDiffSeconds)
sd(dataTimeDiffMinOfGWs$timeDiffSeconds)

# group results by timestamp
dataTimeDiffMinOfGWsGrouped <- dataTimeDiffMinOfGWs %>%
  group_by(mostFrequentTimestampInGroup) %>%
  summarise(
    medianTimeDiff = median(timeDiffSeconds),
    madTimeDiff = mad(timeDiffSeconds, constant=1),
    measCnt = n(),
    uniqueTagIds = length(unique(c(tagA,tagB)))
  )

# convert new timestamps of tage
dataTimeDiffMinOfGWsGrouped$utcDate <- as.POSIXct(as.numeric(dataTimeDiffMinOfGWsGrouped$mostFrequentTimestampInGroup), origin = '1970-01-01', tz = 'GMT')

# group results by timestamps
dataTimeDiffGrouped <- dataTimeDiff %>%
  group_by(mostFrequentTimestampInGroup, gateway) %>%
  summarise(
    medianTimeDiff = median(timeDiffSeconds),
    madTimeDiff = mad(timeDiffSeconds, constant=1),
    measCnt = n(),
    uniqueTagIds = length(unique(c(tagA,tagB)))
  )

# convert new timestamps of tags
dataTimeDiffGrouped$utcDate <- as.POSIXct(as.numeric(dataTimeDiffGrouped$mostFrequentTimestampInGroup), origin = '1970-01-01', tz = 'GMT')

# evaluate max and min numbers of tags participating in proximity detection event at same time
max(dataTimeDiffGrouped$uniqueTagIds)
min(dataTimeDiffGrouped$uniqueTagIds)

# reduce to gateways
dataTimeDiffGroupedA <- dataTimeDiffGrouped[dataTimeDiffGrouped$gateway == "A", ]
dataTimeDiffGroupedB <- dataTimeDiffGrouped[dataTimeDiffGrouped$gateway == "B", ]
dataTimeDiffGroupedC <- dataTimeDiffGrouped[dataTimeDiffGrouped$gateway == "C", ]
dataTimeDiffGroupedD <- dataTimeDiffGrouped[dataTimeDiffGrouped$gateway == "D", ]