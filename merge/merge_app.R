setwd('/Users/lintzujeng/Documents/GitHub/R_Project/merge')
library("tidyverse")
library("openxlsx")

odf <- read.xlsx('orange_market.xlsx')
weather <- read.csv('weather.csv', header = T)


names(odf)[1] = "id"
#weather$平均 <- NULL
#weather[6,] <- NA
weather2 <- weather[-6,]
weather2$平均 <- NULL
weather2$年.月 <- as.numeric(weather2$年.月)

library(stringr)
weather.just <- data.frame(id = c(NA), 氣溫 = c(NA))
for (y in 1:nrow(weather2)) {
  for(m in 1:ncol(weather2)){
    print(weather2[y,m])
    if (is.na(weather2[y, m])){
      print("NA")
      break
    }
    else if (weather2[y, m] > 2000){
      year <- weather2[y, m] - 1911
    }
    else{
      id.y = as.character(year)
      if (m <= 10){
        id.m = str_sub(colnames(weather2)[m], start = 2L, end = 2L)
        id.all = str_c(id.y, "年", "0", id.m, "月")
      }
      else{
        id.m = str_sub(colnames(weather2)[m], start = 2L, end = 3L)
        id.all = str_c(id.y, "年", id.m, "月")
      }
      weather.just[nrow(weather.just) + 1,] = id.all
      weather.just$氣溫[nrow(weather.just)] = weather2[y, m]
    }
  }
}
weather.just$氣溫<-as.numeric(weather.just$氣溫)
weather.just <- na.omit(weather.just)

odf.merged <- inner_join(odf, weather.just, by = "id")

library(writexl)
write_xlsx(odf.merged, "merged_data.xlsx")
