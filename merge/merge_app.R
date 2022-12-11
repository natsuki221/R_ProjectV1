#setwd('/Users/lintzujeng/Documents/GitHub/R_Project/merge')
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
weather.just <- na.omit(weather.just)
weather.just$氣溫<-as.numeric(weather.just$氣溫)


odf.merged <- inner_join(odf, weather.just, by = "id")

temp.ranges <- table(cut(odf.merged$氣溫, breaks = seq(15, 30, by = 2)))
temp.trade <- numeric(7)
for (trade in 1:nrow(odf.merged)) {
  if (odf.merged$氣溫[trade] <= 17){
    temp.trade[1] = temp.trade[1] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 17 && odf.merged$氣溫[trade] <= 19){
    temp.trade[2] = temp.trade[2] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 19 && odf.merged$氣溫[trade] <= 21){
    temp.trade[3] = temp.trade[3] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 21 && odf.merged$氣溫[trade] <= 23){
    temp.trade[4] = temp.trade[4] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 23 && odf.merged$氣溫[trade] <= 25){
    temp.trade[5] = temp.trade[5] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 25 && odf.merged$氣溫[trade] <= 27){
    temp.trade[6] = temp.trade[6] + odf.merged$`交易金額(元)`[trade]
  }
  else if (odf.merged$氣溫[trade] > 27 && odf.merged$氣溫[trade] <= 29){
    temp.trade[7] = temp.trade[7] + odf.merged$`交易金額(元)`[trade]
  }
  
}
temp.df <- data.frame(temp.ranges, temp.trade)

hist.plot <- ggplot(data = temp.df, aes(x = Var1, y = temp.trade)) +
  geom_bar(stat = "identity") + 
  xlab("氣溫(度)") +
  ylab("交易金額(元)")
hist.plot

ggsave("hist.png", hist.plot)
library(writexl)
write_xlsx(odf.merged, "merged_data.xlsx")
