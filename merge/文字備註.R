setwd('/Users/lintzujeng/Documents/GitHub/R_Project/merge')
#指定資料夾
library("tidyverse")
library("openxlsx")
#導入package

odf <- read.xlsx('orange_market.xlsx')
weather <- read.csv('weather.csv', header = T)
#導入資料

names(odf)[1] = "id"
#將日期設為id
weather2 <- weather[-6,]
weather2$平均 <- NULL
weather2$年.月 <- as.numeric(weather2$年.月)
#將氣溫資料選出

library(stringr)
#導入字串調整package
weather.just <- data.frame(id = c(NA), 氣溫 = c(NA))
#建立新dataframe<weather.just>
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
# Normalize id
weather.just <- na.omit(weather.just)
# omit NA
weather.just$氣溫<-as.numeric(weather.just$氣溫)


odf.merged <- inner_join(odf, weather.just, by = "id")
#join 兩個 dataframes

temp.ranges <- table(cut(odf.merged$氣溫, breaks = seq(15, 30, by = 2)))
#氣溫數值分區（水桶）
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
#將交易金額summarize到水桶裡
temp.df <- data.frame(temp.ranges, temp.trade)
#整理成一個dataframe
hist.plot <- ggplot(data = temp.df, aes(x = Var1, y = temp.trade)) +
  geom_bar(stat = "identity") + 
  xlab("氣溫(度)") +
  ylab("交易金額(元)")
hist.plot
#繪圖

ggsave("hist.png", hist.plot)
library(writexl)
write_xlsx(odf.merged, "merged_data.xlsx")
#輸出 histogram 和 merged excel