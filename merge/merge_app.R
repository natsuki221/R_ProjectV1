setwd('/Users/lintzujeng/Documents/GitHub/R_Project/merge')
library("tidyverse")
library("openxlsx")

odf <- read.xlsx('orange_market.xlsx')
weather <- read.csv('weather.csv', header = T, row.names = "年.月")


odf$id <- c(1:length(odf$日期))
weather$平均 <- NULL
weather[6,] <- NA
weather2 <- weather[-6,]

weather3 <- stack(weather2)
names(weather2)
