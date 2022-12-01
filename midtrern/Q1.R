setwd('/Users/lintzujeng/Desktop/Work/R-Project/midtrern/')

babies <- read.table('1-babies_wrong.txt', header = T)
#babies <- na.omit(babies)
b <- split(babies, babies$smoke)
range(na.omit(babies$weight))
frequency(which(babies$weight==250))
which(babies$weight==250)
babies[150,"smoke"] <- 0
range(na.omit(babies$smoke))
which(babies$smoke=='A')
babies[36,]
babies[36, "smoke"] <- NA
#weight.NA <- data.frame(babies[which(is.na(babies$weight)),])
babies2 <- babies[which(!is.na(babies$weight)),]
babies2 <- babies2[which(!is.na(babies$smoke)),]
range(babies2$smoke)
b <- split(babies2, babies2$smoke)

for (i in b) {
  print(max(i$weight))
}
