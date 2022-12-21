setwd('/Users/lintzujeng/Documents/GitHub/R_ProjectV1/final/gulf')
library(openxlsx)
library(tidyverse)

gulf <- read.xlsx('GulfProp.xlsx')
gulf.name <- c(gulf[1,])
gulf = data.frame(lapply(gulf[2:41,], as.numeric))
names(gulf) <- gulf.name
gulf.view <- gulf[,1:3]
gulf.no.view <- gulf[,4:6]
gulf.no.view = na.omit(gulf.no.view)

#At 95% confidence, alpha = 0.05 and alpha/2 = 0.025
#t(0.025) is based on n-1 == 40 - 1 = 39 degrees of freedom.
v.t.val = round(qt(0.025, 39, lower.tail = F), 3)

#At 95% confidence, alpha = 0.05 and alpha/2 = 0.025
#t(0.025) is based on n-1 == 18 - 1 = 17 degrees of freedom.
gulf.nv.t.val = round(qt(0.025, 17, lower.tail = F), 3)

#interval estimate --> xBar +- t(0.025) * s / sqrt(n)
interval.estimate <- function(m){
  interval <- c(NA, NA)
  mean <- mean(m)
  sd <- sd(m)
  n = length(m)
  t.val = qt(0.025, n-1, lower.tail = F)
  in.es <- t.val * sd / sqrt(n)
  interval[1] = round(mean - in.es, 2)
  interval[2] = round(mean + in.es, 2)
  
  return(interval)
}

################################--Question4--###################################
v.price.interval = interval.estimate(gulf.view$`Sale Price`)
v.days.of.sell = interval.estimate(gulf.view$`Days to Sell`)

################################--Question5--###################################
nv.price.interval = interval.estimate(gulf.no.view$`Sale Price`)
nv.days.of.sell = interval.estimate(gulf.no.view$`Days to Sell`)

################################--Question6--###################################
#margin of error E = z(alpha/2) * sd / sqrt(n), so n = (E / z(alpha/2) * sd) ^ 2
v.z = qnorm(0.05/2, lower.tail = F)
v.nss = round((gulf.v.z * gulf.v.sd / gulf.v.in.es) ** 2)

#margin of error E = z(alpha/2) * sd / sqrt(n), so n = (E / z(alpha/2) * sd) ^ 2
gulf.nv.z = qnorm(0.05/2, lower.tail = F)
gulf.nv.nss = round((gulf.nv.z * gulf.nv.sd / gulf.nv.in.es) ** 2)
