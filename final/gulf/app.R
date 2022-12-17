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
gulf.v.t.val = round(qt(0.025, 39, lower.tail = F), 3)

#interval estimate --> xBar +- t(0.025) * s / sqrt(n)
gulf.v.mean <- mean(gulf.view$`Sale Price`)
gulf.v.sd <- sd(gulf.view$`Sale Price`)
gulf.v.n <- nrow(gulf.view)
gulf.v.in.es = gulf.v.t.val * gulf.v.sd / sqrt(gulf.v.n)
gulf.v.posi.in.es = round((gulf.v.mean + gulf.v.in.es) * 1000, 2)
gulf.v.nega.in.es = round((gulf.v.mean - gulf.v.in.es) * 1000, 2)

#margin of error E = z(alpha/2) * sd / sqrt(n), so n = (E / z(alpha/2) * sd) ^ 2
gulf.v.z = qnorm(0.05/2, lower.tail = F)
gulf.v.nss = round((gulf.v.z * gulf.v.sd / gulf.v.in.es) ** 2)

#At 95% confidence, alpha = 0.05 and alpha/2 = 0.025
#t(0.025) is based on n-1 == 18 - 1 = 17 degrees of freedom.
gulf.nv.t.val = round(qt(0.025, 17, lower.tail = F), 3)

#interval estimate --> xBar +- t(0.025) * s / sqrt(n)
gulf.nv.mean <- mean(gulf.no.view$`Sale Price`)
gulf.nv.sd <- sd(gulf.no.view$`Sale Price`)
gulf.nv.n <- nrow(gulf.no.view)
gulf.nv.in.es = gulf.nv.t.val * gulf.nv.sd / sqrt(gulf.nv.n)
gulf.nv.posi.in.es = round((gulf.nv.mean + gulf.nv.in.es) * 1000, 2)
gulf.nv.nega.in.es = round((gulf.nv.mean - gulf.nv.in.es) * 1000, 2)

#margin of error E = z(alpha/2) * sd / sqrt(n), so n = (E / z(alpha/2) * sd) ^ 2
gulf.nv.z = qnorm(0.05/2, lower.tail = F)
gulf.nv.nss = round((gulf.nv.z * gulf.nv.sd / gulf.nv.in.es) ** 2)
