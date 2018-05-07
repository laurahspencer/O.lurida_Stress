getwd()
Oly.size <- read.csv("Documents/Roberts Lab/O.lurida_Stress/Data/2018-05-04_Oly-Size.csv", header = T, stringsAsFactors = T)
library(reshape2)
Oly.size.long <- melt(Oly.size[-1], id.vars = c("GROUP", "SIZE.CLASS"), na.action = na.omit)  
Oly.size.long$value <- as.numeric(Oly.size.long$value)
aggregate(value ~ GROUP, Oly.size.long, mean, na.action = na.omit)
aggregate(value ~ GROUP, Oly.size.long, sd, na.action = na.omit)
plot(value ~ GROUP, data=Oly.size.long)
summary(lm(value ~ GROUP-1, data=Oly.size.long)) #mean and SE for each group 
anova(Size.lm <- lm(value ~ GROUP, data=Oly.size.long))
summary(Size.lm)
t.test(value ~ GROUP, data=Oly.size.long)

library(ggplot2)
library(ggpubr)
hist(Oly.size.long$value)  
ggdensity(Oly.size.long, x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP",
          palette = c("#0073C2FF", "#FC4E07"))