library(reshape2)
View(Oly.size)
Oly.size <- read.csv("Data/2018-05-04_Oly-Size.csv", header = T, stringsAsFactors = T)
tail(Oly.size)
Oly.size.long <- melt(Oly.size, id.vars = c("GROUP", "COHORT", "TREATMENT"), na.action = na.omit)  
Oly.size.long$value <- as.numeric(Oly.size.long$value)
aggregate(value ~ GROUP, Oly.size.long, mean, na.action = na.omit)
aggregate(value ~ GROUP, Oly.size.long, sd, na.action = na.omit)
plot(value ~ GROUP, data=Oly.size.long)
summary(lm(value ~ GROUP-1, data=Oly.size.long)) #mean and SE for each group 
anova(Size.lm <- lm(value ~  COHORT/TREATMENT, data=Oly.size.long))
summary(Size.lm)

library(ggplot2)
library(ggpubr)
# install.packages("ggridges")
library(ggridges)

hist(Oly.size.long$value)  
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP",  palette = c("mediumseagreen", "indianred2", "blue", "orange")) + ggtitle("North Sound Size (mm)\n @ ~9months") 

ggdensity(data=subset(Oly.size.long, COHORT=="HL"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP",  palette = c("mediumseagreen", "indianred2", "blue", "orange")) + ggtitle("Hood Canal Size (mm)\n @ ~9months")

ggdensity(data=subset(Oly.size.long, COHORT=="SN"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP", palette = c("mediumseagreen", "indianred2", "blue", "orange")) + ggtitle("SOUTH SOUND F1 Size (mm)\n @ ~9months")

ggdensity(data=subset(Oly.size.long, COHORT=="K"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP", palette = c("mediumseagreen", "indianred2")) + ggtitle("SOUTH SOUND F2 Size (mm)\n @ ~9months")
