library(reshape2)
Oly.size <- read.csv("Data/2018-05-04_Oly-Size.csv", header = T, stringsAsFactors = T)
head(Oly.size)
Oly.size.long <- melt(Oly.size[c(-1, -5, -6)], id.vars = c("GROUP", "COHORT", "TREATMENT"), na.action = na.omit)  
Oly.size.long$value <- as.numeric(Oly.size.long$value)
aggregate(value ~ GROUP, Oly.size.long, mean, na.action = na.omit)
aggregate(value ~ GROUP, Oly.size.long, sd, na.action = na.omit)
plot(value ~ GROUP, data=Oly.size.long)
summary(lm(value ~ GROUP-1, data=Oly.size.long)) #mean and SE for each group 
anova(Size.lm <- lm(value ~ COHORT/TREATMENT, data=Oly.size.long))
summary(Size.lm)
View(Oly.size.long)
library(ggplot2)
library(ggpubr)
# install.packages("ggridges")
library(ggridges)

hist(Oly.size.long$value)  
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP",  palette = c("mediumseagreen", "indianred2")) + ggtitle("North Sound Size (mm)\n @ ~9months") 

ggdensity(data=subset(Oly.size.long, COHORT=="HL"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP",  palette = c("mediumseagreen", "indianred2")) + ggtitle("Hood Canal Size (mm)\n @ ~9months")

ggdensity(data=subset(Oly.size.long, COHORT=="SN"), x = "value",
          add = "mean", rug = TRUE,
          color = "GROUP", fill = "GROUP", palette = c("mediumseagreen", "indianred2")) + ggtitle("SOUTH SOUND Size (mm)\n @ ~9months")


ggplot(Oly.size.long, aes(x = value, y = GROUP)) +
  geom_density_ridges(aes(fill = GROUP)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "red"))
