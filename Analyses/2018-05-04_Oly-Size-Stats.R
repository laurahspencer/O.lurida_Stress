library(reshape2)

Oly.size <- read.csv("Data/2018-05-04_Oly-Size.csv", header = T, stringsAsFactors = T)
Oly.size.long <- melt(Oly.size[c(-4:-5)], id.vars = c("GROUP", "COHORT", "TREATMENT", "TEMP", "PH"), na.action = na.omit) 
Oly.size.long$value <- as.numeric(Oly.size.long$value)

# Create dataframe with pop/treat groups, stocking density 
groups <- c("HL10-AMB", "HL10-LOW", "HL6-AMB", "HL6-LOW", "K10-AMB", "K10-LOW", "K6-AMB", "K6-LOW", "NF10-AMB", "NF10-LOW", "NF6-AMB", "NF6-LOW", "SN10-AMB", "SN10-LOW", "SN6-AMB", "SN6-LOW")
stocked <- c(1311, 1091, 501, 834, 259, 122, 372, 341, 661, 77, 1508, 684, 54, 35, 128, 211)
stock.dens <- data.frame(cbind(groups, stocked), stringsAsFactors = F)
stock.dens$stocked <- as.numeric(stock.dens$stocked)

Oly.size.summary <- cbind(
  aggregate(value ~ GROUP, Oly.size.long, length, na.action = na.omit),
  aggregate(value ~ GROUP, Oly.size.long, mean, na.action = na.omit)[2],
  aggregate(value ~ GROUP, Oly.size.long, sd, na.action = na.omit)[2]
)
colnames(Oly.size.summary) <- c("Group", "Count", "Mean", "SD")

# merge stocking density dataframe with size summary data 
Oly.size.summary2 <- merge(x=Oly.size.summary, y=stock.dens, by.x="Group", by.y="groups", all.x=T, all.y=F)
Oly.size.summary2$Perc.count <- (Oly.size.summary2$Count/Oly.size.summary2$stocked)*100
Oly.size.summary3 <- merge(x=Oly.size.summary2, y=unique(Oly.size[,c("GROUP", "COHORT", "TREATMENT", "TEMP", "PH")]), by.x = "Group", by.y="GROUP", all.x = T, all.y=F)

pch.list <- as.numeric(Oly.size.summary3$TREATMENT)
plot(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, xlab="Stocking Density", ylab = "Mean Length (mm)" , col=Oly.size.summary3$COHORT, pch=pch.list)

library(ggplot2)
ggplot(Oly.size.summary3[which(Oly.size.summary3$stocked<=500),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$stocked<=500),]$Group)
ggplot(Oly.size.summary3[which(Oly.size.summary3$stocked>500),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$stocked>500),]$Group)


plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$Mean, main="Hood Canal,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$Mean, main="North Sound,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$Mean, main="South Sound F1,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$Mean, main="South Sound F2,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$TREATMENT, pch=19, cex=2)

# merge stocking density with full dataframe to include in stats 
Oly.size.long2 <- merge(x=Oly.size.long, y=stock.dens, by.x="GROUP", by.y="groups", all.x=T, all.y=F)
head(Oly.size.long2)

plot(value ~ GROUP, data=Oly.size.long2)
plot(value ~ stocked, data=Oly.size.long2, col=TREATMENT)

summary(lm(value ~ GROUP-1, data=Oly.size.long)) #mean and SE for each group 

anova(Size.lm <- lm(value ~  stocked, data=Oly.size.long2))
anova(Size.lm <- lm(value ~  COHORT, data=Oly.size.long2))
anova(Size.lm <- lm(value ~  TREATMENT, data=Oly.size.long2))

anova(Size.lm <- lm(value ~  COHORT+TREATMENT, data=Oly.size.long2))
anova(Size.lm <- lm(value ~  COHORT+stocked, data=Oly.size.long2))

anova(Size.lm <- lm(value ~  COHORT+TREATMENT+stocked, data=Oly.size.long2))
anova(Size.lm <- lm(value ~  COHORT*TREATMENT*stocked, data=Oly.size.long2))



library(ggplot2)
library(ggpubr)
library(ggridges)

levels(Oly.size.long$GROUP)
colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")

hist(Oly.size.long$value)  

png("Results/NF-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "TREATMENT", fill = "TREATMENT",  palette = colors) + ggtitle("North Sound Size (mm)\n @ ~9months") 
dev.off()

png("Results/HC-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="HL"), x = "value",
          add = "mean", rug = TRUE,
          color = "TREATMENT", fill = "TREATMENT",  palette = colors) + ggtitle("Hood Canal Size (mm)\n @ ~9months")
dev.off()

png("Results/SSF1-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="SN"), x = "value",
          add = "mean", rug = TRUE,
          color = "TREATMENT", fill = "TREATMENT", palette = colors) + ggtitle("SOUTH SOUND F1 Size (mm)\n @ ~9months")
dev.off()

png("Results/SSF2-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="K"), x = "value",
          add = "mean", rug = TRUE,
          color = "TREATMENT", fill = "TREATMENT", palette = colors) + ggtitle("SOUTH SOUND F2 Size (mm)\n @ ~9months")
dev.off()
