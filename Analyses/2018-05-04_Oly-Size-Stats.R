library(reshape2)
library(rcompanion)
library(car)

Oly.size <- read.csv("Data/9-month-Oly-Size.csv", header = T, stringsAsFactors = T)
Oly.size.long <- melt(Oly.size[c(-1, -7, -8)], id.vars = c("GROUP", "COHORT", "TREATMENT", "TEMP", "PH"), na.action = na.omit) 
Oly.size.long$value <- as.numeric(Oly.size.long$value)
Oly.size.long$TEMP <- as.factor(Oly.size.long$TEMP)

# Create dataframe with pop/treat groups, stocking density 
groups <- c("HL10-AMB", "HL10-LOW", "HL6-AMB", "HL6-LOW", "K10-AMB", "K10-LOW", "K6-AMB", "K6-LOW", "NF10-AMB", "NF10-LOW", "NF6-AMB", "NF6-LOW", "SN10-AMB", "SN10-LOW", "SN6-AMB", "SN6-LOW",  "SN10-AMB-Exp", "SN10-LOW-Exp", "SN6-AMB-Exp", "SN6-LOW-Exp")
stocked <- c(1311, 1091, 501, 834, 259, 122, 372, 341, 661, 77, 720, 684, 54, 35, 128, 211, 4, 19, 68, 194)
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

write.csv(file="data/Oly.10-month-size.summary.csv", x=Oly.size.summary3)

pch.list <- as.numeric(Oly.size.summary3$TREATMENT)
plot(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, xlab="Stocking Density", ylab = "Mean Length (mm)" , col=Oly.size.summary3$COHORT, pch=pch.list)

library(ggplot2)
ggplot(Oly.size.summary3, aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3$Group)
ggplot(Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$Group)
ggplot(Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$Group)
ggplot(Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$Group)
ggplot(Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),], aes(x=stocked, y=Mean)) + geom_point() + geom_text(label=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$Group)


plot(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, main="All Pops,\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3$PH, pch=19, cex=2)


plot(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, main="All Pops,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$Mean, main="Hood Canal,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="HL"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$Mean, main="North Sound,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="NF"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$Mean, main="South Sound F1,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="SN"),]$TREATMENT, pch=19, cex=2)
plot(x=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$stocked, y=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$Mean, main="South Sound F2,\nmean length ~ stocking density", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3[which(Oly.size.summary3$COHORT=="K"),]$TREATMENT, pch=19, cex=2)

# merge stocking density with full dataframe to include in stats 
Oly.size.long2 <- merge(x=Oly.size.long, y=stock.dens, by.x="GROUP", by.y="groups", all.x=T, all.y=F)
plot(value ~ stocked, data=Oly.size.long2, col=COHORT)

#Assess distribution of length data 
hist(subset(Oly.size.long2, TEMP==10)$value) #right skewed 
hist(subset(Oly.size.long2, TEMP==6)$value) #right skewed 

shapiro.test(subset(Oly.size.long2, TEMP==6)$value) #not normal 
shapiro.test(log(subset(Oly.size.long2, TEMP==6)$value)) #still not normal 

# plot length histogram & overlay normal distribution - definitely not normal. Looks like gamma distribution. 
hist(subset(Oly.size.long2, TEMP==6)$value, breaks=2:22, col="green", main="")
lines(seq(2,22,0.1), length(subset(Oly.size.long2, TEMP==6 & value != "NA")$value)*dnorm(seq(2,22,.1), mean(subset(Oly.size.long2, TEMP==6 & value != "NA")$value), sqrt(var(subset(Oly.size.long2, TEMP==6 & value != "NA")$value))))

# replot histogram, and overlay gamma probability distribution. First calculate distribution's rate and shape (rate = mean/var, shape = rate*mean). Also need max length to assign bins and gamma dist. range 
length.6.rate <- mean(subset(Oly.size.long2, TEMP==6 & value != "NA")$value) / var(subset(Oly.size.long2, TEMP==6 & value != "NA")$value)
length.6.shape <- length.6.rate*mean(subset(Oly.size.long2, TEMP==6 & value != "NA")$value)
max(subset(Oly.size.long2, TEMP==6 & value != "NA")$value) #need max for hist. bins 
hist(subset(Oly.size.long2, TEMP==6 & value != "NA")$value, breaks=-0.5:22.5, col="green", main="") #plot histogram
lines(seq(0.01,22,0.1), length(subset(Oly.size.long2, TEMP==6 & value != "NA")$value)*dgamma(seq(0.01,22,0.1),length.6.shape,length.6.rate)) #overlay gamma distribution density function 

# do same for 10C group
length.10.rate <- mean(subset(Oly.size.long2, TEMP==10 & value != "NA")$value) / var(subset(Oly.size.long2, TEMP==10 & value != "NA")$value)
length.10.shape <- length.10.rate*mean(subset(Oly.size.long2, TEMP==10 & value != "NA")$value)
max(subset(Oly.size.long2, TEMP==10 & value != "NA")$value) #need max for hist. bins 
hist(subset(Oly.size.long2, TEMP==10 & value != "NA")$value, breaks=-0.5:24.5, col="green", main="") #plot histogram
lines(seq(0.01,24,0.1), length(subset(Oly.size.long2, TEMP==10 & value != "NA")$value)*dgamma(seq(0.01,24,0.1),length.10.shape,length.10.rate)) #overlay gamma distribution density function 

# conclusion: gamma distribution fits length data adequately

# Generate glm with gamma errors, then construct analysis of deviance table and test significance of predictor variables 

glm.l1 <- glm(value~  stocked + PH,family=Gamma(link="identity"), data=subset(Oly.size.long2, TEMP==10 & value != "NA"))
summary(glm.l1)
anodev.fn(glm.l1)
anova(lm(log(value) ~ stocked + PH, data=subset(Oly.size.long2, TEMP==10 & value != "NA")))

glm.l3 <- glm(value ~ stocked+PH,family=Gamma(link="identity"), data=subset(Oly.size.long2, TEMP==6 & value != "NA"))
summary(glm.l3) 
anodev.fn(glm.l3)
anova(lm(log(value) ~ stocked + PH, data=subset(Oly.size.long2, TEMP==6 & value != "NA")))

# GLM model (assuming Gamma errors), including all predictor variables in one model: # stocked, cohort (population), temperature and pH. 

glm.l5 <- glm(value ~ stocked + COHORT + TEMP + PH, family=Gamma(link="identity"), data=Oly.size.long2)
summary(glm.l5) #AIC = 19879
anodev.fn(glm.l5)

glm.16 <- update(glm.l5, ~.-PH)
summary(glm.16) #AIC smaller with PH 

glm.17 <- update(glm.l5, ~.-TEMP)
summary(glm.17) #AIC smaller with TEMP

glm.18 <- update(glm.l5, ~.-COHORT)
summary(glm.18) #AIC smaller with COHORT

glm.19 <- update(glm.l5, ~.-stocked)
summary(glm.19) #AIC smaller with stocked

# If we can assume normal distribution with log transformation 
hist(log(Oly.size.long2$value)) #looks fairly normal, but it's slighly skewed
lm.l1 <- lm(log(value) ~ stocked + COHORT + TEMP + PH, data=Oly.size.long2) 
summary(lm.l1) #similar results for coefficient 
anova(lm.l1) 
summary(lm(value ~ PH -1, data=Oly.size.long2)) #overall low pH mean size 8.38, ambient pH 8.90 mm 
summary(lm(value ~ TEMP-1, data=Oly.size.long2)) #overall 6C pH mean size 8.51, 10C is 8.96 

lm.l2 <- lm(log(value) ~ stocked + PH, data=subset(Oly.size.long2, TEMP==6)) 
summary(lm.l2) #similar results for coefficient 
anova(lm.l2) 
summary(lm(value ~ stocked + PH -1, data=subset(Oly.size.long2, TEMP==6)))

lm.l3 <- lm(log(value) ~ stocked + PH, data=subset(Oly.size.long2, TEMP==10)) 
summary(lm.l3) #similar results for coefficient 
anova(lm.l3) 
summary(lm(value ~ stocked + PH -1, data=subset(Oly.size.long2, TEMP==10))) 

Oly.size.summary3
# TO DO 
# Remove sn-exp. groups 
# Remove NF-10 low group 
# Can I look at difference between ambient and low pH groups within cohorts?  OR can I do hierarchical or nested model with cohort / temp 

anova(lm(log(value) ~ COHORT/TEMP/PH, data=Oly.size.long2)) 
anodev.fn(glm(value ~ TEMP/COHORT/PH, data = Oly.size.long2, family=Gamma()))

Oly.size.long2.screened <- subset(Oly.size.long2, (TREATMENT != "10-AMB-Exp" & TREATMENT != "10-LOW-Exp" & TREATMENT != "6-AMB-Exp" & TREATMENT != "6-LOW-Exp") & (GROUP != "NF10-LOW" & GROUP != "NF10-AMB"))
Oly.size.long2.screened$TREATMENT <- droplevels(Oly.size.long2.screened$TREATMENT)
Oly.size.summary3

# Test if length at 10months is different between parent's pH exposure 
# Mixed effects model:  
# -- response variable=log(length in mm)
# -- fixed = # stocked in each bag, TEMP, PH 
# -- Random: Broodstock genetic source (cohort)

plot(x=subset(Oly.size.long2, TEMP==6)$stocked, y=subset(Oly.size.long2, TEMP==6)$value, col=subset(Oly.size.long2, TEMP==6)$PH)
subset(Oly.size.summary3, TEMP==6)

library(nlme)

lme.length <- lme(log(value) ~ stocked+TEMP+PH+, random=~1|COHORT, data=subset(Oly.size.long2, value!="NA"))
summary(lme.length) #stocked:PH interaction term not significant, so remove it 
anova(lme.length)


lme.length2 <- lme(log(value) ~ stocked+PH, random=~1|TEMP/COHORT, data=subset(Oly.size.long2, value!="NA"))
summary(lme.length2) #stocked:PH interaction term not significant, so remove it 
plot(lme.length2) #review residuals 
plot(lme.length2,value~fitted(.)) #is the response variable ~linear function of fitted values? 
hist(lme.length2$residuals) #histogram of residuals - normal?  pretty close
qqnorm(lme.length2,~ resid(.)| COHORT) #are residuals normal in each COHORT block? 
qqnorm(lme.length2,~ resid(.)| TEMP) #are residuals normal in each TEMP block? 

# What if I include TEMP as a fixed effect, to test whether PH differences were consistent within 10 and 6 
lme.length3 <- lme(log(value) ~ stocked*PH*TEMP, random=~1|COHORT, data=subset(Oly.size.long2, value!="NA"))
summary(lme.length3) #stocked:PH interaction term not significant, so remove it 
anova(lme.length3) 
plot(lme.length3)
plot(lme.length3,value~fitted(.)) #is the response variable ~linear function of fitted values? 
qqnorm(lme.length3,~ resid(.)| COHORT) #are residuals normal in each COHORT block? 

# Run mixed effect model on temperature groups separately 
lme.length6 <- lme(log(value) ~ stocked*PH, random=~1|COHORT, data=subset(Oly.size.long2, value!="NA" & TEMP ==6))
summary(lme.length6) 
anova(lme.length6) #yes sign. 
exp(-0.2722541)
# low pH: 0.76*ambient 


lme.length10 <- lme(log(value) ~ stocked*PH, random=~1|COHORT, data=subset(Oly.size.long2, value!="NA" & TEMP ==10))
summary(lme.length10)
anova(lme.length10) #yes sign. 

# t-test on all length data, removing pouches with vastly different stocking density (small experimental SN, NF10 grps)
t.test(value ~ TEMP, data=Oly.size.long2.screened) # length not different between temperature groups 
t.test(value ~ PH, data=Oly.size.long2.screened) #overall ambient pH mean length = 8.6, low pH=7.9
t.test(value ~ PH, data=subset(Oly.size.long2.screened, TEMP==6)) #yes 
t.test(value ~ PH, data=subset(Oly.size.long2.screened, TEMP==10)) #p=0.07, not really 

### Kruskal wallis tests on each temp diff. 
kruskal.test(value ~ PH, data=Oly.size.long2.screened) #yes 
kruskal.test(value ~ PH, data=subset(Oly.size.long2.screened, TEMP==6)) #yes 
kruskal.test(value ~ PH, data=subset(Oly.size.long2.screened, TEMP==10)) #no 




library(ggplot2)
library(ggpubr)
library(ggridges)

levels(Oly.size.long$PH)
colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")
colors2 <- c("AMBIENT"="gray", "LOW"="skyblue")

levels(Oly.size.long$TREATMENT)
Oly.size.long$TREAT <- Oly.size.long$TREATMENT 
Oly.size.long$TREAT <- as.factor(gsub("-Exp", "", Oly.size.long$TREAT))
levels(Oly.size.long$TREAT) #confirm only 4 treatment levels



# All populations, 6C
ggdensity(data=subset(Oly.size.long2, TEMP==6 & stocked>=100 & value!="NA"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = c("blue", "lightgreen")) +
  labs(title="Shell length (mm)\n @ ~10months, Early Stage Groups",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

# All populations, 10C
ggdensity(data=subset(Oly.size.long2.screened, TEMP==10 & stocked>=300 & value!="NA"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = c("mediumseagreen", "indianred2"), font.label = list(size=15)) +
  labs(title="Length (mm)\n @ ~10months, 10C groups",x="length (mm)") + 
  font("title", size = 14, face = "bold") +
  font("xlab", size = 12) +
  font("ylab", size = 12) +
  font("xy.text", size = 12)



# Violin plot of length data 
ggplot(subset(Oly.size.long, TEMP==6 & value!="NA"), aes(x=PH, y=value, fill=PH)) + 
  geom_violin(trim=T) +
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Shell length (mm) @ 10 months",y=expression("Shell length (mm)"), x=expression("Parental pH Treatment")) + 
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_y_continuous() + scale_fill_manual(values=c("skyblue3", "seagreen3"))



# Each population separately 
png("Results/NF-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="North Sound Size (mm)\n @ ~10months, n=1,017",x="length (mm)")
dev.off()

png("Results/HC-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="HL"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) + 
  labs(title="Hood Canal Size (mm)\n @ ~10months, n=1,537", x="length (mm)")
dev.off()

png("Results/SSF1-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="SN"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) + 
  labs(title="South Sound F1 Size (mm)\n @ ~10months, n=671", x="length (mm)")
dev.off()

png("Results/SSF2-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="K"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH", palette = colors2) + 
  labs(title="South Sound F2 Size (mm)\n @ ~10months, n=826", x="length (mm)")
dev.off()