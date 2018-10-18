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
pch.list <- as.numeric(Oly.size.summary3$TREATMENT)

write.csv(file="Data/Oly.10-month-size.summary.csv", x=Oly.size.summary3)

# Inspect length by stocking density 
plot(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, main="All Pops,\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3$PH, pch=19, cex=2)
text(x=Oly.size.summary3$stocked, y=Oly.size.summary3$Mean, labels =Oly.size.summary3$Group)

# merge stocking density with full dataframe to include in stats 
Oly.size.long2 <- merge(x=Oly.size.long, y=stock.dens, by.x="GROUP", by.y="groups", all.x=T, all.y=F)

#Assess distribution of length data 
hist(Oly.size.long2$value) #right skewed 
shapiro.test(Oly.size.long2$value) #not normal 

# plot length histogram & overlay normal distribution - definitely not normal. Looks like gamma distribution. 
hist(subset(Oly.size.long2, value != "NA")$value,  breaks=0:26, col="green", main="")
lines(seq(0,26,0.1), length(subset(Oly.size.long2, value != "NA")$value)*dnorm(seq(0,26,.1), mean(subset(Oly.size.long2, value != "NA")$value), sqrt(var(subset(Oly.size.long2, value != "NA")$value))))

# replot histogram, and overlay gamma probability distribution. First calculate distribution's rate and shape (rate = mean/var, shape = rate*mean). Also need max length to assign bins and gamma dist. range 
length.rate <- mean(subset(Oly.size.long2, value != "NA")$value) / var(subset(Oly.size.long2, value != "NA")$value)
length.shape <- length.rate*mean(subset(Oly.size.long2, value != "NA")$value)
max(subset(Oly.size.long2, value != "NA")$value) #need max for hist. bins 
hist(subset(Oly.size.long2, value != "NA")$value, breaks=-0.5:24.5, col="green", main="") #plot histogram
lines(seq(0.01,25,0.1), length(subset(Oly.size.long2, value != "NA")$value)*dgamma(seq(0.01,25,0.1),length.shape,length.rate)) #overlay gamma distribution density function 

# conclusion: gamma distribution fits length data adequately 

# Fit glms with gamma errors, compare with AIC 
summary(glm.l1 <- glm(value~  stocked,family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l2 <- glm(value~  TEMP,family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l3 <- glm(value~  PH,family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l4 <- glm(value~  stocked+TEMP+PH +(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l5 <- glm(value~  stocked+PH +(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l6 <- glm(value~  stocked +(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l7 <- glm(value~  PH +(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l8 <- glm(value~  stocked*TEMP+(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l9 <- glm(value~  stocked*PH+(1/COHORT),family=Gamma(link="identity"), data=subset(Oly.size.long2, value != "NA")))

AIC(glm.l1,glm.l2,glm.l3,glm.l4,glm.l5,glm.l6,glm.l7,glm.l8,glm.l9) #lowest AIC = glm.l4
summary(glm.l4) # lowest AI: length ~ stocked + TEMP + PH + (1/COHORT)
# Coefficients    Estimate      Std. Error  t value   Pr(>|t|)    
# (Intercept) 11.8469577  0.1341813   88.29   <2e-16 ***
#   stocked     -0.0059098  0.0001735  -34.06   <2e-16 ***
#   TEMP10       2.4509355  0.1334543   18.36   <2e-16 ***
#   PHLOW       -1.2186526  0.1008851  -12.08   <2e-16 ***

# (Dispersion parameter for Gamma family taken to be 0.1389996)
# Null deviance: 718.52  on 4050  degrees of freedom
# Residual deviance: 559.71  on 4047  degrees of freedom
# AIC: 20337

plot(case(glm.l4)[,"stu.res"])
plot(case(glm.l4)[,"cook"])

### Kruskal wallis tests - nonparametric rank-order test 

kruskal.test(value ~ PH, data=Oly.size.long2) # ~0 
kruskal.test(value ~ TEMP, data=Oly.size.long2) # 2.632e-07
kruskal.test(value ~ COHORT, data=Oly.size.long2) # ~0 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, TEMP==6)) # ~ 0
kruskal.test(value ~ PH, data=subset(Oly.size.long2, TEMP==10)) #no, 0.5574 



# plots 

library(ggplot2)
library(ggpubr)
library(ggridges)

colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")
colors2 <- c("AMBIENT"="gray", "LOW"="skyblue")

Oly.size.long$TREAT <- Oly.size.long$TREATMENT 
Oly.size.long$TREAT <- as.factor(gsub("-Exp", "", Oly.size.long$TREAT))
levels(Oly.size.long$TREAT) #confirm only 4 treatment levels


# All temps combined  
ggdensity(data=subset(Oly.size.long2,  value!="NA"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = c("blue", "lightgreen")) +
  labs(title="Shell length (mm)\n @ ~10months",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)







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