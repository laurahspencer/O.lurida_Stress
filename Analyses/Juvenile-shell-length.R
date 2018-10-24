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

# Remove the mini-experiment data, very small stocking density and only 1 spawning group 
Oly.size.summary4 <- filter(Oly.size.summary3, !grepl("Exp",TREATMENT))
Oly.size.summary4 <- filter(Oly.size.summary4, !grepl("NF10",Group))

# Inspect length by stocking density 
plot(x=Oly.size.summary4$stocked, y=Oly.size.summary4$Mean, main="All Pops,\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary4$PH, pch=19, cex=2)
text(x=Oly.size.summary4$stocked, y=Oly.size.summary4$Mean, labels =Oly.size.summary4$PH)

# Mean stocking density vary by temp & pH treatment? 
shapiro.test(log(Oly.size.summary4$stocked))  #log-normal OK
hist(log(Oly.size.summary4$stocked)) #normal OK
bartlett.test(log(Oly.size.summary4$stocked) ~ Oly.size.summary4$PH) #variance OK
bartlett.test(log(Oly.size.summary4$stocked) ~ Oly.size.summary4$TEMP) #variance diff, outlier 
anova(lm(log(stocked) ~ COHORT/TEMP/PH, data=Oly.size.summary4)) #diff between cohort, but not by temp & pH w/n cohorts
anova(lm(log(stocked) ~ COHORT/PH, data=Oly.size.summary4)) #not by temp & pH w/n cohorts

# merge stocking density with full dataframe to include in stats 
Oly.size.long2 <- merge(x=Oly.size.long, y=stock.dens, by.x="GROUP", by.y="groups", all.x=T, all.y=F)

# Remove groups with poor survival / low stocking density all 10C groups and mini-exp. groups 
Oly.size.long2 <- filter(Oly.size.long2, !grepl("Exp",GROUP))
Oly.size.long2 <- filter(Oly.size.long2, !grepl("NF10",GROUP))

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

# Fit glms with gamma errors, compare with AIC. Use glm() when cohort not included; use glmer to include cohort as random effect. 
summary(glm.l1 <- glm(value~  log(stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l2 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l3 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6 & value !=2)))
summary(glm.l4 <- glmer(value~  log(stocked)+PH +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l5 <- glmer(value~  log(stocked)+PH +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6 & value !=2))) 
summary(glm.l6 <- glmer(value~  log(stocked) +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l7 <- glmer(value~  PH +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l8 <- glmer(value~  log(stocked)*PH+(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))

AIC(glm.l1,glm.l2,glm.l4,glm.l6,glm.l7,glm.l8) #lowest AIC = glm.l4 & glm.l7
summary(glm.l7) # length ~ PH + (1 | COHORT)
summary(glm.l4) # length ~ log(stocked) + PH + (1 | COHORT), good to see that stocked isn't sign. with population as random effect, but pH still is. 
exp(-0.22256) # coefficient == 0.80

### ------ juvenile length data - sign. smaller oysters from low ph groups, ~20% smaller. No diff. in stocking btn groups. 

Anova(glm.l7) # Use `car` package for Chisq test on analysis of deviance table 
Anova(glm.l4) # Use `car` package for Chisq test on analysis of deviance table 

# Make table of estimates (exp. transformed) with 95% CI
se <- sqrt(diag(vcov(glm.l7)))
print(glm.l.ci <- exp(cbind(Est = fixef(glm.l7), LL = fixef(glm.l7) - 1.96*se, UL = fixef(glm.l7) + 1.96*se)))
1-glm.l.ci[2,] # % smaller due to low pH 
exp(glm.l.ci[2,]) #2.5x 

### Kruskal wallis tests - nonparametric rank-order test 

kruskal.test(value ~ PH, data=Oly.size.long2) # ~0 
kruskal.test(value ~ COHORT, data=Oly.size.long2) # ~0 
# KW test for each cohort 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="HL")) #diff
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="HL" & value !=2)) #diff, even without the 2.0 mm data points 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="K")) #diff
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="NF")) #diff 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="SN")) #not diff 

summary(subset(Oly.size.long2, COHORT=="HL" & value !=2 & PH=="AMBIENT" & TEMP == 6)$value)
summary(subset(Oly.size.long2, COHORT=="HL" & value !=2 & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="HL" & PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="HL" & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="NF" & PH=="AMBIENT"& PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="NF" & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="K" & PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="K" & PH=="LOW")$value)



# plots length data as distributions 

colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")
colors2 <- c("AMBIENT"="gray70", "LOW"="lightsteelblue")

Oly.size.long$TREAT <- Oly.size.long$TREATMENT 
Oly.size.long$TREAT <- as.factor(gsub("-Exp", "", Oly.size.long$TREAT))
levels(Oly.size.long$TREAT) #confirm only 4 treatment levels

# All temps combined  
ggdensity(data=subset(Oly.size.long2,  value!="NA"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="Shell length (mm)\n @ ~10months",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

# Each population separately 
png("Results/NF-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="North Sound Size (mm)\n @ ~10months, n=1,017",x="Length (mm)")
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
