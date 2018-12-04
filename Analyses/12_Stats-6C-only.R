# 6-only larval stats starting at biweekly survival 

###-------------- GONAD HISTOLOGY --------------###

# ---- QUESTION 1.  Are there gonad differences between pH treatments?  

fisher.test(CT.sex.6.pH) #0.2398 No diff. in sex between pH treatments 
(8+7)/sum(CT.sex.6.pH[1,]) #% female & pred.female, ambient 
(5+2)/sum(CT.sex.6.pH[1,]) #% female & pred.female, low

(12+7)/sum(CT.sex.6.pH[1,]) #% male & pred.female, ambient 
(19+3)/sum(CT.sex.6.pH[1,]) #% male & pred.female, low


fisher.test(CT.domstage.6.pH) #0.008287 Yes, difference in dominant gonad stage between pH treatment
fisher.test(CT.secstage.6.pH) #0.9157 No diff. in secondary gonad stage. 

# Apply second test on dominant stage using chi-square test 
print(pH.domstage.chisq <- chisq.test(CT.domstage.6.pH, simulate.p.value = TRUE)) #X-squared = 14.778, df = NA, p-value = 0.004498 

CT.domstage.6.pH # more stage 3 in ambient pH group, more stage 5 in low pH group. 

# Result: sign. difference between low pH and ambient pH in chilled group, with more ripe gametes in ambient (16 vs. 6 stage 3); more early gameto. and spawned/resorbing in low pH (7 vs. 3 stage 1; 8 vs. 2 stage 5)
#NOTE: No brooding oysters were found in either the pre- or post- pH treatment sampling, indication of no active spawning. 

# ---------------> QUESTION 2.  Did gonads develop/regress during pH exposure?

# compare chilled pre-pH to chilled amb pH (chilled = 6C)
fisher.test(CT.sex.6.amb)  #0.08151, not diff. in sex before/after pH treatment in ambient group 
fisher.test(CT.domstage.6.amb)  #1.122e-06, different  <---- significant difference in dominant stage in ambient group, developed 
fisher.test(CT.secstage.6.amb)  #0.03731, also different.   

# compare chilled pre-pH to chilled low pH 
fisher.test(CT.sex.6.low)  #0.5861, not diff. in sex. 
fisher.test(CT.domstage.6.low)  #0.01797, different  <---- sign. diff in dominant stage in low pH group 
fisher.test(CT.secstage.6.low)  #0.081, not diff. 

# stacked bar plots with pre, amb, and low gonad stages (dominant)

# all in one 
jpeg(filename = "Results/6C-Gonad-plots.jpeg", width = 1270, height = 750) 
par(mfrow = c(1,2)) #2x2 grid for plots

# pink stage colors 
# c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon")

jpeg(filename = "Results/Gonad-barplot-chilled-stage", height = 750, width = 520)
par(mar=c(5, 5, 4, 14))
print(chilled.stage <- barplot(t(prop.table(CT.domstage.6, 1)), main="Gonad stage\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray99", "gray60", "gray30", "gray15", "gray75", "gray90"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()

# stacked bar plots with pre, amb and low gonad sexes (dominant)
jpeg(filename = "Results/Gonad-barplot-chilled-sex", height = 750, width = 520)
par(mar=c(5, 5, 4, 14))
print(chilled.sex <- barplot(t(prop.table(CT.sex.6, 1)), main="Gonad sex\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"), cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.5, 0), title="Gonad Sex", cex=1.5)))
dev.off()

# Summary statistics 

100*round(prop.table(summary(subset(Histology, TEMPERATURE==6)$SEX)), 3)    # Percent of each sex (all oysters)
15.9+10.6+9.1
length(subset(Histology, TEMPERATURE==6)$SEX)                               # Number of oysters sampled for histology total = 132
length(subset(Histology, TEMPERATURE==6 & PH=="PRE")$SEX)                   # Number of oysters sampled pre-OA = 54
length(subset(Histology, TEMPERATURE==6 & PH=="AMBIENT")$SEX)               # Number of oysters sampled pre-OA = 39
length(subset(Histology, TEMPERATURE==6 & PH=="LOW")$SEX)                   # Number of oysters sampled pre-OA = 39
CT.domstage.6
16/54
(15+16)/39 #% stage 2 and 3 in ambient pH
(16+6)/39 #% stage 2 and 3 pre oa

(1+6+9+22)/(1+6+9+22+8+8) #percent undeveloped pre-OA
16/(1+6+9+22+8+8) #percent undeveloped pre-OA
54+39+39 #total # oysters

8/39
15/39

# Calculate average & sd gonad stage, but need to convert stages 4 and 5 to -1 and -2 (so higher mean = more advanced)
Histology.num <- Histology
Histology.num$Dominant.Stage <- as.numeric(as.character(Histology.num$Dominant.Stage))
Histology.num$Dominant.Stage <- as.numeric(gsub(5, -2, Histology.num$Dominant.Stage))
Histology.num$Dominant.Stage <- as.numeric(gsub(4, -1, Histology.num$Dominant.Stage))

# mean stage, both sexes
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6), mean)
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6), sd)

# mean stages, female, male, and hermaphroditic separated. 
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "F" | SEX == "HPF" )), mean)
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "F" | SEX == "HPF" )), sd)

aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "M" | SEX == "HPM" )), mean)
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "M" | SEX == "HPM" )), sd)

aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "H")), mean)
aggregate(Dominant.Stage ~ PH, subset(Histology.num, PH!="PRE" & TEMPERATURE==6 & (SEX == "H")), sd)

###-------------- LARVAL RELEASE (FECUNDITY)  --------------###

sum(aggregate(broodstock ~ Spawning.Group, subset(larvae, Temperature==6), mean)$broodstock)                                #number of broodstock total 
summarise(subset(larvae, Temperature==6), total.by.date = sum(total.released))                                              #total larvae released 
aggregate(Tot.Larvae ~ pH + Temperature, subset(larvae, Temperature==6), sum, na.rm=TRUE)                                   #By pH & Temperature
aggregate(Tot.Larvae ~ pH + Temperature + Population, subset(larvae, Temperature==6), sum, na.rm=TRUE)                      #By population
aggregate(Tot.Larvae ~ Spawning.Group, subset(larvae, Temperature==6), sum, na.rm=TRUE)                                     #By population & treatment
nrow(subset(subset(larvae, Temperature==6), total.released >= 10000))                                                       #Number of times >10k larvae were collected (all grps)
median(na.omit(subset(larvae, Temperature==6)$Tot.Larvae))
range(na.omit(subset(larvae, Temperature==6)$Tot.Larvae))

# Comparing larval release metrics between pH groups 
summary(total.released.aov <- aov(log(total.released+1) ~ pH, data=subset(spawning_group_total, Temperature==6))) # <-- daily release data NO DIFF 
summary(overall_Total.aov <- aov(cum.total ~ pH, data=subset(spawning_group_total, Temperature==6))) # <-- cumulative release NO DIFF 
summary(total.percap.aov <- aov(cum.percap ~ pH, data=subset(spawning_group_total, Temperature==6))) # <-- cumulative release per oyster*cm NO DIFF 
summary(mean.larvae.aov <- aov(mean.larvae ~ pH, data=subset(spawning_group_sum, Temperature==6))) # <-- NO DIFF 
summary(cum.total.aov <- aov(log(cum.total+1) ~ pH, data=subset(spawning_group_total, Temperature==6))) # <-- NO DIFF
summary(cum.percap.aov <- aov(log(cum.percap+1) ~ pH, data=subset(spawning_group_total, Temperature==6))) # <-- NO DIFF
summary(first.big.aov <- aov(first.big ~ pH, data=subset(spawning_group_sum, Temperature==6))) # <-- NO DIFF 
summary(max.aov <- aov(max ~ pH, data=subset(spawning_group_sum, Temperature==6))) # <-- NO DIFF 
summary(maxday.aov <- aov(maxday ~ pH, data=subset(spawning_group_sum, Temperature==6))) # <-- NO DIFF
summary(release.days <- aov(release.days ~ pH, data=subset(spawning_group_sum, Temperature==6))) # <-- NO DIFF 



# Compare timing and # released 

# cross correlation function --- try this 
# https://onlinecourses.science.psu.edu/stat510/node/74/
ccf 



# barplots of larvae released - low and ambient separately
# SAVE SIZE 1000W X 500H

ggplot(data=subset(fecundity, pH=="Low"), aes(x=Date, y=spawned)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), fill="seagreen3", col="gray60") + 
  ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) + ggtitle("Larvae Released Over Time, low pH") + theme_bw(base_size = 18) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(fecundity$Date)-1,max=max(fecundity$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity$spawned))) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank())

ggplot(data=subset(fecundity, pH=="Ambient"), aes(x=Date, y=spawned)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), fill="skyblue3", col="gray60") + ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) +
  ggtitle("Larvae released over time, ambient pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(fecundity$Date)-1,max=max(fecundity$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity$spawned))) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank())


###-------------- LARVAL STOCKING DENSITY  --------------###

plot(subset(Bucket.Densities.long, Count=="density" & Temperature==6)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature==6)$Treatment) #boxplot to inspect 
kruskal.test(subset(Bucket.Densities.long, Count=="density" & Temperature==6)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature==6)$pH) # p=0.1123  <-- no pH difference 
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), mean)  
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), max)  
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), median)  


### ------ biweekly bucket cleaning survival 
# Test using aov on square-root arcsine transformed survival % data 
hist(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival))) # looks normal 
shapiro.test(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival)))  # shapiro test confirms 
bartlett.test(x=asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival)), g=subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$pH) #equal variance between pH
Bucket.Densities.wide$survival.t <- asin(sqrt(Bucket.Densities.wide$survival)) #add column with sqrt-asin transformed survival

anova(lm(survival.t ~ pH, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ expected*pH, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ expected, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ Population, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ Population*pH, data=subset(survival.biweekly, Temperature == 6))) #no diff

aggregate(survival ~ pH, subset(survival.biweekly, Temperature == 6), mean) # no difference biweekly 
mean(subset(survival.biweekly, Temperature == 6)$survival)
sd(subset(survival.biweekly, Temperature == 6)$survival)

# sum and average daily stats for each pH
density4barplots <- subset(Bucket.Densities.wide, Temperature==6) %>%
  group_by(Date, pH) %>%
  summarize(setters=sum(setters, na.rm=T), stocked.new=sum(stocked, na.rm=T), stocked.tot=sum(expected, na.rm=T), stocked.mean=mean(expected, na.rm=T), stocked.sd=sd(expected, na.rm=T), counts.live=sum(actual, na.rm=T), survival.mean=mean(survival, na.rm=T), survival.sd=sd(survival, na.rm=T))

density4barplots[density4barplots == 0] <- NA

# SAVE DIMENSIONS 1000W X 500H

# barplot of setters 
ggplot(data=density4barplots, aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae over time, by parental pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(density4barplots$Date)-1,max=max(density4barplots$Date)+1)) +
  theme(legend.position = c(0.15, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
scale_fill_manual(values=c("gray70", "lightsteelblue"))


# barplot of # larvae expected in tanks (i.e. restocked + new stocked)  
ggplot(data=density4barplots, aes(x=Date, y=stocked.mean, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae\n(mean across replicates)") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.mean, ymax=stocked.mean+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larval tank densities over time, by parental pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(density4barplots$Date)-1,max=max(density4barplots$Date)+1)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=density4barplots, aes(x=Date, y=counts.live, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) +
  ggtitle("Biweekly larval counts over time, by parental pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(density4barplots$Date)-1,max=max(density4barplots$Date)+1)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # new larvae stocked by treatment 
ggplot(data=density4barplots, aes(x=Date, y=stocked.new, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) +
  ggtitle("New larvae stocked over time, by parental pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(density4barplots$Date)-1,max=max(density4barplots$Date)+1)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of percent survival over time 
ggplot(data=subset(density4barplots, Date != "2017-05-31"), aes(x=Date, y=survival.mean, fill=pH)) + xlab(label=element_blank()) +
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("% survival") +
  #geom_errorbar(aes(ymin=survival.mean, ymax=survival.mean+survival.sd), width=.1, position=position_dodge(width=2), col="gray30") + 
  ggtitle("Mean % survival between biweekly counts, by parental pH") + theme_bw(base_size = 18) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, colour = "gray30"), 
        axis.title = element_text(size=18, face = "bold", colour = "gray30")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
               limits=c(min=min(density4barplots$Date)-1,max=max(density4barplots$Date)+1)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "lightsteelblue"))



### ------ LARVAL SURVIVAL TO 224um 

hist(subset(Survival.set, Temperature==6)$survival.setters)
shapiro.test(subset(Survival.set, Temperature==6)$survival.setters)
qqPlot(subset(Survival.set, Temperature==6)$survival.setters)
# Normal is OK

# Outliers? 
boxplot(subset(Survival.set, Temperature==6)$survival.setters)
boxplot.stats(subset(Survival.set, Temperature==6)$survival.setters)$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(survival.setters~Population, data=subset(Survival.set, Temperature==6)) #0.2375
bartlett.test(survival.setters~pH, data=subset(Survival.set, Temperature==6)) #0.7367

# RESULT: ANOVA assumptions met 

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 


# First test variables separately. 
summary(test.setters1 <- lm(survival.setters ~ Population, data=subset(Survival.set, Temperature==6)))
summary(test.setters2 <- lm(survival.setters ~ pH, data=subset(Survival.set, Temperature==6)))
summary(test.setters3 <- lm(survival.setters ~ Population+pH, data=subset(Survival.set, Temperature==6))) 
summary(test.setters4 <- lm(survival.setters ~ Larvae.stocked.adjusted, data=subset(Survival.set, Temperature==6))) #0.034727 <--- sign.
summary(test.setters5 <- lm(survival.setters ~ Days.Stocked, data=subset(Survival.set, Temperature==6)))  
summary(test.setters6 <- lm(survival.setters ~ Larvae.stocked.adjusted+pH, data=subset(Survival.set, Temperature==6))) 
anova(lm(survival.setters ~ Mean.stocked, data=subset(Survival.post, Temperature==6))) #0.04879 <---- sign. 

AIC(test.setters1, test.setters2, test.setters3, test.setters4, test.setters5, test.setters6) 
summary(test.setters3) # Model p-value=0.2027, Adjusted R-squared=0.5231 AIC=34.54893
anova(test.setters3) # 

# Df  Sum Sq Mean Sq F value Pr(>F)
# Population  3 21.6257  7.2086  2.7558 0.2136
# pH          1  8.9207  8.9207  3.4103 0.1620
# Residuals   3  7.8474  2.6158       

aggregate(survival.setters ~ pH, data=subset(Survival.set, Temperature==6), mean)
aggregate(survival.setters ~ pH, data=subset(Survival.set, Temperature==6), sd)
range(subset(Survival.set, Temperature==6)$survival.setters)

# NO DIFFERENCES IN SURVIVAL BY PH 

# Jitter / boxplot with symbols for temperature 
ggplot(subset(Survival.set, Temperature==6), aes(x=pH, y=survival.setters, col=pH)) + geom_boxplot() +
  geom_jitter(stat="identity", width = 0.2, size=6) +
  labs(title="Cumulative % Survival\nNew Larvae to Eyed Larvae") + xlab("Parental pH") + ylab("% Survival") +
  theme_bw(base_size = 20) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, color="gray32"),  panel.border = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="gray32"),  axis.title.x = element_text(color="gray32"), axis.title.y = element_text(color="gray32"), legend.position="bottom", legend.background = element_rect(color = NULL, fill = NULL, linetype = NULL),  legend.key.size = unit(1.5, 'lines'), legend.text = element_text(colour="gray32")) + 
  scale_color_manual(values=c('gray25', 'gray25'), guide=FALSE) + scale_shape_discrete(name  =NULL, breaks=c(6, 10), labels=c("Chilled", "Unchilled"))


### ------ survival 224um to post set 

# Test assumptions prior to running anova/lm: 

# normal distribution? 
hist(log(subset(Survival.post, Temperature==6)$survival.postset))
shapiro.test(log(subset(Survival.post, Temperature==6)$survival.postset))
qqPlot(log(subset(Survival.post, Temperature==6)$survival.postset))
# log-normal looks good 

# Outliers? 
boxplot(log(subset(Survival.post, Temperature==6)$survival.postset))
boxplot.stats(log(subset(Survival.post, Temperature==6)$survival.postset))$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(log(survival.postset)~Population, data=subset(Survival.post, Temperature==6)) #0.5991
bartlett.test(log(survival.postset)~pH, data=subset(Survival.post, Temperature==6)) #0.07711 <--- hmmm. close. 

# RESULT: ANOVA assumptions met (not sure about homoscedacity between pH groups ...)

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 

# First test variables separately. None significant on their own 
anova(test.postset1 <- lm(log(survival.postset) ~ Population, data=subset(Survival.post, Temperature==6))) 
anova(test.postset3 <- lm(log(survival.postset) ~ pH, data=subset(Survival.post, Temperature==6)))
anova(test.postset4 <- lm(log(survival.postset) ~ Mean.stocked, data=subset(Survival.post, Temperature==6)))
anova(test.postset5 <- lm(log(survival.postset) ~ Larvae.stocked.adjusted, data=subset(Survival.post, Temperature==6))) # sign! 

# Now test variables together
anova(test.postset6 <- lm(log(survival.postset) ~ Mean.stocked + Population  + pH, data=subset(Survival.post, Temperature==6))) 
anova(test.postset7 <- lm(log(survival.postset) ~ Mean.stocked + Population, data=subset(Survival.post, Temperature==6))) 
anova(test.postset8 <- lm(log(survival.postset) ~ Mean.stocked:pH + Mean.stocked:Population, data=subset(Survival.post, Temperature==6)))
anova(test.postset9 <- lm(log(survival.postset) ~ Mean.stocked:Population, data=subset(Survival.post, Temperature==6)))
anova(test.postset10 <- lm(log(survival.postset) ~ Mean.stocked*pH, data=subset(Survival.post, Temperature==6)))

AIC(test.postset1, test.postset3, test.postset4, test.postset5, test.postset6, test.postset7, test.postset8, test.postset9, test.postset10) 

summary(test.postset10) # Model p-value=0.01247, Adjusted R-squared=0.8553 AIC=12.73209 (smallest AIC)
anova(test.postset10) #sign. interaction between mean stocked & pH, but not pH alone 

# Response: log(survival.postset)
#                  Df Sum Sq Mean Sq F value  Pr(>F)  
# Mean.stocked     1 3.3820  3.3820 20.5261 0.01057 *
# pH               1 0.6930  0.6930  4.2061 0.10958  
# Mean.stocked:pH  1 3.2349  3.2349 19.6332 0.01141 *
# Residuals        4 0.6591  0.1648           

# survival postset ~ stocking density during larval phase 

summary(test.postset6 <- lm(log(survival.postset) ~ Mean.stocked, data=subset(Survival.post, Temperature==6))) 
summary(test.postset6ph <- lm(log(survival.postset) ~ Mean.stocked*pH, data=subset(Survival.post, Temperature==6))) 

plot(x=subset(Survival.post, Temperature==6)$Mean.stocked, y=log(subset(Survival.post, Temperature==6)$survival.postset), bg="black", col=c("gray32", "steelblue3")[as.numeric(subset(Survival.post, Temperature==6)$pH)], pch=c(16), cex=2, main="%  survival, metamorphosis\n ~ larval tank density", xlab="mean larval tank density", ylab="% 224um Survival to postset")
abline(test.postset6) 

#text(x=subset(Survival.post, Temperature==6)$Mean.stocked+1000, y=log(subset(Survival.post, Temperature==6)$survival.postset)-.1, col="gray30", labels=subset(Survival.post, Temperature==6)$Population, xpd=T, cex=0.8)
legend(44000, 3.3, legend=c("Low pH", "Ambient pH"),
       col=c("steelblue3", "gray32"), pch=c(16, 16), cex=1.2)

# strong evidence for stocking density effect on survival from 224um->post-set

aggregate(survival.postset ~ pH, subset(Survival.post, Temperature==6), mean)
aggregate(survival.postset ~ pH, subset(Survival.post, Temperature==6), sd)
range(subset(Survival.post, Temperature==6)$survival.postset)

# Survival from new to post=set
Survival.post$survival.larvae <- Survival.post$Post.set/Survival.post$Larvae.stocked.adjusted
plot(x=subset(Survival.post, Temperature==6)$Mean.stocked, y=log(subset(Survival.post, Temperature==6)$survival.larvae), bg="black", col=c("gray32", "steelblue3")[as.numeric(subset(Survival.post, Temperature==6)$pH)], pch=c(16), cex=2, main="%  survival, new larvae -> postset\n ~ larval tank density")


### ------ JUVENILE LENGTH ------ ###

aggregate(value ~ PH, subset(Oly.size.long2, TEMP==6), mean)
aggregate(value ~ PH, subset(Oly.size.long2, TEMP==6), sd)
aggregate(value ~ PH, subset(Oly.size.long2, TEMP==6), range)

# Subset the oysters I counted so I have equal sample sizes. Smallest group is 120 oysters. Random subset. 

Oly.size.long5 <- subset(Oly.size.long2, value!="NA" & TEMP==6) %>% group_by(GROUP) %>% sample_n(size = 120, replace=F)
hist(Oly.size.long5$value) #still looks gamma 
Anova(glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), Oly.size.long5)) #Significant pH factor 

# Fit glms with gamma errors and stocked as random effect, compare with AIC. 
# first 6C only
summary(glm.l1 <- glm(value~  stocked,family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l2 <- glm(value~  PH,family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l3 <- glm(value~  COHORT,family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l4 <- glmer(value~  PH+(1|stocked),family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l5 <- glmer(value~  COHORT+(1|stocked),family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l6 <- glmer(value~  PH*COHORT+(1|stocked),family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l7 <- glmer(value~  PH+COHORT+(1|stocked),family=Gamma(link="log"), data=Oly.size.long5))
summary(glm.l8 <- glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), Oly.size.long5))

AIC(glm.l1,glm.l2,glm.l3,glm.l4,glm.l5,glm.l6,glm.l7,glm.l8) #lowest AIC = glm.l6
summary(glm.l6) # smallest AIC, length ~ PH*COHORT+(1|stocked)
anova(glm.l6, test="Chi")
Anova(glm.l6) # Use `car` package for Chisq test on analysis of deviance table 
plot(glm.l6)
exp(-0.31958)

pH.lengths.p <- vector(mode="numeric", length=100)
for (i in 1:100) {
  Oly.size.long6 <- subset(Oly.size.long2, value!="NA" & TEMP==6) %>% group_by(GROUP) %>% sample_n(size = 120, replace=F)
  #hist(Oly.size.long5$value) #still looks gamma 
  # quick models looking at pH with population (cohort) and stocked as random effects 
  pH.length <- Anova(glmer(value~  PH*COHORT+(1|stocked),family=Gamma(link="log"), data=Oly.size.long6)) #Significant pH factor 
  pH.lengths.p[i] <- pH.length$`Pr(>Chisq)`
}
summary(pH.lengths.p) 

# Make table of estimates (exp. transformed) with 95% CI
se <- sqrt(diag(vcov(glm.l6)))
print(glm.l.ci <- exp(cbind(Est = fixef(glm.l6), LL = fixef(glm.l6) - 1.96*se, UL = fixef(glm.l6) + 1.96*se)))
1-glm.l.ci[2,] 
plot(glm.l7) #plot residuals 

1.7/8.7

# compare bag densities between PH groups using summary dataframe
shapiro.test(log(subset(Oly.size.summary4, TEMP==6)$stocked))
anova(lm(log(stocked) ~ PH, data=subset(Oly.size.summary4, TEMP==6)))


# plots length data  

colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")
colors2 <- c("AMBIENT"="gray50", "LOW"="lightsteelblue")

# Make multipanel plot 
palette(colors2)
multi.metrics <- c("Mean.larvae.stocked", "Days.Stocked", "survival.setters", "Setters.stocked", "survival.postset", "Juv.stocked")
multi.names <- c("Mean Larvae Stocking Density", "No. Families Stocked\n(estimated by no. stocking days)", "% Survival to Eyed Larvae", "No. Eyed Larvae Stocked", "% Survival through Metamorphosis", "Juvenile Stocking Density")
oly.multivar.plots <- subset(oly.multivar, Temperature==6 & Larvae.stocked.adjusted!=160)


cor.spot <- c("bottomright", "bottomright", "bottomright", "bottomright", "topright", "topright")
par(mfrow=(c(3,2)), mar=c(1,1,4,0), oma=c(5,4,2,2), col="gray25")
for (i in 1:length(multi.metrics)){
  juv.cor <- cor.test(x=oly.multivar.plots[,multi.metrics[i]], y=oly.multivar.plots$Juv.meanlength, method = "spearman")
  rho <- juv.cor$estimate[[1]]
  p <- juv.cor$p.value
  plot(x=oly.multivar.plots[,multi.metrics[i]], y=oly.multivar.plots$Juv.meanlength,  
       col=oly.multivar.plots$PH, cex=2.5, pch=16, xlab=NA, ylab=NA, xaxt="n", yaxt="n", main=NA)
  legend(cor.spot[i], paste("rho = ",round(rho, 2), "\np = ",round(p, 2), sep=""), bty = "n", col = "gray25", cex = 1.2)
  axis(3, at=(pretty(oly.multivar.plots[,multi.metrics[i]])), col="gray25")
  mtext(side=3, outer=F, line=2.5, multi.names[i], col="gray25")
  if ((i==1 | (i==3) | (i==5))) {     #true when i is 1, 3, 5
    axis(side=2, outer=F,labels = pretty(oly.multivar.plots[,"Juv.meanlength"]), 
         at = pretty(oly.multivar.plots[,"Juv.meanlength"]), las=1, col="gray25")        
  }
}
mtext(side=2, outer=T, line=1.5, "Mean Juvenile Length (mm)")
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("Ambient Parental pH", "Low Parental pH"), xpd = TRUE, 
       horiz = FALSE, inset = c(0, 0), bty = "n", pch = 16, col = colors2, cex = 1.5, pt.cex=3)

Oly.size.long$TREAT <- Oly.size.long$TREATMENT 
Oly.size.long$TREAT <- as.factor(gsub("-Exp", "", Oly.size.long$TREAT))
levels(Oly.size.long$TREAT) #confirm only 4 treatment levels

# All cohorts combined (using subset of data)
ggdensity(data=subset(Oly.size.long5,  value!="NA" & TEMP==6), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="Shell length (mm)\nJuveniles by parental pH",x="shell length (mm)") +
  font("title", size = 16, colour="gray30") +
  font("xlab", size = 16, colour="gray30") +
  font("ylab", size = 16, colour="gray30") +
  font("xy.text", size = 1, colour="gray30") +
  theme(legend.position = "none")  
  #font("legend.title", size=18, colour="gray30") +
  #font("legend.text", size=18, colour="gray30") 


theme(axis.title.x = element_text(colour = "gray40"),
      axis.title.y = element_text(colour = "gray40", face="bold")) + ylab("Species abundance") + xlab(element_blank()) + theme(legend.position="top",legend.text=element_text(size=10, colour="gray40"),  legend.key.size = unit(1.8, 'lines')) + theme(legend.title=element_blank()) + 
  theme(strip.text.x = element_text(size = 12, colour = "gray40"))

### -------- Eelgrass deployment --------- #####

# see separate script (#13)

### -------- multivariate screening 

# Inspect correlation between variables, to reduce # variables 
pairs(na.omit(subset(oly.multivar, Temperature==6)[c("Total.Spawned", "Days.Stocked", "Larvae.stocked.adjusted", "Mean.larvae.stocked", "Setters.stocked", "survival.setters", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.stocked", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass")]), lower.panel=panel.smooth, upper.panel=panel.cor)  

# Remove variables that are highly correlated with other variables, to reduce # (total.spawned, days stocked, larvae stocked)
pairs(na.omit(subset(oly.multivar, Temperature==6)[c("Mean.larvae.stocked", "survival.setters","Setters.stocked", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.survival")]), lower.panel=panel.smooth, upper.panel=panel.cor, gap = .65, cex.labels =1.5, labels = c("Mean Larval\nStocking Density", "% Survival to\n Eyed Stage", "# eyed larvae\nin Setting Tank", "% Survival through\nMetamorphosis", "# Post-set stocked\nduring Grow-out", "Juvenile Length", "% Survival\nduring Deployment")) #example result 

# correlation tests,  deployment survival agasint other stuff
cor.test(x=na.omit(subset(oly.multivar, Temperature==6)$survival.postset), y=na.omit(subset(oly.multivar, Temperature==6)$Mean.larvae.stocked), method = "spearman")
cor.test(x=na.omit(subset(oly.multivar, Temperature==6)$survival.postset), y=na.omit(subset(oly.multivar, Temperature==6)$Juv.meanlength), method = "spearman")
cor.test(x=na.omit(subset(oly.multivar, Temperature==6)$Deployment.survival), y=na.omit(subset(oly.multivar, Temperature==6)$survival.setters), method = "spearman")

par(mfrow=c(4,1), mar=c(3,4,1,1), oma=c(1,1,1,1))
plot(y=na.omit(subset(oly.multivar, Temperature==6)$Deployment.survival), x=na.omit(subset(oly.multivar, Temperature==6)$survival.setters), col=na.omit(subset(oly.multivar, Temperature==6)$PH), pch=17, cex=1.5, xlab= "% survival to eyed larval phase",ylab=NA)
plot(y=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$Deployment.survival), x=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$survival.postset), col=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$PH), pch=17, cex=1.5, xlab= "# postset stocked",ylab=NA)
plot(y=na.omit(subset(oly.multivar, Temperature==6)$Deployment.survival), x=na.omit(subset(oly.multivar, Temperature==6)$Setters.stocked), col=na.omit(subset(oly.multivar, Temperature==6)$PH), pch=17, cex=1.5, xlab= "# eyed larvae in setting tank",ylab=NA)
plot(y=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$Deployment.survival), x=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$Juv.stocked), col=na.omit(subset(oly.multivar, Temperature==6 & GROUP != "SN6-AMB-Exp" & GROUP != "SN6-LOW-Exp")$PH), pch=17, cex=1.5, xlab= "# postset stocked",ylab=NA)
mtext(side = 2, "% survival during deployment", outer = T, line=-1)

palette("default")
# Correlation plot for paper (???)
pairs( na.omit(subset(oly.multivar, Temperature==6)[c("survival.setters","survival.postset", "Juv.meanlength", "Deployment.survival", "Mean.larvae.stocked", "Days.Stocked", "Setters.stocked", "Juv.stocked")]), lower.panel=panel.smooth, upper.panel=panel.cor, gap = .65, cex.labels =1.5, labels = c("% Survival to\n Eyed Stage", "% Survival Through\nMetamorphosis", "Juvenile Length\n(mean)", "% Survival During\nDeployment (mean)", "Larval Tank\nDensity (mean)", "# Families Stocked\n(estimate)",  "# Eyed Larvae\nin Setting Tank", "# Post-set Stocked\nDuring Grow-out"), cex=1.5, main="Spearman's Correlations among Life Stages and Stocking Densities")  
# can't figure out how to change symbol type 