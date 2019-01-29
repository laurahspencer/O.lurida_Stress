

# --- GONAD HISTOLOGY -----# 

# Read in histology data. 
Histology <- read.csv("Data/2017-Oly-Histo-Results-REDO-redostage5.csv", header=T, stringsAsFactors = T, na.strings = "NA")
Histology$TEMPERATURE <- as.factor(Histology$TEMPERATURE)         #Convert a few columns to factors 
Histology$Dominant.Stage <- as.factor(Histology$Dominant.Stage)   

# --------------- QUESITON 1.  are there gonad differences between pre-OA treatment temperature groups (chilled, not chilled). 

Histo.pre <- droplevels(subset(Histology, SAMPLING == "FEBRUARY"))      #subset only pre-treatment data (sampled february, 2017)
CT.SEX.pre <- table(Histo.pre$TREATMENT, Histo.pre$SEX)                 #create table of gonad sex by chilled/not chilled (pre-OA)
CT.domstage.pre <- table(Histo.pre$TREATMENT, Histo.pre$Dominant.Stage) #create table of gonad dominant stage by temperature (pre-OA)
CT.secstage.pre <- table(Histo.pre$TREATMENT, Histo.pre$Secondary.Stage)#create table of gonad secondary stage by temperature (pre-OA)

# Compare the sex and stages after chilling / not chilling (pre-OA) via Fisher's test due to low sample size (<200)
fisher.test(CT.SEX.pre)       #p=0.1903 - sex not different 
fisher.test(CT.domstage.pre)  #p=7.413e-05 - stages different between temperature. 10C more advanced. 
fisher.test(CT.secstage.pre)  #p=0.4291 - secondary stages not different 

# Apply second test on dominant stage using chi-square test 
print(Pre.domstage.chisq <- chisq.test(CT.domstage.pre, simulate.p.value = TRUE)) #X-squared = 23.962, df = NA, p-value = 0.0004998 - VERY DIFFERENT 

# ---------------> QUESTION 2.  Are there gonad differences between pH treatments? Analyze temp groups separately  

# --- compare chilled-amb pH to chilled-low pH
CT.sex.6.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==6)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==6)$SEX)
CT.domstage.6.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==6)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==6)$Dominant.Stage)
CT.secstage.6.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==6)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==6)$Secondary.Stage)

fisher.test(CT.sex.6.pH) #0.2398 No diff.
fisher.test(CT.domstage.6.pH) #0.008287 Yes, difference in dominant gonad stage between pH treatment
fisher.test(CT.secstage.6.pH) #0.9157 No diff. 

# Apply second test on dominant stage using chi-square test 
print(pH.domstage.chisq <- chisq.test(CT.domstage.6.pH, simulate.p.value = TRUE)) #X-squared = 14.778, df = NA, p-value = 0.004498 

# --- compare not chilled-amb pH to not chilled-low pH 
CT.sex.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$SEX)
CT.domstage.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$Dominant.Stage)
CT.secstage.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$Secondary.Stage)

fisher.test(CT.sex.10.pH) #0.8848 - no diff between sex ratios. 
fisher.test(CT.domstage.10.pH) #0.08 - no diff in dominant stage
fisher.test(CT.secstage.10.pH) #0.04915 - almost but not quite diff in secondary stage 


# ---------------> QUESTION 3.  Did gonads develop/regress during pH exposure? AKA was there a change from before to after pH treatment? Assess chilled/not chilled groups separately. 

# compare chilled pre-pH to chilled amb pH (chilled = 6C)
CT.sex.6.amb <- table(subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$SEX)
CT.domstage.6.amb <- table(subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$Dominant.Stage)
CT.secstage.6.amb <- table(subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="AMBIENT" | is.na(PH)))$Secondary.Stage)
fisher.test(CT.sex.6.amb)  #0.08151, not diff.
fisher.test(CT.domstage.6.amb)  #1.122e-06, different  <---- 
fisher.test(CT.secstage.6.amb)  #0.03731, different  

# compare chilled pre-pH to chilled low pH 
CT.sex.6.low <- table(subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$SEX)
CT.domstage.6.low <- table(subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$Dominant.Stage)
CT.secstage.6.low <- table(subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==6 & (PH=="LOW" | is.na(PH)))$Secondary.Stage)
fisher.test(CT.sex.6.low)  #0.5861, not diff.
fisher.test(CT.domstage.6.low)  #0.01797, different  <---- 
fisher.test(CT.secstage.6.low)  #0.081, not diff. 

# compare not chilled pre-pH to not chilled amb pH (not chilled = 10C)
CT.sex.10.amb <- table(subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$SEX)
CT.domstage.10.amb <- table(subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$Dominant.Stage)
CT.secstage.10.amb <- table(subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="AMBIENT" | is.na(PH)))$Secondary.Stage)
fisher.test(CT.sex.10.amb)  #0.2929, not diff.
fisher.test(CT.domstage.10.amb)  #0.1683, not diff.  
fisher.test(CT.secstage.10.amb)  #0.4686, not diff.  

# compare not chilled pre-pH to not chilled low pH
CT.sex.10.low <- table(subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$SEX)
CT.domstage.10.low <- table(subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$Dominant.Stage)
CT.secstage.10.low <- table(subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$SAMPLING, subset(Histology, TEMPERATURE==10 & (PH=="LOW" | is.na(PH)))$Secondary.Stage)
fisher.test(CT.sex.10.low)  #0.5202, not diff.
fisher.test(CT.domstage.10.low)  #0.08615, not diff. 
fisher.test(CT.secstage.10.low)  #0.06494, not diff. 

# 2 bar plots - chilled (pre, amb, low), and not chilled (pre, amb, low)

# tables for stacked bar plots 
levels(Histology$PH) <- c(levels(Histology$PH),"PRE")
Histology$PH <- relevel(Histology$PH, ref="PRE")
Histology[is.na(Histology$PH),"PH"] <- "PRE"

CT.domstage.6 <- table(subset(Histology, TEMPERATURE==6)$PH, subset(Histology, TEMPERATURE==6)$Dominant.Stage)
CT.domstage.10 <- table(subset(Histology, TEMPERATURE==10)$PH, subset(Histology, TEMPERATURE==10)$Dominant.Stage)
colnames(CT.domstage.6) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially Spawned (4)", "Regressing (5)")
colnames(CT.domstage.10) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially Spawned (4)", "Regressing (5)")

png(filename = "Results/Gonad-barplot-chilled-stage", height = 750, width = 420)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 2))
print(chilled.stage <- barplot(t(prop.table(CT.domstage.6, 1)), main="A) Stage, pre- & post-pH treatment\nchilled group (6C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = F, cex=1.5))
dev.off()

png(filename = "Results/Gonad-barplot-notchilled-stage", height = 750, width = 420)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 2))
print(notchilled.stage <- barplot(t(prop.table(CT.domstage.10,1)), main="B) Stage, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = F, cex=1.5))
dev.off()



# to plot this one with legend! 
# png(filename = "Results/Gonad-barplot-notchilled-stage", height = 750, width = 650)
# par(mfrow=c(1, 1), mar=c(5, 5, 4, 15))
# print(notchilled.stage <- barplot(t(prop.table(CT.domstage.10,1)), main="B) Stage, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.58, 0), title="Gonad Stage", cex=1.5)))
# dev.off()

# Conclusion: chilled group gonad developed less in low pH treatment. Not chilled group was already developed prior to pH treatment, no significant development or resorption occurred during pH treatment. 


# ---------------> Summary statistics 

100*round(prop.table(summary(Histology$SEX)), 3)    # Percent of each sex (all oysters)
length(Histology$SEX)                               # Number of oysters sampled for histology 



###-------------- LARVAL RELEASE (FECUNDITY)  --------------###

sum(aggregate(broodstock ~ Spawning.Group, larvae, mean)$broodstock)                                #number of broodstock total 
summarise(larvae, total.by.date = sum(total.released))                                              #total larvae released 
aggregate(Tot.Larvae ~ pH + Temperature, larvae, sum, na.rm=TRUE)                                   #By pH & Temperature
aggregate(Tot.Larvae ~ pH + Temperature + Population, larvae, sum, na.rm=TRUE)                      #By population
aggregate(Tot.Larvae ~ Spawning.Group, larvae, sum, na.rm=TRUE)                                     #By population & treatment
nrow(subset(larvae, total.released >= 10000))                                                       #Number of times >10k larvae were collected (all grps)
median(na.omit(larvae$Tot.Larvae))
range(na.omit(larvae$Tot.Larvae))

# Comparing larval release metrics between pH groups 
summary(total.released.aov <- aov(log(total.released+1) ~ pH*Temperature, data=spawning_group_total)) # <-- daily release data NO DIFF 
summary(overall_Total.aov <- aov(cum.total ~ pH*Temperature, data=spawning_group_total)) # <-- cumulative release NO DIFF 
summary(total.percap.aov <- aov(cum.percap ~ pH*Temperature, data=spawning_group_total)) # <-- cumulative release per oyster*cm NO DIFF 
summary(mean.larvae.aov <- aov(mean.larvae ~ pH*Temperature, data=spawning_group_sum)) # <-- NO DIFF 
summary(cum.total.aov <- aov(log(cum.total+1) ~ pH*Temperature, data=spawning_group_total)) # <-- NO DIFF
summary(cum.percap.aov <- aov(log(cum.percap+1) ~ pH*Temperature, data=spawning_group_total)) # <-- NO DIFF
summary(first.big.aov <- aov(first.big ~ pH*Temperature, data=spawning_group_sum)) # <-- NO DIFF 
summary(max.aov <- aov(max ~ pH*Temperature, data=spawning_group_sum)) # <-- NO DIFF 
summary(maxday.aov <- aov(maxday ~ pH*Temperature, data=spawning_group_sum)) # <-- Temperature diff ... remove pH
summary(maxday.aov <- aov(maxday ~ Temperature, data=spawning_group_sum)) # <-- Temperature difference 
summary(release.days <- aov(release.days ~ pH*Temperature, data=spawning_group_sum)) # <-- NO DIFF 
nrow(spawning_group_sum)
# inspect date of maximum larval production by tempeature 
aggregate(maxday ~ Temperature, spawning_group_sum, mean, na.rm=TRUE)                      
152.167-143.833
aggregate(maxday ~ Temperature, spawning_group_sum, sd, na.rm=TRUE)                      
plot(x=spawning_group_sum$Temperature, y=spawning_group_sum$maxday, main="Calendar day of maximum larval relase\nby temperature treatment")

# Compare timing and # released 

# cross correlation function --- try this later ... 
# https://onlinecourses.science.psu.edu/stat510/node/74/
ccf 

# summarize data for each spawning bucket 
fecundity <- group_by(larvae, pH, Temperature) %>% mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% arrange(Date) %>% dplyr::select(Date,CalDay,pH,Temperature,Treatment,total.released,larvae.per.broodcm,cum.total,cum.percap)

# barplots of larvae released - low and ambient separately
# SAVE SIZE 1000W X 500H
#c("gray60", "lightsteelblue3", "gray40", "steelblue")


colors3 <- c("10-Ambient"="gray40", "10-Low"="steelblue", "6-Ambient"="gray60", "6-Low"="lightsteelblue3")
levels(fecundity$Treatment)
levels(fecundity$Treatment)
spawning.titles <- c("Warm winter temp, ambient pH", "Warm winter temp, low pH", "Cool winter temp, ambient pH", "Cool winter temp, low pH")

# save dimensions: 1000x400
for (i in 1:4) {
  print(ggplot(data=subset(fecundity, Treatment==levels(fecundity$Treatment)[i]), aes(x=Date, y=total.released)) + 
    geom_bar(stat="identity",width=1, position = position_dodge(width=2), fill=colors3[i], col="gray30") + 
    ylab("No. of larvae\n(summed across replicates)") + xlab(label=element_blank()) + ggtitle(spawning.titles[i]) + theme_minimal(base_size = 16) + 
    theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"), 
          axis.title = element_text(size=16, colour = "gray30")) +
    scale_x_date(date_breaks = "1 week",date_labels ="%b-%d", 
                 limits=c(min=min(subset(fecundity, Treatment==levels(fecundity$Treatment)[1])$Date)-1,max=max(subset(fecundity, Treatment==levels(fecundity$Treatment)[1])$Date)+1)) +
    scale_y_continuous(limits=c(min=0,max=max(subset(fecundity, Treatment==levels(fecundity$Treatment)[1])$total.released))) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(), panel.border = element_blank()))
}



