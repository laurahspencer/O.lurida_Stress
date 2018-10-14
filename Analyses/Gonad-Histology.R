### Assess gonad stage and sex differences at different time points. Build contingency tables and compare gonad stages using chi-squared / fisher test (sample size dependent)

# Resource: http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
# Resource: log-linear glm to compare multidimentional contingency tables - not used in this analysis. 

library(gplots)       #load packages 
library(corrplot)
library(cowplot)

rm(list=ls())         #start script by deleting all objects - clean slate 

# Read in histology data. 
Histology <- read.csv("Data/2017-Oly-Histo-Results-REDO-redostage5.csv", header=T, stringsAsFactors = T, na.strings = "NA")
Histology$TEMPERATURE <- as.factor(Histology$TEMPERATURE)         #Convert a few columns to factors 
Histology$Dominant.Stage <- as.factor(Histology$Dominant.Stage)   
Histology$Secondary.Stage <- as.factor(Histology$Secondary.Stage)

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

# Assess how chilled / not chilled groups differ using correlation plots 
corrplot(Pre.domstage.chisq$residuals, is.cor = FALSE) #Which cells have positive (blue) and negative (red) associations? More 2 & 3 stages in 10C group, and more regressed (5) in 6C group 

# Calculate % contribution for each cell and plot 
round(Pre.domstage.chisq.contrib <- 100*Pre.domstage.chisq$residuals^2/Pre.domstage.chisq$statistic, 2)
corrplot(Pre.domstage.chisq.contrib, is.cor = FALSE)  # More stage 5 oysters in chilled, more stage 2 & 3 oysters in not-chilled group. 

# Result: sign. difference between 10C (not chilled) and 6C (chilled) gonad stages, where 6C had more regressed/partially spawned gonad tissue (22 oysters, versus 4), and 10C had more oysters in advanced gametogenesis (19 vs. 8, stage 2), and more ripe gonad (20 vs. 8). 

# Plot results 2 ways: balloon plot, stacked bar plot 
colnames(CT.domstage.pre) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially\nSpawned (4)", "Regressing(5)")
rownames(CT.domstage.pre) <- c("Not-chilled", "Chilled")

# balloon plot from contingency table 
balloonplot(CT.domstage.pre, main ="Oly STAGE pre-OA, All Populations \n p-value = 7.413e-05", xlab ="", ylab="",label = T, show.margins = FALSE)

# stacked bar plot 
par(mfrow=c(1, 1), mar=c(5, 5, 4, 16)) # plot window sholud be made very wide for formatting 
barplot(t(CT.domstage.pre), main="Gonad stages pre-pH treatment\np = 7.413e-05", xlab="", ylab="No. oysters", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.5, cex.names = 1.5, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Stage", cex=1.5))


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

# Assess how chilled / not chilled groups differ using correlation plots 
corrplot(pH.domstage.chisq$residuals, is.cor = FALSE) #Which cells have positive (blue) and negative (red) associations? 

# Calculate % contribution for each cell and plot  
round(pH.domstage.chisq.contrib <- 100*pH.domstage.chisq$residuals^2/pH.domstage.chisq$statistic, 2) #pretty even distribution of influence (except stag 1)
CT.domstage.6.pH # more stage 3 in ambient pH group, more stage 5 in low pH group. 

# Result: sign. difference between low pH and ambient pH in chilled group, with more ripe gametes in ambient (16 vs. 6 stage 3); more early gameto. and spawned/resorbing in low pH (7 vs. 3 stage 1; 8 vs. 2 stage 5)

#NOTE: No brooding oysters were found in either the pre- or post- pH treatment sampling, indication of no active spawning. 

# Stacked barplot 
colnames(CT.domstage.6.pH) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially\nSpawned (4)", "Regressing (5)")
rownames(CT.domstage.6.pH) <- c("Ambient pH", "Low pH")
par(mfrow=c(1, 1), mar=c(5, 5, 4, 16))

barplot(t(CT.domstage.6.pH), main="Gonad stages after\n7wk pH Treatment (chilled group)", xlab="", ylab="No. oysters", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.5, cex.names = 1.5, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Stage", cex=1.5))

# --- compare not chilled-amb pH to not chilled-low pH 
CT.sex.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$SEX)
CT.domstage.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$Dominant.Stage)
CT.secstage.10.pH <- table(subset(Histology, PH!="PRE" & TEMPERATURE==10)$PH, subset(Histology, PH!="PRE" & TEMPERATURE==10)$Secondary.Stage)

fisher.test(CT.sex.10.pH) #0.8848 - no diff between sex ratios. 
fisher.test(CT.domstage.10.pH) #0.08 - no diff in dominant stage
fisher.test(CT.secstage.10.pH) #0.04915 - almost but not quite diff in secondary stage 

# Stacked barplot 
colnames(CT.domstage.10.pH) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially\nSpawned (4)", "Regressing (5)")
rownames(CT.domstage.10.pH) <- c("Ambient pH", "Low pH")
par(mfrow=c(1, 1), mar=c(5, 5, 4, 16))
barplot(t(CT.domstage.10.pH), main="Gonad stages after\n7wk pH Treatment (not chilled group)", xlab="", ylab="No. oysters", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.5, cex.names = 1.5, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Stage", cex=1.5))


# ---------------> QUESTION 3.  Did gonads develop/regress during pH exposure? Assess chilled/not chilled groups separately. 

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
colnames(CT.domstage.6) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially\nSpawned (4)", "Regressing(5)")
colnames(CT.domstage.10) <- c("Undifferentiated (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Partially\nSpawned (4)", "Regressing(5)")

jpeg(filename = "Results/Gonad-barplot-chilled-stage", height = 750, width = 520)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 2))
print(chilled.stage <- barplot(t(prop.table(CT.domstage.6, 1)), main="A) Stage, pre- & post-pH treatment\nchilled group (6C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = F, title="Gonad Stage", cex=1.5))
dev.off()

jpeg(filename = "Results/Gonad-barplot-notchilled-stage", height = 750, width = 750)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 18))
print(notchilled.stage <- barplot(t(prop.table(CT.domstage.10,1)), main="B) Stage, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Gonad Stage", cex=1.5)))
dev.off()

# Conclusion: chilled group gonad developed less in low pH treatment. Not chilled group was already developed prior to pH treatment, no significant development or resorption occurred during pH treatment. 

# ---------------> # BONUS 2 bar plots for sex ratios - chilled (pre, amb, low), and not chilled (pre, amb, low)

# tables for stacked bar plots 
Histology$SEX <- factor(Histology$SEX, levels=c("I", "M", "HPM", "H", "HPF", "F"))

CT.sex.6 <- table(subset(Histology, TEMPERATURE==6)$PH, subset(Histology, TEMPERATURE==6)$SEX)
CT.sex.10 <- table(subset(Histology, TEMPERATURE==10)$PH, subset(Histology, TEMPERATURE==10)$SEX)

colnames(CT.sex.6) <- c("Undifferentiated", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
colnames(CT.sex.10) <- c("Undifferentiated", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")

jpeg(filename = "Results/Gonad-barplot-chilled-sex", height = 750, width = 520)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 2))
return(chilled.sex <- barplot(t(prop.table(CT.sex.6, 1)), main="C) Gonad sex, pre- & post-pH treatment\nchilled group (6C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, title="Gonad Sex", cex=1.5))
dev.off()

jpeg(filename = "Results/Gonad-barplot-notchilled-sex", height = 750, width = 750)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 18))
print(notchilled.sex <- barplot(t(prop.table(CT.sex.10,1)), main="D) Gonad sex, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"), cex.main=1.5, cex.lab=1.5, cex.axis = 1.2, cex.names = 1.2, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Gonad Sex", cex=1.5)))
dev.off()

# all in one 
jpeg(filename = "Results/Gonad-plots.jpeg", width = 1270, height = 1500) 
par(mfrow = c(2,2)) #2x2 grid for plots

par(mar=c(5, 5, 4, 2), fig=c(0,0.4094488,0.5,1), new=TRUE)
barplot(t(prop.table(CT.domstage.6, 1)), main="A) Stage, pre- & post-pH treatment\nchilled group (6C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=2, cex.lab=2, cex.axis = 2, cex.names = 2, legend.text = F)

par(mar=c(5, 5, 4, 21), fig=c(0.4094488,1,0.5,1), new=TRUE)
barplot(t(prop.table(CT.domstage.10,1)), main="B) Stage, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=2, cex.lab=2, cex.axis = 2, cex.names = 2, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Gonad Stage", cex=1.8))

par(mar=c(5, 5, 4, 2), fig=c(0,0.4094488,0,0.5), new=TRUE)
barplot(t(prop.table(CT.sex.6, 1)), main="C) Gonad sex, pre- & post-pH treatment\nchilled group (6C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"), cex.main=2, cex.lab=2, cex.axis = 2, cex.names = 2, legend.text = F)

par(mar=c(5, 5, 4, 21), fig=c(0.4094488,1,0,0.5), new=TRUE)
barplot(t(prop.table(CT.sex.10,1)), main="D) Gonad sex, pre- & post-pH treatment\nnot chilled (10C)", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"), cex.main=2, cex.lab=2, cex.axis = 2, cex.names = 2, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.45, 0), title="Gonad Sex", cex=2))

dev.off()


# ---------------> Summary statistics 

100*round(prop.table(summary(Histology$SEX)), 3)    # Percent of each sex (all oysters)
length(Histology$SEX)                               # Number of oysters sampled for histology 
