setwd("~/Documents/Roberts Lab/O.lurida_Stress/Data")
Histo <- read.csv("2017-Oly-Histo-Results.csv", header=T, stringsAsFactors = T)
Histo$Stage <- as.factor(Histo$Stage)
library(plyr)

# All Populations Combined ------------------------------------------------
All.6 <- subset(Histo, Phase %in% "TEMP" & Treatment %in% "6")
All.10 <- subset(Histo, Phase %in% "TEMP" & Treatment %in% "10")
All.6.Low <- subset(Histo, Phase %in% "OA" & Treatment %in% "6-Low")
All.6.Amb <- subset(Histo, Phase %in% "OA" & Treatment %in% "6-Amb")
All.10.Low <- subset(Histo, Phase %in% "OA" & Treatment %in% "10-Low")
All.10.Amb <- subset(Histo, Phase %in% "OA" & Treatment %in% "10-Amb")
Allp.6 <- data.frame(unclass(100*prop.table(table(All.6$Stage))))
Allp.10 <- data.frame(unclass(100*prop.table(table(All.10$Stage))))
Allp.6.Low <- data.frame(unclass(100*prop.table(table(All.6.Low$Stage))))
Allp.6.Amb <- data.frame(unclass(100*prop.table(table(All.6.Amb$Stage))))
Allp.10.Low <- data.frame(unclass(100*prop.table(table(All.10.Low$Stage))))
Allp.10.Amb <- data.frame(unclass(100*prop.table(table(All.10.Amb$Stage))))
All.p <- data.frame(cbind(Allp.6, Allp.10, Allp.6.Low, Allp.6.Amb, Allp.10.Low, Allp.10.Amb))
colnames(All.p) <- c("6", "10", "6.Low", "6.Amb", "10.Low", "10.Amb")
Stages <- c("Stage 1", "Stage 2", "Stage 3")
png(filename = "~/Documents/Roberts Lab/O.lurida_Stress/Analyses/Gonad-Stage-Pie-Charts.png", width = 600, height = 800)
par(mfrow = c(3, 2), mar=c(.5,1,3.5,.5), oma=c(1,2,3,1), cex.main= 1.3, cex=.75)
pie(table(All.6$Stage), main="6 DegC", radius=1, labels=paste(Stages, "\n", "n=", table(All.6$Stage), "\n", round(All.p$`6`), "%", sep=""))
pie(table(All.10$Stage), main="10 DegC", radius=1, labels=paste(Stages, "\n", "n=", table(All.10$Stage),"\n", round(All.p$`10`), "%", sep=""))
pie(table(All.6.Low$Stage), main="6 DegC, Low pH", radius=1, labels=paste(Stages, "\n", "n=", table(All.6.Low$Stage), "\n", round(All.p$`6.Low`), "%", sep=""))
pie(table(All.10.Low$Stage), main="10 DegC, Low pH", radius=1, labels=paste(Stages, "\n", "n=", table(All.10.Low$Stage),"\n", round(All.p$`10.Low`), "%", sep=""))
pie(table(All.6.Amb$Stage), main="6 DegC, Amb pH", radius=1, labels=paste(Stages, "\n", "n=", table(All.6.Amb$Stage), "\n", round(All.p$`6.Amb`), "%", sep=""))
pie(table(All.10.Amb$Stage), main="10 DegC, Amp pH", radius=1, labels=paste(Stages, "\n", "n=", table(All.10.Amb$Stage), "\n", round(All.p$`10.Amb`), "%", sep=""))
title("Gonad Stage by Treatment, all Populations \nOstrea lurida 2016-2017 Experiment", outer=TRUE) 
dev.off()

# Each Population Separately ----------------------------------------------

Pops <- levels(Histo$Population)
Pop.names <- c("Hood Canal", "South Sound F2", "Fidalgo Bay", "South Sound F1")
for (i in 1:length(Pops)) {
  T <- Pops[i]
  T.6 <- subset(Histo, Population %in% T & Phase %in% "TEMP" & Treatment %in% "6")
  T.10 <- subset(Histo, Population %in% T & Phase %in% "TEMP" & Treatment %in% "10")
  T.6.Low <- subset(Histo, Population %in% T & Phase %in% "OA" & Treatment %in% "6-Low")
  T.6.Amb <- subset(Histo, Population %in% T & Phase %in% "OA" & Treatment %in% "6-Amb")
  T.10.Low <- subset(Histo, Population %in% T & Phase %in% "OA" & Treatment %in% "10-Low")
  T.10.Amb <- subset(Histo, Population %in% T & Phase %in% "OA" & Treatment %in% "10-Amb")
  Tp.6 <- data.frame(unclass(100*prop.table(table(T.6$Stage))))
  Tp.10 <- data.frame(unclass(100*prop.table(table(T.10$Stage))))
  Tp.6.Low <- data.frame(unclass(100*prop.table(table(T.6.Low$Stage))))
  Tp.6.Amb <- data.frame(unclass(100*prop.table(table(T.6.Amb$Stage))))
  Tp.10.Low <- data.frame(unclass(100*prop.table(table(T.10.Low$Stage))))
  Tp.10.Amb <- data.frame(unclass(100*prop.table(table(T.10.Amb$Stage))))
  T.p <- data.frame(cbind(Tp.6, Tp.10, Tp.6.Low, Tp.6.Amb, Tp.10.Low, Tp.10.Amb))
  colnames(T.p) <- c("6", "10", "6.Low", "6.Amb", "10.Low", "10.Amb")
  Stages <- c("Stage 1", "Stage 2", "Stage 3")
  filename <- paste("~/Documents/Roberts Lab/O.lurida_Stress/Analyses/Gonad-Stage-Pie-Charts-", T, ".png", sep="")
  png(filename = filename, width = 600, height = 800)
  par(mfrow = c(3, 2), mar=c(.5,1,3.5,.5), oma=c(1,2,3,1), cex.main= 1.3, cex=.75)
  pie(table(T.6$Stage), main="6 DegC", radius=1, labels=paste(Stages, "\n", "n=", table(T.6$Stage), "\n", round(T.p$`6`), "%", sep=""))
  pie(table(T.10$Stage), main="10 DegC", radius=1, labels=paste(Stages, "\n", "n=", table(T.10$Stage), ", ", round(T.p$`10`), "%", sep=""))
  pie(table(T.6.Low$Stage), main="6 DegC, Low pH", radius=1, labels=paste(Stages, "\n", "n=", table(T.6.Low$Stage), "\n", round(T.p$`6.Low`), "%", sep=""))
  pie(table(T.10.Low$Stage), main="10 DegC, Low pH", radius=1, labels=paste(Stages, "\n", "n=", table(T.10.Low$Stage), "\n", round(T.p$`10.Low`), "%", sep=""))
  pie(table(T.6.Amb$Stage), main="6 DegC, Amb pH", radius=1, labels=paste(Stages, "\n", "n=", table(T.6.Amb$Stage), "\n", round(T.p$`6.Amb`), "%", sep=""))
  pie(table(T.10.Amb$Stage), main="10 DegC, Amb pH", radius=1, labels=paste(Stages, "\n", "n=", table(T.10.Amb$Stage), "\n", round(T.p$`10.Amb`), "%", sep=""))
  title(paste("Gonad Stage by Treatment ", Pop.names[i], "\n", "Ostrea lurida 2016-2017 Experiment", sep=""), outer=TRUE)
  dev.off()
}


### Stats
# Resource: http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

library("gplots")

All.Gonad.Stages <- rbind(All.6, All.6.Amb, All.6.Low, All.10, All.10.Amb, All.10.Low)
CT.Sex.preOA.ALL <- table(subset(All.Gonad.Stages, Phase %in% "TEMP")$Treatment, subset(All.Gonad.Stages, Phase %in% "TEMP")$Sex)
CT.stage.preOA.ALL <- table(subset(All.Gonad.Stages, Phase %in% "TEMP")$Treatment, subset(All.Gonad.Stages, Phase %in% "TEMP")$Stage)
CT.Sex.preOA.ALL <- CT.Sex.preOA.ALL[c(1,4),]
CT.stage.preOA.ALL <- CT.stage.preOA.ALL[c(1,4),]
CT.Sex.postOA.ALL <- table(subset(All.Gonad.Stages, Phase %in% "OA")$Treatment, subset(All.Gonad.Stages, Phase %in% "OA")$Sex)
CT.stage.postOA.ALL <- table(subset(All.Gonad.Stages, Phase %in% "OA")$Treatment, subset(All.Gonad.Stages, Phase %in% "OA")$Stage)
CT.Sex.postOA.ALL <- CT.Sex.postOA.ALL[c(-1,-4),]
CT.stage.postOA.ALL <- CT.stage.postOA.ALL[c(-1,-4),]

# Let's compare the pre-OA stage and sex data
chisq.test(CT.Sex.preOA.ALL) 
chisq.test(CT.stage.preOA.ALL) 

balloonplot(CT.Sex.preOA.ALL, main ="Oly Sex pre-OA, All Populations \nX-squared = 4.6128, df = 4, p-value = 0.3294", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.stage.preOA.ALL, main ="Oly Stage pre-OA, All Populations \nX-squared = 1.0756, df = 2, p-value = 0.584", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Let's compare the post-OA stage and sex data, all treatments
fisher.test(CT.Sex.postOA.ALL)
chisq.test(CT.stage.postOA.ALL)

balloonplot(CT.Sex.postOA.ALL, main ="Oly Sex post OA, All Populations \np-value = 0.9143", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.stage.postOA.ALL, main ="Oly Stage post OA, All Populations \nX-squared = 16.09, df = 6, p-value = 0.01328", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Run same stats on each population separately: 
SN.6.preOA.All <- subset(All.Gonad.Stages, Population %in% "SN" & Phase %in% "TEMP")
NF.6.preOA.All <- subset(All.Gonad.Stages, Population %in% "NF" & Phase %in% "TEMP")
K.6.preOA.All <- subset(All.Gonad.Stages, Population %in% "K" & Phase %in% "TEMP")
HL.6.preOA.All <- subset(All.Gonad.Stages, Population %in% "HL" & Phase %in% "TEMP")

SN.6.postOA.All <- subset(All.Gonad.Stages, Population %in% "SN" & Phase %in% "OA")
NF.6.postOA.All <- subset(All.Gonad.Stages, Population %in% "NF" & Phase %in% "OA")
K.6.postOA.All <- subset(All.Gonad.Stages, Population %in% "K" & Phase %in% "OA")
HL.6.postOA.All <- subset(All.Gonad.Stages, Population %in% "HL" & Phase %in% "OA")

# Contingency tables for each population, by Sex and Stage, for post-OA treatment groups
CT.SN.Sex.preOA <- table(SN.6.preOA.All$Treatment, SN.6.preOA.All$Sex)
CT.SN.stage.preOA <- table(SN.6.preOA.All$Treatment, SN.6.preOA.All$Stage)
CT.NF.Sex.preOA <- table(NF.6.preOA.All$Treatment, NF.6.preOA.All$Sex)
CT.NF.stage.preOA <- table(NF.6.preOA.All$Treatment, NF.6.preOA.All$Stage)
CT.K.Sex.preOA <- table(K.6.preOA.All$Treatment, K.6.preOA.All$Sex)
CT.K.stage.preOA <- table(K.6.preOA.All$Treatment, K.6.preOA.All$Stage)
CT.HL.Sex.preOA <- table(HL.6.preOA.All$Treatment, HL.6.preOA.All$Sex)
CT.HL.stage.preOA <- table(HL.6.preOA.All$Treatment, HL.6.preOA.All$Stage)

# remove extraneous rows
CT.SN.Sex.preOA <- CT.SN.Sex.preOA[c(1,4),]
CT.SN.stage.preOA <- CT.SN.stage.preOA[c(1,4),]
CT.NF.Sex.preOA <- CT.NF.Sex.preOA[c(1,4),]
CT.NF.stage.preOA <- CT.NF.stage.preOA[c(1,4),]
CT.K.Sex.preOA <- CT.K.Sex.preOA[c(1,4),]
CT.K.stage.preOA <- CT.K.stage.preOA[c(1,4),]
CT.HL.Sex.preOA <- CT.HL.Sex.preOA[c(1,4),]
CT.HL.stage.preOA <- CT.HL.stage.preOA[c(1,4),]

# Contingency tables for each population, by Sex and Stage, for post-OA treatment groups
CT.SN.Sex.postOA <- table(SN.6.postOA.All$Treatment, SN.6.postOA.All$Sex)
CT.SN.stage.postOA <- table(SN.6.postOA.All$Treatment, SN.6.postOA.All$Stage)
CT.NF.Sex.postOA <- table(NF.6.postOA.All$Treatment, NF.6.postOA.All$Sex)
CT.NF.stage.postOA <- table(NF.6.postOA.All$Treatment, NF.6.postOA.All$Stage)
CT.K.Sex.postOA <- table(K.6.postOA.All$Treatment, K.6.postOA.All$Sex)
CT.K.stage.postOA <- table(K.6.postOA.All$Treatment, K.6.postOA.All$Stage)
CT.HL.Sex.postOA <- table(HL.6.postOA.All$Treatment, HL.6.postOA.All$Sex)
CT.HL.stage.postOA <- table(HL.6.postOA.All$Treatment, HL.6.postOA.All$Stage)

# remove extraneous rows
CT.SN.Sex.postOA <- CT.SN.Sex.postOA[c(-1,-4),]
CT.SN.stage.postOA <- CT.SN.stage.postOA[c(-1,-4),]
CT.NF.Sex.postOA <- CT.NF.Sex.postOA[c(-1,-4),]
CT.NF.stage.postOA <- CT.NF.stage.postOA[c(-1,-4),]
CT.K.Sex.postOA <- CT.K.Sex.postOA[c(-1,-4),]
CT.K.stage.postOA <- CT.K.stage.postOA[c(-1,-4),]
CT.HL.Sex.postOA <- CT.HL.Sex.postOA[c(-1,-4),]
CT.HL.stage.postOA <- CT.HL.stage.postOA[c(-1,-4),]

# Fisher's exact test, pre OA
fisher.test(CT.SN.Sex.preOA)
fisher.test(CT.K.Sex.preOA)
fisher.test(CT.NF.Sex.preOA)
fisher.test(CT.HL.Sex.preOA)
fisher.test(CT.SN.stage.preOA)
fisher.test(CT.K.stage.preOA)
fisher.test(CT.NF.stage.preOA)
fisher.test(CT.HL.stage.preOA)

# Balloon plots, pre OA
balloonplot(CT.SN.Sex.preOA, main ="Oly Sex Pre-OA, South Sound F1 \np-value = 0.6762", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.Sex.preOA, main ="Oly Sex Pre-OA, South Sound F2 \np-value = 0.5732", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.Sex.preOA, main ="Oly Sex Pre-OA, North Sound F1 \np-value = 0.08721", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.Sex.preOA, main ="Oly Sex Pre-OA, Hood Canal F1 \np-value = 0.006993", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.SN.stage.preOA, main ="Oly Stage Pre-OA, South Sound F1 \np-value = 0.1693", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.stage.preOA, main ="Oly Stage Pre-OA, South Sound F2 \np-value = 0.4068", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.stage.preOA, main ="Oly Stage Pre-OA, North Sound F1 \np-value = 1", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.stage.preOA, main ="Oly Stage Pre-OA, Hood Canal F1 \np-value = 1", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Fisher's exact test, post OA
fisher.test(CT.SN.Sex.postOA)
fisher.test(CT.K.Sex.postOA)
fisher.test(CT.NF.Sex.postOA)
fisher.test(CT.HL.Sex.postOA)
fisher.test(CT.SN.stage.postOA)
fisher.test(CT.K.stage.postOA)
fisher.test(CT.NF.stage.postOA)
fisher.test(CT.HL.stage.postOA)

# Balloon plots, post OA
balloonplot(CT.SN.Sex.postOA, main ="Oly Sex post-OA, South Sound F1 \np-value = 0.6234", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.Sex.postOA, main ="Oly Sex post-OA, South Sound F2 \np-value = 0.02684", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.Sex.postOA, main ="Oly Sex post-OA, North Sound F1 \np-value = 0.2749", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.Sex.postOA, main ="Oly Sex post-OA, Hood Canal F1 \np-value = 0.2337", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.SN.stage.postOA, main ="Oly Stage post-OA, South Sound F1 \np-value = 0.7004", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.stage.postOA, main ="Oly Stage post-OA, South Sound F2 \np-value = 0.1614", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.stage.postOA, main ="Oly Stage post-OA, North Sound F1 \np-value = 0.02735", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.stage.postOA, main ="Oly Stage post-OA, Hood Canal F1 \np-value = 0.7569", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Fisher's exact test, post OA
fisher.test(CT.SN.Sex.postOA)
fisher.test(CT.SN.stage.postOA)
fisher.test(CT.K.Sex.postOA)
fisher.test(CT.K.stage.postOA)
fisher.test(CT.NF.Sex.postOA)
fisher.test(CT.NF.stage.postOA)
fisher.test(CT.HL.Sex.postOA)
fisher.test(CT.HL.stage.postOA)












All.6.Gonad.Stages <- rbind(All.6, All.6.Amb, All.6.Low)
CT.Sex <- table(All.6.Gonad.Stages$Treatment, All.6.Gonad.Stages$Sex)
CT.Sex.postOA <- table(subset(All.6.Gonad.Stages, Phase %in% "OA")$Treatment, subset(All.6.Gonad.Stages, Phase %in% "OA")$Sex)
CT.stage <- table(All.6.Gonad.Stages$Treatment, All.6.Gonad.Stages$Stage)
CT.stage.postOA <- table(subset(All.6.Gonad.Stages, Phase %in% "OA")$Treatment, subset(All.6.Gonad.Stages, Phase %in% "OA")$Stage)
CT.Sex <- CT.Sex[-1:-3,]
CT.stage <- CT.stage[-1:-3,]
CT.Sex.postOA <- CT.Sex.postOA[-1:-4,]
CT.stage.postOA <- CT.stage.postOA[-1:-4,]

balloonplot(CT.Sex, main ="Oly Sex", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.stage, main ="Oly Stage", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.Sex.postOA, main ="Oly Sex, post OA only \np-value = 0.8784", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.stage.postOA, main ="Oly Stage, post OA only \nX-squared = 13.491, df = 2, p-value = 0.001176", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Stats on Sex data
chisq.sex <- chisq.test(CT.Sex) #p-value = 0.2932; also I read that the warning message may be a result of little data.
fisher.test(CT.Sex) #p-value = 0.2942, using Fisher's Exact Test for Count Data 
fisher.test(CT.Sex.postOA) #p-value = 0.8784

# Stats on Stage data
chisq.stage <- chisq.test(CT.stage) #p-value = 0.002478
fisher.test(CT.stage) #p-value = 0.003477 #using this test too, since I had to use it for the Sex data. 
chisq.stage.postOA <- chisq.test(CT.stage.postOA) #p-value = 0.001176
fisher.test(CT.stage.postOA) #p-value = 0.0007534

install.packages("corrplot")
library(corrplot)
corrplot(chisq.stage$residuals, is.corr = F, main="Oly Stage, Correlation Plot")
corrplot(chisq.sex$residuals, is.corr = F, main="Oly Sex, Correlation Plot")
corrplot(chisq.stage.postOA$residuals, is.corr = F, main="Oly Stage - Post OA Only, Correlation Plot")

# Run same stats on each population separately: 
SN.6.postOA <- subset(All.6.Gonad.Stages, Population %in% "SN" & Phase %in% "OA")
NF.6.postOA <- subset(All.6.Gonad.Stages, Population %in% "NF" & Phase %in% "OA")
K.6.postOA <- subset(All.6.Gonad.Stages, Population %in% "K" & Phase %in% "OA")
HL.6.postOA <- subset(All.6.Gonad.Stages, Population %in% "HL" & Phase %in% "OA")

# Contingency tables for each population, by Sex and Stage
CT.SN.Sex <- table(SN.6.postOA$Treatment, SN.6.postOA$Sex)
CT.SN.stage <- table(SN.6.postOA$Treatment, SN.6.postOA$Stage)
CT.NF.Sex <- table(NF.6.postOA$Treatment, NF.6.postOA$Sex)
CT.NF.stage <- table(NF.6.postOA$Treatment, NF.6.postOA$Stage)
CT.K.Sex <- table(K.6.postOA$Treatment, K.6.postOA$Sex)
CT.K.stage <- table(K.6.postOA$Treatment, K.6.postOA$Stage)
CT.HL.Sex <- table(HL.6.postOA$Treatment, HL.6.postOA$Sex)
CT.HL.stage <- table(HL.6.postOA$Treatment, HL.6.postOA$Stage)

# Remove extraneous rows
CT.SN.Sex <- CT.SN.Sex[-1:-4,]
CT.SN.stage <- CT.SN.stage[-1:-4,]
CT.NF.Sex <- CT.NF.Sex[-1:-4,]
CT.NF.stage <- CT.NF.stage[-1:-4,]
CT.K.Sex <- CT.K.Sex[-1:-4,]
CT.K.stage <- CT.K.stage[-1:-4,]
CT.HL.Sex <- CT.HL.Sex[-1:-4,]
CT.HL.stage <- CT.HL.stage[-1:-4,]

# Sample size small, so use fisher's test
fisher.test(CT.SN.Sex)
fisher.test(CT.K.Sex)
fisher.test(CT.NF.Sex)
fisher.test(CT.HL.Sex)

fisher.test(CT.SN.stage)
fisher.test(CT.K.stage)
fisher.test(CT.NF.stage)
fisher.test(CT.HL.stage)

balloonplot(CT.SN.Sex, main ="Oly Sex, South Sound F1 \np-value = 1", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.Sex, main ="Oly Sex, South Sound F2 \np-value = 0.2164", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.Sex, main ="Oly Sex, North Sound F1 \np-value = 0.2629", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.Sex, main ="Oly Sex, Hood Canal F1 \np-value = 0.4545", xlab ="", ylab="",
            label = T, show.margins = FALSE)


balloonplot(CT.SN.stage, main ="Oly Stage, South Sound F1 \np-value = 0.2542", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.stage, main ="Oly Stage, South Sound F2 \np-value = 0.04611", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.stage, main ="Oly Stage, North Sound F1 \np-value = 0.1399", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.stage, main ="Oly Stage, Hood Canal F1 \np-value = 0.2857", xlab ="", ylab="",
            label = T, show.margins = FALSE)

### boneyard

# Histo$Stage <- as.factor(Histo$Stage)
# counts.temp <- count(df=subset(Histo, Phase %in% "TEMP"), c("Phase", "Treatment", "Stage"))
# counts.OA <- count(df=subset(Histo, Phase %in% "OA"), c("Phase", "Treatment", "Stage"))
# counts.all <- count(df=Histo, c("Phase", "Treatment", "Stage"))
