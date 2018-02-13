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
All.6.Gonad.Stages <- rbind(All.6, All.6.Amb, All.6.Low)
CT.Sex <- table(All.6.Gonad.Stages$Treatment, All.6.Gonad.Stages$Sex)
CT.stage <- table(All.6.Gonad.Stages$Treatment, All.6.Gonad.Stages$Stage)
CT.Sex <- CT.Sex[-1:-3,]
CT.stage <- CT.stage[-1:-3,]

balloonplot(CT.Sex, main ="Oly Sex", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.stage, main ="Oly Stage", xlab ="", ylab="",
            label = T, show.margins = FALSE)

library("graphics")
mosaicplot(CT.Sex, shade = TRUE, las=2, main = "Oly Sex")
mosaicplot(CT.stage, shade = TRUE, las=2, main = "Oly Stage")

# Stats on Sex data
chisq.sex <- chisq.test(CT.Sex) #p-value = 0.2932; also I read that the warning message may be a result of little data.
fisher.test(CT.Sex) #p-value = 0.2942, using Fisher's Exact Test for Count Data 

# Stats on Stage data
chisq.stage <- chisq.test(CT.stage) #p-value = 0.002478
fisher.test(CT.stage) #p-value = 0.003477 #using this test too, since I had to use it for the Sex data. 

chisq.stage$observed
chisq.stage$expected
round(chisq.stage$residuals, 3)

install.packages("corrplot")
library(corrplot)
corrplot(chisq.stage$residuals, is.corr = F, main="Oly Stage, Correlation Plot")
corrplot(chisq.sex$residuals, is.corr = F, main="Oly Sex, Correlation Plot")

# Run same stats on each population separately: 
SN.6 <- subset(All.6.Gonad.Stages, Population %in% "SN")
NF.6 <- subset(All.6.Gonad.Stages, Population %in% "NF")
K.6 <- subset(All.6.Gonad.Stages, Population %in% "K")
HL.6 <- subset(All.6.Gonad.Stages, Population %in% "HL")

# Contingency tables for each population, by Sex and Stage
CT.SN.Sex <- table(SN.6$Treatment, SN.6$Sex)
CT.SN.stage <- table(SN.6$Treatment, SN.6$Stage)
CT.NF.Sex <- table(NF.6$Treatment, NF.6$Sex)
CT.NF.stage <- table(NF.6$Treatment, NF.6$Stage)
CT.K.Sex <- table(K.6$Treatment, K.6$Sex)
CT.K.stage <- table(K.6$Treatment, K.6$Stage)
CT.HL.Sex <- table(HL.6$Treatment, HL.6$Sex)
CT.HL.stage <- table(HL.6$Treatment, HL.6$Stage)

# Remove extraneous rows
CT.SN.Sex <- CT.SN.Sex[-1:-3,]
CT.SN.stage <- CT.SN.stage[-1:-3,]
CT.NF.Sex <- CT.NF.Sex[-1:-3,]
CT.NF.stage <- CT.NF.stage[-1:-3,]
CT.K.Sex <- CT.K.Sex[-1:-3,]
CT.K.stage <- CT.K.stage[-1:-3,]
CT.HL.Sex <- CT.HL.Sex[-1:-3,]
CT.HL.stage <- CT.HL.stage[-1:-3,]

chisq.test(CT.SN.Sex)
chisq.test(CT.SN.stage)
chisq.test(CT.NF.Sex)
chisq.test(CT.NF.stage)
chisq.test(CT.K.Sex)
chisq.test(CT.K.stage)
chisq.test(CT.HL.Sex)
chisq.test(CT.HL.stage)

# Sample size small, so use fisher's test
fisher.test(CT.SN.Sex)
fisher.test(CT.SN.stage)
fisher.test(CT.NF.Sex)
fisher.test(CT.NF.stage)
fisher.test(CT.K.Sex)
fisher.test(CT.K.stage)
fisher.test(CT.HL.Sex)
fisher.test(CT.HL.stage)


balloonplot(CT.SN.Sex, main ="Oly Sex, South Sound F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.SN.stage, main ="Oly Stage, South Sound F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)

balloonplot(CT.NF.Sex, main ="Oly Sex, North Sound F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.stage, main ="Oly Stage, North Sound F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)

balloonplot(CT.K.Sex, main ="Oly Sex, South Sound F2", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.K.stage, main ="Oly Stage, South Sound F2", xlab ="", ylab="",
            label = T, show.margins = FALSE)

balloonplot(CT.HL.Sex, main ="Oly Sex, Hood Canal F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.stage, main ="Oly Stage, Hood Canal F1", xlab ="", ylab="",
            label = T, show.margins = FALSE)

### boneyard

# Histo$Stage <- as.factor(Histo$Stage)
# counts.temp <- count(df=subset(Histo, Phase %in% "TEMP"), c("Phase", "Treatment", "Stage"))
# counts.OA <- count(df=subset(Histo, Phase %in% "OA"), c("Phase", "Treatment", "Stage"))
# counts.all <- count(df=Histo, c("Phase", "Treatment", "Stage"))
