Histo.postOA.redo <- read.csv("../Data/2017-Oly-Histo-Results-REDO.csv", header=T, stringsAsFactors = T, na.strings = "TBD")
Histo.postOA.redo$STAGE <- as.factor(Histo.postOA.redo$STAGE)
View(Histo.postOA.redo)

### Stats
# Resource: http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
library("gplots")
CT.SEX.postOA.ALL.redo <- table(Histo.postOA.redo$TREATMENT, Histo.postOA.redo$SEX)
CT.STAGE.postOA.ALL.redo <- table(Histo.postOA.redo$TREATMENT, Histo.postOA.redo$STAGE)

# Let's compare the post-OA STAGE and SEX data
fisher.test(CT.SEX.postOA.ALL.redo) 
fisher.test(CT.STAGE.postOA.ALL.redo) 

balloonplot(CT.SEX.postOA.ALL.redo, main ="Oly SEX pre-OA, All Populations \np-value = 0.3509", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.STAGE.postOA.ALL.redo, main ="Oly STAGE pre-OA, All Populations \np-value = 0.1012", xlab ="", ylab="",
            label = T, show.margins = FALSE)

# Run same stats on each population separately: 
SN.6.postOA.All.redo <- subset(Histo.postOA.redo, POPULATION %in% "SN")
NF.6.postOA.All.redo <- subset(Histo.postOA.redo, POPULATION %in% "NF")
K.6.postOA.All.redo <- subset(Histo.postOA.redo, POPULATION %in% "K")
HL.6.postOA.All.redo <- subset(Histo.postOA.redo, POPULATION %in% "HL")

# Contingency tables for each population, by SEX and STAGE, for post-OA treatment groups
CT.SN.SEX.postOA.redo <- table(SN.6.postOA.All.redo$TREATMENT, SN.6.postOA.All.redo$SEX)
CT.SN.STAGE.postOA.redo <- table(SN.6.postOA.All.redo$TREATMENT, SN.6.postOA.All.redo$STAGE)
CT.NF.SEX.postOA.redo <- table(NF.6.postOA.All.redo$TREATMENT, NF.6.postOA.All.redo$SEX)
CT.NF.STAGE.postOA.redo <- table(NF.6.postOA.All.redo$TREATMENT, NF.6.postOA.All.redo$STAGE)
CT.K.SEX.postOA.redo <- table(K.6.postOA.All.redo$TREATMENT, K.6.postOA.All.redo$SEX)
CT.K.STAGE.postOA.redo <- table(K.6.postOA.All.redo$TREATMENT, K.6.postOA.All.redo$STAGE)
CT.HL.SEX.postOA.redo <- table(HL.6.postOA.All.redo$TREATMENT, HL.6.postOA.All.redo$SEX)
CT.HL.STAGE.postOA.redo <- table(HL.6.postOA.All.redo$TREATMENT, HL.6.postOA.All.redo$STAGE)

# Fisher's exact test, pre OA
fisher.test(CT.SN.SEX.postOA.redo)
fisher.test(CT.NF.SEX.postOA.redo)
fisher.test(CT.HL.SEX.postOA.redo)
fisher.test(CT.SN.STAGE.postOA.redo)
fisher.test(CT.NF.STAGE.postOA.redo)
fisher.test(CT.HL.STAGE.postOA.redo)

# Balloon plots, post OA
balloonplot(CT.SN.SEX.postOA.redo, main ="Oly SEX post-OA, South Sound F1 \np-value = 0.2723", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.SEX.postOA.redo, main ="Oly SEX post-OA, North Sound F1 \np-value = 0.3415", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.SEX.postOA.redo, main ="Oly SEX post-OA, Hood Canal F1 \np-value = 0.4761", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.SN.STAGE.postOA.redo, main ="Oly STAGE post-OA, South Sound F1 \np-value = 0.8189", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.NF.STAGE.postOA.redo, main ="Oly STAGE post-OA, North Sound F1 \np-value = 0.1394", xlab ="", ylab="",
            label = T, show.margins = FALSE)
balloonplot(CT.HL.STAGE.postOA.redo, main ="Oly STAGE post-OA, Hood Canal F1 \np-value = 0.9413", xlab ="", ylab="",
            label = T, show.margins = FALSE)