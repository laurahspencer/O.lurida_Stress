# After redoing separate male/female stages, re-do analysis. 
Histology.Jan.redo <- read.csv("Data/2017-Oly-Histo-Results-REDO-January2019.csv", header=T, stringsAsFactors = T, na.strings = "NA")
Histology.Jan.redo.10 <- subset(Histology.Jan.redo, TEMPERATURE==10)
Histology.Jan.redo.10$TEMPERATURE <- as.factor(Histology.Jan.redo.10$TEMPERATURE)         #Convert a few columns to factors 
Histology.Jan.redo.10$PH <- factor(Histology.Jan.redo.10$PH, levels = c("PRE","LOW","AMBIENT")) #reorder pH factors for plots
Histology.Jan.redo.10$Female.Stage <- as.factor(Histology.Jan.redo.10$Female.Stage)   
Histology.Jan.redo.10$Male.Stage <- as.factor(Histology.Jan.redo.10$Male.Stage)
Histology.Jan.redo.10$Sex.redo <- as.factor(Histology.Jan.redo.10$Sex.redo)
Histology.Jan.redo.10$Sex.redo <- droplevels(Histology.Jan.redo.10$Sex.redo, exclude = "")
Histology.Jan.redo.10$Sex.redo <- factor(Histology.Jan.redo.10$Sex.redo, levels=c("I", "M", "HPM", "H", "HPF", "F"))
Histology.Jan.redo.10$Dom.Stage.redo <- as.factor(Histology.Jan.redo.10$Dom.Stage.redo)
Histology.Jan.redo.10$Dom.Stage.redo <- droplevels(Histology.Jan.redo.10$Dom.Stage.redo, exclude = "#N/A")

(13+11)/54 #pre 6
(9+12)/39 #low 6
(15+16)/39 #amb 6

(11+27)/54 #pre 10
(13+21)/39 #low 10
(19+7)/39 #amb 10



# Prepare contingency tables 

CT.sex <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Sex.redo)
CT.sex.pop <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Sex.redo, subset(Histology.Jan.redo.10, TEMPERATURE==10)$POPULATION)
colnames(CT.sex) <- c("Undifferentiated", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
28/sum(CT.sex)
(CT.sex[1,])/sum(CT.sex[1,]) #pre treatment
0.25925926+0.12962963+0.12962963 # % hermaphroditic
0.38888889+0.25925926 # % male/male dominant pre-treatment
0.12962963+0.03703704 # % female/female dominant pre-treatment

(CT.sex[2,])/sum(CT.sex[2,]) #low pH
0.17948718+0.17948718+0.25641026 # % hermaphroditic low-pH
0.28205128+0.17948718 # % male/male dominant low-pH
0.25641026+0.05128205 # % female/female dominant low-pH

(CT.sex[3,])/sum(CT.sex[3,]) #ambient pH
0.17948718+0.17948718+0.20512821 # % hermaphroditic amb-pH
0.20512821+0.17948718 # % male/male dominant amb-pH
0.20512821+0.15384615 # % female/female dominant amb-pH

CT.malestage <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Male.Stage)
CT.malestage.pop <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Male.Stage, subset(Histology.Jan.redo.10, TEMPERATURE==10)$POPULATION)

CT.femstage <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Female.Stage)
CT.femstage.pop <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Female.Stage, subset(Histology.Jan.redo.10, TEMPERATURE==10)$POPULATION)

CT.domsex.stage <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Dom.Stage.redo)
CT.domsex.stage.pop <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10)$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10)$Dom.Stage.redo, subset(Histology.Jan.redo.10, TEMPERATURE==10)$POPULATION)
colnames(CT.domsex.stage) <- c("Empty Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
rowSums(CT.domsex.stage)
3/54 # % stage 4 pre
0/39 # % stage 4 low
2/39 # % stage 4 amb

11/54# % stage 3 pre
13/39 # % stage 3 low
19/39 # % stage 3 amb

27/54 # % stage 2 pre
21/39 # % stage 2 low
7/39 # % stage 2 amb

11/54 # % stage 1 pre
4/39 # % stage 1 low
8/39 # % stage 1 amb

2/54 # % stage 0 pre
1/39 # % stage 0 low
3/39 # % stage 0 amb

rowSums(CT.domsex.stage)
100*CT.domsex.stage[1,]/54
50.000000+20.370370 #% stage 2&3 in pre
100*CT.domsex.stage[2,]/39 #low pH 
53.846154+33.333333 #% stage 2&3 in low pH
100*CT.domsex.stage[3,]/39 #ambient pH
17.948718+48.717949 #% stage 2&3 in ambient pH ... likely spawned during pH treatment ? 

# barplots - saved as 450x650 

------ # Dominant stage
par(mar=c(5, 5, 4, 19))
print(barplot(t(prop.table(CT.domsex.stage, 1)), main="Gonad stage by sex, pre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.domsex.stage, 1)), main="Gonad stage, all populations, 10°C\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.domsex.stage[-1,], simulate.p.value = T, B = 100000) #ambient vs. low, X-squared=12.458, p=0.007699
chisq.test(CT.domsex.stage[-2,], simulate.p.value = T, B = 100000) #pre to ambient, X-squared=12.682 p=0.009299
chisq.test(CT.domsex.stage[-3,], simulate.p.value = T, B = 100000) #pre to low, X-squared=5.2335, p=0.2837

(11+27)/(11+27+2+11+3)

# female only 
colnames(CT.femstage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)")
#par(mar=c(5, 5, 4, 19))
print(barplot(t(prop.table(CT.femstage, 1)), main="Female", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.femstage[-1,-1], simulate.p.value = T, B = 100000) #ambient vs. low, p=1
chisq.test(CT.femstage[-2,-1], simulate.p.value = T, B = 100000) #pre to ambient, p=0.1439
chisq.test(CT.femstage[-3,-1], simulate.p.value = T, B = 100000) #pre to low, p=0.06893

# male only
colnames(CT.malestage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

print(barplot(t(prop.table(CT.malestage, 1)), main="Male", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-0.5, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.malestage[-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low, p=0.01399
chisq.test(CT.malestage[-2,-1], simulate.p.value = T, B = 10000) #pre to ambient, X-squared=20.434, p=0.006993
chisq.test(CT.malestage[-3,-1], simulate.p.value = T, B = 10000) #pre to low, X-squared=16.548, p=0.9261

par(mar=c(5, 5, 4, 17))
#  Sex
print(barplot(t(prop.table(CT.sex, 1)), main="Gonad sex, all populations, 10°C\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.sex[-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: X-squared=2.9, p=0.733
chisq.test(CT.sex[-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: X-squared=8.02, p=0.158
chisq.test(CT.sex[-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: X-squared=8.1267, p=0.1598


------- # Dominant gonad stage by each population

# Fidalgo Bay
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"NF"], 1)), main="Fidalgo Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.domsex.stage.pop[,,"NF"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; p=0.01199
fisher.test(CT.domsex.stage.pop[,,"NF"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; p=0.2318
fisher.test(CT.domsex.stage.pop[,,"NF"][-3,], simulate.p.value = T, B = 10000) #pre to low; p=0.3916

# Dabob Bay
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"HL"], 1)), main="Dabob Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.domsex.stage.pop[,,"HL"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"HL"][-2,], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"HL"][-3,], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

# Oyster Bay Cohort 1
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"SN"], 1)), main="Oyster Bay C1", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.domsex.stage.pop[,,"SN"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"SN"][-2,], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"SN"][-3,], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

# Oyster Bay Cohort 2
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"K"], 1)), main="Oyster Bay C2", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.domsex.stage.pop[,,"K"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"K"][-2,], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.domsex.stage.pop[,,"K"][-3,], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

----- # Male gonad stage by each population

# Fidalgo Bay
print(barplot(t(prop.table(CT.malestage.pop[,,"NF"], 1)), main="Fidalgo Bay Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"NF"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; NOT DFF
fisher.test(CT.malestage.pop[,,"NF"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.03996
fisher.test(CT.malestage.pop[,,"NF"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; NOT DIFF

# Dabob Bay
print(barplot(t(prop.table(CT.malestage.pop[,,"HL"], 1)), main="Dabob Bay Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"HL"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.malestage.pop[,,"HL"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.malestage.pop[,,"HL"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

# Oyster Bay Cohort 1
print(barplot(t(prop.table(CT.malestage.pop[,,"SN"], 1)), main="Oyster Bay C1 Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"SN"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.malestage.pop[,,"SN"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.malestage.pop[,,"SN"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

# Oyster Bay Cohort 2
print(barplot(t(prop.table(CT.malestage.pop[,,"K"], 1)), main="Oyster Bay C2 Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"K"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; NOT DIFF
fisher.test(CT.malestage.pop[,,"K"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;  NOT DIFF
fisher.test(CT.malestage.pop[,,"K"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;  NOT DIFF

---- # Female gonad stage by each population
  
  # Fidalgo Bay
print(barplot(t(prop.table(CT.femstage.pop[,,"NF"], 1)), main="Fidalgo Bay Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"NF"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low;  NOT DIFF
fisher.test(CT.femstage.pop[,,"NF"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.004995
fisher.test(CT.femstage.pop[,,"NF"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.01998

# Dabob Bay
print(barplot(t(prop.table(CT.femstage.pop[,,"HL"], 1)), main="Dabob Bay Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"HL"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low;   NOT DIFF
fisher.test(CT.femstage.pop[,,"HL"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;   NOT DIFF
fisher.test(CT.femstage.pop[,,"HL"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;   NOT DIFF

# Oyster Bay Cohort 1
print(barplot(t(prop.table(CT.femstage.pop[,,"SN"], 1)), main="Oyster Bay C1 Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"SN"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low;   NOT DIFF
fisher.test(CT.femstage.pop[,,"SN"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;   NOT DIFF
fisher.test(CT.femstage.pop[,,"SN"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;   NOT DIFF

# Oyster Bay Cohort 2
print(barplot(t(prop.table(CT.femstage.pop[,,"K"], 1)), main="Oyster Bay C2 Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"K"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; NOT DIFF
fisher.test(CT.femstage.pop[,,"K"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient;   NOT DIFF
fisher.test(CT.femstage.pop[,,"K"][-3,-1], simulate.p.value = T, B = 10000) #pre to low;   NOT DIFF

4/39

# --------- Compare sexes within Populations 

print(barplot(t(prop.table(CT.sex.pop[,,"NF"], 1)), main="Gonad sex, Fidalgo Bay\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.sex.pop[,,"NF"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH:   NOT DIFF
fisher.test(CT.sex.pop[,,"NF"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.007992
fisher.test(CT.sex.pop[,,"NF"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.02198

print(barplot(t(prop.table(CT.sex.pop[,,"HL"], 1)), main="Gonad sex, Dabob Bay\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.sex.pop[,,"HL"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH:  NOT DIFF
fisher.test(CT.sex.pop[,,"HL"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH:  NOT DIFF
fisher.test(CT.sex.pop[,,"HL"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH:  NOT DIFF

print(barplot(t(prop.table(CT.sex.pop[,,"SN"], 1)), main="Gonad sex, Oyster Bay C1\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.sex.pop[,,"SN"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: NOT DIFF
fisher.test(CT.sex.pop[,,"SN"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH:  NOT DIFF
fisher.test(CT.sex.pop[,,"SN"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: NOT DIFF

print(barplot(t(prop.table(CT.sex.pop[,,"K"], 1)), main="Gonad sex, Oyster Bay C2\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.sex.pop[,,"K"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: NOT DIFF
fisher.test(CT.sex.pop[,,"K"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: NOT DIFF
fisher.test(CT.sex.pop[,,"K"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH:  NOT DIFF

# Generate male & female plots/tables with stages only dominant stage

CT.stage.female <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="F" | Sex.redo=="HPF")$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="F" | Sex.redo=="HPF")$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.female, 1)), main="Gonad stage - Female Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))

CT.stage.male <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="M" | Sex.redo=="HPM")$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="M" | Sex.redo=="HPM")$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.male, 1)), main="Gonad stage - Male Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))

CT.stage.herm <- table(subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="H" | Sex.redo=="I")$PH, subset(Histology.Jan.redo.10, TEMPERATURE==10 & Sex.redo=="H" | Sex.redo=="I")$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.herm, 1)), main="Gonad stage - Herm. Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))

