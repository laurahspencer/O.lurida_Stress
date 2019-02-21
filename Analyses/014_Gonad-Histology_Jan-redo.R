# After redoing separate male/female stages, re-do analysis. 
Histology.Jan.redo <- read.csv("Data/2017-Oly-Histo-Results-REDO-January2019.csv", header=T, stringsAsFactors = T, na.strings = "NA")

#Histology.Jan.redo <- subset(Histology.Jan.redo, TEMPERATURE==6)
Histology.Jan.redo$TEMPERATURE <- as.factor(Histology.Jan.redo$TEMPERATURE)         #Convert a few columns to factors 
Histology.Jan.redo$PH <- factor(Histology.Jan.redo$PH, levels = c("PRE","LOW","AMBIENT")) #reorder pH factors for plots
Histology.Jan.redo$Female.Stage <- as.factor(Histology.Jan.redo$Female.Stage)   
Histology.Jan.redo$Male.Stage <- as.factor(Histology.Jan.redo$Male.Stage)
Histology.Jan.redo$Sex.redo <- as.factor(Histology.Jan.redo$Sex.redo)
Histology.Jan.redo$Sex.redo <- droplevels(Histology.Jan.redo$Sex.redo, exclude = "")
Histology.Jan.redo$Sex.redo <- factor(Histology.Jan.redo$Sex.redo, levels=c("I", "M", "HPM", "H", "HPF", "F"))
Histology.Jan.redo$Dom.Stage.redo <- as.factor(Histology.Jan.redo$Dom.Stage.redo)
Histology.Jan.redo$Dom.Stage.redo <- droplevels(Histology.Jan.redo$Dom.Stage.redo, exclude = "#N/A")

# Prepare contingency tables 

CT.sex <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Sex.redo)
CT.sex.pop <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Sex.redo, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)
colnames(CT.sex) <- c("Undifferentiated", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")

(CT.sex[1,])/sum(CT.sex[1,]) #pre treatment
0.12962963+0.05555556+0.29629630 # % hermaphroditic
0.38888889+0.12962963 # % male/male dominant pre-treatment
0.29629630+0.03703704 # % female/female dominant pre-treatment

(CT.sex[2,])/sum(CT.sex[2,]) #low pH
0.23076923+0.17948718+0.12820513 # % hermaphroditic low-pH
0.30769231+0.23076923 # % male/male dominant low-pH
0.12820513+0.07692308 # % female/female dominant low-pH

(CT.sex[3,])/sum(CT.sex[3,]) #ambient pH
0.35897436+0.02564103+0.12820513 # % hermaphroditic amb-pH
0.23076923+0.35897436 # % male/male dominant amb-pH
0.12820513+0.17948718 # % female/female dominant amb-pH

CT.domsex.stage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Dom.Stage.redo)
CT.domsex.stage.pop <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)
colnames(CT.domsex.stage) <- c("Empty Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

CT.malestage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Male.Stage)
CT.malestage.pop <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Male.Stage, Histology.Jan.redo$POPULATION)

CT.femstage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Female.Stage)
CT.femstage.pop <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Female.Stage, Histology.Jan.redo$POPULATION)

rowSums(CT.domsex.stage)

16/54 # % stage 4 pre
5/39 # % stage 4 low
1/39 # % stage 4 amb

13/54 # % stage 3 pre
9/39 # % stage 3 low
15/39 # % stage 3 amb

11/54 # % stage 2 pre
12/39 # % stage 2 low
16/39 # % stage 2 amb

12/54 # % stage 1 pre
12/39 # % stage 1 low
4/39 # % stage 1 amb

2/54 # % stage 0 pre
1/39 # % stage 0 low
3/39 # % stage 0 amb

rowSums(CT.domsex.stage)
100*CT.domsex.stage[1,]/54
20.370370+24.074074 
100*CT.domsex.stage[2,]/39 #low pH 
38.46154+23.076923
100*CT.domsex.stage[3,]/39 #ambient pH
41.025641+33.333333

# barplots - save as 845x650

------ # Dominant stage

print(barplot(t(prop.table(CT.domsex.stage, 1)), main="Gonad stage, all populations\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

chisq.test(CT.domsex.stage[-1,], simulate.p.value = T, B = 10000) #ambient vs. low, X-squared= 9.7381, p-value = 0.0364
chisq.test(CT.domsex.stage[-2,], simulate.p.value = T, B = 10000) #pre to ambient, X-squared=16.514 p=0.0019
chisq.test(CT.domsex.stage[-3,], simulate.p.value = T, B = 10000) #pre to low, X-squared=4.5654, p=0.3521

# female only 
colnames(CT.femstage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
par(mar=c(5, 5, 4, 20))
print(barplot(t(prop.table(CT.femstage, 1)), main="Female", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.femstage[-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low, p=0.2185
chisq.test(CT.femstage[-2,-1], simulate.p.value = T, B = 10000) #pre to ambient, p=0.07639
chisq.test(CT.femstage[-3,-1], simulate.p.value = T, B = 10000) #pre to low, p=0.3464

# male only
colnames(CT.malestage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

print(barplot(t(prop.table(CT.malestage, 1)), main="Male", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-.5, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.malestage[-1,], simulate.p.value = T, B = 10000) #ambient vs. low, X-squared = 8.975, p-value = 0.0297
chisq.test(CT.malestage[-2,], simulate.p.value = T, B = 10000) #pre to ambient, X-squared=24.197, p=9.999e-05
chisq.test(CT.malestage[-3,], simulate.p.value = T, B = 10000) #pre to low, X-squared=15.159, p=0.0011

par(mar=c(5, 5, 4, 17))
#  Sex
print(barplot(t(prop.table(CT.sex, 1)), main="Dominant gonad sex", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
chisq.test(CT.sex[-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: X-squared=7.3468, p=0.2018
chisq.test(CT.sex[-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: X-squared=15.148, p=0.006499
chisq.test(CT.sex[-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: X-squared=8.57, p=0.1319

------- # Dominant gonad stage by each population

# Fidalgo Bay
pdf(file="Results/6C-gonad-stage-FB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"NF", "6"], 1)), main="Fidalgo Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5854
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.1239
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.2677

# Dabob Bay
pdf(file="Results/6C-gonad-stage-DB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"HL", "6"], 1)), main="Dabob Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5395
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.008799
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.7423

# Oyster Bay Cohort 1
pdf(file="Results/6C-gonad-stage-OB1", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"SN", "6"], 1)), main="Oyster Bay C1", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; P=1
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.2458
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.1698

# Oyster Bay Cohort 2
pdf(file="Results/6C-gonad-stage-OB2", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"K", "6"], 1)), main="Oyster Bay C2", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.08539
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.002
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.005799

# Fidalgo Bay
pdf(file="Results/10C-gonad-stage-FB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"NF", "10"], 1)), main="Fidalgo Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5854
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.1239
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.2677

# Dabob Bay
pdf(file="Results/10C-gonad-stage-DB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"HL", "10"], 1)), main="Dabob Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5395
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.008799
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.7423

# Oyster Bay Cohort 1
pdf(file="Results/10C-gonad-stage-OB1", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"SN", "10"], 1)), main="Oyster Bay C1", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; P=1
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.2458
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.1698

# Oyster Bay Cohort 2
pdf(file="Results/10C-gonad-stage-OB2", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.domsex.stage.pop[,,"K", "10"], 1)), main="Oyster Bay C2", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.08539
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.002
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.005799


----- # Male gonad stage by each population

# Fidalgo Bay
print(barplot(t(prop.table(CT.malestage.pop[,,"NF"], 1)), main="Fidalgo Bay Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"NF"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.8721
fisher.test(CT.malestage.pop[,,"NF"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.6344
fisher.test(CT.malestage.pop[,,"NF"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3057

# Dabob Bay
print(barplot(t(prop.table(CT.malestage.pop[,,"HL"], 1)), main="Dabob Bay Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"HL"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.6294
fisher.test(CT.malestage.pop[,,"HL"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.5834
fisher.test(CT.malestage.pop[,,"HL"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3896

# Oyster Bay Cohort 1
print(barplot(t(prop.table(CT.malestage.pop[,,"SN"], 1)), main="Oyster Bay C1 Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"SN"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.07592
fisher.test(CT.malestage.pop[,,"SN"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.05095
fisher.test(CT.malestage.pop[,,"SN"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3357

# Oyster Bay Cohort 2
print(barplot(t(prop.table(CT.malestage.pop[,,"K"], 1)), main="Oyster Bay C2 Male\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage.pop[,,"K"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 1
fisher.test(CT.malestage.pop[,,"K"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 9.999e-05
fisher.test(CT.malestage.pop[,,"K"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.0007999









---- # Female gonad stage by each population
  
  # Fidalgo Bay
print(barplot(t(prop.table(CT.femstage.pop[,,"NF"], 1)), main="Fidalgo Bay Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"NF"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.042
fisher.test(CT.femstage.pop[,,"NF"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.0396
fisher.test(CT.femstage.pop[,,"NF"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.4875

# Dabob Bay
print(barplot(t(prop.table(CT.femstage.pop[,,"HL"], 1)), main="Dabob Bay Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"HL"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1
fisher.test(CT.femstage.pop[,,"HL"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.2118
fisher.test(CT.femstage.pop[,,"HL"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.2198

# Oyster Bay Cohort 1
print(barplot(t(prop.table(CT.femstage.pop[,,"SN"], 1)), main="Oyster Bay C1 Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"SN"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1
fisher.test(CT.femstage.pop[,,"SN"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.3147
fisher.test(CT.femstage.pop[,,"SN"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3976

# Oyster Bay Cohort 2
print(barplot(t(prop.table(CT.femstage.pop[,,"K"], 1)), main="Oyster Bay C2 Female\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.femstage.pop[,,"K"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=0.4685
fisher.test(CT.femstage.pop[,,"K"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.7263
fisher.test(CT.femstage.pop[,,"K"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.7153

4/39

# --------- Compare sexes within Populations 
CT.sex.pop

# 6 temps! 
pdf(file="Results/6C-gonad-sex-FB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"NF", "6"], 1)), main="Fidalgo Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"NF"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=1
fisher.test(CT.sex.pop[,,"NF"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0979
fisher.test(CT.sex.pop[,,"NF"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.3576

pdf(file="Results/6C-gonad-sex-DB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"HL", "6"], 1)), main="Dabob Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"HL"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.6843
fisher.test(CT.sex.pop[,,"HL"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.3307
fisher.test(CT.sex.pop[,,"HL"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.1169

pdf(file="Results/6C-gonad-sex-OB1", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"SN", "6"], 1)), main="Oyster Bay C1", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"SN"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.0428
fisher.test(CT.sex.pop[,,"SN"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0425
fisher.test(CT.sex.pop[,,"SN"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.95

pdf(file="Results/6C-gonad-sex-OB2", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"K", "6"], 1)), main="Oyster Bay C2", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"K"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.1778
fisher.test(CT.sex.pop[,,"K"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0218
fisher.test(CT.sex.pop[,,"K"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.0195


# 10 temps! 

pdf(file="Results/10C-gonad-sex-FB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"NF", "10"], 1)), main="Fidalgo Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"NF"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=1
fisher.test(CT.sex.pop[,,"NF"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0979
fisher.test(CT.sex.pop[,,"NF"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.3576

pdf(file="Results/10C-gonad-sex-DB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"HL", "10"], 1)), main="Dabob Bay", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"HL"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.6843
fisher.test(CT.sex.pop[,,"HL"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.3307
fisher.test(CT.sex.pop[,,"HL"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.1169

pdf(file="Results/10C-gonad-sex-OB1", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"SN", "10"], 1)), main="Oyster Bay C1", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"SN"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.0428
fisher.test(CT.sex.pop[,,"SN"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0425
fisher.test(CT.sex.pop[,,"SN"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.95

pdf(file="Results/10C-gonad-sex-OB2", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"K", "10"], 1)), main="Oyster Bay C2", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()
fisher.test(CT.sex.pop[,,"K"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.1778
fisher.test(CT.sex.pop[,,"K"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0218
fisher.test(CT.sex.pop[,,"K"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.0195


# Generate male & female plots/tables with stages only dominant stage

CT.stage.female <- table(subset(Histology.Jan.redo, TEMPERATURE==6 & Sex.redo=="F" | Sex.redo=="HPF")$PH, subset(Histology.Jan.redo, TEMPERATURE==6 & Sex.redo=="F" | Sex.redo=="HPF")$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.female, 1)), main="Gonad stage - Female Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.stage.female[-1,], simulate.p.value = T, B = 10000) #ambient vs. low, p=1
fisher.test(CT.stage.female[-2,], simulate.p.value = T, B = 10000) #pre to ambient, p=0.0163
fisher.test(CT.stage.female[-3,], simulate.p.value = T, B = 10000) #pre to low, p=0.1369

CT.stage.male <- table(subset(Histology.Jan.redo, TEMPERATURE==6 & c(Sex.redo=="M" | Sex.redo=="HPM"))$PH, subset(Histology.Jan.redo, TEMPERATURE==6 & c(Sex.redo=="M" | Sex.redo=="HPM"))$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.male, 1)), main="Gonad stage - Male Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.stage.male[-1,], simulate.p.value = T, B = 10000) #ambient vs. low, p=0.6114
chisq.test(CT.stage.male[-2,], simulate.p.value = T, B = 10000) #pre to ambient,  24.192, p-value = 9.999e-05
chisq.test(CT.stage.male[-3,], simulate.p.value = T, B = 10000) #pre to low,  20.627, p-value = 2e-04

CT.stage.herm <- table(subset(Histology.Jan.redo, TEMPERATURE==6 & Sex.redo=="H" | Sex.redo=="I")$PH, subset(Histology.Jan.redo, TEMPERATURE==6 & Sex.redo=="H" | Sex.redo=="I")$Dom.Stage.redo)
print(barplot(t(prop.table(CT.stage.herm, 1)), main="Gonad stage - Herm. Dominant\npre- & post-pH treatment", xlab="pH Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
fisher.test(CT.malestage[-1,], simulate.p.value = T, B = 10000) #ambient vs. low, p=0.0253
fisher.test(CT.malestage[-2,], simulate.p.value = T, B = 10000) #pre to ambient, X-squared=19.733, p=9.999e-05
fisher.test(CT.malestage[-3,], simulate.p.value = T, B = 10000) #pre to low, X-squared=17.181, p=0.0041s

## Compare 6C to 10C GONAD - effect of temperature? 

Histology.Jan.redo.10 #10C
Histology.Jan.redo #6C

Histology.prepH <- subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")
sum(CT.domsex.stage.pre[1,])
4/108

CT.domsex.stage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Dom.Stage.redo)
CT.domsex.stage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Dom.Stage.redo, Histology.prepH$POPULATION)
chisq.test(CT.domsex.stage.pre, simulate.p.value = T, B = 10000) #10 vs. 6C: X-squared = 15.842 p=0.0026
fisher.test(CT.domsex.stage.pre.pop[,,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1347
fisher.test(CT.domsex.stage.pre.pop[,,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.5044
fisher.test(CT.domsex.stage.pre.pop[,,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1968
fisher.test(CT.domsex.stage.pre.pop[,,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB -  0.0021

CT.malestage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Male.Stage)
CT.malestage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Male.Stage, Histology.prepH$POPULATION)
chisq.test(CT.malestage.pre[,-1], simulate.p.value = T, B = 10000) #10 vs. 6C: 31.081, p-value = 9.999e-05
fisher.test(CT.malestage.pre.pop[,-1,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1025
fisher.test(CT.malestage.pre.pop[,-1,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.05429
fisher.test(CT.malestage.pre.pop[,-1,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.2695
fisher.test(CT.malestage.pre.pop[,-1,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - p=9.999e-05

CT.femstage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Female.Stage)
CT.femstage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Female.Stage, Histology.prepH$POPULATION)
chisq.test(CT.femstage.pre[,-1], simulate.p.value = T, B = 10000) #10 vs. 6C: 2.1488 p-value = 0.669
fisher.test(CT.femstage.pre.pop[,-1,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - p=1
fisher.test(CT.femstage.pre.pop[,-1,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.2053
fisher.test(CT.femstage.pre.pop[,-1,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.5977
fisher.test(CT.femstage.pre.pop[,-1,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - P=1

CT.sex.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Sex.redo)
CT.sex.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Sex.redo, Histology.prepH$POPULATION)
chisq.test(CT.sex.pre, simulate.p.value = T, B = 10000) #10 vs. 6C: X-squared = 7.9551, p-value = 0.1634
fisher.test(CT.sex.pre.pop[,,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.3182
fisher.test(CT.sex.pre.pop[,,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.06819
fisher.test(CT.sex.pre.pop[,,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - P=1
fisher.test(CT.sex.pre.pop[,,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB -0.209

sum((CT.sex.pre)[1,])
sum((CT.sex.pre)[2,])

# Compare 6-amb to 10-low ... do T/pH cancel? 

CT.domsex.stage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$TEMPERATURE)
chisq.test(rbind(CT.domsex.stage[3,,c("6")],CT.domsex.stage[2,,c("10")]), simulate.p.value = T, B=10000) #no diff. x=2.8185, p=0.63

CT.sex <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Sex.redo, Histology.Jan.redo$TEMPERATURE)
chisq.test(rbind(CT.sex[3,,c("6")],CT.sex[2,,c("10")]), simulate.p.value = T, B=10000) # yes diff, X=11.7, p=0.037. 

CT.malestage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Male.Stage, Histology.Jan.redo$TEMPERATURE)
chisq.test(rbind(CT.malestage[3,-1,c("6")],CT.malestage[2,-1,c("10")]), simulate.p.value = T, B=10000) # not diff. p=0.6495

CT.femstage <- table(Histology.Jan.redo$PH, Histology.Jan.redo$Female.Stage, Histology.Jan.redo$TEMPERATURE)
fisher.test(rbind(CT.femstage[3,-1,c("6")],CT.femstage[2,-1,c("10")]), simulate.p.value = T, B=10000) # not diff. p=0.7785

# How many females in stage 2 or 3, both sampling? 

nrow(subset(Histology.Jan.redo, (Female.Stage==2 | Female.Stage==3) & SAMPLING=="FEBRUARY"))/nrow(subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")) #24.1%, from 108 oysters 

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) #15.4% of oysters have stage 3 female gametes, from April sampling of 156 oysters 

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL" & PH=="LOW"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL" & PH=="LOW")) #14.1% of low pH treated oysters have stage 3 female gametes  (n=78 sampled)

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL" & PH=="AMBIENT"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL" & PH=="AMBIENT")) #16.7% of ambient pH treated oysters have stage 3 female gametes  (n=78 sampled)

nrow(subset(Histology.Jan.redo, Female.Stage==2 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) #23.7% of oysters have stage 2 female gametes, from April sampling of 156 oysters 

# How many males in stage 2 or 3, both sampling? 
nrow(subset(Histology.Jan.redo, (Male.Stage==2 | Male.Stage==3) & SAMPLING=="FEBRUARY"))/nrow(subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")) #45.4%, from 108 oysters 

nrow(subset(Histology.Jan.redo, Male.Stage==2 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) #26.3% of oysters have stage 2 or stage 3 male gametes, from April sampling of 156 oysters 

nrow(subset(Histology.Jan.redo, Male.Stage==3 & PH=="AMBIENT"))/nrow(subset(Histology.Jan.redo, PH=="AMBIENT")) #24.4% of oysters have stage 3 male gametes, from April sampling of 156 oysters 

summary(Histology.Jan.redo$Sex.redo)/264

# Compare sex ratios between populations ... all treatments combined! 
CT.sex.pop.allph <- t(table(Histology.Jan.redo$Sex.redo, Histology.Jan.redo$POPULATION))
chisq.test(CT.sex.pop.allph, simulate.p.value = T, B=10000) #very sign. 
pairwiseNominalIndependence(CT.sex.pop.allph, fisher=FALSE, gtest=FALSE, chisq=TRUE,  method="fdr", simulate.p.value=T)

round(100*CT.sex.pop.allph/rowSums(CT.sex.pop.allph), digits = 1) # of each % sex of ALL sampled oysters within pops 

# % female or hpf 
19+14.3   #HL 33%
11.1+0    #K 11%
18.2+6.1  #NF 24%
31.8+18.2 #SN 50%

# % male or hpf 
38.1+9.5   #HL 48%
41.1+27.8   #K 69%
28.8+30.3  #NF 59%
15.2+13.6 #SN 29%

