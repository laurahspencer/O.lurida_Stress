# After redoing separate male/female stages, re-do analysis. 

Histology.Jan.redo <- read.csv("Data/2017-Oly-Histo-Results-REDO-January2019.csv", header=T, stringsAsFactors = T, na.strings = "NA")
Histology.Jan.redo$TEMPERATURE <- as.factor(Histology.Jan.redo$TEMPERATURE)         #Convert a few columns to factors 
Histology.Jan.redo$PCO2 <- factor(Histology.Jan.redo$PCO2, levels = c("Pre","High","Ambient")) #reorder PCO2 factors for plots
Histology.Jan.redo$Female.Stage <- as.factor(Histology.Jan.redo$Female.Stage)   
Histology.Jan.redo$Male.Stage <- as.factor(Histology.Jan.redo$Male.Stage)
Histology.Jan.redo$Sex.redo <- as.factor(Histology.Jan.redo$Sex.redo)
Histology.Jan.redo$Sex.redo <- droplevels(Histology.Jan.redo$Sex.redo, exclude = "")
Histology.Jan.redo$Sex.redo <- factor(Histology.Jan.redo$Sex.redo, levels=c("I", "M", "HPM", "H", "HPF", "F"))
Histology.Jan.redo$Dom.Stage.redo <- as.factor(Histology.Jan.redo$Dom.Stage.redo)
Histology.Jan.redo$Dom.Stage.redo <- droplevels(Histology.Jan.redo$Dom.Stage.redo, exclude = "#N/A")


## Compare 6C to 10C - effect of temperature?  alpha = 0.0125 (n=4 comparisons)
Histology.prepH <- subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")

# make an empty dataframe for test statistic results 
tests <-  data.frame(test=character(), chisquare=character(), pvalue=numeric(), stringsAsFactors=FALSE) 

CT.domsex.stage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Dom.Stage.redo)
tests[1,] <- as.vector(unlist(chisq.test(CT.domsex.stage.pre, simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: X-squared = 15.842 p=0.0026

CT.malestage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Male.Stage)
tests[2,] <- as.vector(unlist(chisq.test(CT.malestage.pre[,-1], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: 31.081, p-value = 9.999e-05 

CT.femstage.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Female.Stage)
tests[3,] <- as.vector(unlist(chisq.test(CT.femstage.pre[,-1], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: 2.1488 p-value = 0.669

CT.sex.pre <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Sex.simple)
tests[4,] <- as.vector(unlist(chisq.test(CT.sex.pre, simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: X-squared = 7.9551, p-value = 0.1634

# ------- Compare pH treatments - effect of pH? 

# Prepare contingency tables 
CT.sex <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.simple, Histology.Jan.redo$TEMPERATURE)
CT.sex.plots <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.redo, Histology.Jan.redo$TEMPERATURE)
CT.sex.pop <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.simple, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)
CT.sex.pop.plots <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.redo, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)

CT.sex.temp <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.simple, Histology.Jan.redo$TEMPERATURE)
CT.domsex.stage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$TEMPERATURE)
CT.domsex.stage.pop <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)
CT.malestage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Male.Stage, Histology.Jan.redo$TEMPERATURE)
CT.malestage.pop <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Male.Stage, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)
CT.femstage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Female.Stage, Histology.Jan.redo$TEMPERATURE)
CT.femstage.pop <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Female.Stage, Histology.Jan.redo$POPULATION, Histology.Jan.redo$TEMPERATURE)

colnames(CT.sex.pop.plots) <- c("Indeterminate", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
colnames(CT.sex.plots) <- c("Indeterminate", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
colnames(CT.domsex.stage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

# Compare using chi-square if N is large enough, fisher exact test if not.  n=6 comparisons per factor, alpha=0.0083333

# Dominant stage, alpha=0.0083333
tests[5,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-1,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared= 9.738, p-value = 0.0364   
tests[6,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-2,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=16.514 p=0.0019          
tests[7,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-3,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=4.5654, p=0.3521             

tests[8,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-1,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared=12.458,  p-value = 0.009099
tests[9,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-2,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=12.682, p-value = 0.009999     
tests[10,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-3,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=5.2335, p-value = 0.285        

# female only, alpha=0.0083333
tests[11,] <- as.vector(unlist(chisq.test(CT.femstage[-1,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, p=0.2185                                        
tests[12,] <- as.vector(unlist(chisq.test(CT.femstage[-2,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, p=0.07639                                        
tests[13,] <- as.vector(unlist(chisq.test(CT.femstage[-3,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, p=0.3464                                             

tests[14,] <- as.vector(unlist(chisq.test(CT.femstage[-1,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, p=1                                  
tests[15,] <- as.vector(unlist(chisq.test(CT.femstage[-2,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, p=0.1274                                        
tests[16,] <- as.vector(unlist(chisq.test(CT.femstage[-3,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, p=0.06159                                          

# male only, alpha=0.0083333
tests[17,] <- as.vector(unlist(chisq.test(CT.malestage[-1,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared = 8.975, p-value = 0.0297              
tests[18,] <- as.vector(unlist(chisq.test(CT.malestage[-2,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=24.197, p=9.999e-05                     
tests[19,] <- as.vector(unlist(chisq.test(CT.malestage[-3,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=15.159, p=0.0011                            

tests[20,] <- as.vector(unlist(chisq.test(CT.malestage[-1,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared = 12.949, p-value = 0.008699            
tests[21,] <- as.vector(unlist(chisq.test(CT.malestage[-2,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=15.387, p-value = 0.0036                    
tests[22,] <- as.vector(unlist(chisq.test(CT.malestage[-3,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#pre to low, X-squared=0.55353, p=0.9742   

# Sex, alpha=0.0083333
tests[23,] <- as.vector(unlist(chisq.test(CT.sex[-1,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn ambient and low pH: X-squared=7.3468, p=0.2018                 
tests[24,] <- as.vector(unlist(chisq.test(CT.sex[-2,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and ambient pH: X-squared=15.148, p=0.006499               
tests[25,] <- as.vector(unlist(chisq.test(CT.sex[-3,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and low pH: X-squared=8.57, p=0.1319                       

tests[26,] <- as.vector(unlist(chisq.test(CT.sex[-1,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn ambient and low pH: X-squared=2.8959, p-value = 0.7332              
tests[27,] <- as.vector(unlist(chisq.test(CT.sex[-2,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and ambient pH: X-squared=8.0168, p-value = 0.1625              
tests[28,] <- as.vector(unlist(chisq.test(CT.sex[-3,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and low pH: X-squared=3.869, p-value = 0.6058         

# Compare 6-amb to 10-low ... do T/pH treatments cancel? n=4 comparisons, alpha=0.0125

CT.domsex.stage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$TEMPERATURE)
tests[29,] <- as.vector(unlist(chisq.test(rbind(CT.domsex.stage[3,,c("6")],CT.domsex.stage[2,,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
#X=2.8185, p=0.63

CT.sex <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Sex.simple, Histology.Jan.redo$TEMPERATURE)
tests[30,] <- as.vector(unlist(chisq.test(rbind(CT.sex[3,,c("6")],CT.sex[2,,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
# X=11.7, p=0.037 

CT.malestage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Male.Stage, Histology.Jan.redo$TEMPERATURE)
tests[31,] <- as.vector(unlist(chisq.test(rbind(CT.malestage[3,-1,c("6")],CT.malestage[2,-1,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
# p=0.6495

CT.femstage <- table(Histology.Jan.redo$PCO2, Histology.Jan.redo$Female.Stage, Histology.Jan.redo$TEMPERATURE)
tests[32,] <- as.vector(unlist(fisher.test(rbind(CT.femstage[3,-1,c("6")],CT.femstage[2,-1,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
#  p=0.7785

# Compare sex ratios between populations ... all treatments combined! 
CT.stage.pop.allph <- t(table(Histology.Jan.redo$Dom.Stage.redo, Histology.Jan.redo$POPULATION))
tests[33,] <- as.vector(unlist(chisq.test(CT.stage.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 

CT.sex.pop.allph <- t(table(Histology.Jan.redo$Sex.simple, Histology.Jan.redo$POPULATION))
tests[34,] <- as.vector(unlist(chisq.test(CT.sex.pop.allph, simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) #very sign. 
pairwiseNominalIndependence(CT.sex.pop.allph, fisher=FALSE, gtest=FALSE, chisq=TRUE,  method="fdr", simulate.p.value=T)

CT.male.pop.allph <- t(table(Histology.Jan.redo$Male.Stage, Histology.Jan.redo$POPULATION))
tests[35,] <- as.vector(unlist(chisq.test(CT.male.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 

CT.female.pop.allph <- t(table(Histology.Jan.redo$Female.Stage, Histology.Jan.redo$POPULATION))
tests[36,] <- as.vector(unlist(chisq.test(CT.female.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 

tests$holm <- p.adjust(tests$pvalue, "holm")
tests$BH <- p.adjust(tests$pvalue, "BH")
View(tests) # review - opting to go with the less conservative, due to many different correlations in data 

# After BH correction, sigificance retained for ... 
# temp treatment differences
CT.malestage.pre[, -1]      # Male stage after temp treatment 
CT.domsex.stage.pre         # Dominant sex's stage after temp treatment 

# pCO2 treatment differences 
CT.malestage[-1, -1, "10"]    # Male stage between ambient and high pCO2, 10C group 
CT.domsex.stage[-1, , "10"] # Dominant sex's stage between ambient and high pCO2, 10C group 

# Ambient pCO2 treatment 
CT.domsex.stage[-2, , "6"]  # Dominant sex's stage after ambient pCO2
CT.domsex.stage[-2, , "10"] # Dominant sex's stage after ambient pCO2 treatment, 10C group
CT.malestage[-2, , "6"]     # Male stage after ambient pCO2, 6C group 
CT.malestage[-2, , "10"]    # Male stage after ambient pCO2, 10C group 

# High pCO2 treatment 
CT.malestage[-3, , "6"]     # Male stage between ambient and high pCO2, 6C group 

# Sex differences among populations 
CT.sex.pop.allph


# PLOTS FOR PAPER AS OF 4/12/2019 

# -------  Dominant gonad stage by each population
  
plot.cohort <- c("NF","HL", "SN", "K", "NF","HL", "SN", "K")
plot.names <- c("Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2", "Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2")
plot.temps <- c("6","6","6","6","10","10","10","10")

# check out colors for stage plots

# c(lighten("#E2E6BD", amount = 0.1),  lighten("#EAAB28", amount = 0.3), lighten("#E78A38", amount = 0.3),lighten("#D33F6A", amount = 0.5), lighten("#DF6753", amount = 0.3), lighten("lightsalmon", amount = 0.3))
# show_col(c('#f7f7f7','#cccccc','#969696','#636363','#252525'))

pdf(file="Results/gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.domsex.stage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Gonad stage by temperature, ", pCO[2], ", and cohort")))
dev.off()

# make a plot for the legend 
colnames(CT.domsex.stage.pop) <- c("Empty/No follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

pdf(file="Results/gonad-stage-legend", height = 5.2, width = 5.6)
par(mar=c(0,0,0,18))
barplot(t(prop.table(CT.domsex.stage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.8, 0), title="Gonad Stage", cex=1.5, text.col="gray30", text.font=1))
dev.off()


# Gonad sex for each population 

# check out colors for stage plots
#show_col(c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2", lighten("gray75", amount = 0.1),  lighten("royalblue3", amount = 0.3), lighten("mediumpurple3", amount = 0.3),lighten("purple3", amount = 0.3), lighten("mediumorchid3", amount = 0.3), lighten("hotpink2", amount = 0.3)))

show_col(c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62"))

pdf(file="Results/gonad-sex-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.sex.pop.plots[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Gonad sex by temperature, ", pCO[2], ", and cohort")))
dev.off()

# make a plot for the legend 
colnames(CT.sex.pop.plots) <- c("Indeterminate", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")

pdf(file="Results/gonad-sex-legend", height = 5.2, width = 5.6)
par(mar=c(0,0,0,18))
barplot(t(prop.table(CT.sex.pop.plots[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.6, 0), title="Gonad Sex", cex=1.5, text.col="gray30", text.font=1))
dev.off()


# plots with all populations combined 
CT.sex.plots
colnames(CT.malestage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
colnames(CT.femstage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")


pdf(file="Results/gonad-stage-sex-all-cohorts", height = 5.75, width = 6.05)

par(mfrow=c(2,3), oma=c(5,5,0,3), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
barplot(t(prop.table(CT.sex.plots[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Sex", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.malestage[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Male", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.femstage[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Female", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.sex.plots[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Sex", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.malestage[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Male", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.femstage[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Female", line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)

mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Gonad sex and male/female gamete stage, cohorts combined")))
dev.off()



# -------  Male gonad stage by each population
CT.malestage.pop
plot.cohort <- c("NF","HL", "SN", "K", "NF","HL", "SN", "K")
plot.names <- c("Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2", "Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2")
plot.temps <- c("6","6","6","6","10","10","10","10")

# check out colors for stage plots

# c(lighten("#E2E6BD", amount = 0.1),  lighten("#EAAB28", amount = 0.3), lighten("#E78A38", amount = 0.3),lighten("#D33F6A", amount = 0.5), lighten("#DF6753", amount = 0.3), lighten("lightsalmon", amount = 0.3))
# show_col(c('#f7f7f7','#cccccc','#969696','#636363','#252525'))
show_col(c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ))

pdf(file="Results/male-gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.malestage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Male gamete stage by temperature, ", pCO[2], ", and cohort")))
dev.off()


pdf(file="Results/female-gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.femstage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="% Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Female gamete stage by temperature, ", pCO[2], ", and cohort")))
dev.off()






# -------------


# Examine within population (aka cohort) differences (not reported)

CT.domsex.stage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Dom.Stage.redo, Histology.prepH$POPULATION)
CT.malestage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Male.Stage, Histology.prepH$POPULATION)
CT.femstage.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Female.Stage, Histology.prepH$POPULATION)
CT.sex.pre.pop <- table(Histology.prepH$TEMPERATURE, Histology.prepH$Sex.redo, Histology.prepH$POPULATION)

fisher.test(CT.domsex.stage.pre.pop[,,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1347
fisher.test(CT.domsex.stage.pre.pop[,,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.5044
fisher.test(CT.domsex.stage.pre.pop[,,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1968
fisher.test(CT.domsex.stage.pre.pop[,,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB -  0.0021

fisher.test(CT.malestage.pre.pop[,-1,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.1025
fisher.test(CT.malestage.pre.pop[,-1,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.05429
fisher.test(CT.malestage.pre.pop[,-1,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.2695
fisher.test(CT.malestage.pre.pop[,-1,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - p=9.999e-05

fisher.test(CT.femstage.pre.pop[,-1,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - p=1
fisher.test(CT.femstage.pre.pop[,-1,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.2053
fisher.test(CT.femstage.pre.pop[,-1,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.5977
fisher.test(CT.femstage.pre.pop[,-1,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - P=1

fisher.test(CT.sex.pre.pop[,,"NF"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.3182
fisher.test(CT.sex.pre.pop[,,"HL"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.06819
fisher.test(CT.sex.pre.pop[,,"SN"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - P=1
fisher.test(CT.sex.pre.pop[,,"K"], simulate.p.value = T, B = 10000) #10 vs. 6C FB - 0.209

### ----- Test dominant sex's stage differences within cohorts aka populations

# Fidalgo Bay 6C
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5854
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.1239
fisher.test(CT.domsex.stage.pop[,,"NF", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.2677

# Dabob Bay 6C
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5395
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.008799     # corrected (n = 4): 0.0352
fisher.test(CT.domsex.stage.pop[,,"HL", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.7423

# Oyster Bay Cohort 1 6C
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; P=1
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.2458
fisher.test(CT.domsex.stage.pop[,,"SN", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.1698

# Oyster Bay Cohort 2 6C
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.08539
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.002       # corrected (n=4): 0.008
fisher.test(CT.domsex.stage.pop[,,"K", "6"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.005799        # corrected (n=4): 0.0232

# Fidalgo Bay 10C 
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5854     UPDATE! 0.013   # corrected (n=4): 0.0412
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.1239      UPDATE! 0.244
fisher.test(CT.domsex.stage.pop[,,"NF", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.2677          UPDATE! 0.394

# Dabob Bay 10C
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.5395     UPDATE! P=1
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.008799    UPDATE! P=0.0979
fisher.test(CT.domsex.stage.pop[,,"HL", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.7423          UPDATE! P=0.253

# Oyster Bay Cohort 1 10C
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; P=1
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.2458      UPDATE! P=0.410
fisher.test(CT.domsex.stage.pop[,,"SN", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.1698          UPDATE! P=0.515

# Oyster Bay Cohort 2 10C
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-1,], simulate.p.value = T, B = 10000) #ambient vs. low; 0.08539     UPDATE! P=0.127
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-2,], simulate.p.value = T, B = 10000) #pre to ambient; 0.002        UPDATE! P=0.068
fisher.test(CT.domsex.stage.pop[,,"K", "10"][-3,], simulate.p.value = T, B = 10000) #pre to low; 0.005799         UPDATE! P=0.405

# -----  Male gonad stage by each population

# Fidalgo Bay
fisher.test(CT.malestage.pop[,,"NF", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.8721
fisher.test(CT.malestage.pop[,,"NF", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.6344
fisher.test(CT.malestage.pop[,,"NF", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3057

fisher.test(CT.malestage.pop[,,"NF", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.08619
fisher.test(CT.malestage.pop[,,"NF", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.0292
fisher.test(CT.malestage.pop[,,"NF", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.4235

# Dabob Bay
fisher.test(CT.malestage.pop[,,"HL", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.6294
fisher.test(CT.malestage.pop[,,"HL", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.5834
fisher.test(CT.malestage.pop[,,"HL", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3896

fisher.test(CT.malestage.pop[,,"HL", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.4369
fisher.test(CT.malestage.pop[,,"HL", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.1432
fisher.test(CT.malestage.pop[,,"HL", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.06279

# Oyster Bay Cohort 1
fisher.test(CT.malestage.pop[,,"SN", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.07592
fisher.test(CT.malestage.pop[,,"SN", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.05095
fisher.test(CT.malestage.pop[,,"SN", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3357

fisher.test(CT.malestage.pop[,,"SN", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.2256
fisher.test(CT.malestage.pop[,,"SN", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.3593
fisher.test(CT.malestage.pop[,,"SN", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; P=1

# Oyster Bay Cohort 2
fisher.test(CT.malestage.pop[,,"K", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.2776
fisher.test(CT.malestage.pop[,,"K", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 2e-04
fisher.test(CT.malestage.pop[,,"K", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.0009999

fisher.test(CT.malestage.pop[,,"K", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.2839 
fisher.test(CT.malestage.pop[,,"K", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.2776
fisher.test(CT.malestage.pop[,,"K", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.8154

# ---- Female gonad stage by each population

# Fidalgo Bay
fisher.test(CT.femstage.pop[,,"NF", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.042
fisher.test(CT.femstage.pop[,,"NF", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.0021
fisher.test(CT.femstage.pop[,,"NF", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.4875

fisher.test(CT.femstage.pop[,,"NF", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; 0.042  UPDATE! P=1
fisher.test(CT.femstage.pop[,,"NF", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.0396  UPDATE! P=0.00620
fisher.test(CT.femstage.pop[,,"NF", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.4875      UPDATE! P=0.0228

# Dabob Bay
fisher.test(CT.femstage.pop[,,"HL", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1
fisher.test(CT.femstage.pop[,,"HL", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.2118
fisher.test(CT.femstage.pop[,,"HL", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.2198

fisher.test(CT.femstage.pop[,,"HL", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1    UPDATE! P=0.683
fisher.test(CT.femstage.pop[,,"HL", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.2118
fisher.test(CT.femstage.pop[,,"HL", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.2198

# Oyster Bay Cohort 1
fisher.test(CT.femstage.pop[,,"SN", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1
fisher.test(CT.femstage.pop[,,"SN", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.3147
fisher.test(CT.femstage.pop[,,"SN", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3976

fisher.test(CT.femstage.pop[,,"SN", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=1    UPDATE! P=0.565
fisher.test(CT.femstage.pop[,,"SN", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.3147  UPDATE! P=1
fisher.test(CT.femstage.pop[,,"SN", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.3976      UPDATE! P=0.631

# Oyster Bay Cohort 2
fisher.test(CT.femstage.pop[,,"K", "6"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=0.4685 UPDATE! P=0.270
fisher.test(CT.femstage.pop[,,"K", "6"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.7263    UPDATE! P=1
fisher.test(CT.femstage.pop[,,"K", "6"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.7153        UPDATE! P=0.285

fisher.test(CT.femstage.pop[,,"K", "10"][-1,-1], simulate.p.value = T, B = 10000) #ambient vs. low; p=0.4685  UPDATE! P=0.806
fisher.test(CT.femstage.pop[,,"K", "10"][-2,-1], simulate.p.value = T, B = 10000) #pre to ambient; 0.7263     UPDATE! P=1
fisher.test(CT.femstage.pop[,,"K", "10"][-3,-1], simulate.p.value = T, B = 10000) #pre to low; 0.7153         UPDATE! P=1

# --------- Compare sex within Populations 

# FIDALGO BAY
fisher.test(CT.sex.pop[,,"NF", "6"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=1
fisher.test(CT.sex.pop[,,"NF", "6"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0979
fisher.test(CT.sex.pop[,,"NF", "6"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.3576

fisher.test(CT.sex.pop[,,"NF", "10"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.1327
fisher.test(CT.sex.pop[,,"NF", "10"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0112
fisher.test(CT.sex.pop[,,"NF", "10"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.0253

# DABOB BAY
fisher.test(CT.sex.pop[,,"HL", "6"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.6843
fisher.test(CT.sex.pop[,,"HL", "6"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.3307
fisher.test(CT.sex.pop[,,"HL", "6"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.1169

fisher.test(CT.sex.pop[,,"HL", "10"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.8478
fisher.test(CT.sex.pop[,,"HL", "10"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.1661
fisher.test(CT.sex.pop[,,"HL", "10"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.6256

# OYSTER BAY C1
fisher.test(CT.sex.pop[,,"SN", "6"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.0428
fisher.test(CT.sex.pop[,,"SN", "6"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0425
fisher.test(CT.sex.pop[,,"SN", "6"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.95

fisher.test(CT.sex.pop[,,"SN", "10"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.6281
fisher.test(CT.sex.pop[,,"SN", "10"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.09919
fisher.test(CT.sex.pop[,,"SN", "10"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.9007

# OYSTER BAY C2
fisher.test(CT.sex.pop[,,"K", "6"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.1778
fisher.test(CT.sex.pop[,,"K", "6"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=0.0218
fisher.test(CT.sex.pop[,,"K", "6"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.0195

fisher.test(CT.sex.pop[,,"K", "10"][-1,], simulate.p.value = T, B = 10000) #sex btwn ambient and low pH: p=0.4568
fisher.test(CT.sex.pop[,,"K", "10"][-2,], simulate.p.value = T, B = 10000) #sex btwn pre and ambient pH: p=1
fisher.test(CT.sex.pop[,,"K", "10"][-3,], simulate.p.value = T, B = 10000) #sex btwn pre and low pH: p=0.6685




# Plots showing gonad by pH treatment, both temps and all cohorts combined

# Dominant sex's stage 
print(barplot(t(prop.table(CT.domsex.stage, 1)), main="Gonad stage, all cohorts", xlab=(expression(paste(pCO[2], " treatment"))), ylab="% Sampled", las=1, col=c(lighten("#E2E6BD", amount = 0.1),  lighten("#EAAB28", amount = 0.3), lighten("#E78A38", amount = 0.3),lighten("#D33F6A", amount = 0.5), lighten("#DF6753", amount = 0.3), lighten("lightsalmon", amount = 0.3)),  cex.main=1.5, cex.lab=1.4, cex.axis = 1.3, cex.names = 1.3, col.axis = "gray30", col.lab = "gray30", col.main = "gray30", font.main = 1, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

# Female stage
colnames(CT.femstage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
print(barplot(t(prop.table(CT.femstage, 1)), main="Female", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))

# Male stage
colnames(CT.malestage) <- c("None present (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
print(barplot(t(prop.table(CT.malestage, 1)), main="Male", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"), cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-.5, 0), title="Gonad Stage", cex=1.5)))

#  Sex
print(barplot(t(prop.table(CT.sex, 1)), main="Dominant gonad sex", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))


print(barplot(t(prop.table(CT.malestage.pop[,,"NF"], 1)), main="Fidalgo Bay Male\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.malestage.pop[,,"HL"], 1)), main="Dabob Bay Male\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.malestage.pop[,,"SN"], 1)), main="Oyster Bay C1 Male\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.malestage.pop[,,"K"], 1)), main="Oyster Bay C2 Male\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))


print(barplot(t(prop.table(CT.femstage.pop[,,"NF"], 1)), main="Fidalgo Bay Female\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.femstage.pop[,,"HL"], 1)), main="Dabob Bay Female\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.femstage.pop[,,"SN"], 1)), main="Oyster Bay C1 Female\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

print(barplot(t(prop.table(CT.femstage.pop[,,"K"], 1)), main="Oyster Bay C2 Female\npre- & post-PCO2 treatment", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("#E2E6BD",  "#EAAB28", "#E78A38","#D33F6A", "#DF6753", "lightsalmon"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = F, args.legend = list(x = "topright", bty = "n", inset=c(-1.5, 0), title="Gonad Stage", cex=1.5)))

# Barplots of sex for each cohort

pdf(file="Results/6C-gonad-sex-FB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"NF", "6"], 1)), main="Fidalgo Bay", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()

pdf(file="Results/6C-gonad-sex-DB", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"HL", "6"], 1)), main="Dabob Bay", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()

pdf(file="Results/6C-gonad-sex-OB1", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"SN", "6"], 1)), main="Oyster Bay C1", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()

pdf(file="Results/6C-gonad-sex-OB2", height = 6.5, width = 4.5)
print(barplot(t(prop.table(CT.sex.pop[,,"K", "6"], 1)), main="Oyster Bay C2", xlab="PCO2 Treatment", ylab="% Sampled", las=1, col=c("gray75",  "royalblue3", "mediumpurple3", "purple3","mediumorchid3", "hotpink2"),  cex.main=1.7, cex.lab=1.5, cex.axis = 1.3, cex.names = 1.3, legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1, 0), title="Gonad Stage", cex=1.5)))
dev.off()


# --- Some summary statistics 

# How many females in stage 2 or 3, both sampling? 
nrow(subset(Histology.Jan.redo, (Female.Stage==2 | Female.Stage==3) & SAMPLING=="FEBRUARY"))/nrow(subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")) 
#24.1%, from 108 oysters 

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) 
#15.4% of oysters have stage 3 female gametes, from April sampling of 156 oysters 

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL" & PCO2=="High"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL" & PCO2=="LOW")) 
#14.1% of low pH treated oysters have stage 3 female gametes  (n=78 sampled)

nrow(subset(Histology.Jan.redo, Female.Stage==3 & SAMPLING=="APRIL" & PCO2=="Ambient"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL" & PCO2=="Ambient")) 
#16.7% of ambient pH treated oysters have stage 3 female gametes  (n=78 sampled)

nrow(subset(Histology.Jan.redo, Female.Stage==2 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) 
#23.7% of oysters have stage 2 female gametes, from April sampling of 156 oysters 

# How many males in stage 2 or 3, both sampling? 
nrow(subset(Histology.Jan.redo, (Male.Stage==2 | Male.Stage==3) & SAMPLING=="FEBRUARY"))/nrow(subset(Histology.Jan.redo, SAMPLING=="FEBRUARY")) 
#45.4%, from 108 oysters 

nrow(subset(Histology.Jan.redo, Male.Stage==2 & SAMPLING=="APRIL"))/nrow(subset(Histology.Jan.redo, SAMPLING=="APRIL")) 
#26.3% of oysters have stage 2 or stage 3 male gametes, from April sampling of 156 oysters 

nrow(subset(Histology.Jan.redo, Male.Stage==3 & PCO2=="Ambient"))/nrow(subset(Histology.Jan.redo, PCO2=="Ambient")) 
#24.4% of oysters have stage 3 male gametes, from April sampling of 156 oysters 

summary(Histology.Jan.redo$Sex.redo)/264


# % sexes by population
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






