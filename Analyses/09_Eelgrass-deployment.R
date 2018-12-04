library(reshape2)
library(plyr)

Deploy.data <- read.csv("Data/2018-Deployment-Data.csv", header=T, na.strings = c("NA", "?", "Unknown", "TBD"))
Deploy.data <- subset(Deploy.data, SURVIVED != "NA")
Deploy.data$BAY <- droplevels(Deploy.data$BAY)
Deploy.data$HABITAT <- droplevels(Deploy.data$HABITAT)
Deploy.data$POPULATION <- droplevels(Deploy.data$POPULATION)
Deploy.data$PH <- droplevels(Deploy.data$PH)
Deploy.data$SIZE <- droplevels(Deploy.data$SIZE)
Deploy.data$HAB.PH <- droplevels(Deploy.data$HAB.PH)
Deploy.data$POP.PH.HAB <- droplevels(Deploy.data$POP.PH.HAB)
str(Deploy.data)
aggregate(DEPLOYED ~ POPULATION + PH, Deploy.data, mean) #number deployed in each group

# Compare survival between habitats overall

anova(glm.outplant1 <- glm(cbind(SURVIVED, DEPLOYED) ~ BAY, data=Deploy.data, binomial), test="Chi") # survival differed between bays. Should include as random effect. 
anova(glm.outplant2 <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=Deploy.data, binomial), test="Chi") # survival differed between population. Should include as random effect. 
anova(glm.outplant3 <- glm(cbind(SURVIVED, DEPLOYED) ~ HABITAT, data=Deploy.data, binomial), test="Chi") # survival did not differ between habitats 
anova(glm.outplant4 <- glm(cbind(SURVIVED, DEPLOYED) ~ PH, data=Deploy.data, binomial), test="Chi") # survival differed by parental pH 
anova(glm.outplant5 <- glm(cbind(SURVIVED, DEPLOYED) ~ SIZE, data=Deploy.data, binomial), test="Chi") # survival differed by size category, can include as random effect 

Anova(glm.outplant6 <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT + (1|POPULATION/BAY), data=Deploy.data, binomial), test="Chi")  # what I really care about - ph and habitat 
plot(glm.outplant6)

Anova(glm.outplant6.noFB <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT + (1|POPULATION/BAY), data=subset(Deploy.data, POPULATION!="FB"), binomial), test="Chi")  # pH and Habitat, without Fidalgo Bay - Fidalgo Bay obviously highly influential 

# Inspect PH differences in populations separately 
Anova(glm.outplant7 <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT + (1|BAY), data=subset(Deploy.data, POPULATION=="FB"), binomial), test="Chi") # PH DIFF, no interaction
Anova(glm.outplant7 <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH + (1|BAY), data=subset(Deploy.data, POPULATION=="FB"), binomial), test="Chi") # PH DIFF, no interaction
Anova(glm.outplant8 <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT + (1|BAY), data=subset(Deploy.data, POPULATION=="HC"), binomial), test="Chi") #NOT DIFF 
Anova(glm.outplant9 <- glmer(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT + (1|BAY), data=subset(Deploy.data, POPULATION=="SSF1"), binomial), test="Chi") #Interaction only
anova(glm.outplant10 <- glm(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT, data=subset(Deploy.data, POPULATION=="SSF2"), binomial), test="Chi") #DIFF 

# Survival differences between bay, population, and interaction? 
Anova(glm.outplant11 <- glm(cbind(SURVIVED, DEPLOYED) ~ BAY*POPULATION, data=Deploy.data, binomial)) 
summary(glm.outplant11)

Deploy.data$perc.surv <- Deploy.data$SURVIVED/Deploy.data$DEPLOYED
aggregate(perc.surv ~ PH + SIZE, data=Deploy.data, mean, na.rm=TRUE)
aggregate(perc.surv ~ PH + SIZE, data=Deploy.data, var, na.rm=TRUE)
aggregate(perc.surv ~ POPULATION + PH + SIZE, data=Deploy.data, mean, na.rm=TRUE)

aggregate((SURVIVED/DEPLOYED) ~ PH, Deploy.data, mean, na.rm=TRUE)
#   PH      SIZE      (SURVIVED/DEPLOYED)
#  Ambient    L           0.3284651
#      Low    L           0.4426619
#  Ambient    S           0.2538462
#      Low    S           0.4305556

aggregate((SURVIVED/DEPLOYED) ~ PH + POPULATION, subset(Deploy.data, HABITAT != "NA"), mean, na.rm=TRUE)
#PH POPULATION (SURVIVED/DEPLOYED)
# Ambient         FB          0.266
#     Low         FB          0.616
# Ambient         HC          0.300
#     Low         HC          0.337
# Ambient       SSF1          0.376
#     Low       SSF1          0.577
# Ambient       SSF2          0.197
#     Low       SSF2          0.044

aggregate((SURVIVED/DEPLOYED) ~ PH + POPULATION, subset(Deploy.data, HABITAT != "NA"), sd, na.rm=TRUE)


aggregate(DEPLOYED ~ PH, Deploy.data, sum, na.rm=TRUE)
aggregate(SURVIVED ~ PH, Deploy.data, sum, na.rm=TRUE)
100*186/677
100*290/664

aggregate((SURVIVED/DEPLOYED) ~ HABITAT+PH, Deploy.data, mean, na.rm=TRUE) 
# HABITAT PH (SURVIVED/DEPLOYED)
# B   A   0.228
# E   A   0.371
# B   L   0.472
# E   L   0.405
aggregate((SURVIVED/DEPLOYED) ~ POPULATION, Deploy.data, mean, na.rm=TRUE) 
# POPULATION (SURVIVED/DEPLOYED)
# FB    0.439
# HC    0.319
# SSF1  0.471
# SSF2  0.117
aggregate((SURVIVED/DEPLOYED) ~ BAY, Deploy.data, mean, na.rm=TRUE) 
# BAY (SURVIVED/DEPLOYED)
# CI   0.291
# FB   0.389
# PG   0.485
# SK   0.317

aggregate(DEPLOYED ~ POPULATION+PH, Deploy.data, mean, na.rm=TRUE) 
# POPULATION  PH DEPLOYED
#   FB  Ambient      240
#   HC  Ambient      240
# SSF1  Ambient       85
# SSF2  Ambient      112
#   FB     Low      257
#   HC     Low      240
# SSF1     Low       77
# SSF2     Low       90
14*8
11*8

# Colored plot of survival between habitat, parental pH exposure, all bays combined 
plot(Deploy.data$SURVIVED/Deploy.data$DEPLOYED ~ Deploy.data$HAB.PH, main="Survival by Parental Exposure & Habitat, all bays", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# same plot, w/o SS-F3 group (since none really survived)
plot(subset(Deploy.data, POPULATION!="SSF2")$SURVIVED/subset(Deploy.data, POPULATION!="SSF2")$DEPLOYED ~ subset(Deploy.data, POPULATION!="SSF2")$HAB.PH, main="Survival by Parental Exposure & Habitat, all bays", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

Deploy.data$PH <- gsub("A", "Ambient", Deploy.data$PH)
Deploy.data$PH <- gsub("L", "Low", Deploy.data$PH)
Deploy.data$PH <- as.factor(Deploy.data$PH)
Deploy.data$HABITAT <- gsub("B", "Unvegetated", Deploy.data$HABITAT)
Deploy.data$HABITAT <- gsub("E", "Eelgrass", Deploy.data$HABITAT)
Deploy.data$HABITAT <- as.factor(Deploy.data$HABITAT)
Deploy.data$HAB.PH <- gsub("AMB.BARE", "AMB.UNVEG", Deploy.data$HAB.PH)
Deploy.data$HAB.PH <- gsub("LOW.BARE", "LOW.UNVEG", Deploy.data$HAB.PH)
Deploy.data$HAB.PH <- as.factor(Deploy.data$HAB.PH)


###### --------- GOOD PLOTS ---------- ########### 
# survival by population and pH - did survival correlate with parental pH consistently across populations? 
deploy.col <- c("gray60", "steelblue3")[as.numeric(Deploy.data$PH)]
names(deploy.col) <- Deploy.data$PH

# Plot survival by populatio and parental pH
ggplot(subset(Deploy.data, HAB.PH !="NA"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), color=PH)) +  geom_jitter(width = 0.35, size=4, aes(shape=PH)) + 
  labs(title="Percent survival, population and pH",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_color_manual(values=deploy.col)

# Plot each population separately by habitat and parental pH 
pops <- levels(Deploy.data$POPULATION)
# jitter plots 
for (i in 1:4) {
  print(ggplot(subset(Deploy.data, POPULATION == pops[i] & HAB.PH !="NA"), aes(x=HAB.PH, y= 100*(SURVIVED/DEPLOYED), color=PH)) +  geom_jitter(width = 0.35, size=4, aes(shape=PH)) + 
    labs(title=paste("Percent survival, Habitat and parental pH\n", pops[i], sep=" ") ,y=expression("Percent Survival")) + 
    theme_bw(base_size = 14) + xlab("Deployment Habitat & Parental pH") +
    theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_color_manual(values=deploy.col)) 
}

# Barplots 
for (i in 1:4) {
  print(ggplot(subset(Deploy.data, POPULATION == pops[i] & HAB.PH !="NA"), aes(x=HAB.PH, y= 100*(SURVIVED/DEPLOYED), color=PH)) +  geom_boxplot() + 
          labs(title=paste("Percent survival, Habitat and parental pH\n", pops[i], sep=" ") ,y=expression("Percent Survival")) + 
          theme_bw(base_size = 14) + xlab("Deployment Habitat & Parental pH") +
          theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_color_manual(values=deploy.col)) 
}



View(Deploy.data)


ggplot(subset(Deploy.data, HAB.PH !="NA"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) +
  geom_boxplot() +
  labs(title="Percent survival by pH within population",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values = deploy.col) 


ggplot(subset(Deploy.data, HABITAT != "NA" & HAB.PH != "NA"), aes(x=HAB.PH, y=100*(SURVIVED/DEPLOYED), fill=PH, colour=HABITAT)) + 
  geom_boxplot() +
  labs(title="Survival by Parental Exposure & Habitat", y=expression("% Survival")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), legend.title = element_text(size=20, face="bold"), legend.text = element_text(size=16)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + scale_colour_manual(values=c("black", "red"))

ggplot(subset(Deploy.data, HABITAT != "NA" & HAB.PH != "NA"), aes(x=PH, y=100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_boxplot() +
  theme_bw(base_size = 16) + 
  labs(title="Survival by Parental pH Exposure", y=expression("% Survival")) + 
  theme_bw(base_size = 14) + xlab("Parental pH") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=26, face="bold"), legend.text = element_text(size=16)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) 

nrow(subset(Deploy.data, PH=="Low"))
nrow(subset(Deploy.data, PH=="Ambient")) #57 pouches in each eelgrass 

# survival by habitat/parental pH, 
ggplot(subset(Deploy.data, HAB.PH !="NA"), aes(x=HABITAT, y= 100*(SURVIVED/DEPLOYED), colour=PH)) + 
  geom_jitter(width = 0.25, size=4, aes(shape=PH)) +
  labs(title="Percent survival by habitat and parental pH",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = 1.5, colour="gray")


# survival by population & Bay - did populations do better on their home turf? 
ggplot(subset(Deploy.data, HAB.PH !="NA"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), colour=BAY)) + 
  geom_jitter(width = 0.4, size=4, aes(shape=PH)) +
  labs(title="Percent survival, population and pH",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray")


# survival by population & Bay - did populations do better on their home turf? 
ggplot(subset(Deploy.data, HAB.PH !="NA"), aes(x=BAY, y= 100*(SURVIVED/DEPLOYED), colour=POPULATION)) + 
  geom_jitter(width = 0.4, size=4, aes(shape=PH)) +
  labs(title="Percent survival, population and pH",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 18, hjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray")





# Survival difference between habitats in ambient parent groups 
anova(glm(cbind(SURVIVED, DEPLOYED) ~ BAY+HABITAT, data=subset(Deploy.data, PH=="A"), binomial), test="Chi") #ambient parents, p=0.06018
anova(glm(cbind(SURVIVED, DEPLOYED) ~ BAY+HABITAT, data=subset(Deploy.data, PH=="L"), binomial), test="Chi") #ambient parents, p=0.8123
anova(glm(cbind(SURVIVED, DEPLOYED) ~ BAY*PH, data=Deploy.data, binomial), test="Chi") #ambient parents, p=0.0001019 ***

# Survival 
plot(subset(Deploy.data, BAY=="FB")$SURVIVED/subset(Deploy.data, BAY=="FB")$DEPLOYED ~ subset(Deploy.data, BAY=="FB")$HAB.PH, main="Fidalgo Bay", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

plot(subset(Deploy.data, BAY=="PG")$SURVIVED/subset(Deploy.data, BAY=="PG")$DEPLOYED ~ subset(Deploy.data, BAY=="PG")$HAB.PH, main="Port Gamble", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

plot(subset(Deploy.data, BAY=="CI")$SURVIVED/subset(Deploy.data, BAY=="CI")$DEPLOYED ~ subset(Deploy.data, BAY=="CI")$HAB.PH, main="Case Inlet", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

plot(subset(Deploy.data, BAY=="SK")$SURVIVED/subset(Deploy.data, BAY=="SK")$DEPLOYED ~ subset(Deploy.data, BAY=="SK")$HAB.PH, main="Skokomish", xlab="Parent PH, Habitat", ylab="% Survival", size=I(4), col=c("skyblue3", "skyblue3", "seagreen3", "seagreen3"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# Compare survival between habitats in groups W/O low pH exposure 
plot(subset(Deploy.data, PH == "A")$SURVIVED/subset(Deploy.data, PH == "A")$DEPLOYED ~ subset(Deploy.data, PH == "A")$HABITAT, main="Survival by habitat\nAmbient pH parental history")
anova(glm(cbind(SURVIVED, DEPLOYED) ~ HABITAT, data=subset(Deploy.data, PH == "A"), binomial), test="Chi") # p= 0.059 - higher survival in eelgrass if parent not exposed to low pH 

# Compare survival between habitats in groups WITH low pH exposure 
plot(subset(Deploy.data, PH == "L")$SURVIVED/subset(Deploy.data, PH == "L")$DEPLOYED ~ subset(Deploy.data, PH == "L")$HABITAT, main="Survival by habitat\nLow pH parental history", ylab = "% Survival", xlab="Habitat")
anova(glm(cbind(SURVIVED, DEPLOYED) ~ HABITAT, data=subset(Deploy.data, PH == "L"), binomial), test="Chi") # p= 0.87 - not different if parent exposed to low pH 
View(subset(Deploy.data, PH == "L"))

# Fidalgo Bay
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT, data=subset(Deploy.data, BAY=="FB"), binomial)) #No differences between habitat, or habitat*PH 
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH, data=subset(Deploy.data, BAY=="FB"), binomial)) #lower 

# Port Gamble 
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT, data=subset(Deploy.data, BAY=="PG"), binomial)) #diff all comparisons

# Case Inlet 
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT, data=subset(Deploy.data, BAY=="CI"), binomial)) #not diff, p=0.1733

# Skokomish 
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH*HABITAT, data=subset(Deploy.data, BAY=="SK"), binomial)) #no differences 

# ----------

# Survival between populations overall, and at each location 
plot(Deploy.data$SURVIVED/Deploy.data$DEPLOYED ~ Deploy.data$POPULATION, main="survival by population", xlab="Population", ylab="% Survival")
anova(Deploy.survival.glm.pop <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=Deploy.data, binomial), test="Chi") #very different survival between populations, p=8.0e-10

# In Fidalgo Bay 
plot(subset(Deploy.data, BAY == "FB")$SURVIVED/subset(Deploy.data, BAY == "FB")$DEPLOYED ~ subset(Deploy.data, BAY == "FB")$POPULATION, main="survival by population\nFidalgo Bay", xlab="Population", ylab="% Survival")
anova(Deploy.survival.glm.pop.FB <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=subset(Deploy.data, BAY == "FB"), binomial), test="Chi") #very different between pops, p=0.0004

# In Port Gamble Bay 
plot(subset(Deploy.data, BAY == "PG")$SURVIVED/subset(Deploy.data, BAY == "PG")$DEPLOYED ~ subset(Deploy.data, BAY == "PG")$POPULATION, main="survival by population\nPort Gamble Bay", xlab="Population", ylab="% Survival")
anova(Deploy.survival.glm.pop.PG <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=subset(Deploy.data, BAY == "PG"), binomial), test="Chi") #very different, p=0.0004

# In Case Inlet 
plot(subset(Deploy.data, BAY == "CI")$SURVIVED/subset(Deploy.data, BAY == "CI")$DEPLOYED ~ subset(Deploy.data, BAY == "CI")$POPULATION, main="survival by population\nCase Inlet", xlab="Population", ylab="% Survival")
anova(Deploy.survival.glm.pop.CI <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=subset(Deploy.data, BAY == "CI"), binomial), test="Chi") #very different, p=0.0007

# In Skokomish 
plot(subset(Deploy.data, BAY == "SK")$SURVIVED/subset(Deploy.data, BAY == "SK")$DEPLOYED ~ subset(Deploy.data, BAY == "SK")$POPULATION, main="survival by population\nSkokomish", xlab="Population", ylab="% Survival")
anova(Deploy.survival.glm.pop.SK <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=subset(Deploy.data, BAY == "SK"), binomial), test="Chi") #not so different, p=0.143

#-------- Assess length data 
library(reshape2)
Deploy.data.long <- melt(Deploy.data, id.vars = c("DATE", "BAY", "HABITAT", "POUCH", "POPULATION", "PH", "SIZE", "HAB.PH", "POP.PH.HAB", "DEPLOYED", "SURVIVED", "MASS.F"), variable.name = "oyster.rep", value.name = "length.mm")
Deploy.data.length <- subset(Deploy.data.long, length.mm != "NA")

# Remove empty factor levels 
Deploy.data.length$PH <- droplevels(Deploy.data.length$PH)
Deploy.data.length$HABITAT <- droplevels(Deploy.data.length$HABITAT)
Deploy.data.length$BAY <- droplevels(Deploy.data.length$BAY)
Deploy.data.length$POPULATION <- droplevels(Deploy.data.length$POPULATION)
Deploy.data.length$SIZE <- droplevels(Deploy.data.length$SIZE)
Deploy.data.length$POUCH <- as.factor(Deploy.data.length$POUCH)
str(Deploy.data.length)

# Check out length between habitat across all locations 
plot(Deploy.data.length$length.mm ~ Deploy.data.length$HABITAT, main="Length by habitat, all oysters")

# Check out length between habitat, within parental history, across all locations 
plot(subset(Deploy.data.length, PH == "A")$length.mm ~ subset(Deploy.data.length, PH == "A")$HABITAT, main="Length by habitat\nAmbient pH parental history")
plot(subset(Deploy.data.length, PH == "L")$length.mm ~ subset(Deploy.data.length, PH == "L")$HABITAT, main="Length by habitat\nLow pH parental history")

# Fidalgo Bay length 
plot(subset(Deploy.data.length, PH == "A" & BAY == "FB")$length.mm ~ subset(Deploy.data.length, PH == "A" & BAY == "FB")$HABITAT, main="Length by habitat\nAmbient pH parental history\nFidalgo Bay", xlab="Habitat", ylab="Length (mm)")
plot(subset(Deploy.data.length, PH == "L" & BAY == "FB")$length.mm ~ subset(Deploy.data.length, PH == "L" & BAY == "FB")$HABITAT, main="Length by habitat\nLow pH parental history\nFidalgo Bay", xlab="Habitat", ylab="Length (mm)")

# Port Gamble Bay length 
plot(subset(Deploy.data.length, PH == "A" & BAY == "PG")$length.mm ~ subset(Deploy.data.length, PH == "A" & BAY == "PG")$HABITAT, main="Length by habitat\nAmbient pH parental history\nPort Gamble Bay", xlab="Habitat", ylab="Length (mm)")
plot(subset(Deploy.data.length, PH == "L" & BAY == "PG")$length.mm ~ subset(Deploy.data.length, PH == "L" & BAY == "PG")$HABITAT, main="Length by habitat\nLow pH parental history\nPort Gamble Bay", xlab="Habitat", ylab="Length (mm)")

# Case Inlet length 
plot(subset(Deploy.data.length, PH == "A" & BAY == "CI")$length.mm ~ subset(Deploy.data.length, PH == "A" & BAY == "CI")$HABITAT, main="Length by habitat\nAmbient pH parental history\nCase Inlet", xlab="Habitat", ylab="Length (mm)")
plot(subset(Deploy.data.length, PH == "L" & BAY == "CI")$length.mm ~ subset(Deploy.data.length, PH == "L" & BAY == "CI")$HABITAT, main="Length by habitat\nLow pH parental history\nCase Inlet", xlab="Habitat", ylab="Length (mm)")

# Skokomish length 
plot(subset(Deploy.data.length, PH == "A" & BAY == "SK")$length.mm ~ subset(Deploy.data.length, PH == "A" & BAY == "SK")$HABITAT, main="Length by habitat\nAmbient pH parental history\nSkokomish", xlab="Habitat", ylab="Length (mm)")
plot(subset(Deploy.data.length, PH == "L" & BAY == "SK")$length.mm ~ subset(Deploy.data.length, PH == "L" & BAY == "SK")$HABITAT, main="Length by habitat\nLow pH parental history\nSkokomish", xlab="Habitat", ylab="Length (mm)")

# Length stats 
hist(Deploy.data.length$length.mm) #looks pretty normal 
qqnorm(Deploy.data.length$length.mm) 
shapiro.test(Deploy.data.length$length.mm) #p=0.07051 - good enough! 
plot(x=Deploy.data.length$SURVIVED/Deploy.data.length$DEPLOYED, y=Deploy.data.length$length.mm, main="Length against % Survival", xlab="% Survival", ylab="Final Shell Length") #very high survival groups (>80%) larger 


# Size differences remain after deployment? 

Deploy.data.length$PH <- gsub("A", "AMBIENT", Deploy.data.length$PH)
Deploy.data.length$PH <- gsub("L", "LOW", Deploy.data.length$PH)
colors3 <- c("AMBIENT"= "royalblue3", "LOW"="seagreen2")

# length distribution before deployment 
ggdensity(data=Pre.length, x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors3) +
  labs(title="Shell length (mm)\n@ 12-months",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

aggregate(value ~ PH, Pre.length, mean)

# length distribution after deployment
ggdensity(data=Deploy.data.length, x = "length.mm",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors3) +
  labs(title="Shell length (mm)\n Post-Deployment (@ 15 months)",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

summary(glm.l6 <- glmer(length.mm~  PH*HABITAT+(1|POPULATION),family=Gamma(link="log"), data=Oly.size.long5))
hist(Deploy.data.length$length.mm)

# ----- Calculate growth: mean final length - mean initial length 

# Calculate mean final length within each pouch 
Deploy.length.mean <- aggregate(length.mm ~ POUCH + PH + BAY + HABITAT + POPULATION + SIZE, Deploy.data.length, mean, na.rm=TRUE)
colnames(Deploy.length.mean) <- c("POUCH", "PH", "BAY", "HABITAT", "POPULATION", "SIZE", "POST.LENGTH")

# Calculate mean initial length within each pouch (requires pulling in dataset from previous script)
Pre.length <- melt(subset(Oly.size, TEMP==6)[,c(-2,-4,-5,-8)], id.vars = c("BAG", "COHORT", "PH", "SIZE.CLASS"), na.action = na.omit)
Pre.length$value <- as.numeric(Pre.length$value)
Pre.length <- subset(Pre.length, BAG != "NA" & (value != "NA" & value != "s" & value != ""))
Pre.length$BAG <- as.factor(Pre.length$BAG)
Pre.length.mean <- aggregate(value ~ BAG + COHORT + PH + SIZE.CLASS, Pre.length, mean, na.rm=TRUE)
colnames(Pre.length.mean) <- c("BAG", "COHORT", "PH", "SIZE.CLASS", "PRE.LENGTH")

# Merge pre and post length data 
Deploy.growth <- merge(x=Pre.length.mean, y=Deploy.length.mean, by.x="BAG", by.y="POUCH")
Deploy.growth$Growth <- Deploy.growth$POST.LENGTH - Deploy.growth$PRE.LENGTH
plot(subset(Deploy.growth, PH.x=="AMBIENT")$Growth ~ subset(Deploy.growth, PH.x=="AMBIENT")$HABITAT)
plot(subset(Deploy.growth, PH.x=="LOW")$Growth ~ subset(Deploy.growth, PH.x=="LOW")$HABITAT)

plot(subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="FB")$Growth ~ subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="FB")$HABITAT, main="FB Ambient")
plot(subset(Deploy.growth, PH.x=="LOW" & BAY=="FB")$Growth ~ subset(Deploy.growth, PH.x=="LOW" & BAY=="FB")$HABITAT, main="FB Low")

plot(subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="PG")$Growth ~ subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="PG")$HABITAT, main="PG Ambient")
plot(subset(Deploy.growth, PH.x=="LOW" & BAY=="PG")$Growth ~ subset(Deploy.growth, PH.x=="LOW" & BAY=="PG")$HABITAT, main="PG Low")

plot(subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="CI")$Growth ~ subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="CI")$HABITAT, main="CI Ambient")
plot(subset(Deploy.growth, PH.x=="LOW" & BAY=="CI")$Growth ~ subset(Deploy.growth, PH.x=="LOW" & BAY=="CI")$HABITAT, main="CI Low")

plot(subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="SK")$Growth ~ subset(Deploy.growth, PH.x=="AMBIENT" & BAY=="SK")$HABITAT, main="SK Ambient")
plot(subset(Deploy.growth, PH.x=="LOW" & BAY=="SK")$Growth ~ subset(Deploy.growth, PH.x=="LOW" & BAY=="SK")$HABITAT, main="SK Low")

anova(lm(Growth ~ BAY*PH.y*HABITAT, data=Deploy.growth)) #only difference in growth is between bays, p=0.0067
TukeyHSD(aov(Growth ~ BAY, data=Deploy.growth)) #sig diff: SK < CI, SK < PG

Deploy.growth$BAY.HAB <- as.factor(paste(Deploy.growth$BAY, Deploy.growth$HABITAT, sep = "."))
Deploy.growth$BAY.PH.HAB <- as.factor(paste(Deploy.growth$BAY, Deploy.growth$PH.y,  Deploy.growth$HABITAT, sep = "."))
Deploy.growth$PH.HAB <- as.factor(paste(Deploy.growth$PH.y,  Deploy.growth$HABITAT, sep = "."))

library(ggplot2)
ggplot(Deploy.growth, aes(x=BAY.PH.HAB, y=Growth, fill=PH.y, colour=HABITAT)) + 
  geom_boxplot() +
  labs(title="Growth by Parental pH, Habitat",y=expression("Growth (mean length change by pouch)")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + scale_colour_manual(values=c("red", "black"))

ggplot(Deploy.growth, aes(x=PH.HAB, y=Growth, fill=PH.y, colour=HABITAT)) + 
  geom_boxplot() +
  labs(title="Growth by Parental pH, Habitat",y=expression("Growth (mean length change within pouch, mm)")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + scale_colour_manual(values=c("red", "black"))




#----------- Assess mass data 

plot(Deploy.data.length$MASS.F ~ Deploy.data.length$HABITAT)
plot(subset(Deploy.data.length, PH == "A")$MASS.F ~ subset(Deploy.data.length, PH == "A")$HABITAT, main="Group mass by habitat\nAmbient pH parental history")
plot(subset(Deploy.data.length, PH == "L")$MASS.F ~ subset(Deploy.data.length, PH == "L")$HABITAT, main="Group mass by habitat\nLow pH parental history")

Pre.mass <- subset(Oly.size, TEMP==6 & BAG!="NA")[,c(1:8)]
Post.mass <- subset(Deploy.data, POUCH != "NA" & (POUCH != 167 & POUCH != 12))[,c(1:12)]
Pre.mass <- merge(x=Pre.mass, y=Post.mass[c("POUCH", "DEPLOYED")], by.x = "BAG", by.y="POUCH")
str(Pre.mass)

Pre.mass$Mass.pre.per <- Pre.mass$BAG.WEIGHT/Pre.mass$DEPLOYED
Post.mass$Mass.post.per <- Post.mass$MASS.F/Post.mass$SURVIVED
Post.mass$POP.PH.HAB <- droplevels(Post.mass$POP.PH.HAB)
plot(Pre.mass$Mass.pre.per ~ Pre.mass$PH)
plot(Post.mass$Mass.post.per ~ Post.mass$POP.PH.HAB)
Post.mass$BAY.PH.HAB <- as.factor(paste(Post.mass$BAY, Post.mass$PH, Deploy.growth$HABITAT, sep = "."))
View(Post.mass)

ggplot(subset(Post.mass, Mass.post.per!="NA" & PH != "NA"), aes(x=BAY.PH.HAB, y=Mass.post.per, fill=PH)) + 
  geom_boxplot() +
  labs(title="Mean mass per oyster (g) by Parental pH, Habitat",y=expression("Mass/oyster (g)")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("skyblue3", "seagreen3"))

ggplot(subset(Post.mass, Mass.post.per!="NA" & PH != "NA"), aes(x=HAB.PH, y=Mass.post.per, fill=PH, colour=HABITAT)) + 
  geom_boxplot() +
  labs(title="Mean mass per oyster (g) by\nparental pH, habitat",y=expression("Mass/oyster (g)")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + scale_colour_manual(values=c("black", "red"))


