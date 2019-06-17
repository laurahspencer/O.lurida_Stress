# Compared overall survival from stocked larvae to 224 (eyed larvae), and from eyed larvae to # post-set 

rm(list=ls())         #start script by deleting all objects - clean slate 

Survival <- read.csv("Data/Survival-Data.csv", header=T, stringsAsFactors = F, na.strings = c("NA", "unknown")) #read in data 
Survival[,c("Group", "Population", "Treatment", "Temperature", "pH")] <- lapply(Survival[,c("Group", "Population", "Treatment", "Temperature", "pH")], factor) #convert to factors 

# Assess survival from stocking to near metamorphosis 224um ("setters"): 

# Change factor descriptions 
Survival$Treatment <- gsub("10C, Ambient pH", "Unchilled\nAmbient pH", Survival$Treatment)
Survival$Treatment <- gsub("10C, Low pH", "Unchilled\nLow pH", Survival$Treatment)
Survival$Treatment <- gsub("6C, Ambient pH", "Chilled\nAmbient pH", Survival$Treatment)
Survival$Treatment <- gsub("6C, Low pH", "Chilled\nLow pH", Survival$Treatment)
Survival$Treatment <- as.factor(Survival$Treatment)

# Calculate % survival 
Survival$survival.setters <- Survival$Setters.stocked / Survival$Larvae.stocked.adjusted  #larvae stocked -> 224um 
Survival$survival.postset <- Survival$Post.set / Survival$Setters.stocked #224um -> post-set 
Survival.set <- droplevels(subset(Survival, survival.setters != "NA"))
Survival.set$survival.setters <- Survival.set$survival.setters*100
density.mean <- aggregate(density ~ Population+Temperature+pH, survival.biweekly, mean)
density.mean$Group <- paste(density.mean$Population, density.mean$Temperature, sep="-")
density.mean$Group <- factor(paste(density.mean$Group, density.mean$pH, sep=" "))
Survival.set <- merge(x=Survival.set, density.mean[,c("density", "Group")], by.x="Group", by.y="Group")

aggregate(survival.setters ~ pH + Temperature, data=Survival[,c("Population", "pH", "Temperature", "survival.setters")], mean)
aggregate(survival.postset ~ pH + Temperature + Population, data=Survival[,c("Population", "pH", "Temperature", "survival.postset")], mean)


par(mar = c(5,4,4,2)) ## default is c(5,4,4,2) + 0.1
# Plot survival to 224um by pH treatment  (all populations combined) 
palette(c("skyblue3", "seagreen3", "orange1", "indianred2"))
plot(Survival$Treatment, 100*(Survival$survival.setters), main="Percent Survival\nveliger to eyed larvae (224um)", 
     size=I(4), xlab="Treatment", ylab="Percent Survival", col=Survival$Treatment, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)
plot(Survival$Population, 100*(Survival$survival.setters), main="Percent Survival\nveliger to eyed larvae (224um)", 
     size=I(4), xlab="Population", ylab="Percent Survival", col=Survival$Population, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)
plot(Survival$Total.Spawned, 100*(Survival$survival.setters), main="Percent Survival\nveliger to eyed larvae (224um)", 
    xlab="Total Spawned", ylab="Percent Survival", col=Survival$pH, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# Inspect correlation plots between % survival to setting size (224um), total spawned, # days stocked, # larvae stocked. NOTE: expand plot window 
pairs(Survival.set[!rowSums(is.na(
  Survival.set[c("Larvae.stocked.adjusted", "density", "survival.setters")])),
  c("Larvae.stocked.adjusted", "density", "survival.setters")], lower.panel=panel.smooth, upper.panel=panel.cor)
# RESULT: No significant correlation between % survival & other variables. Other variables highly correlated.

# Test assumptions prior to running anova/lm: 

# normal distribution? 
hist(Survival.set$survival.setters)
shapiro.test(Survival.set$survival.setters)
qqPlot(Survival.set$survival.setters)
# Normal is OK

# Outliers? 
boxplot(Survival.set$survival.setters)
boxplot.stats(Survival.set$survival.setters)$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(survival.setters~Population, data=Survival.set) #0.199
bartlett.test(survival.setters~Temperature, data=Survival.set) #0.9249
bartlett.test(survival.setters~pH, data=Survival.set) #0.8795

# RESULT: ANOVA assumptions met 

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 

# First test variables separately. None significant on their own 
anova(test.setters0 <- lm(survival.setters ~ density, data=Survival.set)) 
anova(test.setters1 <- lm(survival.setters ~ Temperature, data=Survival.set)) 
anova(test.setters2 <- lm(survival.setters ~ pH, data=Survival.set)) 
anova(test.setters3 <- lm(survival.setters ~ Population, data=Survival.set))
# Now test variables together, using drop1 
anova(test.setters4 <- lm(survival.setters ~ Population+Temperature+pH, data=Survival.set)) 
anova(test.setters5 <- lm(survival.setters ~ Population*pH+Temperature, data=Survival.set)) 
anova(test.setters6 <- lm(survival.setters ~ Population+Temperature*pH, data=Survival.set)) 
anova(test.setters7 <- lm(survival.setters ~ Population*Temperature+pH, data=Survival.set)) 
anova(test.setters8 <- lm(survival.setters ~ Temperature+pH+Population:Temperature, data=Survival.set)) # most parsimonious
anova(test.setters9 <- lm(survival.setters ~ Temperature+pH, data=Survival.set)) 

AIC(test.setters0, test.setters1, test.setters2, test.setters3, test.setters4, test.setters5, test.setters6, test.setters7, test.setters8, test.setters9) 
summary(test.setters8) # Model p-value=0.06885, Adjusted R-squared=0.5459 

anova(test.setters8) 
# Predictor                  Df SumSq  MeanSq  F-value Pr(>F)  
# Temperature             1 10.087 10.0872  3.9599 0.08690 .
# pH                      1 11.974 11.9744  4.7008 0.06680 .
# Temperature:Population  6 44.252  7.3753  2.8953 0.09518 .
# Residuals               7 17.831  2.5473                  

plot(test.setters8$residuals, main="studentized residuals\n%survival to eyed larvae model") # inspect residuals. look good.s 

## RESULT, all data: Best fit/most parsimonious model includes Total.Spawned, Temperature, Total.Spawned:Temperature, and Total.Spawned:pH covariates. Temperature is retained, but not sign. pH is not retained. 

aggregate(survival.setters ~ pH, Survival.set, mean) #mean survival between pH
aggregate(survival.setters ~ pH, Survival.set, var) #mean var between pH
aggregate(survival.setters ~ pH, Survival.set, sd) #sd

# power analysis 
(3.032142-4.762341)/2.202150 #calculate effect size (diff mean/sd)
pwr.anova.test(k=2 , n=8, f=0.78 , sig.level =.05 , power =NULL) #power=0.826
pwr.anova.test(k=2 , n=8, f=0.78 , sig.level =.1 , power =NULL) #power=0.91


# Jitter / boxplot with symbols for temperature 
ggplot(Survival.set, aes(x=pH, y=survival.setters, col=pH)) + geom_boxplot() +
  geom_jitter(stat="identity", width = 0.2, size=6, aes(shape=Temperature)) +
  labs(title="Cumulative % Survival\nNew Larvae to Eyed Larvae") + xlab("Parental pH") + ylab("% Survival") +
  theme_bw(base_size = 20) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0, color="gray32"),  panel.border = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="gray32"),  axis.title.x = element_text(color="gray32"), axis.title.y = element_text(color="gray32"), legend.position="bottom", legend.background = element_rect(color = NULL, fill = NULL, linetype = NULL),  legend.key.size = unit(1.5, 'lines'), legend.text = element_text(colour="gray32")) + 
  scale_color_manual(values=c('gray25', 'gray25'), guide=FALSE) + scale_shape_discrete(name  =NULL, breaks=c(6, 10), labels=c("Chilled", "Unchilled"))


# Jitter / boxplot with symbols for temperature 
ggplot(Survival.set, aes(x=pH, y=survival.setters, col=pH)) + geom_boxplot() +
  geom_jitter(stat="identity", width = 0.2, size=4) +
  labs(title="Cumulative % Survival\nNew Larvae to Eyed Larvae") + xlab("Parental pH") + ylab("% Survival") +
  theme_bw(base_size = 18) + 
  theme(plot.title = element_text(face = 'bold',size = 16, hjust = 0, color="gray32"),  panel.border = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="gray32"),  axis.title.x = element_text(color="gray32"), axis.title.y = element_text(color="gray32"), legend.position="bottom", legend.background = element_rect(color = NULL, fill = NULL, linetype = NULL),  legend.key.size = unit(1.5, 'lines'), legend.text = element_text(colour="gray32")) + 
  scale_color_manual(values=c('gray40', 'steelblue'), guide=FALSE)




# ----- PLOTS, TBdone

ggplot(Survival, aes(x=Temperature, y=survival.setters, col=pH)) +
  geom_point() +
  labs(title="Overall Percent Survival\nNew Larvae -> Eyed Larvae",y=expression("Percent Survival",x=expression("Chilled, Unchilled"))) +
  theme_bw(base_size = 16) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=26, face="bold"), legend.text = element_text(size=20)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + guides(fill=guide_legend(title="Parental pH"))


# plot - survival for each population, color coded by treatment with circle size = # larvae stocked 
ggplot(Survival, aes(x=Population, y= 100*(Setters.stocked/Larvae.stocked.adjusted), colour=Treatment, size=sqrt(Larvae.stocked.adjusted))) + 
  geom_point(colour=c("orange1", "indianred2", "skyblue3", "seagreen3")) +
  labs(title="Percent survival\nlarvae -> 224um",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))



# boxplot of just 6C group % survival from new larvae -> 224um 

ggplot(subset(Survival, Temperature==6), aes(x=pH, y=100*(Setters.stocked/Larvae.stocked.adjusted), fill=pH)) +
  geom_boxplot() +
  labs(title="Percent Survival\nNew Larvae -> Eyed Larvae",y=expression("Percent Survival",x=expression("Parental pH Treatment"))) +
  theme_bw(base_size = 16) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=26, face="bold"), legend.text = element_text(size=20)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + guides(fill=guide_legend(title="Parental pH"))

