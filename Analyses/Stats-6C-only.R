# 6-only larval stats starting at biweekly survival 

# ------- LARVAL STOCKING DENSITY 

plot(subset(Bucket.Densities.long, Count=="density" & Temperature==6)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature==6)$Treatment) #boxplot to inspect 
kruskal.test(subset(Bucket.Densities.long, Count=="density" & Temperature==6)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature==6)$pH) # p=0.5352  <-- no pH difference 
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 6), mean)  

### ------ biweekly bucket cleaning survival 
# Test using aov on square-root arcsine transformed survival % data 
hist(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival))) # looks normal 
shapiro.test(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival)))  # shapiro test confirms 
bartlett.test(x=asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$survival)), g=subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1 & Temperature ==6))$pH) #equal variance between pH
Bucket.Densities.wide$survival.t <- asin(sqrt(Bucket.Densities.wide$survival)) #add column with sqrt-asin transformed survival

anova(lm(survival.t ~ pH, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ expected*pH, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ expected, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ Population, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ Population*pH, data=subset(survival.biweekly, Temperature == 6))) #no diff

### ------ LARVAL SURVIVAL TO 224um 

hist(subset(Survival.set, Temperature==6)$survival.setters)
shapiro.test(subset(Survival.set, Temperature==6)$survival.setters)
qqPlot(subset(Survival.set, Temperature==6)$survival.setters)
# Normal is OK

# Outliers? 
boxplot(subset(Survival.set, Temperature==6)$survival.setters)
boxplot.stats(subset(Survival.set, Temperature==6)$survival.setters)$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(survival.setters~Population, data=subset(Survival.set, Temperature==6)) #0.2375
bartlett.test(survival.setters~pH, data=subset(Survival.set, Temperature==6)) #0.7367

# RESULT: ANOVA assumptions met 

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 

# First test variables separately. None significant on their own 
summary(test.setters1 <- lm(survival.setters ~ Population, data=subset(Survival.set, Temperature==6)))
summary(test.setters2 <- lm(survival.setters ~ pH, data=subset(Survival.set, Temperature==6)))
summary(test.setters3 <- lm(survival.setters ~ Population+pH, data=subset(Survival.set, Temperature==6))) 
summary(test.setters4 <- lm(survival.setters ~ Larvae.stocked.adjusted, data=subset(Survival.set, Temperature==6))) 
summary(test.setters5 <- lm(survival.setters ~ Days.Stocked, data=subset(Survival.set, Temperature==6))) 
summary(test.setters6 <- lm(survival.setters ~ Larvae.stocked.adjusted+pH, data=subset(Survival.set, Temperature==6))) 

AIC(test.setters1, test.setters2, test.setters3, test.setters4, test.setters5, test.setters6) 
summary(test.setters3) # Model p-value=0.2026, Adjusted R-squared=0.5127 AIC=65.31575
anova(test.setters3) # 1 fewer covariates, best R-squared and total p-value. Most parsimonious.  

# Df  Sum Sq Mean Sq F value Pr(>F)
# Population  3 21.6257  7.2086  2.7558 0.2136
# pH          1  8.9207  8.9207  3.4103 0.1620
# Residuals   3  7.8474  2.6158       

# NO DIFFERENCES IN SURVIVAL BY PH 

### ------ survival 224um to post set 

# Test assumptions prior to running anova/lm: 

# normal distribution? 
hist(log(subset(Survival.post, Temperature==6)$survival.postset))
shapiro.test(log(subset(Survival.post, Temperature==6)$survival.postset))
qqPlot(log(subset(Survival.post, Temperature==6)$survival.postset))
# log-normal looks good 

# Outliers? 
boxplot(log(subset(Survival.post, Temperature==6)$survival.postset))
boxplot.stats(log(subset(Survival.post, Temperature==6)$survival.postset))$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(log(survival.postset)~Population, data=subset(Survival.post, Temperature==6)) #0.5991
bartlett.test(log(survival.postset)~pH, data=subset(Survival.post, Temperature==6)) #0.07711 <--- hmmm. close. 

# RESULT: ANOVA assumptions met (not sure about homoscedacity between pH groups ...)

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 

# First test variables separately. None significant on their own 
anova(test.postset1 <- lm(log(survival.postset) ~ Population, data=subset(Survival.post, Temperature==6))) 
anova(test.postset3 <- lm(log(survival.postset) ~ pH, data=subset(Survival.post, Temperature==6)))
anova(test.postset4 <- lm(log(survival.postset) ~ Mean.stocked, data=subset(Survival.post, Temperature==6)))
anova(test.postset5 <- lm(log(survival.postset) ~ Larvae.stocked.adjusted, data=subset(Survival.post, Temperature==6))) # sign! 

# Now test variables together
anova(test.postset6 <- lm(log(survival.postset) ~ Mean.stocked + Population  + pH, data=subset(Survival.post, Temperature==6))) 
anova(test.postset7 <- lm(log(survival.postset) ~ Mean.stocked + Population, data=subset(Survival.post, Temperature==6))) 
anova(test.postset8 <- lm(log(survival.postset) ~ Mean.stocked:pH + Mean.stocked:Population, data=subset(Survival.post, Temperature==6)))
anova(test.postset9 <- lm(log(survival.postset) ~ Mean.stocked:Population, data=subset(Survival.post, Temperature==6)))
anova(test.postset10 <- lm(log(survival.postset) ~ Mean.stocked*pH, data=subset(Survival.post, Temperature==6)))

AIC(test.postset1, test.postset3, test.postset4, test.postset5, test.postset6, test.postset7, test.postset8, test.postset9, test.postset10) 

summary(test.postset10) # Model p-value=0.01247, Adjusted R-squared=0.8553 AIC=12.73209 (smallest AIC)
anova(test.postset10) #sign. interaction between mean stocked & pH, but not pH alone 


### ------ juvenile length data - sign. smaller oysters from low ph groups, ~20% smaller. No diff. in stocking btn groups. 

# Fit glms with gamma errors, compare with AIC. Use glm() when cohort not included; use glmer to include cohort as random effect. 
summary(glm.l1 <- glm(value~  log(stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l3 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l4 <- glmer(value~  log(stocked)+PH +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l6 <- glmer(value~  log(stocked) +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l7 <- glmer(value~  PH +(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l9 <- glmer(value~  log(stocked)*PH+(1|COHORT),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))

AIC(glm.l1,glm.l3,glm.l4,glm.l6,glm.l7,glm.l9) #lowest AIC = glm.l4
summary(glm.l7) # lowest AI: length ~ PH + (1 | COHORT)
summary(glm.l7)  # significant! 
summary(glm.l4) # also, good to see that stocked isn't sign. with population as random effect, but pH still is. 
exp(-0.22256) # coefficient == 0.80

### -------- Eelgrass deployment results the same. 

### -------- multivariate screening 

# Inspect correlation between variables, to reduce # variables 
pairs(subset(oly.multivar, Temperature==6)[c("Total.Spawned", "Days.Stocked", "Larvae.stocked.adjusted", "Mean.larvae.stocked", "Setters.stocked", "survival.setters", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.stocked", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass")], lower.panel=panel.smooth, upper.panel=panel.cor)  

# Remove variables that are highly correlated with other variables, to reduce # (total.spawned, days stocked, larvae stocked)
pairs(subset(oly.multivar, Temperature==6)[c("Mean.larvae.stocked", "survival.setters","Setters.stocked", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass")], lower.panel=panel.smooth, upper.panel=panel.cor) #example result 



