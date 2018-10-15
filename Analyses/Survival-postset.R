Survival.post <- droplevels(subset(Survival, survival.postset != "NA"))
Survival.post$survival.postset <- Survival.post$survival.postset*100

par(mar = c(5,4,4,2)) ## default is c(5,4,4,2) + 0.1
# Plot survival from 224um to post-set by pH treatment  (all populations combined) 

palette(c("skyblue3", "seagreen3", "orange1", "indianred2"))
plot(Survival.post$Treatment, 100*(Survival.post$survival.postset), main="Percent Survival\neyed larvae to postset", size=I(4), xlab="Treatment", ylab="Percent Survival", col=Survival.post$Treatment, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

plot(Survival.post$Population, 100*(Survival.post$survival.postset), main="Percent Survival\nveliger to eyed larvae (224um)", 
     size=I(4), xlab="Population", ylab="Percent Survival", col=Survival.post$Population, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)
plot(Survival.post$Setters.stocked, 100*(Survival.post$survival.postset), main="Percent Survival\nveliger to eyed larvae (224um)", 
     xlab="Total Spawned", ylab="Percent Survival", col=Survival.post$pH, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# Inspect correlation plots between % survival to setting size (224um), total spawned, # days stocked, # larvae stocked. NOTE: expand plot window 
pairs(Survival.post[!rowSums(is.na(
  Survival.post[c("Total.Spawned", "Days.Stocked", "Larvae.Stocked", "Larvae.stocked.adjusted", "Setters.stocked", "survival.setters", "survival.postset")])),
  c("Total.Spawned", "Days.Stocked", "Larvae.Stocked", "Larvae.stocked.adjusted", "Setters.stocked", "survival.setters", "survival.postset")], lower.panel=panel.smooth, upper.panel=panel.cor)
# RESULT: No significant correlation between % survival & other variables. Other variables highly correlated. Include Total.Spawned variable in models. 

# Test assumptions prior to running anova/lm: 

# normal distribution? 
hist(log(Survival.post$survival.postset))
shapiro.test(log(Survival.post$survival.postset))
qqPlot(log(Survival.post$survival.postset))
# log-normal looks good 

# Outliers? 
boxplot(log(Survival.post$survival.postset))
boxplot.stats(log(Survival.post$survival.postset))$out  # no outliers  

#  Bartlett Test of Homogeneity of Variances between factors 
bartlett.test(log(survival.postset)~Population, data=Survival.post) #0.5991
bartlett.test(log(survival.postset)~Temperature, data=Survival.post) #0.4026
bartlett.test(log(survival.postset)~pH, data=Survival.post) #0.05279 <--- hmmm. close. 

# RESULT: ANOVA assumptions met (not sure about homoscedacity between pH groups ...)

# Test percent survival data against factors via summary(lm()). Do not apply weight based on # larvae reared. Drop variables sequentially with highest p-value. 

# First test variables separately. None significant on their own 
summary(test.postset1 <- lm(log(survival.postset) ~ Population, data=Survival.post)) # Sign.
summary(test.postset2 <- lm(log(survival.postset) ~ Temperature, data=Survival.post))
summary(test.postset3 <- lm(log(survival.postset) ~ pH, data=Survival.post))
summary(test.postset4 <- lm(log(survival.postset) ~ Days.Stocked, data=Survival.post)) #highly sign! 

# Now test variables together, using drop1 
drop1(test.postset5 <- lm(log(survival.postset) ~ Days.Stocked + Population + Temperature + pH, data=Survival.post)) 
drop1(test.postset5 <- lm(log(survival.postset) ~ Days.Stocked + Population + Temperature, data=Survival.post)) 
drop1(test.postset6 <- lm(log(survival.postset) ~ Days.Stocked + Population, data=Survival.post)) 
drop1(test.postset7 <- lm(log(survival.postset) ~ Days.Stocked*pH + Days.Stocked*Population + Days.Stocked*Temperature, data=Survival.post)) 
drop1(test.postset8 <- lm(log(survival.postset) ~ Days.Stocked*pH + Days.Stocked*Temperature, data=Survival.post)) 
drop1(test.postset9 <- lm(log(survival.postset) ~ Days.Stocked*pH, data=Survival.post)) 
drop1(test.postset10 <- lm(log(survival.postset) ~ pH*Days.Stocked + pH*Population + pH*Temperature, data=Survival.post)) 
drop1(test.postset11 <- lm(log(survival.postset) ~ pH*Days.Stocked + pH*Population, data=Survival.post)) 
drop1(test.postset12 <- lm(log(survival.postset) ~ Population:Days.Stocked + Population:pH + Population:Temperature, data=Survival.post)) 
drop1(test.postset13 <- lm(log(survival.postset) ~ Population:Days.Stocked + Population:Temperature, data=Survival.post)) 
drop1(test.postset14 <- lm(log(survival.postset) ~ Population:Days.Stocked, data=Survival.post)) 
drop1(test.postset15 <- lm(log(survival.postset) ~ Temperature:Days.Stocked + Temperature:pH + Temperature:Population, data=Survival.post)) 
drop1(test.postset16 <- lm(log(survival.postset) ~ Temperature:Days.Stocked + Temperature:Population, data=Survival.post)) 
drop1(test.postset17 <- lm(log(survival.postset) ~ Temperature:Days.Stocked, data=Survival.post)) 

AIC(test.postset1, test.postset2, test.postset3, test.postset4, test.postset5, test.postset6, test.postset7, test.postset8, test.postset9, test.postset10, test.postset11, test.postset12, test.postset13, test.postset14, test.postset15, test.postset16, test.postset17) 

summary(test.postset12) # Model p-value=0.0399, Adjusted R-squared=0.8814 AIC=20.53216 (smallest AIC)
anova(test.postset12)   
# Predictor               Df SumSq  MeanSq  F-value Pr(>F)  
# Population:Days.Stocked  4 18.0886  4.5222 23.0953 0.01368 *
# Population:pH            4  3.0853  0.7713  3.9393 0.14456  
# Population:Temperature   4  3.0058  0.7515  3.8378 0.14904  
# Residuals                3  0.5874  0.1958                   

## RESULT, all data: Best fit/most parsimonious model includes Population interaction with Days Stocked, pH and Temperature. Only significant covariate is the Population:days.stocked.  This indicates that the primary factor was how many different "groups" of new larvae were stocked - does this mean that genetic selection/competition was occurring/dominant during metamorphosis?  

# survival 224 -> postset ~ # days new larvae stocked 
plot(x=Survival.post$Days.Stocked, y=log(Survival.post$survival.postset), col=c("skyblue3","orange1")[as.numeric(Survival.post$Temperature)], pch=c(15, 17)[as.numeric(Survival.post$pH)], cex=1.5, main="% postset surv. ~ days new laravae stocked\nColor=temp, Symbol=pH", xlab="days larvae stocked", ylab="% 224um Survival to postset")
abline(test.postset4) 
# strong evidence for competition between families during metamorphosis 

# survival new larvae -> 224 ~ # days new larvae stocked 
plot(x=Survival.set$Days.Stocked, y=Survival.set$survival.setters, col=c("skyblue3","orange1")[as.numeric(Survival.set$Temperature)], pch=c(15, 17)[as.numeric(Survival.set$pH)], cex=1.5, main="% surv. to eyed ~ days new laravae stocked\nColor=temp, Symbol=pH", xlab="days larvae stocked", ylab="% new larvae survival to eyed")
# little evidence for competition between families during larval phase 

# do the # days stocked differ by treatment? 
plot(Survival.post$Treatment, Survival.post$Days.Stocked, main="# Days stocked", size=I(4), xlab="Treatment", ylab="Days stocked", col=Survival.post$Treatment, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)
hist(Survival.post$Days.Stocked)
shapiro.test(Survival.post$Days.Stocked)
bartlett.test(Survival.post$Days.Stocked, g=Survival.post$Treatment) 
summary(lm(Days.Stocked ~ Population+Temperature+pH+Population:pH, data=Survival.post)) #differs by pop, but not by pH














#--------- plots and glms, etc. 

# try plotting % setter survival ~ % larvae survival
plot(x=Survival.post$survival.setters, y=Survival.post$survival.postset, col=c("skyblue3","orange1")[as.numeric(Survival.post$Temperature)], pch=c(15, 17)[as.numeric(Survival.post$pH)], cex=1.5, main="% postset surv. ~ % larvae surv.\nColor=temp, Symbol=pH", xlab="% Larvae Survival to 224um", ylab="% 224um Survival to postset")

plot(x=Survival.post$Setters.stocked, y=log(Survival.post$survival.postset), col=c("skyblue3","orange1")[as.numeric(Survival.post$Temperature)], pch=c(15, 17)[as.numeric(Survival.post$pH)], cex=1.5, main="log-% postset surv. ~ setters stocked\nColor=temp, Symbol=pH", xlab="setters stocked", ylab="% 224um Survival to postset")


# Assess survival from near metamorphosis 224um ("setters") to post set: 
plot(x=Survival$Setters.stocked, y=(Survival$Post.set/Survival$Setters.stocked), col=Survival$pH)

# remove the outlier - very high stocking density group (SN10-Ambient)
plot(x=subset(Survival, Setters.stocked !=29595)$Setters.stocked, y=100*(subset(Survival, Setters.stocked !=29595)$Post.set/subset(Survival, Setters.stocked !=29595)$Setters.stocked), col=subset(Survival, Setters.stocked !=29595)$pH)

# Percent survival from near metamorphosis (224um) to post set, color coded by population, grouped by treatment
# Without outlier (SN10-ambient, likely very influenced by stocking density). 
plot(subset(Survival, Setters.stocked !=29595)$Treatment, 100*(subset(Survival, Setters.stocked !=29595)$Post.set / subset(Survival, Setters.stocked !=29595)$Setters.stocked), main="Percent survival through metamorphosis\n224um to post-set", size=I(4), xlab="Treatment", ylab="Percent Survival", legend=c(0.2, 0.75), col=c("skyblue3", "seagreen3", "orange1", "indianred2"), cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# plot survival, 224-post set 
ggplot(subset(Survival, Setters.stocked !=29595), aes(x=pH, y=100*(Post.set/Setters.stocked), fill=pH)) + 
  geom_boxplot() + 
  labs(title="Percent Survival\nEyed Larvae -> Post-Set",y=expression("Percent Survival")) + 
  theme_bw(base_size = 16) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=26, face="bold"), legend.text = element_text(size=20)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + guides(fill=guide_legend(title="Parental pH"))

aggregate(100*(Post.set/Setters.stocked) ~pH + Temperature, data=subset(Survival, Setters.stocked !=29595), mean)

ggplot(subset(Survival, Setters.stocked !=29595), aes(x=Population, y= 100*(Post.set/Setters.stocked), colour=Treatment, size=Setters.stocked)) + 
  geom_point() +
  labs(title="Percent survival\n224um -> post-set\nBy population, point size= #224's",y=expression("Percent Survival")) + 
  theme_bw(base_size = 14) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))

glm.postset.6 <- glm(cbind(Post.set, I(Setters.stocked-Post.set)) ~ pH, data=subset(Survival, Temperature==6), binomial)
summary(glm.postset.6)
3441.8/3441.4 # null/residual deviance = 1.00
anodev.fn(glm.postset.6, test.in = "Chi") # Not significantly different - metamorphosis not sign. different in parents exposed to low/ambient pH 
anova(glm.postset.6, test="Chi") #not sign. 

glm.postset.10 <- glm(cbind(Post.set, I(Setters.stocked-Post.set)) ~ pH, data=subset(Survival, Temperature==10), binomial)
summary(glm.postset.10)
4239.9/4209.3 # null/residual deviance = 1.00
anodev.fn(glm.postset.10, test.in = "Chi") # Yes, significantly different 
anova(glm.postset.10, test = "Chi")

glm.postset.10.noout <- glm(cbind(Post.set, I(Setters.stocked-Post.set)) ~ pH, data=subset(Survival, Setters.stocked !=29595 & Temperature==10), binomial)
summary(glm.postset.10.noout) 
1846.1/1666.6 # null/residual deviance = 1.1
anodev.fn(glm.postset.10.noout, test.in="Chi")
# Yes, significantly different between treatments -  metamorphosis more challenging if parent exposed to both low pH & high temp 


