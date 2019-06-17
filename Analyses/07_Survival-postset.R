Survival.post <- droplevels(subset(Survival, survival.postset != "NA"))
Survival.post$survival.postset <- Survival.post$survival.postset*100

aggregate(Singles ~ pH + Temperature, data=Survival.post[,c("Population", "Temperature", "pH", "Post.set", "Singles")], sum)

# Add average larval stocking density to dataframe for modeling 
mean.stock <- aggregate(value ~ Groups, data=subset(Bucket.Densities.long, Count=="density"), mean) #average stocking density  
Survival.post <- merge(x=mean.stock, y=Survival.post, by.x="Groups", by.y="Group", all.x=F, all.y=F)
colnames(Survival.post)[2] <- "Mean.stocked"
summary(test.postsety <- lm(log(survival.postset) ~ Mean.stocked, data=Survival.post)) 
summary(test.postsety2 <- lm(log(survival.postset) ~ Mean.stocked*factor(pH)*factor(Temperature), data=Survival.post)) 

par(mar = c(5,4,4,2)) ## default is c(5,4,4,2) + 0.1
# Plot survival from 224um to post-set by pH treatment  (all populations combined) 

palette(c("skyblue3", "seagreen3", "orange1", "indianred2"))
plot(Survival.post$Treatment, 100*(Survival.post$survival.postset), main="Percent Survival\neyed larvae to postset", size=I(4), xlab="Treatment", ylab="Percent Survival", col=Survival.post$Treatment, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

plot(Survival.post$Population, 100*(Survival.post$survival.postset), main="Percent Survival\nveliger to eyed larvae (224um)", 
     size=I(4), xlab="Population", ylab="Percent Survival", col=Survival.post$Population, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

# Inspect correlation plots between % survival to setting size (224um), total spawned, # days stocked, # larvae stocked. NOTE: expand plot window 
  pairs(Survival.post[!rowSums(is.na(
  Survival.post[c("Larvae.stocked.adjusted", "Mean.stocked", "Setters.stocked", "survival.setters", "survival.postset")])),
  c("Larvae.stocked.adjusted",  "Mean.stocked", "Setters.stocked", "survival.setters", "survival.postset")], lower.panel=panel.smooth, upper.panel=panel.cor)
# RESULT: Survival to post set not correlated with # setters stocked, but highly correlated with larval tank density - also high correlation among larval tank metrics; include Larvae.stocked.adjusted in model

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

# First test variables separately. 
anova(test.postset0 <- lm(log(survival.postset) ~ Larvae.stocked.adjusted, data=Survival.post)) # Sign., add as random effect 
anova(test.postset1 <- lm(log(survival.postset) ~ Population + (1|Larvae.stocked.adjusted), data=Survival.post)) # Sign.
TukeyHSD(aov(test.postset1 <- lm(log(survival.postset) ~ Population + (1|Larvae.stocked.adjusted), data=Survival.post))) # Sign.
anova(lm(log(survival.postset) ~ Population + (1|Larvae.stocked.adjusted), data=subset(Survival.post, Population!="SN"))) # not
anova(test.postset2 <- lm(log(survival.postset) ~ Temperature + (1|Larvae.stocked.adjusted), data=Survival.post))
anova(test.postset3 <- lm(log(survival.postset) ~ pH + (1|Larvae.stocked.adjusted), data=Survival.post))
anova(test.postset4 <- lm(log(survival.postset) ~ Population + Temperature*pH + (1|Larvae.stocked.adjusted), data=Survival.post)) 
anova(test.postset5 <- lm(log(survival.postset) ~ Population*Temperature+pH + (1|Larvae.stocked.adjusted), data=Survival.post)) 
anova(test.postset6 <- lm(log(survival.postset) ~ Population*pH+Temperature + (1|Larvae.stocked.adjusted), data=Survival.post))  #smallest AIC
anova(test.postset7 <- lm(log(survival.postset) ~ Population*pH + (1|Larvae.stocked.adjusted), data=Survival.post)) 
anova(test.postset8 <- lm(log(survival.postset) ~ Population + Population:pH + (1|Larvae.stocked.adjusted), data=Survival.post)) 
anova(test.postset9 <- lm(log(survival.postset) ~ Population:pH + (1|Larvae.stocked.adjusted), data=Survival.post)) 

AIC(test.postset1, test.postset2, test.postset3, test.postset4, test.postset5, test.postset6, test.postset7, test.postset8, test.postset9) 
summary(test.postset6) # Model p-value=0.007055, Adjusted R-squared=0.7819 

# Predictor               Df SumSq  MeanSq  F-value Pr(>F)  
# Population              3 15.8643  5.2881 14.6812 0.002102 **
#   pH                    1  0.0012  0.0012  0.0034 0.955229   
# Temperature             1  1.3681  1.3681  3.7982 0.092317 . 
# Population:pH           3  5.0122  1.6707  4.6384 0.043394 * 
#   Residuals             7  2.5214  0.3602    

TukeyHSD(aov(test.postset6 <- lm(log(survival.postset) ~ Population*pH+Temperature + (1|Larvae.stocked.adjusted), data=Survival.post))) #pairwise 


## RESULT: Best fit model includes significant Population effect, Population:pH interaction, larvae stocked in culture tanks, Population and Temperature. Population highly sign., Population:pH covariate sign.

# larvae stocked vary by temp & pH treatment? 
shapiro.test(Survival.post$Larvae.stocked.adjusted) #normal OK
hist(Survival.post$Larvae.stocked.adjusted) #normal OK
bartlett.test(Survival.post$Larvae.stocked.adjusted ~ Survival.post$pH) #variance OK
bartlett.test(Survival.post$Larvae.stocked.adjusted ~ Survival.post$Temperature) #variance OK
anova(lm(Larvae.stocked.adjusted ~ Temperature*pH, data=Survival.post)) #no sign. 

# survival postset ~ stocking density during larval phase 
plot(x=Survival.post$Larvae.stocked.adjusted, y=log(Survival.post$survival.postset), bg="black", col=c("gray32", "steelblue3")[as.numeric(Survival.post$pH)], pch=c(16,17)[as.numeric(Survival.post$Temperature)], cex=2, main="Log(%) survival through metamorphosis ~\n# days larvae stocked", xlab="mean larval tank density", ylab="log(%) survival")
#abline(test.postsety) 
text(x=Survival.post$Larvae.stocked.adjusted+2000,
     y=log(Survival.post$survival.postset)-.1, col="gray30", labels=Survival.post$Population, xpd=T, cex=0.8)
legend(70000, 3, legend=c("6-Low", "10-Low", "6-Amb", "10-Amb"),
       col=c("steelblue3", "steelblue3", "gray32", "gray32"), pch=c(16, 17, 16, 17), cex=1)

# strong evidence for stocking density effect on survival from 224um->post-set

# survival postset ~ stocking density during larval phase 
plot(x=Survival.post$Mean.stocked, y=log(Survival.post$survival.setters), bg="black", col=c("gray32", "steelblue3")[as.numeric(Survival.post$pH)], pch=c(16,17)[as.numeric(Survival.post$Temperature)], cex=2, main="Log(%) survival to eyed-larvae ~\nmean larval tank density", xlab="mean larval tank density", ylab="log(%) survival")
text(x=Survival.post$Mean.stocked+2000,
     y=log(Survival.post$survival.setters), col="gray30", labels=Survival.post$Population, xpd=T, cex=0.8)
#legend(78000, -4, legend=c("6-Low", "10-Low", "6-Amb", "10-Amb"), col=c("steelblue3", "steelblue3", "gray32", "gray32"), pch=c(16, 17, 16, 17), cex=1)

# survival new larvae -> 224 ~ # days new larvae stocked 
plot(x=Survival.post$Mean.stocked, y=Survival.post$survival.setters, col=c("skyblue3","orange1")[as.numeric(Survival.set$pH)], pch=17, cex=1.5, main="% surv. to eyed ~ days new laravae stocked\nColor=pH", xlab="mean larval tank density", ylab="% new larvae survival to eyed")
# little evidence for mortality from new->eyed larvae due to average tank density 

# survival new larvae -> 224 ~ # days new larvae stocked 
plot(x=Survival.post$Setters.stocked, y=Survival.post$survival.postset, col=c("skyblue3","orange1")[as.numeric(Survival.set$pH)], pch=17, cex=1.5, main="% surv. to eyed ~ setters stocked\nColor=pH", xlab="setters stocked", ylab="% eyed to postset survival")
# some evidence for mortality from eyed larvae -> postset due to # setters stocked, but not strong 














#--------- plots and glms, etc. in boneyard 

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


