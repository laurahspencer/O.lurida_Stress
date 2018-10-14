# Compared overall survival from stocked larvae to 224 (eyed larvae), and from eyed larvae to # post-set 

library(ggplot2)

Survival <- read.csv("Data/Survival-Data.csv", header=T, stringsAsFactors = F, na.strings = c("NA", "unknown")) #read in data 
Survival[,c("Group", "Population", "Treatment", "Temperature", "pH")] <- lapply(Survival[,c("Group", "Population", "Treatment", "Temperature", "pH")], factor)
str(Survival) #confirm formatting 
 
# Summary survival and stocking stats 
# Pops <- c("NF", "NF","NF","NF", "SN", "SN", "SN", "SN", "HL","HL","HL","HL", "K","K","K","K", "SN", "SN", "SN", "SN")
# Trts <- c("10 Ambient", "10 Low", "6 Ambient", "6 Low", "10 Ambient", "10 Low", "6 Ambient", "6 Low","10 Ambient", "10 Low", "6 Ambient", "6 Low","10 Ambient", "10 Low", "6 Ambient", "6 Low", "10 Low", "6 Low","10 Ambient", "6 Ambient")
# Stocked <- as.numeric(c( 319277,211417,131675,391716,750793,660450,477403,532417,283858,310875,65667,255371, 324165,225873,353768, 179717, 800, 800, 800, 800))
# Stocked.adj <- as.numeric(c(274464, 210907, 131005, 390136, 708904, 646529, 435238, 515788, 269869, 300065, 48929, 227711, 314438, 221487, 221487, 171998, 800, 800, 800, 800))
# Setters <- as.numeric(c(3698, 2977, 11119, 11780, 29595, 4757, 11931, 6029,13917,19858, 2496, 10686, 13932,  2106, 22186, 9735, NA, NA, NA, NA))
# Setters.stocked <- c()
# Juvenile <- as.numeric(c(626,75,1503,670,52,34,124,192,1311,1091,501,834,246,113,356,334, 31,275,4,96))
# Survival.summ <- cbind.data.frame(Pops,Trts,Stocked,Stocked.adj,Setters,Juvenile)
# Survival.summ$Temp <- Survival.summ$Trts
# Survival.summ$pH <- Survival.summ$Trts
# Survival.summ$Temp <- factor(sub(" Ambient| Low", "", Survival.summ$Trts))
# Survival.summ$pH <- factor(sub("10 |6 ", "", Survival.summ$pH))

# Assess survival from stocking to near metamorphosis 224um ("setters"): 

Survival$Treatment <- gsub("10C, Ambient pH", "Unchilled\nAmbient pH", Survival$Treatment)
Survival$Treatment <- gsub("10C, Low pH", "Unchilled\nLow pH", Survival$Treatment)
Survival$Treatment <- gsub("6C, Ambient pH", "Chilled\nAmbient pH", Survival$Treatment)
Survival$Treatment <- gsub("6C, Low pH", "Chilled\nLow pH", Survival$Treatment)
Survival$Treatment <- as.factor(Survival$Treatment)
Survival$survival.setters <- Survival$Setters.stocked / Survival$Larvae.stocked.adjusted
Survival$survival.postset <- Survival$Post.set / Survival$Setters.stocked

par(mar = c(5,4,4,2)) ## default is c(5,4,4,2) + 0.1
# Plot survival to 224um by pH treatment  (all populations combined) 
palette(c("skyblue3", "seagreen3", "orange1", "indianred2"))
plot(Survival$Treatment, 100*(Survival$survival.setters), main="Percent Survival\nveliger to eyed larvae (224um)", size=I(4), xlab="Treatment", ylab="Percent Survival", col=Survival$Treatment, cex.lab=1.3, cex.main=1.5, cex.axis=1.3)

hist(subset(Survival, survival.setters != "NA")$survival.setters)
hist(sqrt(subset(Survival, survival.setters != "NA")$survival.setters))
hist(log(subset(Survival, survival.setters != "NA")$survival.setters))
hist(asin(sqrt(subset(Survival, survival.setters != "NA")$survival.setters)))
shapiro.test(asin(sqrt(subset(Survival, survival.setters != "NA")$survival.setters))) 
shapiro.test(log(subset(Survival, survival.setters != "NA")$survival.setters))
shapiro.test(log(subset(Survival, survival.setters != "NA")$survival.setters))

par(pch=19, cex=1.2)
plot(x=Survival$Larvae.stocked.adjusted, y=log(Survival$survival.setters), col=Survival$pH)
test <- lm(log(survival.setters) ~ Larvae.stocked.adjusted + factor(pH), data=Survival)
abline(test)
CI <- predict.lm(object=test, interval="prediction")
lines(x=Survival$Larvae.stocked.adjusted, y=CI[,3], lty=2)
lines(x=Survival$Larvae.stocked.adjusted, y=CI[,2], lty=2)
summary(test)
anova(test)


plot(x, y, col="red", pch=19)   #plot the points
lmfit <- lm(y~x)                 #find the best fit using a linear model lm, store result in object lmfit
abline(lmfit)                    #use the linear model object as input to abline
CIs <- predict.lm(object=lmfit, interval="prediction") #find the 95% confidence intervals
print(CIs[1:10,])                #show fittled y values for 1:10 
lines(x=x,y=CIs[,3], lty=2)      #plot the upper CI with dashed lines (lty=2)     
lines(x=x,y=CIs[,2], lty=2)      #plot the lower CI with dashed lines (lty=2)


# try plotting % setter survival ~ % larvae survival -- need to finish (add legend, etc.)
plot(x=Survival$survival.setters, y=Survival$survival.postset, col=c("skyblue3","orange1")[as.numeric(Survival$Temperature)], pch=c(15, 17)[as.numeric(Survival$pH)], cex=1.5)

# Try doing a survival curve  instead 
---- 

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


# How does survival to 224um differ between pH, within temperature groups, using GLM?

glm.setters <- glm(cbind(Setters.stocked, I(Larvae.stocked.adjusted-Setters.stocked)) ~ pH, data=subset(Survival, Temperature==10), binomial) 
summary(glm.setters)
anodev.fn(glm.setters, test.in="Chi")
anova(glm.setters, test="Chi")
anova(glm.setters,update(glm.setters,~.-pH),test="Chisq") 

# 6C
glm.setters.6 <- glm(cbind(Setters.stocked, I(Larvae.stocked.adjusted-Setters.stocked)) ~ pH, data=subset(Survival, Temperature==6), binomial) 
summary(glm.setters.6)
39567/29654 # null/residual deviance = 1.3
anodev.fn(glm.setters.6, test.in = "Chi") #significant ? 
anova(glm.setters.6, test = "Chi") #significant ? 
anova(glm.setters.6,update(glm.setters.6,~.-pH),test="Chisq") 

# 10C
glm.setters.10 <- glm(cbind(Setters.stocked, I(Larvae.stocked.adjusted-Setters.stocked)) ~ pH, data=subset(Survival, Temperature==10), binomial) 
summary(glm.setters.10)
44630/36946 # null/residual deviance - 1.2
anodev.fn(glm.setters.10, test.in = "Chi") #significant ? 
anova(glm.setters.10, test="Chi")

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

#### Compare survival from larvae stocked -> post set (Post.set) between pH treatments 
plot(Survival$Treatment, log(100*(Survival$Post.set / Survival$Larvae.stocked.adjusted)), main="Percent Survival, larvae -> Post-set", size=I(4), xlab="Treatment", ylab="Log Percent Survival", legend=c(0.2, 0.75), col=c("orange1", "indianred2", "skyblue3", "seagreen3"))

# Stocking density definitely had an effect, but it was mixed amongst pH treatments. 
plot(x=Survival$Larvae.stocked.adjusted, y=log(Survival$Post.set / Survival$Larvae.stocked.adjusted), col=Survival$pH, xlab="Total Larvae Stocked", ylab="Survival to Post Set") #using total stocked, looks like that has an effect
legend(x="topright", legend = c("Ambient", "Low"), col=Survival$pH, pch=1)

# Just 6C groups 
plot(x=subset(Survival, Temperature==6)$Larvae.stocked.adjusted, y=log(subset(Survival, Temperature==6)$Post.set / subset(Survival, Temperature==6)$Larvae.stocked.adjusted), col=subset(Survival, Temperature==6)$pH, xlab="Total Larvae Stocked", ylab="Survival to Post Set") #using total stocked, looks like that has an effect

# Just 10C groups 
plot(x=subset(Survival, Temperature==10)$Larvae.stocked.adjusted, y=log(subset(Survival, Temperature==10)$Post.set / subset(Survival, Temperature==10)$Larvae.stocked.adjusted), col=subset(Survival, Temperature==10)$pH, xlab="Total Larvae Stocked", ylab="Survival to Post Set") #using total stocked, looks like that has an effect

# Can I do glm here? Which distribution? 
summary(glm((subset(Survival, Temperature==6)$Stocked) ~ pH, data=subset(Survival, Temperature==6))) 
summary(glm((subset(Survival, Temperature==10)$Stocked) ~ pH, data=subset(Survival, Temperature==10))) 

## Compare larvae -> post set survival 
glm.survival.6 <- glm(cbind(Post.set, Larvae.stocked.adjusted) ~ pH, data=subset(Survival, Temperature==6), binomial)
summary(glm.survival.6)
anodev.fn(glm.survival.6, test.in = "Chi") # Yes different 

glm.survival.10 <- glm(cbind(Post.set, Larvae.stocked.adjusted) ~ pH, data=subset(Survival, Temperature==10), binomial)
summary(glm.survival.10)
anodev.fn(glm.survival.10, test.in = "Chi") # Yes, significantly different ... ?



# ----- extra plots and stuff 

# boxplot of just 6C group % survival from new larvae -> 224um 

ggplot(subset(Survival, Temperature==6), aes(x=pH, y=100*(Setters.stocked/Larvae.stocked.adjusted), fill=pH)) +
  geom_boxplot() +
  labs(title="Percent Survival\nNew Larvae -> Eyed Larvae",y=expression("Percent Survival",x=expression("Parental pH Treatment"))) +
  theme_bw(base_size = 16) + 
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=26, face="bold"), legend.text = element_text(size=20)) + scale_fill_manual(values=c("skyblue3", "seagreen3")) + guides(fill=guide_legend(title="Parental pH"))