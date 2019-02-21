# Oyster length 

broodstock.length <- read.csv("Data/broodstock-length-data.csv", header=T, na.strings = NA)
broodstock.length$Length <- as.numeric(as.character(broodstock.length$Length))
broodstock.length$Temperature <- as.factor(as.character(broodstock.length$Temperature))

plot(x=broodstock.length$Population, y=broodstock.length$Length)

ggplot(broodstock.length, aes(x=Population, y=Length, col=pH)) + geom_boxplot() 
ggplot(broodstock.length, aes(x=Population, y=Length, fill=Temperature)) + geom_boxplot() 
ggplot(broodstock.length, aes(x=Population, y=Length, fill=pH)) + geom_boxplot() 
ggplot(broodstock.length, aes(x=Population, y=Weight, fill=pH)) + geom_boxplot() 
ggplot(broodstock.length, aes(x=Population, y=Length, fill=Population)) + geom_boxplot() 

qqnorm(broodstock.length$Length)
hist(broodstock.length$Length)
summary(aov(Length ~ Population*Temperature*pH, data=broodstock.length))
summary(aov(Length ~ Population*pH, data=broodstock.length))
TukeyHSD(aov(Length ~ Population*pH, data=broodstock.length))
summary(aov(Length ~ Population*pH, data=subset(broodstock.length, pH!="PRE")))
summary(aov(Length ~ Population*Temperature*pH, data=subset(broodstock.length, pH!="PRE")))

shapiro.test(subset(broodstock.length, Population=="NF")$Length)
shapiro.test(subset(broodstock.length, Population=="HL")$Length)
shapiro.test(subset(broodstock.length, Population=="SN")$Length)
shapiro.test(subset(broodstock.length, Population=="K")$Length)

# Temp or Pop diff before pH treatmemt?
summary(aov(Length ~ Population*Temperature, data=subset(broodstock.length, pH=="Pre-pH")))
summary(aov(Length ~ Population, data=subset(broodstock.length, pH=="Pre-pH")))
TukeyHSD(aov(Length ~ Population, data=subset(broodstock.length, pH=="Pre-pH"))) # NF > HL only 

# Temp or Pop diff after pH treatmemt?
summary(aov(Length ~ Population*pH, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K")))
TukeyHSD(aov(Length ~ Population*pH, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K")))
summary(aov(Length ~ Population, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K")))
TukeyHSD(aov(Length ~ Population, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K"))) # NF & SN > HL  

# Grow in ambient pH? 
anova(lm(Length ~ Population/pH, data=subset(broodstock.length, pH!="Low"))) # Yes - pH factor within pop = 0.002531 **
anova(lm(Length ~ Population*pH, data=subset(broodstock.length, pH!="Low"))) # Yes - pH factor within pop = 0.005035 **
aggregate(Length ~ Population + pH, data=subset(broodstock.length, pH!="Low"), mean) # mean lengths 

# Grow in low pH? 
anova(aov(Length ~ Population/pH, data=subset(broodstock.length, pH!="Ambient"))) # Tended to - pH factor withi pop = 0.06189
anova(aov(Length ~ Population*pH, data=subset(broodstock.length, pH!="Ambient"))) # Tended to - pH factor withi pop = 0.0338
aggregate(Length ~ Population+pH, data=subset(broodstock.length, pH!="Ambient"), mean) # mean lengths 

aggregate(Length ~ pH, data=subset(broodstock.length, Population!="K"), mean) # mean lengths for F1 oysters 
aggregate(Length ~ pH, data=subset(broodstock.length, Population=="K"), mean) # mean lengths for OB-F2 oysters (weird- shrank?)
  


shapiro.test(subset(broodstock.length, pH!="Low" & Population=="NF")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="NF"))) #YES, but after p-adj, no
shapiro.test(subset(broodstock.length, pH!="Low" & Population=="HL")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="HL"))) #NO 
shapiro.test(subset(broodstock.length, pH!="Low" & Population=="SN")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="SN"))) #YES
0.00297*4 #p-adj
shapiro.test(subset(broodstock.length, pH!="Low" & Population=="K")$Length) #NO
kruskal.test(x = subset(broodstock.length, pH!="Pre-pH" & Population=="K")$Length, g = subset(broodstock.length, pH!="Ambient" & Population=="K")$Length) #NO

# Grow in LOW pH? 
shapiro.test(subset(broodstock.length, pH!="Ambient" & Population=="NF")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="NF"))) #NO 
shapiro.test(subset(broodstock.length, pH!="Ambient" & Population=="HL")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="HL"))) #NO 
shapiro.test(subset(broodstock.length, pH!="Ambient" & Population=="SN")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="SN"))) #NO
shapiro.test(subset(broodstock.length, pH!="Ambient" & Population=="K")$Length) #OK
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="K"))) #YES, but after p-adj, no


aggregate(Length ~ Population + pH + Temperature, broodstock.length, mean)

broodstock.length$pH <- factor(broodstock.length$pH, levels = c("Pre-pH",  "Low", "Ambient"))
broodstock.length$Population <- factor(broodstock.length$Population, levels = c("K", "HL", "NF", "SN"))
broodstock.length$Temperature <- factor(broodstock.length$Temperature, levels = c(6, 10))
brood.col.lengths <- c("gray50","lightsteelblue", "gray90")[as.numeric(broodstock.length$pH)]
names(brood.col.lengths) <-broodstock.length$pH

pdf(file="Results/broodstock-lengths-6.pdf", width=6.5, height = 3)
# BOXPLOT PLOT, survival by population and bay pH
ggplot(subset(broodstock.length, Temperature==6), aes(x=Population, y= Length, fill=pH)) +  geom_boxplot() + 
  labs(title="Shell length by population, pH - 6°C",y=expression("Shell length (mm)")) + 
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, colour = "gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=brood.col.lengths, name="pH treatment")+ scale_x_discrete(labels = c('Oyster Bay C2','Dabob Bay','Fidalgo Bay', "Oyster Bay C1")) + scale_y_continuous(limits=c(min=min(broodstock.length$Length),max=max(broodstock.length$Length))) 
dev.off()

pdf(file="Results/broodstock-lengths-10.pdf", width=6.5, height = 3)
ggplot(subset(broodstock.length, Temperature==10), aes(x=Population, y= Length, fill=pH)) +  geom_boxplot() + 
  labs(title="Shell length by population, pH - 10°C",y=expression("Shell length (mm)")) + 
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, colour = "gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank()) +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=brood.col.lengths, name="pH treatment")+ scale_x_discrete(labels = c('Oyster Bay C2','Dabob Bay','Fidalgo Bay', "Oyster Bay C1")) + scale_y_continuous(limits=c(min=min(broodstock.length$Length),max=max(broodstock.length$Length))) 
dev.off()


pdf(file="Results/broodstock-lengths.pdf", width=5.5, height = 4.5)
ggplot(broodstock.length, aes(x=Population, y= Length, fill=pH)) +  geom_boxplot() + 
  labs(title="Shell length by population, pH",y=expression("Shell length (mm)")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, colour = "gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), legend.position="bottom") +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=brood.col.lengths, name="pH treatment")+ scale_x_discrete(labels = c('Oyster Bay C2','Dabob Bay','Fidalgo Bay', "Oyster Bay C1")) + scale_y_continuous(limits=c(min=min(broodstock.length$Length),max=max(broodstock.length$Length)))
dev.off()
