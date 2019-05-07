## Larval release plots and statistics, adapted from Katherine Silliman R code 

rm(list=ls())         #start script by deleting all objects - clean slate 
str(larvae)
# Import data, convert groups to date, numeric and factors 
larvae <- read.csv("Data/Spawning-Data.csv", header = TRUE, na.strings = "n/a", stringsAsFactors = F)
larvae$Date <- as.Date(larvae$Date, "%m/%d/%y")
larvae[c("Vol.counted", "Vol.total", "CountA", "CountB", "CountC", "Larvae.mL", "Tot.Larvae")] <- as.numeric(as.character(unlist(larvae[c("Vol.counted", "Vol.total", "CountA", "CountB", "CountC", "Larvae.mL", "Tot.Larvae")])))
larvae[c("Group", "Spawning.Group", "Population", "Treatment", "Temperature", "pH", "Bucket")] <- lapply(larvae[c("Group", "Spawning.Group", "Population", "Treatment", "Temperature", "pH", "Bucket")], factor)
larvae <- arrange(larvae, Date) %>% mutate(CalDay = format(Date,"%j"))  # Add column with calendar day 

# Re-calculate total larvae released to confirm spreadsheet accuracy  
larvae$total.released <- round(((rowMeans(larvae[,c("CountA", "CountB", "CountC")], na.rm = TRUE, dims = 1))/larvae$Vol.counted)*larvae$Vol.total, digits = -1)
print(larvae$error <- larvae$total.released - larvae$Tot.Larvae) #looks good, just rounding error

# --- Normalize # larvae produced by the number of broodstock per spawning group (perbrood). # oysters: 
# NF-10 Ambient A = 15 
# NF-10 Ambient B = 14 
# NF-10 Low A = 14 
# NF-10 Low B = 14 
# NF-6 Ambient A = 15 
# NF-6 Ambient B = 14 
# NF-6 Low A = 14 
# NF-6 Low B = 15 
# SN-10 Ambient A = 17 
# SN-10 Ambient B = 17 
# SN-10 Low A = 15 
# SN-10 Low B = 15 
# SN-6 Ambient A = 15 
# SN-6 Ambient B = 16 
# SN-6 Low A = 17 
# SN-6 Low B = 17 
# HL-10 Ambient = 9 
# HL-10 Low = 16 
# HL-6 Ambient = 14 
# HL-6 Low = 15 
# K-10 Ambient = 115  (K groups were SN-F2's produced by Katherine, younger/smaller and likely inbred)
# K-10 Low = 111  
# K-6 Ambient" = 117
# K-6 Low" = 126

# add number of broodstock in each spawning group for normalization 
larvae$broodstock <- larvae$Spawning.Group
larvae$broodstock <- gsub("NF-10 Ambient A|NF-6 Ambient A|NF-6 Low B|SN-10 Low A|SN-10 Low B|SN-6 Ambient A|HL-6 Low", 15, larvae$broodstock)
larvae$broodstock <- gsub("NF-10 Ambient B|NF-10 Low A|NF-10 Low B|NF-6 Ambient B|NF-6 Low A|HL-6 Ambient", 14, larvae$broodstock)
larvae$broodstock <- gsub("SN-10 Ambient A|SN-10 Ambient B|SN-6 Low A|SN-6 low A|SN-6 Low B", 17, larvae$broodstock)
larvae$broodstock <- gsub("HL-10 Ambient", 9, larvae$broodstock)
larvae$broodstock <- gsub("SN-6 Ambient B|HL-10 Low", 16, larvae$broodstock)
larvae$broodstock <- gsub("K-10 Ambient", 115, larvae$broodstock)
larvae$broodstock <- gsub("K-10 Low", 111, larvae$broodstock)
larvae$broodstock <- gsub("K-6 Ambient", 117, larvae$broodstock)
larvae$broodstock <- gsub("K-6 Low", 126, larvae$broodstock)
larvae$broodstock <- as.numeric(larvae$broodstock)

#Add mean length for each population for normalization 
larvae$mean.length <- larvae$Population
larvae$mean.length <- gsub("NF|SN", 3.6, larvae$mean.length)
larvae$mean.length <- gsub("HL", 3.0, larvae$mean.length)
larvae$mean.length <- gsub("K", 2.2, larvae$mean.length)
larvae$mean.length <- as.numeric(larvae$mean.length)

# Normalize # larvae spawned daily by #broodstock*mean length - result is # larvae per broodstock centimeter 
larvae$larvae.per.broodcm <- larvae$total.released/(larvae$broodstock*larvae$mean.length)

# Summary statistics 
summarise(larvae, total.by.date = sum(total.released))                                              #total larvae released 
summarise(larvae, total.by.date = mean(total.released))                                              #mean larvae released 
max(larvae$Tot.Larvae, na.rm=T)
aggregate(Tot.Larvae ~ pH + Temperature, larvae, sum, na.rm=TRUE)                                   #By pH & Temperature
aggregate(Tot.Larvae ~ pH + Temperature + Spawning.Group, larvae, sum, na.rm=TRUE)                      #By population
aggregate(Tot.Larvae ~ Spawning.Group, larvae, sum, na.rm=TRUE)                                     #By population & treatment
aggregate(Tot.Larvae ~ pH + Temperature, larvae, mean, na.rm=TRUE)                                     #Overal daily mean release by population & treatment
aggregate(Tot.Larvae ~ pH + Temperature, larvae, sd, na.rm=TRUE)                                     #Overal daily sd release by population & treatment
aggregate(larvae.per.broodcm ~  pH + Temperature, larvae, sum, na.rm=TRUE)                             #total larvae released by # broodstock
aggregate(larvae.per.broodcm ~ pH + Temperature + Spawning.Group, larvae, sum, na.rm=TRUE)                 #By population & treatment
nrow(subset(larvae, total.released >= 10000))                                                       #Number of times >10k larvae were collected (all grps)
sum(aggregate(broodstock ~ Spawning.Group, subset(larvae), mean)$broodstock)

# ---- Create larval release plots with daily release events (bars), and cumulative release lines 

#Calculate cumulative larvae released through time for each pH/temp treatment (combine replicates)
all_total <- group_by(larvae, Treatment) %>% dplyr::mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% dplyr::arrange(Date) %>% dplyr::select(Date,CalDay,Treatment,total.released,larvae.per.broodcm,cum.total,cum.percap)

# Rename treatments for plots 
all_total$Treatment <- gsub("10-Ambient", "Unchilled, Ambient pH", all_total$Treatment)
all_total$Treatment <- gsub("6-Ambient", "Chilled, Ambient pH", all_total$Treatment)
all_total$Treatment <- gsub("10-Low", "Unchilled, Low pH", all_total$Treatment)
all_total$Treatment <- gsub("6-Low", "Chilled, Low pH", all_total$Treatment)
all_total$Treatment <- as.factor(all_total$Treatment)

temp <- aggregate(broodstock ~ Spawning.Group+pH, larvae, mean)
aggregate(broodstock ~ pH, temp, sum)
sum(temp$broodstock)
8.03e6/395

# create bar plot of daily larval release over time by treatment 
all.release.bars <- ggplot(data=all_total, aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Daily Larvae Released") + xlab("# Days in reproductive conditioning/spawning temperature (18C)") +
  ggtitle("Olympia oyster larval release by pH and temperature treatment") + theme_bw(base_size = 20) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"), panel.border=element_blank(), axis.line=element_line(), panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank(), legend.position = c(0.15, 0.85)) + scale_x_date(date_breaks = "1 week", date_labels = c( "93", "30", "37", "44", "51", "58", "65", "72", "79", "86")) + scale_fill_manual(values=c("gray60", "lightsteelblue3", "gray40", "steelblue"))

# add line plot of cumulative larval release by treatment to the above barplot 
All.release <- all.release.bars + geom_line(data=all_total, aes(x=Date, y=cum.total/6, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("gray60", "lightsteelblue3", "gray40", "steelblue")) +
  scale_y_continuous(sec.axis = sec_axis(label=,~.*6,name="Cumulative Larvae Released"))

jpeg(file="Results/Larval-release-plot.jpeg", width = 1000, height=650)
All.release #call plots 
dev.off()

# ----- Statistics 

# Assess data distributions 

# summarize data for each spawning bucket 
spawning_group_total <- group_by(larvae, Spawning.Group, Population, Treatment, pH, Temperature) %>% mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% arrange(Date) %>% dplyr::select(Date,CalDay,Spawning.Group,Population,Treatment,pH,Temperature,total.released,larvae.per.broodcm,cum.total,cum.percap)

# Summarize data for each treatment treatment, pulling out key dates and summing/averaging larvae 

spawning_group_sum <- spawning_group_total %>% group_by(Population, pH, Temperature, Spawning.Group) %>% dplyr::summarize(overall_Total = sum(total.released, na.rm = T), mean.larvae = mean(total.released,na.rm=T), se.larvae = std.error(total.released,na.rm=T), mean.percap = mean(larvae.per.broodcm,na.rm=T), total.percap = sum(larvae.per.broodcm,na.rm=T), maxday = as.numeric(CalDay[which.max(total.released)]), max = max(total.released), max.percap = max(larvae.per.broodcm), first.big = as.numeric(CalDay[which(total.released > 10000)[1]]), release.days = as.numeric(length(CalDay[total.released > 10000])))

mean(subset(spawning_group_sum, Temperature==10 & pH=="Ambient")$maxday)
mean(subset(spawning_group_sum, Temperature==10 & pH=="Low")$maxday)

metrics <- list("total.released"=spawning_group_total$total.released, 
                "overall_Total"=spawning_group_sum$overall_Total, 
                "total.percap"=spawning_group_sum$total.percap, 
                "mean.larvae"=spawning_group_sum$mean.larvae, 
                "cum.total"=spawning_group_total$cum.total, 
                "cum.percap"=spawning_group_total$cum.percap, 
                "first.big"=spawning_group_sum$first.big, 
                "max"=spawning_group_sum$max, 
                "maxday"=spawning_group_sum$maxday, 
                "total.bigdays"=spawning_group_sum$release.days)

# Assess data distributions for metrics (normal, lognormal, and poisson)
for(i in 1:length(metrics)) {
  par(mfrow=c(2,2))
  qqp(metrics[[i]], "norm", main=paste(names(metrics[i]), "-normal"))
  qqp(metrics[[i]], "lnorm", main=paste(names(metrics[i]), "-lognormal")) 
}

# Best fit distributions: 
# spawning_group_total$total.released = daily larvae released/collected (not normalized)                <-- LOG NORMAL 
# spawning_group_sum$overall_Total = sum of larvae released in each bucket                              <-- NORMAL
# spawning_group_sum$total.percap = sum of larvae released in each bucket, normalized by broodstock cm  <-- NORMAL 
# spawning_group_sum$mean.larvae = average # larvae per release event                                   <-- NORMAL 
# spawning_group_total$cum.total = running total # larvae = cum.total                                   <-- LOG NORMAL 
# spawning_group_total$cum.percap = running total # larvae per broodstock cm                            <-- LOG NORMAL 
# spawning_group_sum$first.big = First big larval release (>10k)                                        <-- NORMAL OK
# spawning_group_sum$max = max larvae collected in one day for each spawning bucket                     <-- NORMAL OK
# spawning_group_sum$maxday = date that max larvae were collected                                       <-- NORMAL OK
# spawning_group_sum$release.days = number of days >10k larvae collected                                <-- NORMAL OK

# Compare raw daily collection data 
summary(test1 <- aov(log(total.released+1) ~ Population*Temperature*pH, data=spawning_group_total)) # <-- daily release data NO DIFF 
summary(test2 <- aov(log(total.released+1) ~ Population*Temperature, data=spawning_group_total)) # <-- daily release data NO DIFF 
summary(test3 <- aov(log(total.released+1) ~ Population, data=spawning_group_total)) # <-- daily release data NO DIFF 
anova(test2, test3) #no diff. Use simplest = Population only 
TukeyHSD(total.released.aov <- aov(log(total.released+1) ~ Population, data=spawning_group_total)) # Diff by Pop

# Total larval release, normalized by #brood-cm
anova(test1 <- aov(total.percap ~ Population*pH*Temperature, data=spawning_group_sum)) # <-- Pop
TukeyHSD(test1 <- aov(total.percap ~ Population*pH*Temperature, data=spawning_group_sum)) # <-- Pop
anova(test1 <- aov(total.percap ~ Population*pH*Temperature, data=subset(spawning_group_sum, Population!="K"))) # <-- Pop
TukeyHSD(test1 <- aov(total.percap ~ Population*pH*Temperature, data=subset(spawning_group_sum, Population!="K"))) # <-- Pop


# summary(test2 <- aov(total.percap ~ Population*pH*Temperature - (pH + Population:pH + Population:Temperature), data=spawning_group_sum)) 
# summary(test3 <- aov(total.percap ~ Population + Temperature + pH:Temperature, data=spawning_group_sum)) 
# summary(test4 <- aov(total.percap ~ Population  + pH:Temperature, data=spawning_group_sum)) 
# summary(test5 <- aov(total.percap ~ Population, data=spawning_group_sum)) 
# summary(test6 <- aov(total.percap ~ Population+pH + pH/Temperature, data=spawning_group_sum)) 
# AIC(test1, test2, test3, test4, test5, test6, test7)
# anova(test1, test5) #use simplest, just population 
# TukeyHSD(aov(total.percap ~ Population, data=spawning_group_sum)) # <-- Population differences
# summary(aov(total.percap ~ Population, data=subset(spawning_group_sum, Population!="K"))) #still there
# TukeyHSD(aov(total.percap ~ Population, data=subset(spawning_group_sum, Population!="K"))) #

aggregate(total.percap ~ Population, spawning_group_sum, sum)
aggregate(overall_Total ~ Population, spawning_group_sum, sum)

# Test cumulative larvae within OB & NF/HL groups: 
spawning_group_sum$Groups <- spawning_group_sum$Population
spawning_group_sum$Groups <- gsub("HL|NF", "F&D", spawning_group_sum$Groups)
spawning_group_sum$Groups <- gsub("SN|K", "OB", spawning_group_sum$Groups)
spawning_group_sum$Groups <- as.factor(spawning_group_sum$Groups)

summary(test1 <- aov(total.percap ~ pH*Groups, data=subset(spawning_group_sum, Temperature==6))) 
summary(test1 <- aov(total.percap ~ pH*Groups, data=subset(spawning_group_sum, Temperature==10))) 

# mean larvae per broodstock
summary(test1 <- aov(mean.percap ~Population*pH*Temperature, data=spawning_group_sum))
TukeyHSD(test1 <- aov(mean.percap ~Population*pH*Temperature, data=spawning_group_sum))

summary(test1 <- aov(mean.percap ~Population + pH:Temperature, data=spawning_group_sum))
TukeyHSD(test1 <- aov(mean.percap ~Population + pH:Temperature, data=spawning_group_sum))

summary(test1 <- aov(mean.percap ~Population*pH*Temperature, data=subset(spawning_group_sum, Population!="K")))
TukeyHSD(test1 <- aov(mean.percap ~Population*pH*Temperature, data=subset(spawning_group_sum, Population!="K")))


# summary(test2 <- aov(mean.percap ~Population*pH*Temperature - pH - Population:pH -Population:Temperature, data=spawning_group_sum))
# summary(test3 <- aov(mean.percap ~Population+Temperature+pH:Temperature, data=spawning_group_sum))
# summary(test4 <- aov(mean.percap ~Population+pH:Temperature, data=spawning_group_sum))
# summary(test5 <- aov(mean.percap ~Population, data=spawning_group_sum))
# AIC(test1, test2, test3, test4, test5)
# anova(test5, test4) # no diff, just population 
# TukeyHSD(aov(mean.percap ~ Population, data=spawning_group_sum)) #all differences attributed to K. 

summary(aov(mean.percap ~ Population, data=subset(spawning_group_sum, Population!="K"))) # without K, not sign. 

# remake models without K
summary(test1 <- aov(mean.percap ~Population*pH*Temperature, data=subset(spawning_group_sum, Population!="K")))
summary(test2 <- aov(mean.percap ~Population*pH*Temperature - pH - Population:pH -Population:Temperature, data=subset(spawning_group_sum, Population!="K")))
summary(test3 <- aov(mean.percap ~Population+Temperature+pH:Temperature, data=subset(spawning_group_sum, Population!="K")))
summary(test4 <- aov(mean.percap ~Population+pH:Temperature, data=subset(spawning_group_sum, Population!="K")))
summary(test5 <- aov(mean.percap ~Population, data=subset(spawning_group_sum, Population!="K")))
AIC(test1, test2, test3, test4, test5)
anova(test4, test5) # without K, differences 
TukeyHSD(test4)
summary(test4)

# running cumulative larvae, which incorporates timing of release 
summary(test1 <- aov(log(cum.total+1) ~ Population*Temperature*pH, data=spawning_group_total))
summary(test2 <- aov(log(cum.total+1) ~ Population*Temperature*pH-pH-Temperature-pH:Temperature-pH:Population, data=spawning_group_total))
summary(test3 <- aov(log(cum.total+1) ~ Population*Temperature, data=spawning_group_total))
summary(test4 <- aov(log(cum.total+1) ~ Population + Population:Temperature, data=spawning_group_total))
summary(test5 <- aov(log(cum.total+1) ~ Population, data=spawning_group_total))
AIC(test1, test2, test3, test4, test5)
anova(test4, test5) #yes diff, test4 better (smaller residual sum sq)
anova(test4, test1) 
TukeyHSD(test4)
summary(aov(log(cum.total+1) ~ Population+Population:Temperature, data=subset(spawning_group_total, Population!="K")))
TukeyHSD(aov(log(cum.total+1) ~ Population+Population:Temperature, data=subset(spawning_group_total, Population!="K")))

# running cumulative larvae per broodstock*length, which incorporates timing of release 
summary(test1 <- aov(log(cum.percap+1) ~ Population*Temperature*pH, data=spawning_group_total))
TukeyHSD(test1 <- aov(log(cum.percap+1) ~ Population*Temperature*pH, data=spawning_group_total))


summary(test2 <- aov(log(cum.percap+1) ~ Population*Temperature, data=spawning_group_total))
summary(test3 <- aov(log(cum.percap+1) ~ Population + Population:Temperature, data=spawning_group_total))
summary(test4 <- aov(log(cum.percap+1) ~ Population, data=spawning_group_total))
AIC(test1, test2, test3, test4)
anova(test3, test4) #yes diff, test3 better (smaller residual sum sq)
TukeyHSD(test3)

# Timing- first big release (>10k)
summary(aov(first.big ~ Population*Temperature*pH, data=spawning_group_sum))
TukeyHSD(aov(first.big ~ Population*Temperature*pH, data=spawning_group_sum))$`Population:pH`

summary(aov(first.big ~ Temperature, data=spawning_group_sum))


aggregate(first.big ~ Groups, data=spawning_group_sum, mean)
145.8-135.9

# summary(aov(first.big ~ Population*Temperature, data=spawning_group_sum))
# summary(aov(first.big ~ Population+Temperature, data=spawning_group_sum))
# summary(aov(first.big ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# TukeyHSD(aov(first.big ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# summary(aov(first.big ~ Population*pH, data=subset(spawning_group_sum, Temperature==10)))

# Day of maximum release
summary(test1 <- aov(maxday ~ Population*Temperature*pH, data=spawning_group_sum)) 
summary(test2 <- aov(maxday ~ Temperature*pH, data=spawning_group_sum)) 
summary(test3 <- aov(maxday ~ Population*pH, data=spawning_group_sum)) 
summary(test4 <- aov(maxday ~ Temperature, data=spawning_group_sum)) 
summary(test5 <- aov(maxday ~ pH, data=spawning_group_sum)) 
AIC(test1, test2, test3, test4, test5)
anova(test1,test4) #simplest best 
TukeyHSD(test4)


# Max release
summary(test1 <- aov(max ~ Population*Temperature*pH, data=spawning_group_sum)) # 
summary(test1 <- aov(max ~ Population*Temperature*pH, data=subset(spawning_group_sum, Population!="K"))) # 
TukeyHSD(test1 <- aov(max ~ Population*Temperature*pH, data=spawning_group_sum)) # 
aggregate(max ~ Population, data=spawning_group_sum, mean)
aggregate(max ~ Population, data=spawning_group_sum, sd)

# summary(test2 <- aov(max ~ Population*pH, data=spawning_group_sum)) # 
# summary(test3 <- aov(max ~ Population*Temperature, data=spawning_group_sum)) #
# summary(test4 <- aov(max ~ Population, data=spawning_group_sum)) # 
# TukeyHSD(test4) #SN > NF, K

# No. release days 
summary(test1 <- aov(release.days ~ Population*Temperature*pH, data=subset(spawning_group_sum, Population!="K")))
TukeyHSD(test1 <- aov(release.days ~ Population*Temperature*pH, data=spawning_group_sum))
TukeyHSD(test1 <- aov(release.days ~ Population*Temperature*pH, data=subset(spawning_group_sum, Population!="K")))
aggregate(release.days ~ Population, spawning_group_sum, sd)

# summary(test2 <- aov(release.days ~ Population*pH, data=spawning_group_sum))
# summary(test3 <- aov(release.days ~ Temperature*pH, data=spawning_group_sum))
# summary(test4 <- aov(release.days ~ Population:Temperature:pH, data=spawning_group_sum))
# summary(test5 <- aov(release.days ~  Population:Temperature:pH, data=spawning_group_sum))
# summary(test6 <- aov(release.days ~ Population+Temperature, data=spawning_group_sum))
# summary(test7 <- aov(release.days ~ Population, data=spawning_group_sum))
# summary(test8 <- aov(release.days ~ Temperature, data=spawning_group_sum))
# AIC(test1, test2, test3, test4, test5, test6, test7, test8) #test6
# anova(test1, test6) #use simplest 
# TukeyHSD(test6)
# 
# summary(aov(release.days ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# summary(aov(release.days ~ Population*pH, data=subset(spawning_group_sum, Temperature==10)))
# summary(aov(maxday ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# summary(aov(maxday ~ Population*pH, data=subset(spawning_group_sum, Temperature==10)))
# summary(aov(max ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# summary(aov(max ~ Population*pH, data=subset(spawning_group_sum, Temperature==10)))
# summary(aov(first.big ~ Population*pH, data=subset(spawning_group_sum, Temperature==6)))
# summary(aov(first.big ~ Population*pH, data=subset(spawning_group_sum, Temperature==10)))

# What about just within 6 groups?
summary(test1 <- aov(total.percap ~ pH, data=subset(spawning_group_sum,Temperature==6))) 
summary(test1 <- aov(mean.percap ~pH, data=subset(spawning_group_sum,Temperature==6)))
summary(test1 <- aov(first.big ~pH, data=subset(spawning_group_sum,Temperature==6))) 
TukeyHSD(test1 <- aov(first.big ~pH, data=subset(spawning_group_sum,Temperature==6)))
summary(test1 <- aov(maxday ~pH, data=subset(spawning_group_sum,Temperature==6)))
summary(test1 <- aov(max ~pH, data=subset(spawning_group_sum,Temperature==6)))
summary(test1 <- aov(release.days ~pH, data=subset(spawning_group_sum,Temperature==6)))


test <- aggregate(total.percap ~ Temperature+Population+pH, data=spawning_group_sum, mean)

aggregate(total.percap ~ Temperature+pH, data=test, mean)

aggregate(max ~ Population, data=spawning_group_sum, mean)
aggregate(max ~ Population, data=spawning_group_sum, sd)
aggregate(maxday ~ Temperature, data=spawning_group_sum, mean)
aggregate(maxday ~ Temperature, data=spawning_group_sum, sd)
aggregate(first.big ~ Temperature, data=spawning_group_sum, mean)
aggregate(first.big ~ Temperature, data=spawning_group_sum, sd)
138.2500-143.4167

mean(subset(spawning_group_sum, Population=="K" | Population=="SN")$release.days)
sd(subset(spawning_group_sum, Population=="K" | Population=="SN")$release.days)

mean(subset(spawning_group_sum, Population=="NF" | Population=="HL")$release.days)
sd(subset(spawning_group_sum, Population=="NF" | Population=="HL")$release.days)

# Post-hoc tests on sign. different metrics via Tukey 
TukeyHSD(cum.total.aov) # running total significantly higher in the 10-ambient vs. 6-ambient 
TukeyHSD(cum.percap.aov) # running total per broodstock cm significantly higher in the 10-ambient vs. 6-ambient 
TukeyHSD(maxday.aov) # max larval release earler in the 10C group 
plot (x=spawning_group_sum$Treatment, y=spawning_group_sum$maxday, main="Calendar day at max larval release") 

# How many calendar days later was the maxday in the chilled group?  
aggregate(maxday ~ pH+Temperature, spawning_group_sum, mean) 
152.1667 - 143.8333

### ---------------------- extra plots for reference and fun 

# Spawn plots for chilled/unchilled group separately 
all_total.6 <- subset(all_total, Treatment == "Chilled, Ambient pH" | Treatment == "Chilled, Low pH")
all_total.6$Treatment <- as.factor(all_total.6$Treatment)
release.bars.6 <- ggplot(data=subset(all_total.6, Date<="2017-06-22"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Olympia oyster larval release by pH treatment, chilled groups") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=20), legend.text = element_text(size=20), axis.title = element_text(size=20, face = "bold"), axis.text = element_text(size=16))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("skyblue3", "seagreen3"))
release.6 <- release.bars.6 + geom_line(data=all_total.6, aes(x=Date, y=cum.total/8, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("skyblue3", "seagreen3")) +
  scale_y_continuous(sec.axis = sec_axis(label=,~.*8,name="Cumulative Larvae Released"))
release.6

# Unchilled treatments (all populations)
all_total.10 <- subset(all_total, Treatment == "Unchilled, Ambient pH" | Treatment == "Unchilled, Low pH")
all_total.10$Treatment <- as.factor(all_total.10$Treatment)
release.bars.10 <- ggplot(data=subset(all_total.10, Date<="2017-06-22"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Olympia oyster larval release by pH treatment, unchilled groups") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=20), legend.text = element_text(size=20), axis.title = element_text(size=20, face = "bold"), axis.text = element_text(size=16))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2"))
release.10 <- release.bars.10 + geom_line(data=all_total.10, aes(x=Date, y=cum.total/8, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("orange1", "indianred2")) +
  scale_y_continuous(sec.axis = sec_axis(label=,~.*8,name="Cumulative Larvae Released"))
release.10

#Calculate cumulative larvae released through time by spawning group (SN & NF groups had duplicate buckets)
pop_total <- group_by(larvae, Group, Population, Treatment) %>% mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% arrange(Date) %>% dplyr::select(Date,CalDay,Group,Population,Treatment,total.released,larvae.per.broodcm,cum.total,cum.percap)

# Rename treatments for use in population-specific spawning plots
pop_total$Treatment <- gsub("10-Ambient", "Unchilled, Ambient pH", pop_total$Treatment)
pop_total$Treatment <- gsub("6-Ambient", "Chilled, Ambient pH", pop_total$Treatment)
pop_total$Treatment <- gsub("10-Low", "Unchilled, Low pH", pop_total$Treatment)
pop_total$Treatment <- gsub("6-Low", "Chilled, Low pH", pop_total$Treatment)

# Create day 1 dataframe for time series plots for all spawning groups. 
day1 <- pop_total %>% 
  group_by(Group) %>%
  slice(which.min(Date))

# Create day 0 entries for all spawning groups, starting at 5/10/2017 (so line plots start at 0) 
day0 <- pop_total[!duplicated(pop_total$Group), ]
day0$Date <- rep(as.Date("2017-05-10"), times=nrow(day0))
day0$CalDay <- rep(130, times=nrow(day0))
day0$total.released <- rep(0, times=nrow(day0))
day0$larvae.per.broodcm <- rep(0, times=nrow(day0))
day0$cum.total <- rep(0, times=nrow(day0))
day0$cum.percap <- rep(0, times=nrow(day0))

# Create last day entries for all spawning groups 
dayf <- pop_total %>% 
  group_by(Group) %>%
  slice(which.max(Date))
dayf$Date <- rep(as.Date("2017-07-10"), times=nrow(dayf))
dayf$CalDay <- rep(191, times=nrow(dayf))

# Combine day0, day1, dayf to pop_total dataframe
pop_total <- rbind(pop_total, day1, day0, dayf)


# South Sound
p1 <- ggplot(data=subset(pop_total, Population == "SN"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Oyster Bay F1, South Sound\nTiming of Larvae Release by Treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))
SN.release <- p1+ geom_line(data=subset(pop_total, Population == "SN"), aes(x=Date, y=cum.total/4.5, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*4.5,name="Cumulative Larvae Released"), label=comma)
SN.release

# Fidalgo Bay 
p2 <- ggplot(data=subset(pop_total, Population == "NF"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Fidalgo Bay, North Sound\nTiming of Larvae Release by Treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))
NF.release <- p2+ geom_line(data=subset(pop_total, Population == "NF"), aes(x=Date, y=cum.total/5, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*5,name="Cumulative Larvae Released"), label=comma)
NF.release

# Hood Canal 
p3 <- ggplot(data=subset(pop_total, Population == "HL"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Dabob Bay, Hood Canal\nTiming of Larvae Release by Treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))
HL.release <- p3 + geom_line(data=subset(pop_total, Population == "HL"), aes(x=Date, y=cum.total/2.5, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*2.5,name="Cumulative Larvae Released"), label=comma)
HL.release

# South Sound F2
p4 <- ggplot(data=subset(pop_total, Population == "K"), aes(x=Date, y=total.released, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Larvae Released") +
  ggtitle("Oyster Bay F2, South Sound\nTiming of Larvae Release by Treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))
K.release <- p4 + geom_line(data=subset(pop_total, Population == "K"), aes(x=Date, y=cum.total/3, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*3,name="Cumulative Larvae Released"), label=comma)
K.release


# All populations combined by treatment normalized by # broodstock (doesn't change the pattern)

all.release.bars.perbrood <- ggplot(data=all_total, aes(x=Date, y=larvae.per.broodcm, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Daily larvae release (per broodstock)") +
  ggtitle("Olympia oyster larval release by pH and chilled/unchilled (normalized by broodstock)") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("skyblue3", "seagreen3", "orange1", "indianred2"))

All.release.perbrood <- all.release.bars.perbrood + geom_line(data=all_total, aes(x=Date, y=cum.percap/7, group=Treatment, color=Treatment),size=.75) +
  scale_color_manual(values=c("skyblue3", "seagreen3", "orange1", "indianred2")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*7,name="Cumulative larvae released, norm. by broodstock cm"), label=comma)

fecundity.pop <- aggregate(Tot.Larvae ~ pH + Temperature + Population + Date, larvae, sum, na.rm=TRUE)

par(mfrow=c(2,1))

test.amb <- ts(subset(fecundity.pop, pH=="Ambient")[,c("Date", "Tot.Larvae")])
test.low <- ts(subset(fecundity.pop, pH=="Low")[,c("Date", "Tot.Larvae")])
plot(smooth(test.amb))
plot(smooth(test.low))

test.amb.c <- decompose(test.amb)
test.low.c <- decompose(test.low)
plot(test.amb.c)
plot(test.low.c)

plot(x=subset(fecundity, pH=="Ambient")$Date, y=smooth(subset(fecundity, pH=="Ambient")$Tot.Larvae)) #first peak 5/17-5/19, second 6/1-6/3

plot(x=subset(fecundity, pH=="Low")$Date, y=smooth(subset(fecundity, pH=="Low")$Tot.Larvae)) #first peak 5/22-5/24; second peak 6/12-6/19 

plot(smooth(ts(subset(fecundity.pop, pH=="Ambient" & Population=="NF")[,c("Date", "Tot.Larvae")])), main="F-amb")
plot(smooth(ts(subset(fecundity.pop, pH=="Low" & Population=="NF")[,c("Date", "Tot.Larvae")])), main="F-low")
plot(smooth(ts(subset(fecundity.pop, pH=="Ambient" & Population=="HL")[,c("Date", "Tot.Larvae")])), main="D-amb")
plot(smooth(ts(subset(fecundity.pop, pH=="Low" & Population=="HL")[,c("Date", "Tot.Larvae")])), main="D-low")
plot(smooth(ts(subset(fecundity.pop, pH=="Ambient" & Population=="SN")[,c("Date", "Tot.Larvae")])), main="O1-amb")
plot(smooth(ts(subset(fecundity.pop, pH=="Low" & Population=="SN")[,c("Date", "Tot.Larvae")])), main="O1-low")
plot(smooth(ts(subset(fecundity.pop, pH=="Ambient" & Population=="K")[,c("Date", "Tot.Larvae")])), main="O2-amb")
plot(smooth(ts(subset(fecundity.pop, pH=="Low" & Population=="K")[,c("Date", "Tot.Larvae")])), main="O2-low")

