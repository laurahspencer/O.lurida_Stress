
rm(list=ls())         #start script by deleting all objects - clean slate 

# ----- import spreadsheet of bucket densities, data from screening days (2-3x per week) plus new larvae added to buckets daily 
Bucket.Densities <- read.csv("Data/Bucket-Densities-revised.csv", header = F, stringsAsFactors = F, na.strings = c("NA", "#DIV/0!", "#VALUE!"))
colnames(Bucket.Densities) <- Bucket.Densities[2,]
Bucket.Densities <- Bucket.Densities[-1:-2,]

# Groups with 2 larval buckets initially:  
# NF10-Low, started new bucket on 5/24 
# NF10-Ambient, started new bucket on 5/25
# NF6-Low, started new bucket on 5/25
# HL10-Low, started new bucket on 5/26
# SN10-Low, started new bucket on 5/26
# SN10-Ambient, started new bucket on 6/4
# As of 6/9, only 1 bucket per treatment group in bucket counts, so on previous screening day (6/5) conjoined all duplicated groups 
# After 6/9, used connected 2-bucket flow-through system to remove dead larvae (dead stayed in 1st bucket, live swan and flowed to 2nd bucket). 

# Assign bucket numbers to treatment/population groups 
Buckets <- c(3,12,8,10,5,7,16,13,19,21,24,23,22,18,17,20,1,9,11,4)
Groups <- c("NF-10 Ambient", "NF-10 Low", "NF-6 Ambient", "NF-6 Low", "SN-10 Ambient", "SN-10 Low", "SN-6 Ambient", "SN-6 Low", "HL-10 Ambient","HL-10 Low", "HL-6 Ambient", "HL-6 Low", "K-10 Ambient", "K-10 Low","K-6 Ambient", "K-6 Low", "SN-6 Low", "SN-10 Low", "SN-10 Ambient", "NF-10 Low")
Group.Buckets <- as.data.frame(cbind(Buckets, Groups)) #combine density data with bucket info 

# ---- Reformat density dataframe, laboriously 

Bucket.Densities.long <- na.omit(melt(Bucket.Densities[,-2], id.vars = "Date")) #melt into long format 
Bucket.Densities.long$variable <- as.character(Bucket.Densities.long$variable)  #
Bucket.Densities.long[,c("Bucket", "Count")] <- str_split_fixed(Bucket.Densities.long$variable, "\\.", 2)
Bucket.Densities.long$Bucket <- as.factor(Bucket.Densities.long$Bucket)
Bucket.Densities.long$Count <- as.factor(Bucket.Densities.long$Count)
Bucket.Densities.long$Date <- as.Date(Bucket.Densities.long$Date, "%m/%d/%y")
Bucket.Densities.long$value <- as.numeric(Bucket.Densities.long$value)

# add columns to break up factors (temp, pH, population)
Bucket.Densities.long$Group <- Bucket.Densities.long$Bucket
Bucket.Densities.long <- merge(Bucket.Densities.long, Group.Buckets, by.x = "Bucket", by.y = "Buckets", all.x = T, all.y = T)
Bucket.Densities.long$Bucket.group <- as.factor(paste(Bucket.Densities.long$Bucket,Bucket.Densities.long$Groups))
Bucket.Densities.long$Treatment <- as.factor(gsub(".*-","",Bucket.Densities.long$Groups))
Bucket.Densities.long$Population <- as.factor(gsub("\\-.*","",Bucket.Densities.long$Groups))
Bucket.Densities.long[,c("Temperature", "pH")] <- str_split_fixed(Bucket.Densities.long$Treatment, "\\s+", 2)
Bucket.Densities.long[,"Temperature"] <- as.factor(Bucket.Densities.long[,"Temperature"])
Bucket.Densities.long[,"pH"] <- as.factor(Bucket.Densities.long[,"pH"])
Bucket.Densities.long <- subset(Bucket.Densities.long, value != 0)

# Reformat again to wide, with each bucket density from day to day (includes larvae added, removed, over time)
Bucket.Densities.wide <- dcast(Bucket.Densities.long, Bucket+Date+Population+Treatment+Temperature+pH ~ Count, value.var = "value")
Bucket.Densities.wide$survival <- Bucket.Densities.wide$actual/Bucket.Densities.wide$expected # survival = (larvae out / larvae in)
Bucket.Densities.wide$dead <- Bucket.Densities.wide$expected - Bucket.Densities.wide$actual   # dead = (larvae in - larvae out)

# ------- Inspect stocking density data to see if densities were different between treatments. Cap was ~200k larvae / group at a time, but varied over time & between buckets 

plot(subset(Bucket.Densities.long, Count=="density")$value ~ subset(Bucket.Densities.long, Count=="density")$Treatment) #boxplot to inspect 
kruskal.test(subset(Bucket.Densities.long, Count=="density")$value ~ subset(Bucket.Densities.long, Count=="density")$pH) # p=0.5352  <-- no pH difference 
kruskal.test(subset(Bucket.Densities.long, Count=="density")$value ~ subset(Bucket.Densities.long, Count=="density")$Temperature) # p=0.02606 <-- density diff by temperature  
kruskal.test(subset(Bucket.Densities.long, Count=="density" & Temperature == 6)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature == 6)$pH) # no density differences between pH in 6C
kruskal.test(subset(Bucket.Densities.long, Count=="density" & Temperature == 10)$value ~ subset(Bucket.Densities.long, Count=="density" & Temperature == 10)$pH) #density diff between pH in 10C ... but how? 
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), mean) #stocking density lower in low pH group, in 10C 

# Summary stats on density between treatments 
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), median)
aggregate(value ~ pH, data=subset(Bucket.Densities.long, Count=="density" & Temperature == 10), sd)
aggregate(value ~ Treatment, data=subset(Bucket.Densities.long, Count=="density"), mean) 
aggregate(value ~ Treatment, data=subset(Bucket.Densities.long, Count=="density"), median) 
aggregate(value ~ Treatment, data=subset(Bucket.Densities.long, Count=="density"), sd) 

#### Plot bucket density across time color coded by treatment for trends 
plot_ly(data = Bucket.Densities.wide, x = ~Date, y = ~density, type="scatter", mode="lines", color=~Treatment, colors = c("orange1", "indianred2", "skyblue3", "seagreen3"), hovertext=~Bucket) %>%  #generate plotly plot
        layout(title="2017 O. lurida Bucket Densities",
               yaxis = list(title = '# Larvae in Bucket'),
               legend = list(x=.75, y=.75))

# plot % survival over time for trends 
plot_ly(data = subset(Bucket.Densities.wide, dead >=0), x = ~Date, y = ~actual/expected, type="scatter", mode="markers", symbol=~pH, color=~Temperature, colors = c("orangered3","blue3"), size=~sqrt(expected), hovertext=~Population) %>%  #generate plotly plot
  layout(title="2017 O. lurida survival between screenings\n
         marker size ~ # larvae stocked, color by pH treatment",
         yaxis = list(title = '% survival between screenings'),
         legend = list(x=100, y=.9))

# plot # survived against # stocked 
plot_ly(data = subset(Bucket.Densities.wide, dead >=0), x = ~expected, y = ~actual, type="scatter", mode="markers", symbol=~Temperature, color=~pH, colors = c("orangered3","blue3"), size=~sqrt(expected), hovertext=~Population) %>%  #generate plotly plot
  layout(title="2017 O. lurida survival between screenings\n
         marker size ~ # larvae stocked, color by pH treatment",
         yaxis = list(title = 'Survived'),
         xaxis = list(title = 'Stocked'),
         legend = list(x=100, y=.9))


# plot % survival against bucket density - is there a pattern? Is survival density dependent? 
plot_ly(data = subset(Bucket.Densities.wide, dead >=0), x = ~expected, y = ~actual/expected, type="scatter", mode="markers", symbol=~Temperature, color=~pH, colors = c("orangered3","blue3"), size=~sqrt(expected), hovertext=~Population) %>%  #generate plotly plot
  layout(title="O. lurida larvae survival between screenings\n
         marker size ~ # larvae stocked, color by pH treatment",
         yaxis = list(title = '% Survived between screenings'),
         xaxis = list(title = 'Bucket Density (# stocked)'),
         legend = list(x=100, y=.9))


# ------- Compare % larval survival between biweekly screenings, using # larvae stocked (included new larvae added), and # larvae counted. Remember: 224um larvae were counted, but then removed from the larval buckets. 

# Boxplot of % survival between screenings by treatment - trend? No obvious diff. 
ggplot(subset(Bucket.Densities.wide, survival<1.5), aes(x=Treatment, y= survival, fill=Treatment)) + 
  geom_boxplot() +
  labs(title="Percent survival between screenings\n(2x weekly, n=19)",y=expression("Percent Survival")) + 
  theme_bw(base_size = 16) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))


# Test differences in survival between screening dates (counted & restocked 2x weekly) 
aggregate(actual/(actual+dead) ~ Temperature + pH, data=subset(Bucket.Densities.wide, dead>0), mean)
aggregate(actual/(actual+dead) ~ Temperature + pH, data=subset(Bucket.Densities.wide, dead>0), median)
aggregate(actual/(actual+dead) ~ Temperature + pH, data=subset(Bucket.Densities.wide, dead>0), var)
aggregate(actual/(actual+dead) ~ Temperature + pH, data=subset(Bucket.Densities.wide, dead>0), sd)
# summary stats indicate likely no difference 

# test using binomial glm 
anova(glm(cbind(actual, dead) ~ Temperature+pH, data=subset(Bucket.Densities.wide, dead>0), binomial), test="Chi") #Sign., but do we trust this?  Not sure ... 

# Test using aov on square-root arcsine transformed survival % data 
hist(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$survival))) # looks normal 
shapiro.test(asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$survival)))  # shapiro test confirms 
bartlett.test(x=asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$survival)), g=subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$pH) #equal variance between pH
bartlett.test(x=asin(sqrt(subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$survival)), g=subset(Bucket.Densities.wide, (survival!= "NA" & survival <= 1))$Temperature) #equal variance between Temps
Bucket.Densities.wide$survival.t <- asin(sqrt(Bucket.Densities.wide$survival)) #add column with sqrt-asin transformed survival

#save new dataframe with just the valid transformed % survival data, for models 
survival.biweekly <- subset(Bucket.Densities.wide, (survival.t!= "NA" & survival.t!="NaN")) 

# Compare % survival (sqrt-asin transformed) between chilled/unchiled (temperature) and pH treatment groups 
anova(lm(survival.t ~ pH, data=survival.biweekly)) #no diff
anova(lm(survival.t ~ Temperature, data=survival.biweekly)) #no diff 
summary(lm(survival.t ~ expected, data=survival.biweekly)) # Include stocking density (aka "expected", for the # larvae put into bucket). yes diff, but how? the scatter plot above doesn't indicate +/- stocking influence ... 

summary(lm(survival.t ~ expected*Temperature*pH, data=survival.biweekly)) # include all 3 factors 
summary(lm(survival.t ~ expected*Temperature, data=survival.biweekly)) # just temp & # larvae put in bucket 
anova(lm(survival.t ~ Temperature*pH*expected, data=survival.biweekly)) # re-order to assess F-values 

# test 6 and 10 groups separately 
anova(lm(survival.t ~ pH, data=subset(survival.biweekly, Temperature == 6))) #no diff
anova(lm(survival.t ~ pH, data=subset(survival.biweekly, Temperature == 10))) #no diff
anova(lm(survival.t ~ expected, data=subset(survival.biweekly, Temperature == 6))) #no diff
summary(lm(survival.t ~ expected, data=subset(survival.biweekly, Temperature == 10))) #Difference 

plot(x=subset(survival.biweekly, Temperature==10)$expected, y=subset(survival.biweekly, Temperature==10)$survival.t)

# Conclusion no sign. differences in % survival between low pH and ambient pH groups, nor between chilled/unchilled, as per biweekly count data 

mean(subset(Bucket.Densities.wide, survival!="NA")$survival) #average % survival between screenings 
sd(subset(Bucket.Densities.wide, survival!="NA")$survival) #average % survival between screenings 

plot(x=survival.biweekly$expected, y=survival.biweekly$survival.t, col=survival.biweekly$Treatment, main="% survival (sqrt-asin transformed) ~ # stocked\n biweekly count data",  xlab="# stocked", ylab="% Survived (sqrt-asin transf.)", pch=16)
legend(x="bottom", c("Unchilled-Ambient", "Unchilled-Low", "Chilled-Ambient", "Chilled-Low"),  box.col="gray75", cex=1, pch=16, col=survival.biweekly$Treatment) #can't get the legend points to be colored by treatment ... not super important 

# Survival data by population 
aggregate(survival ~ Population, survival.biweekly, mean, na.rm=TRUE)
aggregate(survival ~ Population, survival.biweekly, sd, na.rm=TRUE)

### Compare # setters in setting tanks (used 2 setting tanks, new one started on 6/22)
setters.stocking <- read.csv("Data/Setters-Stocking.csv", header = T, stringsAsFactors = T)
kruskal.test(x=setters.stocking$Count, g=setters.stocking$Temperature)
kruskal.test(x=setters.stocking$Count, g=setters.stocking$pH)
kruskal.test(x=setters.stocking$Count, g=setters.stocking$Population)
kruskal.test(x=subset(setters.stocking, Temperature==10)$Count, g=subset(setters.stocking, Temperature==10)$pH) 
kruskal.test(x=subset(setters.stocking, Temperature==6)$Count, g=subset(setters.stocking, Temperature==6)$pH)
# RESULT: no sign. differences in setter stocking density between temperature, pH or population 

# barplot of setters - reminder that the large # of larvae stocked on June 15th were tossed (accidentally mixed with others)
ggplot(data=Bucket.Densities.wide, aes(x=Date, y=setters, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Setters Counted") +
  ggtitle("Olympia oyster setters over time, by treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))

# barplot of # larvae stocked  
ggplot(data=Bucket.Densities.wide, aes(x=Date, y=expected, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of larvae stocked") +
  ggtitle("Olympia oyster stocked over time, by treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=Bucket.Densities.wide, aes(x=Date, y=actual, fill=Treatment)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("# live larvae counted") +
  ggtitle("Olympia oyster # larvae counted in bucket over time, by treatment") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))


# ---------- Extra population-level plots 

# Boxplot - survival by population 
ggplot(subset(Bucket.Densities.wide, survival<1.5), aes(x=Population, y= survival, fill=Population)) + 
  geom_boxplot() +
  labs(title="Percent survival between screenings\n(2x weekly, n=19)",y=expression("Percent Survival")) + 
  theme_bw(base_size = 16) + xlab("Population") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c("royalblue4", "royalblue1", "turquoise4", "turquoise2"))

# barplot of setters - reminder that the large # of SN larvae stocked on June 15th were tossed (accidentally mixed with others). The ones stocked June 19th were kept. 
ggplot(data=Bucket.Densities.wide, aes(x=Date, y=setters, fill=Population)) + 
  geom_bar(stat="identity",width=.5, position = position_dodge(width=2)) + ylab("Number of Setters Counted") +
  ggtitle("Olympia oyster setters over time, by Population") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("orange1", "indianred2", "skyblue3", "seagreen3"))
