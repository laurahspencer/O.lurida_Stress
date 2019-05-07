fecundity <- aggregate(Tot.Larvae ~ pH + Temperature + Date, subset(larvae, Temperature==6), sum, na.rm=TRUE)
fecundity <- aggregate(Tot.Larvae ~ pH + Temperature + Date, larvae, sum, na.rm=TRUE)
fecundity.temp <- aggregate(Tot.Larvae ~ pH + Temperature + Date, larvae, sum, na.rm=TRUE)

#fecundity.pop <- aggregate(Tot.Larvae ~ pH + Temperature + Population + Date, subset(larvae, Temperature==6), sum, na.rm=TRUE)

#Calculate cumulative larvae released through time for each pH/temp treatment (combine replicates)
fecundity.pop <- group_by(larvae, Population, Temperature, pH) %>% dplyr::mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% dplyr::arrange(Date) %>% dplyr::select(Date,CalDay,pH,Temperature, Population,total.released,larvae.per.broodcm,cum.total,cum.percap) %>% mutate(Date = as.Date(Date))

# sum and average daily stats for each pH
density4barplots <- Bucket.Densities.wide %>%
  group_by(Date, pH, Temperature) %>%
  dplyr::summarize(setters=sum(setters, na.rm=T), stocked.new=sum(stocked, na.rm=T), stocked.tot=sum(expected, na.rm=T), stocked.mean=mean(expected, na.rm=T), stocked.sd=sd(expected, na.rm=T), counts.live=sum(actual, na.rm=T), survival.mean=mean(survival, na.rm=T), survival.sd=sd(survival, na.rm=T))
density4barplots[density4barplots == 0] <- NA

density4barplots.pops <- Bucket.Densities.wide %>%
  group_by(Date, pH, Temperature, Population) %>%
  dplyr::summarize(setters=sum(setters, na.rm=T), stocked.new=sum(stocked, na.rm=T), stocked.tot=sum(expected, na.rm=T), stocked.mean=mean(expected, na.rm=T), stocked.sd=sd(expected, na.rm=T), counts.live=sum(actual, na.rm=T), survival.mean=mean(survival, na.rm=T), survival.sd=sd(survival, na.rm=T))
density4barplots.pops[density4barplots.pops == 0] <- NA


#### larval release scatter plots 

spawning_group_sum$Population.Temp <- as.factor(paste(spawning_group_sum$Population, spawning_group_sum$Temperature, sep="-"))
spawning_group_sum$Population.Temp <- factor(spawning_group_sum$Population.Temp, levels = c("NF-6", "NF-10", "HL-6", "HL-10", "SN-6", "SN-10", "K-6", "K-10"))

spawning_group_sum$Population <- factor(spawning_group_sum$Population, levels=c("NF", "HL", "SN", "K"))

# Release magnitude  (550x375)
pdf(file = "Results/release-mag-scatter.pdf", width = 7.45, height = 3.75)
ggplot(spawning_group_sum, aes(x=release.days, y=overall_Total)) +
  geom_point(size=4, stroke=2, aes(shape=Population, colour=pH:Temperature), position = position_jitter(w = 0.3, h = 0)) + scale_color_manual(values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient", "10°C / High"), name="Treatment") + scale_shape_manual(values=c(15, 16, 2, 5), labels=c("Fidalgo Bay", "Dabob Bay", "South Sound C1", "South Sound C2")) +
  ggtitle("Larval release magnitude") + theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"),
        legend.position = "right", axis.title.x = element_text(colour="gray30", size=14), axis.text.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14)) +
  xlab("No. large larval pulses (~families)") + ylab("Cumulative larvae released") +
  scale_x_continuous(limits=c(min=0,max=max(spawning_group_sum$release.days))) +
  scale_y_continuous(limits=c(min=0,max=max(spawning_group_sum$overall_Total)), label=comma) 
dev.off()

# Release Timing (590x375)
# first.big and maxday are plotted as x-100, since the calendar day that reproductive conditioning was day 100 (april 11th, 2017); this shows the # days in reproductive conditioning. 

pdf(file = "Results/release-timing-scatter.pdf", width = 7.45, height = 3.75)
ggplot(spawning_group_sum, aes(x=first.big-100, y=maxday-100)) +
  geom_point(size=4, stroke=2, aes(shape=Population, colour=pH:Temperature), position = position_jitter(w = 0.8, h = 0.8)) + scale_color_manual(values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient", "10°C / High"), name="Treatment") + scale_shape_manual(values=c(15, 16, 2, 5), labels=c("Fidalgo Bay", "Dabob Bay", "South Sound C1", "South Sound C2")) +
  ggtitle("Larval release timing") + theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"), axis.text.x = element_text(colour="gray30", size=14), axis.title.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14), legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14)) +
  xlab("Days to release onset") + ylab("Days to maximum release")
dev.off()


#Try each population, color coded by treatment 

pdf(file = "Results/FidalgoBay-larval-time.pdf", width = 7, height = 4.5)
ggplot(data=subset(fecundity.pop, Population=="NF"), aes(x=CalDay-131, y=total.released, fill=Temperature:pH)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=("Fidalgo Bay larval release")) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000, 1000000)) +
  theme(legend.position = c(0.75, 0.8), legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(name=expression(paste("Temperature / ", pCO[2], " Exposure")), values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient", "10°C / High"))
dev.off()


pdf(file = "Results/DabobBay-larval-time.pdf", width = 7, height = 4.5)
ggplot(data=subset(fecundity.pop, Population=="HL"), aes(x=CalDay-131, y=total.released, fill=Temperature:pH)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=("Dabob Bay larval release")) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000, 1000000)) +
  theme(legend.position = "none", legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(name="Temperature/pCO2",values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C/Amb", "6°C/High", "10°C/Amb", "10°C/High"))
dev.off()

pdf(file = "Results/OysterBayC1-larval-time.pdf", width = 7, height = 4.5)
ggplot(data=subset(fecundity.pop, Population=="SN"), aes(x=CalDay-131, y=total.released, fill=Temperature:pH)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=("Oyster Bay cohort 1 larval release")) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000, 1000000)) +
  theme(legend.position =  "none", legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(name="Temperature/pCO2",values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C/Amb", "6°C/High", "10°C/Amb", "10°C/High"))
dev.off()

pdf(file = "Results/OysterBayC2-larval-time.pdf", width = 7, height = 4.5)
ggplot(data=subset(fecundity.pop, Population=="K"), aes(x=CalDay-131, y=total.released, fill=Temperature:pH)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label="Days after larval release onset") +
  labs(title=("Oyster Bay cohort 2 larval release")) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), 
        axis.title.y = element_blank(), axis.title.x = element_text(colour="gray30"), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000, 1000000)) +
  theme(legend.position =  "none", legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(name="Temperature/pCO2",values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C/Amb", "6°C/High", "10°C/Amb", "10°C/High"))
dev.off()







# Other plots showing larval release, new larvae stocked, eyed larvae, and stocking density 

all.larvae <- ggplot(data=density4barplots, aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity$Date)-1,max=max(density4barplots$Date)+1))  +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

all.eyed <- ggplot(data=density4barplots, aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity$Date)-1,max=max(density4barplots$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(all.fecundity, all.larvae, all.eyed, ncol = 1, heights = c(1, 1, 1))


# For each population separately 
mindate <- min(fecundity.pop$Date)-1
maxdate <- max(density4barplots.pops.10$Date)+1

FB.fecundity.6 <- 
  ggplot(data=subset(fecundity.pop, Population == "NF" & Temperature==6), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  geom_line(data=subset(fecundity.pop, Population == "NF" & Temperature==6), aes(x=Date, y=cum.total/4, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(sec.axis = sec_axis(label=,~.*4,name="Cumulative Larvae Released"))

FB.stocked.6 <- ggplot(data=subset(density4barplots.pops, Population=="NF" & Temperature==6), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

FB.eyed.6 <- ggplot(data=subset(density4barplots.pops, Population=="NF" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae, Fidalgo Bay") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

png(file = "Results/Larval-plots-FB-6C.png")
grid.arrange(FB.fecundity, FB.stocked, FB.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# -------- 

DB.fecundity.6 <- ggplot(data=subset(fecundity.pop, Population == "HL" & Temperature==6), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.stocked.6 <- ggplot(data=subset(density4barplots.pops, Population=="HL" & Temperature==6), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.eyed.6 <- ggplot(data=subset(density4barplots.pops, Population=="HL" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

png(file = "Results/Larval-plots-DB-6C.png")
grid.arrange(DB.fecundity, DB.stocked, DB.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB1.fecundity.6 <- ggplot(data=subset(fecundity.pop, Population == "SN" & Temperature==6), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()


OB1.stocked.6 <- ggplot(data=subset(density4barplots.pops, Population=="SN" & Temperature==6), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB1.eyed.6 <- ggplot(data=subset(density4barplots.pops, Population=="SN" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

png(file = "Results/Larval-plots-OB1-6C.png")
grid.arrange(OB1.fecundity, OB1.stocked, OB1.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB2.fecundity.6 <- ggplot(data=subset(fecundity.pop, Population == "K" & Temperature==6), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.stocked.6 <- ggplot(data=subset(density4barplots.pops, Population=="K" & Temperature==6), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.eyed.6 <- ggplot(data=subset(density4barplots.pops, Population=="K" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

png(file = "Results/Larval-plots-OB2-6C.png")
grid.arrange(OB2.fecundity, OB2.stocked, OB2.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()


## --------------- SAME FOR 10C GROUPS 

fecundity.10 <- aggregate(Tot.Larvae ~ pH + Temperature + Date, subset(larvae, Temperature==10), sum, na.rm=TRUE)
fecundity.pop.10 <- aggregate(Tot.Larvae ~ pH + Temperature + Population + Date, subset(larvae, Temperature==10), sum, na.rm=TRUE)

density4barplots.10 <- subset(Bucket.Densities.wide, Temperature==10) %>%
  group_by(Date, pH) %>%
  dplyr::summarize(setters=sum(setters, na.rm=T), stocked.new=sum(stocked, na.rm=T), stocked.tot=sum(expected, na.rm=T), stocked.mean=mean(expected, na.rm=T), stocked.sd=sd(expected, na.rm=T), counts.live=sum(actual, na.rm=T), survival.mean=mean(survival, na.rm=T), survival.sd=sd(survival, na.rm=T))
density4barplots.10[density4barplots.10 == 0] <- NA

density4barplots.pops.10 <- subset(Bucket.Densities.wide, Temperature==10) %>%
  group_by(Date, pH, Population) %>%
  dplyr::summarize(setters=sum(setters, na.rm=T), stocked.new=sum(stocked, na.rm=T), stocked.tot=sum(expected, na.rm=T), stocked.mean=mean(expected, na.rm=T), stocked.sd=sd(expected, na.rm=T), counts.live=sum(actual, na.rm=T), survival.mean=mean(survival, na.rm=T), survival.sd=sd(survival, na.rm=T))
density4barplots.pops.10[density4barplots.pops.10 == 0] <- NA

all.fecundity.10 <- ggplot(data=fecundity.10, aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("All Populations\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.10$Date)-1,max=max(density4barplots.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.10$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

all.larvae.10 <- ggplot(data=density4barplots.10.10, aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.10$Date)-1,max=max(density4barplots.10$Date)+1))  +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.10$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

all.eyed.10 <- ggplot(data=density4barplots.10, aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.10$Date)-1,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(all.fecundity.10, all.larvae.10, all.eyed.10, ncol = 1, heights = c(1, 1, 1))


# For each population separately 

FB.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "NF"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop.10$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

FB.stocked.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="NF"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

FB.eyed.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="NF"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae, Fidalgo Bay") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

png(file = "Results/Larval-plots-FB-10C.png")
grid.arrange(FB.fecundity.10, FB.stocked.10, FB.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# -------- 

DB.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "HL"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop.10$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.stocked.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="HL"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.eyed.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="HL"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

png(file = "Results/Larval-plots-DB-10C.png")
grid.arrange(DB.fecundity.10, DB.stocked.10, DB.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB1.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "SN"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop.10$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()


OB1.stocked.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="SN"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB1.eyed.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="SN"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

png(file = "Results/Larval-plots-OB1-10C.png")
grid.arrange(OB1.fecundity.10, OB1.stocked.10, OB1.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB2.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "K"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop.10$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.stocked.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="K"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.eyed.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="K"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

png(file = "Results/Larval-plots-OB2-10C.png")
grid.arrange(OB2.fecundity.10, OB2.stocked.10, OB2.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()


## --------------- Daily & Cumulative Larval Release Plots by Population  
mindate <- min(fecundity.pop$Date)-1
maxdate <- max(fecundity.pop$Date)+1


FB.fecundity.6 <- 
  ggplot(data=subset(fecundity.pop, Population == "NF" & Temperature==6), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay, 6°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +  
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray60", "lightsteelblue4")) + 
  geom_line(data=subset(fecundity.pop, Population == "NF" & Temperature==6), 
            aes(x=Date, y=cum.total/6, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray60", "lightsteelblue4")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="NF")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*6, name="Cumulative Larvae Released"))
 
FB.fecundity.10 <- 
  ggplot(data=subset(fecundity.pop, Population == "NF" & Temperature==10), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay, 10°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  geom_line(data=subset(fecundity.pop, Population == "NF" & Temperature==10), 
            aes(x=Date, y=cum.total/5, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="NF")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*5,name="Cumulative Larvae Released"))

DB.fecundity.6 <- 
  ggplot(data=subset(fecundity.pop, Population == "HL" & Temperature==6), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 6°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray60", "lightsteelblue4")) + 
  geom_line(data=subset(fecundity.pop, Population == "HL" & Temperature==6), 
            aes(x=Date, y=cum.total/2.5, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray60", "lightsteelblue4")) + 
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="HL")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*2.5,name="Cumulative Larvae Released"))

DB.fecundity.10 <- 
  ggplot(data=subset(fecundity.pop, Population == "HL" & Temperature==10), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 10°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  geom_line(data=subset(fecundity.pop, Population == "HL" & Temperature==10), 
            aes(x=Date, y=cum.total/2.5, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="HL")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*2.5,name="Cumulative Larvae Released"))

OB1.fecundity.6 <- 
  ggplot(data=subset(fecundity.pop, Population == "SN" & Temperature==6), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 6°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray60", "steelblue")) + 
  geom_line(data=subset(fecundity.pop, Population == "SN" & Temperature==6), 
            aes(x=Date, y=cum.total/4, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="SN")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*4,name="Cumulative Larvae Released"))

OB1.fecundity.10 <- 
  ggplot(data=subset(fecundity.pop, Population == "SN" & Temperature==10), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 10°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  geom_line(data=subset(fecundity.pop, Population == "SN" & Temperature==10), 
            aes(x=Date, y=cum.total/4, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="SN")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*4,name="Cumulative Larvae Released"))

OB2.fecundity.6 <- 
  ggplot(data=subset(fecundity.pop, Population == "K" & Temperature==6), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 6°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) + 
  geom_line(data=subset(fecundity.pop, Population == "K" & Temperature==6), 
            aes(x=Date, y=cum.total/3, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="K")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*3,name="Cumulative Larvae Released"))

OB2.fecundity.10 <- 
  ggplot(data=subset(fecundity.pop, Population == "K" & Temperature==10), aes(x=Date, y=total.released, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 10°C") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 12, hjust = 0, colour = "gray30")) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=mindate,max=maxdate)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue")) +
  geom_line(data=subset(fecundity.pop, Population == "K" & Temperature==10), 
            aes(x=Date, y=cum.total/3, group=pH, color=pH),size=.75) +
  scale_color_manual(values=c("gray40", "steelblue")) +
  scale_y_continuous(limits=c(min=0,max=max(subset(fecundity.pop, Population=="K")$total.released)), label=comma, 
                     sec.axis = sec_axis(label=comma,~.*3,name="Cumulative Larvae Released"))

pdf(file = "Results/fecundity-FB-6C.pdf", width = 7, height = 4)
FB.fecundity.6
dev.off()
pdf(file = "Results/fecundity-FB-10C.pdf", width = 7, height = 4)
FB.fecundity.10
dev.off()
pdf(file = "Results/fecundity-DB-6C.pdf", width = 7, height = 4)
DB.fecundity.6
dev.off()
pdf(file = "Results/fecundity-DB-10C.pdf", width = 7, height = 4)
DB.fecundity.10
dev.off()
pdf(file = "Results/fecundity-OB1-6C.pdf", width = 7, height = 4)
OB1.fecundity.6
dev.off()
pdf(file = "Results/fecundity-OB1-10C.pdf", width = 7, height = 4)
OB1.fecundity.10
dev.off()
pdf(file = "Results/fecundity-OB2-6C.pdf", width = 7, height = 4)
OB2.fecundity.6
dev.off()
pdf(file = "Results/fecundity-OB2-10C.pdf", width = 7, height = 4)
OB2.fecundity.10
dev.off()





# Stocked & Setters, by population 

density4barplots.pops.long <- melt(density4barplots.pops, measure.vars = c("setters", "stocked.new"), id.vars = c("Date", "pH", "Temperature", "Population"))

NF.6.amb <- subset(density4barplots.pops, Population == "NF" & Temperature==6 & pH=="Ambient")
NF.6.low <- subset(density4barplots.pops, Population == "NF" & Temperature==6 & pH=="Low")
NF.10.amb <- subset(density4barplots.pops, Population == "NF" & Temperature==10 & pH=="Ambient")
NF.10.low <- subset(density4barplots.pops, Population == "NF" & Temperature==6 & pH=="Low")

HL.6.amb <- subset(density4barplots.pops, Population == "HL" & Temperature==6 & pH=="Ambient")
HL.6.low <- subset(density4barplots.pops, Population == "HL" & Temperature==6 & pH=="Low")
HL.10.amb <- subset(density4barplots.pops, Population == "HL" & Temperature==10 & pH=="Ambient")
HL.10.low <- subset(density4barplots.pops, Population == "HL" & Temperature==6 & pH=="Low")

SN.6.amb <- subset(density4barplots.pops, Population == "SN" & Temperature==6 & pH=="Ambient")
SN.6.low <- subset(density4barplots.pops, Population == "SN" & Temperature==6 & pH=="Low")
SN.10.amb <- subset(density4barplots.pops, Population == "SN" & Temperature==10 & pH=="Ambient")
SN.10.low <- subset(density4barplots.pops, Population == "SN" & Temperature==6 & pH=="Low")

K.6.amb <- subset(density4barplots.pops, Population == "K" & Temperature==6 & pH=="Ambient")
K.6.low <- subset(density4barplots.pops, Population == "K" & Temperature==6 & pH=="Low")
K.10.amb <- subset(density4barplots.pops, Population == "K" & Temperature==10 & pH=="Ambient")
K.10.low <- subset(density4barplots.pops, Population == "K" & Temperature==6 & pH=="Low")

density.list <- list(NF.6.amb, NF.6.low, NF.10.amb, NF.10.low, HL.6.amb, HL.6.low, HL.10.amb, HL.10.low, SN.6.amb, SN.6.low, SN.10.amb, SN.10.low, K.6.amb, K.6.low, K.10.amb, K.10.low)
names(density.list) <- c("NF.6.amb", "NF.6.low", "NF.10.amb", "NF.10.low", "HL.6.amb", "HL.6.low", "HL.10.amb", "HL.10.low", "SN.6.amb", "SN.6.low", "SN.10.amb", "SN.10.low", "K.6.amb", "K.6.low", "K.10.amb", "K.10.low")


density.names <- c("Fidalgo Bay 6°C, Amb pH", "Fidalgo Bay 6°C, Low pH", "Fidalgo Bay 10°C, Amb pH", "Fidalgo Bay 10°C, Low pH", "Dabob Bay 6°C, Amb pH", "Dabob Bay 6°C, Low pH", "Dabob Bay 10°C, Amb pH", "Dabob Bay 10°C, Low pH", "Oyster Bay C1 6°C, Amb pH", "Oyster Bay C1 6°C, Low pH", "Oyster Bay C1 10°C, Amb pH", "Oyster Bay C1 10°C, Low pH", "Oyster Bay C2 6°C, Amb pH", "Oyster Bay C2 6°C, Low pH", "Oyster Bay C2 10°C, Amb pH", "Oyster Bay C2 10°C, Low pH")
mindate <- min(density4barplots.pops$Date)
maxdate <- max(density4barplots.pops$Date)

# left edge:      1,5,9
# right edge      4,8,12
# middles:        2,3,6,7,10,11
# middle bottom:  14,15 
# left bottom:    13
# right bottom:   16
left <- c(1,5,9)
right <- c(4,8,12)
middle <- c(2,3,6,7,10,11)
mid.bot <- c(14,15)
left.bot <- c(13)
right.bot <- c(16)

secaxis <- c(5,5,5,5,4,4,4,4,10,10,10,10,3,3,3,3)
yaxis.max <- vector()
for (i in 1:length(density.list)){
  yaxis.max[i] <- max(density.list[[i]]$stocked.new, na.rm=T)
}
yaxis.max[1:4] <- max(yaxis.max[1:4])
yaxis.max[5:8] <- max(yaxis.max[5:8])
yaxis.max[9:12] <- max(yaxis.max[9:12])
yaxis.max[13:16] <- max(yaxis.max[13:16])



larval.plots <- list()

for (i in left){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),
                          max=maxdate), breaks=NULL) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=comma,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(~./15,name=NULL, breaks = NULL))
  larval.plots[[i]] <- a
}
for (i in right){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate), breaks=NULL) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(~./15, name=NULL, label=comma))
  larval.plots[[i]] <- a
}
for (i in middle){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate), breaks=NULL) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in mid.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in left.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=comma,limits = c(min=0, max=yaxis.max[i]),
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in right.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1.5), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL, breaks=NULL, limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(~./15, name=NULL, label=comma))
  larval.plots[[i]] <- a
}
do.call("grid.arrange", c(larval.plots, ncol=4))





larval.plots <- list()
ggplot(data=subset(density4barplots.pops, Population=="HL")) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                          fill="gray60", aes(x=Date, y=settera)) + 
  ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
  theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
  scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),
                        max=maxdate), breaks=NULL) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
  scale_y_continuous(label=comma,limits = c(min=0, max=yaxis.max[i]), 
                     sec.axis = sec_axis(~./15,name=NULL, breaks = NULL))

for (i in left){
  a <- 
  larval.plots[[i]] <- a
}
for (i in right){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate), breaks=NULL) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(~./15, name=NULL, label=comma))
  larval.plots[[i]] <- a
}
for (i in middle){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate), breaks=NULL) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in mid.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL,breaks=NULL,limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in left.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=comma,limits = c(min=0, max=yaxis.max[i]),
                       sec.axis = sec_axis(label=NULL,~./15, name=NULL, breaks=NULL))
  larval.plots[[i]] <- a
}
for (i in right.bot){
  a <- ggplot(data=density.list[[i]]) + geom_bar(stat="identity",width=1.5, position = position_dodge(width=1), 
                                                 fill="gray60", aes(x=Date, y=stocked.new)) + 
    ylab(label=element_blank()) + xlab(label=element_blank()) + ggtitle(density.names[i]) + theme_bw(base_size = 8) + 
    theme(plot.title = element_text(face = 'bold',size = 10, hjust = 0, colour = "gray50"), plot.margin=unit(c(0,0,0,0), "lines")) +
    scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
                 limits=c(min=min(subset(density.list[[i]], stocked.new>0)$Date-1),max=maxdate)) +
    theme(legend.position = "none", panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat="identity",width=1.5, position = position_dodge(width=1.5), fill="gray20", aes(x=Date, y=setters*15)) + 
    scale_y_continuous(label=NULL, breaks=NULL, limits = c(min=0, max=yaxis.max[i]), 
                       sec.axis = sec_axis(~./15, name=NULL, label=comma))
  larval.plots[[i]] <- a
}
do.call("grid.arrange", c(larval.plots, ncol=4))


### 6C groups 

all.fecundity <- ggplot(data=fecundity, aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("All Populations\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity$Date)-1,max=max(density4barplots$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))


# Stacked barplots, color coded by population and two separate ones by pH 
fecundity.pop$Population <- factor(fecundity.pop$Population, levels=c("NF", "HL", "SN", "K"))

# 6C Ambient pH 
pdf(file = "Results/ambpH-larval-time.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Ambient"), aes(x=Date, y=total.released, fill=Population)) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Ambient pH larval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, vjust = -.7, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels ="%b-%d",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(fecundity.pop$Date)+1)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=max(fecundity.pop$total.released)*1.7)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  geom_line(data = subset(fecundity, pH=="Ambient"), aes(x=Date, y=smooth(Tot.Larvae)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()

# 6C Low pH 
pdf(file = "Results/lowpH-larval-time.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Low"), aes(x=Date, y=total.released, fill=Population)) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Ambient pH larval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, vjust = -.7, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels ="%b-%d",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(fecundity.pop$Date)+1)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=max(fecundity.pop$total.released)*1.7)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  geom_line(data = subset(fecundity, pH=="Low"), aes(x=Date, y=smooth(Tot.Larvae)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()



# Stacked barplots, color coded by temperature and two separate ones by pH 

# Ambient pH by temperature
pdf(file = "Results/ambpH-larval-time-temp.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Ambient"), aes(x=Date, y=total.released, fill=Temperature)) + 
  geom_bar(stat="identity",width=1, col="gray60", position = position_dodge(width=2)) + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Ambient pH larval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, vjust = -.7, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels ="%b-%d",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(fecundity.pop$Date)+1)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=max(fecundity.pop$total.released)*1.1)) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("#67a9cf", "#ef8a62"), label=c("6°C pre-treatment", "10°C pre-treatment")) + 
  geom_line(data = subset(fecundity, pH=="Ambient"), aes(x=Date, y=smooth(Tot.Larvae)*1.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()

# Low pH by temperature
pdf(file = "Results/lowpH-larval-time-temp.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Low"), aes(x=Date, y=total.released, fill=Temperature)) + 
  geom_bar(stat="identity",width=1, col="gray60", position = position_dodge(width=2)) + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Low pH larval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 14, hjust = 0, vjust = -.7, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels ="%b-%d",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(fecundity.pop$Date)+1)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=max(fecundity.pop$total.released)*1.1)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("#67a9cf", "#ef8a62"), label=c("6°C pre-treatment", "10°C pre-treatment")) + 
  geom_line(data = subset(fecundity, pH=="Low"), aes(x=Date, y=smooth(Tot.Larvae)*1.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()





# --- Stacked barplots, color coded by population, four separate ones by pH & temp 

# 6C Ambient pH 
pdf(file = "Results/6ambpH-larval-time.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Ambient" & Temperature==6), aes(x=CalDay-131, y=total.released, fill=Population)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=(expression(paste("6°C / Ambient ", pCO[2], " larval release")))) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000)) +
  theme(legend.position = c(0.85, 0.8), legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(name="Cohort",values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) #+ 
#  geom_line(data = subset(fecundity, pH=="Ambient" & Temperature==6), aes(x=CalDay-131, y=smooth(Tot.Larvae, endrule="copy", do.ends = T)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()


# 6C Low pH 
pdf(file = "Results/6lowpH-larval-time.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Low" & Temperature==6), aes(x=CalDay-131, y=total.released, fill=Population)) + 
  theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=(expression(paste("6°C / High ", pCO[2], " larval release")))) +
  theme(plot.title = element_text(size = 16,  margin = margin(t = 30, b = -45), colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) #+ 
#  geom_line(data = subset(fecundity, pH=="Low" & Temperature==6), aes(x=CalDay-131, y=smooth(Tot.Larvae, endrule="copy", do.ends = T)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()


# 10 ambient pH 
pdf(file = "Results/10ambpH-larval-time.pdf", width = 7, height = 4)
ggplot(data=subset(fecundity.pop, pH=="Ambient" & Temperature==10), aes(x=CalDay-131, y=total.released, fill=Population)) + 
  theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + 
  labs(title=(expression(paste("10°C / Ambient ", pCO[2], " larval release")))) +
  theme(plot.title = element_text(size = 16,  margin = margin(t = 30, b = -45), colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) #+ 
#  geom_line(data = subset(fecundity, pH=="Ambient" & Temperature==10), aes(x=CalDay-131, y=smooth(Tot.Larvae, endrule="copy", do.ends = T)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()

# 10 low pH 
pdf(file = "Results/10lowpH-larval-time.pdf", width = 7, height = 4.5)
ggplot(data=subset(fecundity.pop, pH=="Low" & Temperature==10), aes(x=CalDay-131, y=total.released, fill=Population)) + theme_bw(base_size = 14) + 
  geom_bar(stat="identity",width=1, col="gray60") + xlab(label="Days after larval release onset") + 
  labs(title=(expression(paste("10°C / High ", pCO[2], " larval release")))) +
  theme(plot.title = element_text(size = 16, margin = margin(t = 30, b = -45), colour = "gray30"), 
        axis.title.y = element_blank(), axis.title.x = element_text(colour="gray30"), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_continuous(breaks = c(0,7,14,21,28,35,42,49,56)) +
  scale_y_continuous(label=comma, limits=c(min=0,max=1000000), breaks=c(0, 250000, 500000, 750000)) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9"), label=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) #+ 
#  geom_line(data = subset(fecundity, pH=="Low" & Temperature==10), aes(x=CalDay-131, y=smooth(Tot.Larvae, endrule="copy", do.ends = T)*2.5), col="gray40", size=1, linetype="dashed", inherit.aes = F)
dev.off()


# old scatter plot colors 


# Release magnitude  (550x375)
pdf(file = "Results/release-mag-scatter.pdf", width = 5.5, height = 3.75)
ggplot(spawning_group_sum, aes(x=release.days, y=overall_Total)) +
  geom_point(size=4, aes(shape=pH, colour=Population.Temp), position = position_jitter(w = 0.3, h = 0)) + scale_color_manual(values=c("#d73027","#f46d43","#fdae61","#fee090","#4575b4", "#74add1","#abd9e9","#e0f3f8"), labels=c("Fidalgo Bay-6°C","Fidalgo Bay-10°C","Dabob Bay-6°C", "Dabob Bay-10°C","Oyster Bay C1-6°C", "Oyster Bay C1-10°C","Oyster Bay C2-6°C","Oyster Bay C2-10°C")) +
  ggtitle("Larval release magnitude") + 
  theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"),
        legend.position = "none", axis.title.x = element_text(colour="gray30", size=14), axis.text.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14)) +
  xlab("No. release days (~families)") + ylab("Cumulative larvae released") +
  scale_x_continuous(limits=c(min=0,max=max(spawning_group_sum$release.days))) +
  scale_y_continuous(limits=c(min=0,max=max(spawning_group_sum$overall_Total)), label=comma) 
dev.off()

# values=c("darkseagreen3","seagreen4","skyblue2","skyblue4", "goldenrod1", "goldenrod3","coral1","coral4")

# Release Timing (590x375)
pdf(file = "Results/release-timing-scatter.pdf", width = 7.45, height = 3.75)
ggplot(spawning_group_sum, aes(x=first.big, y=maxday)) +
  geom_point(size=4, aes(shape=pH, colour=Population.Temp), position = position_jitter(w = 0.8, h = 0.8)) + scale_color_manual(name="Cohort & Temperature", values=c("#d73027","#f46d43","#fdae61","#fee090", "#4575b4", "#74add1","#abd9e9","#e0f3f8"), labels=c("Fidalgo Bay-6°C","Fidalgo Bay-10°C","Dabob Bay-6°C", "Dabob Bay-10°C","Oyster Bay C1-6°C", "Oyster Bay C1-10°C","Oyster Bay C2-6°C","Oyster Bay C2-10°C")) + scale_shape_manual(values=c(19, 17), labels=c("Ambient", "High"), name=expression(pCO[2])) +
  ggtitle("Larval release timing") + 
  theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"), axis.text.x = element_text(colour="gray30", size=14), axis.title.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14), legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14)) +
  xlab("Cal. day of first big release") + ylab("Cal. day of maximum release")
dev.off()