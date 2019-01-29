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

FB.fecundity <- ggplot(data=subset(fecundity.pop, Population == "NF"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

FB.stocked <- ggplot(data=subset(density4barplots.pops, Population=="NF"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

FB.eyed <- ggplot(data=subset(density4barplots.pops, Population=="NF"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae, Fidalgo Bay") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

pdf(file = "Results/Larval-plots-FB-6C.pdf")
grid.arrange(FB.fecundity, FB.stocked, FB.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# -------- 

DB.fecundity <- ggplot(data=subset(fecundity.pop, Population == "HL"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.stocked <- ggplot(data=subset(density4barplots.pops, Population=="HL"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

DB.eyed <- ggplot(data=subset(density4barplots.pops, Population=="HL"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

pdf(file = "Results/Larval-plots-DB-6C.pdf")
grid.arrange(DB.fecundity, DB.stocked, DB.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB1.fecundity <- ggplot(data=subset(fecundity.pop, Population == "SN"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#, axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()


OB1.stocked <- ggplot(data=subset(density4barplots.pops, Population=="SN"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB1.eyed <- ggplot(data=subset(density4barplots.pops, Population=="SN"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

pdf(file = "Results/Larval-plots-OB1-6C.pdf")
grid.arrange(OB1.fecundity, OB1.stocked, OB1.eyed, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB2.fecundity <- ggplot(data=subset(fecundity.pop, Population == "K"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 6°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.stocked <- ggplot(data=subset(density4barplots.pops, Population=="K"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

OB2.eyed <- ggplot(data=subset(density4barplots.pops, Population=="K"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

pdf(file = "Results/Larval-plots-OB2-6C.pdf")
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
               limits=c(min=min(fecundity.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

pdf(file = "Results/Larval-plots-FB-10C.pdf")
grid.arrange(FB.fecundity.10, FB.stocked.10, FB.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# -------- 

DB.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "HL"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

pdf(file = "Results/Larval-plots-DB-10C.pdf")
grid.arrange(DB.fecundity.10, DB.stocked.10, DB.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB1.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "SN"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(fecundity.pop.10$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#, axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()


OB1.stocked.10 <- ggplot(data=subset(density4barplots.pops.10, Population=="SN"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

pdf(file = "Results/Larval-plots-OB1-10C.pdf")
grid.arrange(OB1.fecundity.10, OB1.stocked.10, OB1.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()

# ----------

OB2.fecundity.10 <- ggplot(data=subset(fecundity.pop.10, Population == "K"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2, 10°C\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
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
               limits=c(min=min(fecundity.pop.10$Date)-1,max=max(density4barplots.pops.10$Date)+1)) +
  #scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops.10$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))
#,axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()

pdf(file = "Results/Larval-plots-OB2-10C.pdf")
grid.arrange(OB2.fecundity.10, OB2.stocked.10, OB2.eyed.10, ncol = 1, heights = c(1, 1, 1))
dev.off()
