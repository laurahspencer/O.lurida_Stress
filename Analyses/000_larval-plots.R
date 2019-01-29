all.fecundity <- ggplot(data=fecundity, aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("All Populations\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity$Date)-1,max=max(density4barplots$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity$Tot.Larvae))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots$stocked.tot, na.rm = T))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(all.fecundity, all.larvae, all.eyed, ncol = 1, heights = c(1, 1, 1))

# For each population separately 

FB.fecundity <- ggplot(data=subset(fecundity.pop, Population == "NF"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Fidalgo Bay\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(FB.fecundity, FB.stocked, FB.eyed, ncol = 1, heights = c(1, 1, 1))

# -------- 

DB.fecundity <- ggplot(data=subset(fecundity.pop, Population == "HL"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Dabob Bay\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
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
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(DB.fecundity, DB.stocked, DB.eyed, ncol = 1, heights = c(1, 1, 1))

# ----------

OB1.fecundity <- ggplot(data=subset(fecundity.pop, Population == "SN"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C1\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = c(0.85, 0.85), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

OB1.stocked <- ggplot(data=subset(density4barplots.pops, Population=="SN"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

OB1.eyed <- ggplot(data=subset(density4barplots.pops, Population=="SN"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(OB1.fecundity, OB1.stocked, OB1.eyed, ncol = 1, heights = c(1, 1, 1))

# ----------

OB2.fecundity <- ggplot(data=subset(fecundity.pop, Population == "K"), aes(x=Date, y=Tot.Larvae, fill=pH)) + 
  geom_bar(stat="identity",width=1, position = position_dodge(width=2), col="gray60") + 
  ylab("No. of larvae") + xlab(label=element_blank()) + ggtitle("Oyster Bay C2\nLarval release") + theme_bw(base_size = 12) + 
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(fecundity.pop$Tot.Larvae))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

OB2.stocked <- ggplot(data=subset(density4barplots.pops, Population=="K"), aes(x=Date, y=stocked.tot, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  #geom_errorbar(aes(ymin=stocked.tot, ymax=stocked.tot+stocked.sd),width=.1, position=position_dodge(width=2), col="gray30") +
  ggtitle("Larvae stocked") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$stocked.tot, na.rm = T))) +
  theme(legend.position = "none",  panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

OB2.eyed <- ggplot(data=subset(density4barplots.pops, Population=="K"), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2), col="gray60") + ylab("No. of larvae") + xlab(label=element_blank()) +
  ggtitle("Eyed larvae") + theme_bw(base_size = 12) +   
  theme(plot.title = element_text(face = 'bold',size = 12, hjust = 0, colour = "gray30"), 
        axis.title = element_blank()) +
  scale_x_date(date_breaks = "2 weeks",date_labels ="%b-%d", 
               limits=c(min=min(fecundity.pop$Date)-1,max=max(density4barplots.pops$Date)+1)) +
  scale_y_continuous(limits=c(min=0,max=max(density4barplots.pops$setters, na.rm = T))) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), panel.border = element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("gray70", "steelblue"))

grid.arrange(OB2.fecundity, OB2.stocked, OB2.eyed, ncol = 1, heights = c(1, 1, 1))
