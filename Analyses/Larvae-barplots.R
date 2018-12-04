#### ------------------------ ALL POPULATIONS 

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide,  Temperature==6), aes(x=Date, y=stocked, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# new larvae") +
  ggtitle("New larvae stocked over time, by parental pH") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of setters 
ggplot(data=subset(Bucket.Densities.wide,  Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae >224um") +
  ggtitle("Eyed larvae over time, by parental pH") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae stocked  
ggplot(data=subset(Bucket.Densities.wide, Temperature==6), aes(x=Date, y=expected, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae stocked") +
  ggtitle("Larvae stocked in tanks over time, by parental pH") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide, Population !="NF" & Temperature==6), aes(x=Date, y=actual, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# live larvae") +
  ggtitle("Biweekly larval counts over time, by parental pH") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))






#### ------------------------ FIDALGO BAY 

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide,  Population =="NF" & Temperature==6), aes(x=Date, y=stocked, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# new larvae") +
  ggtitle("New larvae stocked over time, by parental pH\nFidalgo Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of setters 
ggplot(data=subset(Bucket.Densities.wide, Population=="NF" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae >224um") +
  ggtitle("Eyed larvae over time, by parental pH\nFidalgo Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae stocked  
ggplot(data=subset(Bucket.Densities.wide, Population=="NF" & Temperature==6), aes(x=Date, y=expected, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae stocked") +
  ggtitle("Larvae stocked in tanks over time, by parental pH\nFidalgo Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide, Population=="NF" & Temperature==6), aes(x=Date, y=actual, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# live larvae") +
  ggtitle("Biweekly larval counts over time, by parental pH\nFidalgo Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

#### ------------------------ DABOB BAY

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide,  Population=="HL" & Temperature==6), aes(x=Date, y=stocked, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# new larvae") +
  ggtitle("New larvae stocked over time, by parental pH\nDabob Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of setters 
ggplot(data=subset(Bucket.Densities.wide, Population=="HL" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae >224um") +
  ggtitle("Eyed larvae over time, by parental pH\nDabob Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae stocked  
ggplot(data=subset(Bucket.Densities.wide, Population=="HL" & Temperature==6), aes(x=Date, y=expected, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae stocked") +
  ggtitle("Larvae stocked in tanks over time, by parental pH\nDabob Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide, Population=="HL" & Temperature==6), aes(x=Date, y=actual, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# live larvae") +
  ggtitle("Biweekly larval counts over time, by parental pH\nDabob Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

###### -------------- OYSTER BAY F1

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide,  Population=="SN" & Temperature==6), aes(x=Date, y=stocked, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# new larvae") +
  ggtitle("New larvae stocked over time, by parental pH\nOyster Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of setters 
ggplot(data=subset(Bucket.Densities.wide, Population=="SN" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae >224um") +
  ggtitle("Eyed larvae over time, by parental pH\nOyster Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae stocked  
ggplot(data=subset(Bucket.Densities.wide, Population=="SN" & Temperature==6), aes(x=Date, y=expected, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae stocked") +
  ggtitle("Larvae stocked in tanks over time, by parental pH\nOyster Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide, Population=="SN" & Temperature==6), aes(x=Date, y=actual, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# live larvae") +
  ggtitle("Biweekly larval counts over time, by parental pH\nOyster Bay") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

###### -------------- OYSTER BAY F2

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide,  Population=="K" & Temperature==6), aes(x=Date, y=stocked, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# new larvae") +
  ggtitle("New larvae stocked over time, by parental pH\nOyster Bay F2") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold")) +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of setters 
ggplot(data=subset(Bucket.Densities.wide, Population=="K" & Temperature==6), aes(x=Date, y=setters, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae >224um") +
  ggtitle("Eyed larvae over time, by parental pH\nOyster Bay F2") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.15, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae stocked  
ggplot(data=subset(Bucket.Densities.wide, Population=="K" & Temperature==6), aes(x=Date, y=expected, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# larvae stocked") +
  ggtitle("Larvae stocked in tanks over time, by parental pH\nOyster Bay F2") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))

# barplot of # larvae counted during biweekly screenings/counts   
ggplot(data=subset(Bucket.Densities.wide, Population=="K" & Temperature==6), aes(x=Date, y=actual, fill=pH)) + 
  geom_bar(stat="identity",width=1.5, position = position_dodge(width=2)) + ylab("# live larvae") +
  ggtitle("Biweekly larval counts over time, by parental pH\nOyster Bay F2") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0), legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title = element_text(size=18, face = "bold"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d") +
  theme(legend.position = c(0.85, 0.85)) + scale_fill_manual(values=c("gray70", "lightsteelblue"))
