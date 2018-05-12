## Larval release plots, adapted from Katherine Silliman R code 
## Oly 2017 project 

library(ggplot2) #for plotting
library(dplyr) #Data summary
library(plotrix)#for SE calculation
library("grid")#plotting
library(gridExtra) #plotting
library(scales) #plotting

larvae <- read.csv("Data/Spawning-Data.csv", header = TRUE, stringsAsFactors = T)
head(larvae)
pop_larvae <- subset(larvae, select = c(Date,Group, Population,Treatment,Tot.Larvae)) %>% group_by(Date,Population,Treatment)
pop_larvae$Date <- as.Date(pop_larvae$Date, "%m/%d/%y")
pop_larvae <- arrange(pop_larvae, Date) %>% mutate(CalDay = format(Date,"%j"))

#Calculate total larvae released across families
pop_total <- summarise(pop_larvae, total.by.date = sum(Tot.Larvae))

#Get the number of larvae produced per oyster "percap"
percap.norms <- function(x, z) {  #write function
  if((x == "NF") & (z=="10-Ambient")) y <- 29
  if((x == "NF") & (z=="10-Low")) y <- 28
  if((x == "NF") & (z=="6-Ambient")) y <- 29
  if((x == "NF") & (z=="6-Low")) y <- 29
  
  
  return(y) #return result
}


if(x == "SN-10 Ambient A") y <- 17
if(x == "SN-10 Ambient B") y <- 17 
if(x == "SN-10 Low A") y <- 15
if(x == "SN-10 Low B") y <- 15 
if(x == "SN-6 Ambient A") y <- 15 
if(x == "SN-6 Ambient B") y <- 16 
if(x == "SN-6 Low A") y <- 17
if(x == "SN-6 Low B") y <- 17

if(x == "HL-10 Ambient") y <- 9
if(x == "HL-10 Low") y <- 16
if(x == "HL-6 Ambient") y <- 14
if(x == "HL-6 Low") y <- 15 
if(x == "K-10 Ambient") y <- 115
if(x == "K-10 Low") y <- 111
if(x == "K-6 Ambient") y <- 117
if(x == "K-6 Low") y <- 126

pop_total$percap.norm <- as.numeric(sapply(pop_total, percap.norms, x=pop_total$Population, z=pop_total$Treatment))

pop_total$total.per.cap <- pop_total$total.by.date/pop_total$percap.norm 

#Calculate cumulative larvae released through time
pop_total <- group_by(pop_total, Population) %>% mutate(cum.total=cumsum(total.by.date),cum.percap = cumsum(total.per.cap),CalDay = format(Date,"%j")) %>% arrange(Date) %>% select(Date,CalDay,Population,total.by.date,total.per.cap,cum.total,cum.percap)

#Overlay graph with color
bb <- ggplot(data=pop_total, aes(x=Date, y=total.per.cap, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larvae Released") +
  ggtitle("Timing of Larvae Release by Population") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

bl <- bb+ geom_line(data=pop_total, aes(x=Date, y=cum.percap/5, group=Population, colour=Population),size=1.2) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*5,name="Cumulative Larvae Released"), label=comma)

jpeg(filename = "Figure1_Reproduction.jpeg")
grid.newpage()
bl
dev.off()

setEPS()
postscript("Figure1_Reproduction.eps")
bl
dev.off()
# Using buckets as replicates

family_larvae <- group_by(pop_larvae,Family) %>% filter(Family !="")
fam.sum <- summarize(family_larvae, overall_Total = sum(Total.Larvae, na.rm = T), 
                     mean.larvae = mean(Total.Larvae,na.rm=T), 
                     se.larvae = std.error(Total.Larvae,na.rm=T), 
                     mean.percap = mean(Total.per.capita,na.rm=T), 
                     total.percap = sum(Total.per.capita,na.rm=T), 
                     maxday = as.numeric(CalDay[which.max(Total.Larvae)]), 
                     max = max(Total.Larvae), max.percap = max(Total.per.capita), 
                     Population = first(Population), 
                     first.big = as.numeric(CalDay[which(Total.Larvae > 0)[1]]))

#Cumulative larvae violin plots, with bucket as replicate
tc.plot <- ggplot(fam.sum, aes(x=Population, y=total.percap,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Cumulative Larvae",y=expression("Cumulative Larvae")) + 
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_y_continuous(label=comma)

#First day of larval release violin plots, with bucket as replicate
fd.plot <- ggplot(fam.sum, aes(x=Population, y=first.big,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Date of First Larval Release",y="Calendar Day") + 
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))

tc.plot <- tc.plot + theme(legend.position="none",legend.text = element_text(size = 18)) + labs(title="a. Cumulative Larvae") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
fd.plot <- fd.plot + labs(title="b. Date of First Larval Release") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#jpeg(filename = "Cum_1stDay_Comb_PS2015_color.jpeg", width = 1156, height = 400)
grid.arrange(tc.plot,fd.plot, ncol = 2, widths = c(1.68,2))
dev.off()