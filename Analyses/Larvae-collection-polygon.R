#polygon plots with larval release data 

#create a blank plot with the right dimensions (xlim, ylim)
par(mfcol=c(2,2), oma=c(1,2,2,1), mar=c(4,4,0,0))

plot(1,type="n",ann=F, 
     xlim=c(0,60),ylim=c(0,6e5),
     yaxs="i", xaxs="i")
polygon(x=c(subset(all_total, Treatment=="Unchilled, Ambient pH")$CalDay-131,60,0), 
        y=c(subset(all_total, Treatment=="Unchilled, Ambient pH")$total.released,0,0), col="gray50", border="black")

plot(1,type="n",ann=F, 
     xlim=c(0,60),ylim=c(0,4e5),
     yaxs="i", xaxs="i")
polygon(x=c(subset(all_total, Treatment=="Unchilled, Low pH")$CalDay-131,60,0), 
        y=c(subset(all_total, Treatment=="Unchilled, Low pH")$total.released,0,0), col="lightsteelblue3", border="black")

plot(1,type="n",ann=F, 
     xlim=c(0,60),ylim=c(0,5e5),
     yaxs="i", xaxs="i")
polygon(x=c(subset(all_total, Treatment=="Chilled, Ambient pH")$CalDay-131,60,0), 
        y=c(subset(all_total, Treatment=="Chilled, Ambient pH")$total.released,0,0), col="gray32", border="black")

plot(1,type="n",ann=F, 
     xlim=c(0,60),ylim=c(0,8.2e5),
     yaxs="i", xaxs="i")
polygon(x=c(subset(all_total, Treatment=="Chilled, Low pH")$CalDay-131,60,0), 
        y=c(subset(all_total, Treatment=="Chilled, Low pH")$total.released,0,0), col="steelblue", border="black")
mtext(side=1,outer=T,line=-1,"Week")                 #label X AXIS on the outside of all plots 
mtext(side=2,outer=T,line=-1,"Larvae released")   #label Y AXIS on the outside of all plots

