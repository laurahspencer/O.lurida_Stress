new.length <- read.csv("data/New_larvae-measurements.csv", header=T, stringsAsFactors = F)[c(1:7)]
new.length$Sample <- as.factor(new.length$Sample)
plot(x=new.length$Sample, y=new.length$mm)
str(Collection)
new.length.ann <- merge(x=new.length, y=Collection[,c("Group", "Sample.number")], by.x="Sample", by.y="Sample.number", all.x=T, all.y=F)
new.length.ann <- merge(x=new.length.ann, y=survival[,c("Date.stocked", "Family", "Rep", "TRT", "TRT.REP", "Live.35.days", "Live.50.days", "TEMP", "FOOD", "TREAT")], by.x="Group", by.y="Family")


length.surv <- aggregate(mm ~ Sample + Live.50.days + Length.Width, new.length.ann, mean)
length.surv$sd <- aggregate(mm ~ Sample + Live.50.days + Length.Width, new.length.ann,  sd)[4]
colnames(length.surv[,5]) <- "sd"
plot(x=1000*subset(length.surv, Length.Width=="length")$mm, y=subset(length.surv, Length.Width=="length")$Live.50.days/(800*3), ylab="% Survival to Postset", xlab="Shell length upon release (um)", pch=16, cex=1.4, col="gray45")

plot(x=1000*subset(length.surv, Length.Width=="width")$mm, y=subset(length.surv, Length.Width=="width")$Live.50.days/(800*3), ylab="% Survival to Postset", xlab="Shell width upon release (um)", pch=16, cex=1.4, col="gray45")
length.surv$no.in <- 800

length.surv$perc.surv <- length.surv$Live.50.days/(800*3)
anova(glm(cbind(Live.50.days, no.in) ~ mm, data=subset(length.surv, Length.Width=="length"), binomial), test="Chi")
anova(glm(cbind(Live.50.days, no.in) ~ mm, data=subset(length.surv, Length.Width=="width"), binomial), test="Chi")

