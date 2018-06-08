Bucket.Densities <- read.csv("Data/Bucket-Densities.csv", header = T, stringsAsFactors = F, na.strings = " -   ")
library(plotly)
library(reshape2)
colnames(Bucket.Densities) <- sub(".Dens", "", colnames(Bucket.Densities))

#as.numeric(Bucket.Densities[,-1])
#test <- as.numeric(as.character(unlist(Bucket.Densities[,-1])), na.action=na.omit)

# Groups with 2 larval buckets, and times:  
# NF10-Low, started new bucket on 5/24 
# NF10-Ambient, started new bucket on 5/25
# NF6-Low, started new bucket on 5/25
# HL10-Low, started new bucket on 5/26
# SN10-Low, started new bucket on 5/26
# SN10-Ambient, started new bucket on 6/4

# As of 6/9, only 1 bucket per treatment group in bucket counts, so on previous screening day (6/5) conjoined all 2-bucket systems. 

Bucket.Densities$NF10Low.b



head(Bucket.Densities)

Bucket.Densities.long <- na.omit(melt(Bucket.Densities, id.vars = "Date"))
head(Bucket.Densities.long)

Density.plot <- plot_ly(data = Bucket.Densities.long, x = ~Date, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="2017 O. lurida Bucket Densities",
         yaxis = list(title = '# Larvae in Bucket'),
         legend = list(x=.75, y=.75))
