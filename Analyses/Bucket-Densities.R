setwd("~/Documents/Roberts Lab/O.lurida_Stress")
Bucket.Densities <- read.csv("Data/Bucket-Densities.csv", header = T, stringsAsFactors = F, na.strings = " -   ")
library(plotly)
library(reshape2)
as.numeric(Bucket.Densities[,-1])
test <- as.numeric(as.character(unlist(Bucket.Densities[,-1])), na.action=na.omit)
str(Bucket.Densities[,-1])

Bucket.Densities.long <- melt(Bucket.Densities, id.vars = "Date") 
Density.plot <- plot_ly(data = Bucket.Densities.long, x = ~Date, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="2017 O. lurida Bucket Densities",
         yaxis = list(title = '# Larvae in Bucket'),
         legend = list(x=.75, y=.75))
