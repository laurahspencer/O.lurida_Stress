##### Map with bay locations 

# Import deployment latitude and longitude data
locationCords <- read.csv("Data/Population-origin-coordinates.csv", header = T, stringsAsFactors = F) #Import outplant coordinate information
locationCords <- locationCords[order(locationCords$Lat.dec),] #Reorder location coordinates by latittude (south to north)
#marker1 = c("sienna1", "goldenrod1", "steelblue2", "royalblue3") #marker colors for each bay, from south to north 
symbols <- c(23, 23, 23, 23) #symbol shapes for each bay, south to north 
data(nepacLLhigh) #Load set of polygons for the NE Pacific Ocean in high resolution from PBSmapping

# Create file to save map 
# svg(filename = "results/Deployment-map.svg") # uncomment/comment depending on which file type you want 
jpeg(filename = "Results/Population-origin-map.jpeg", height = 800, width = 800) 

# Create base map of coastal WA state 
plotMap(nepacLLhigh, xlim = c(-123.4, -121.95), ylim = c(46.95, 48.8), col = "gray92", bg = "gray85", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ann = FALSE) #Create a map with high resolution NE Pacific Ocean data. Remove axes since those will be manually added

# Modify base map 
axis(side = 1, at = c(-123, -122.5, -122), labels=c("123°W", "122.5°W", "122°W"), tick = TRUE, col.axis = "grey20") #Add longitude axis
axis(side = 2, at = c(46.5, 47, 47.5, 48, 48.5), labels=c("46.5ºN", "47°N", "47.5°N", "48°N", "48.5°N"), tick = TRUE, col.axis = "grey20") #Add latitude axis

#Add points to map 
for (i in 1:length(symbols)) {
  points(x = locationCords$Long.dec[i], y = locationCords$Lat.dec[i], pch= symbols[i], add = TRUE, bg="gray20", lwd=2, cex=4)
  text(x = (locationCords$Long.dec[i]), y = (locationCords$Lat.dec[i]), col="white", labels=locationCords$Population[i], font=2, cex=1.2)
}

# Add legend 
# legend("topleft", inset=0.05, legend=rev(locationCords$Site), cex=0.8, pt.cex = 1.5, pt.bg="gray20", bg="gray92", pch=rev(symbols), box.lty=1, box.lwd=1, box.col="black")

dev.off() #Turn off plotting device
