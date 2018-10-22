# create master summary statistic df (for multivariate analysis)

View(Oly.size.summary3)

# Copy dataframes so I don't mess up the other ones 
Oly.size.summary3.c <- Oly.size.summary3
Survival.post.c <- Survival.post
Deploy.data.c <- Deploy.data
Deploy.growth.c <- Deploy.growth
Post.mass.c <- Post.mass

# CREATE COMMON GROUP ID IN EACH DATAFRAME 

head(Survival.post.c) #All larval summary stats (huge df)
Survival.post.c$pH <- gsub("Low", "LOW", Survival.post.c$pH)
Survival.post.c$pH <- gsub("Ambient", "AMB", Survival.post.c$pH)
Survival.post.c$GROUP <- paste(paste(Survival.post.c$Population, Survival.post.c$Temperature, sep=""), Survival.post.c$pH, sep="-")

head(Oly.size.summary3.c) #Juvenile summary stats after 10month growth
names(Oly.size.summary3.c)[1] <- "GROUP" #rename group to GROUP 

head(Deploy.data.c) # Juvenile deployment survival 
Deploy.data.c$PH <- gsub("Low", "LOW", Deploy.data.c$PH)
Deploy.data.c$PH <- gsub("Ambient", "AMB", Deploy.data.c$PH)
Deploy.data.c$GROUP <- Deploy.data.c$POPULATION 
Deploy.data.c$GROUP <- gsub("FB", "NF", Deploy.data.c$GROUP)
Deploy.data.c$GROUP <- gsub("HC", "HL", Deploy.data.c$GROUP)
Deploy.data.c$GROUP <- gsub("SSF1", "SN", Deploy.data.c$GROUP)
Deploy.data.c$GROUP <- gsub("SSF2", "K", Deploy.data.c$GROUP)
Deploy.data.c$GROUP <- paste(paste(Deploy.data.c$GROUP, "6", sep=""), Deploy.data.c$PH, sep="-")
#change group names to match the size.summary

head(Deploy.growth.c) #Juvenile deployment growth data 
# Create Group column 
Deploy.growth.c$PH.y <- gsub("Ambient", "AMB", Deploy.growth.c$PH.y)
Deploy.growth.c$PH.y <- gsub("Low", "LOW", Deploy.growth.c$PH.y)
Deploy.growth.c$GROUP <- paste(paste(Deploy.growth.c$COHORT, "6", sep=""), Deploy.growth.c$PH.y, sep="-")

head(Post.mass.c) # Juvenile deployment mass data 
Post.mass.c$PH <- gsub("Ambient", "AMB", Post.mass.c$PH)
Post.mass.c$PH <- gsub("Low", "LOW", Post.mass.c$PH)
Post.mass.c$GROUP <- Post.mass.c$POPULATION 
Post.mass.c$GROUP <- gsub("FB", "NF", Post.mass.c$GROUP)
Post.mass.c$GROUP <- gsub("HC", "HL", Post.mass.c$GROUP)
Post.mass.c$GROUP <- gsub("SSF1", "SN", Post.mass.c$GROUP)
Post.mass.c$GROUP <- gsub("SSF2", "K", Post.mass.c$GROUP)
Post.mass.c$GROUP <- paste(paste(Post.mass.c$GROUP, "6", sep=""), Post.mass.c$PH, sep="-")

# Inspect tables, do they need to be summarized? 
Survival.post.c #ready to go 
Deploy.data.c$Survival <- Deploy.data.c$SURVIVED/Deploy.data.c$DEPLOYED 
Deploy.survival <- aggregate(Survival ~ GROUP, Deploy.data.c, mean) #average survival by group (parental origin, temp, pH)
Deploy.growth.ave <- aggregate(Growth ~ GROUP, Deploy.growth.c, mean) #average growth/oyster by group (parental origin, temp, pH)
Deploy.mass.ave <- aggregate(Mass.post.per ~ GROUP, Post.mass.c, mean) #average mass change/oyster by group (parental origin, temp, pH) 
# Note: only have deployment data for 6C groups (did not deploy 10C groups)

# Add deployment stocking density (varied by population)
Deploy.stock <- aggregate(DEPLOYED ~ POPULATION + PH, Deploy.data, mean) #number deployed in each group
Deploy.stock$POPULATION <- gsub("FB", "NF", Deploy.stock$POPULATION)
Deploy.stock$POPULATION <- gsub("HC", "HL", Deploy.stock$POPULATION)
Deploy.stock$POPULATION <- gsub("SSF1", "SN", Deploy.stock$POPULATION)
Deploy.stock$POPULATION <- gsub("SSF2", "K", Deploy.stock$POPULATION)
Deploy.stock$PH <- gsub("Ambient", "AMB", Deploy.stock$PH)
Deploy.stock$PH <- gsub("Low", "LOW", Deploy.stock$PH)
Deploy.stock$GROUP <- paste(paste(Deploy.stock$POPULATION, "6", sep=""), Deploy.stock$PH, sep="-")

# ADD MEAN % BIWEEKLY SURVIVAL 

# merge to make master dataset 
require(plyr)
master <- join_all(list(Survival.post.c,
                        Oly.size.summary3.c,
                        Deploy.stock,
                        Deploy.survival,
                        Deploy.growth.ave,
                        Deploy.mass.ave), by = 'GROUP', type = 'full')

View(master)
plot(x=master$Mean.stocked, y=master$Survival)
plot(x=master$Mean.stocked, y=master$Mean, col=master$PH)

# Select candidate variables to include in multivariate analysis 

master.list <- master[c("Population", "Temperature", "PH", "GROUP", "Total.Spawned", "Days.Stocked", "Larvae.stocked.adjusted", "Mean.stocked", "Setters.stocked", "survival.setters", "survival.postset", "stocked",  "Mean", "DEPLOYED", "Survival", "Growth", "Mass.post.per")]

# Rename columns for clarity 
master.list <- setNames(master.list, c("Population", "Temperature", "PH", "GROUP", "Total.Spawned", "Days.Stocked", "Larvae.stocked.adjusted", "Mean.larvae.stocked", "Setters.stocked", "survival.setters", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.stocked", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass"))

# Save resulting dataframe as .csv 
write.csv(master.list, file="Analyses/multivariate-summary-table.csv")
# NOTE:  If wanting to include the SN mini-experiment, use the file called multivariate-summary-table-adj.csv (manually edited version)

write.csv(master, file="Analyses/master-summary-table.csv")
View(master.list)
aggregate(Days.Stocked ~ Population+PH, master.list, sum)
