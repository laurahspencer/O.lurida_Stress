# Multivariate analysis, part 1 

oly.multivar <- read.csv("Analyses/multivariate-summary-table-adj.csv", header=T, stringsAsFactors = F)
str(oly.multivar)
oly.multivar[c("Population", "Temperature", "PH", "GROUP")] <- lapply(oly.multivar[c("Population", "Temperature", "PH", "GROUP")], factor)

# Inspect correlation between variables, to reduce # variables 
pairs(oly.multivar[c("Total.Spawned", "Days.Stocked", "Larvae.stocked.adjusted", "Mean.larvae.stocked", "Setters.stocked", "survival.setters", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.stocked", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass")], lower.panel=panel.smooth, upper.panel=panel.cor)  

# Remove variables that are highly correlated with other variables, to reduce # (total.spawned, days stocked, larvae stocked)
pairs(oly.multivar[c("Mean.larvae.stocked", "survival.setters","Setters.stocked", "survival.postset", "Juv.stocked", "Juv.meanlength", "Deployment.survival", "Deployment.mean.growth", "Deployment.mean.mass")], lower.panel=panel.smooth, upper.panel=panel.cor) #example result 

# Next step ... normality? 

