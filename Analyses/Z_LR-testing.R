# Linear plots & models 

# was mean stocking density a function of total larvae released per group? 
test <- merge(spawning_group_sum, aggregate(value ~ Population+Treatment, data=Bucket.Densities.long, mean), by.x = c("Population", "Treatment"), by.y = c("Population", "Treatment"))
plot(x=test$overall_Total, y=test$value)
test$Treatment <- as.factor(test$Treatment)

# was survival a function of total released per group?
Pops <- c("NF", "NF","NF","NF", "SN", "SN", "SN", "SN", "HL","HL","HL","HL", "K","K","K","K")
Trts <- c("10 Ambient", "10 Low", "6 Ambient", "6 Low", "10 Ambient", "10 Low", "6 Ambient", "6 Low","10 Ambient", "10 Low", "6 Ambient", "6 Low","10 Ambient", "10 Low", "6 Ambient", "6 Low")
Stocked <- as.numeric(c( 319277,211417,131675,391716,750793,660450,477403,532417,283858,310875,65667,255371, 324165,225873,353768, 179717))
Stocked.adj <- as.numeric(c(274464, 210907, 131005, 390136, 708904, 646529, 435238, 515788, 269869, 300065, 48929, 227711, 314438, 221487, 221487, 171998))
Setters <- as.numeric(c(3698, 2977, 11119, 11780, 29595, 4757, 11931, 6029,13917,19858, 2496, 10686, 13932,  2106, 22186, 9735 ))
Juvenile <- as.numeric(c(626,75,1503,670,52,34,124,192,1311,1091,501,834,246,113,356,334))


test2 <- cbind.data.frame(Pops,Trts,Stocked,Stocked.adj,Setters,Juvenile)
str(test2)

test3 <- merge(test, test2, by.x=c("Population", "Treatment"), by.y=c("Pops", "Trts"))
str(test3)

plot(x=test3$Stocked, y=test3$Setters, col=test3$Population, main="#Setters ~ #Stocked")
plot(x=test3$Stocked, y=test3$Juvenile, col=test3$Population, main="#Juvenile ~ #Stocked")
plot(x=test3$Setters, y=test3$Juvenile, col=test3$Population, main="#Juvenile ~ #Setters")
plot(x=test3$overall_Total, y=test3$Juvenile, col=test3$Population, main="#Juvenile ~ #Spawned")
plot(x=test3$overall_Total, y=test3$Setters, col=test3$Population, main="#Setters ~ #Spawned")
plot(x=test3$Stocked, y=(test3$Setters/test3$Stocked.adj), col=test3$Population, main="%Survive to Setters ~ #Stocked")
plot(x=test3$value, y=(test3$Setters/test3$Stocked.adj), col=test3$Treatment, main="%Survive to Setters ~ #Ave. Bucket Density")
plot(x=test3$Setters, y=(test3$Juvenile/test3$Setters), col=test3$Population,main="%Survive to Setters -> Juvenile ~ #Juvenile Stocked")

# Survival data: Larvae -> Setters 

# Is larvae -> setter survival dependent on mean stocking density alone? 
set.survival.density <- aov((I(Setters/Stocked.adj)*100) ~ value, data=test3) #no 
summary(set.survival.density) 

# Is larvae -> setter survival dependent on population alone? 
set.survival.pop <- aov((I(Setters/Stocked.adj)*100) ~ Population, data=test3)
summary(set.survival.pop)

# Is larvae -> setter survival dependent on treatment alone? 
set.survival.trt <- aov((I(Setters/Stocked.adj)*100) ~ Treatment, data=test3)
summary(set.survival.trt) #yes 

# Covariates combined 
# Is larvae -> setter survival dependent on pop & treatment alone? 
set.survival.poptrt <- aov((I(Setters/Stocked.adj)*100) ~ Population+Treatment, data=test3)
summary(set.survival.poptrt) #yes 

anova(glm(cbind((test3$Stocked.adj-test3$Setters), test3$Setters) ~ Population+Treatment, family=binomial(link="logit"), data=test3, weights = test3$Stocked.adj))

glm(cbind(Dead, Alive) ~ Concentration, family=binomial(link="logit"), data=data5)

# was survival a function of stocking density? 
