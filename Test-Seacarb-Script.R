install.packages("seacarb")
n
library("seacarb")

day <- c(5, 33, 48)
temp.low <- c(mean(10.65, 10.86, 10.9), mean(11.74, 10.68, 11.11), mean(10.92, 10.96, 11.09))
temp.ambient <- c(mean(10.2, 10.22, 10.25), mean(11.46, 11.2, 11.4), mean(11.75, 11.81, 11.99))
salinity.low <- c(mean(28.3, 28.1, 28.0), mean(28.2, 28.1, 282.2), mean(26.4, 26.5, 26.5))
salinity.ambient <- c(mean(28.1, 28.1, 28.2), mean(28.3, 28.2, 28.2), mean(26.5, 26.5, 26.5))
pH.ambient <- c(7.82, 7.81, 7.82) #unit = pH(total)
pH.low <- c(7.33, 7.31, 7.29) #unit = pH(total)
ta.ambient <- c(2307, 2747, 2611) #unit = µmol/kg, need to convert to mol/kg
ta.low <- c(2332, 2918, 2808) #unit = µmol/kg, need to convert to mol/kg 

# pH and AT are known:
test.pCO2.ambient <- carb(flag=8, var1=pH.ambient, var2=(ta.ambient/1e6), S=salinity.ambient, T=temp.ambient, pHscale="T") # 
round(test.pCO2.ambient[["pCO2"]]) #extract pCO2 in uatm 
# Resulting pCO2 (uatm) for ambient tanks: 748, 919, 866

test.pCO2.low <- carb(flag=8, var1=pH.low, var2=(ta.low/1e6), S=salinity.low, T=temp.low, pHscale="T")  
round(test.pCO2.low[["pCO2"]]) #extract pCO2 in uatm  
# Resulting pCO2 (uatm) for low pH tanks: 2469, 3267, 3320