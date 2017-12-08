library(dplyr)
library(car)

# Modeling Random to test
#data <- mutate(data, newSN = round(rnorm(66,4,2.75)))
#data <- mutate(data, newSN =  ifelse(newSN <  1, 1,newSN))
#data <- mutate(data, newSN =  ifelse(newSN >  7, 7,newSN))
#plot(data$EI,data$newSN)
#abline(lm(data$EI ~ data$newSN))
#fit=lm(EI ~ newSN, data=data)
#summary(fit)

setwd("/Users/lukasmohs/Desktop/Innovation-Analysis/")
data = read.csv("cleaned-responses.csv",sep=';')
summary(data)
data <- mutate(data, PA = (X11a+X11b+X11c+X11d+X11e)/5)
data <- mutate(data, SN = (X13a+X13b+X13c)/3)
data <- mutate(data, PBC = (X15a + X15b + X15c + X15d + X15e + X15f)/6)
data <- mutate(data, EI = (X18a + X18b + X18c + X18d + X18e + X18f)/6)
data <- mutate(data, SEBA = ifelse(Attended.SEBA == 'yes', 1, 0))
data <- mutate(data, Entrepreneur = ifelse(Already.Entrepreneur == 'yes', 1, 0))

########    EFFECT OF EI on ACTION    #############
#Modeling effect of IE on Entrepreneur
fitIEonEntrpreneur <- lm(Entrepreneur ~ EI, data=data)
summary(fitIEonEntrpreneur)

########    EFFECT OF SEBA    #############
#Modeling effect of SEBA on PA
fitPA <- lm(PA ~ SEBA, data=data)
summary(fitPA)
#Modeling effect of SEBA on SN
fitSN <- lm(SN ~ SEBA, data=data)
summary(fitSN)
#Modeling effect of SEBA on PBC
fitPBC <- lm(PBC ~ SEBA, data=data)
summary(fitPBC)
#Modeling effect of SEBA on IE
fitSEBAonEI <- lm(EI ~ SEBA, data=data)
summary(fitSEBAonEI)

########    EFFECT OF AJZEN ANTECEDENT    #############
#Modeling effect of Ajzens antecedents on IE
fitAjzen <- lm(EI ~ PA + SN + PBC, data=data)
summary(fitAjzen)
#plot(fitAjzen)

#Plot effect of PA on EI
plot(data$PA,data$EI,  main="EI vs. PA", 
     xlab="PA ", ylab="EI", pch=19)
abline(lm(EI ~ PA, data = data), col="red")

#Plot effect of SN on EI
plot(data$SN,data$EI,  main="EI vs. SN", 
     xlab="SN ", ylab="EI", pch=19)
abline(lm(EI ~ SN, data = data), col="red")

#Plot effect of PBC on EI
plot(data$PBC,data$EI,  main="EI vs. PBC", 
     xlab="SN ", ylab="EI", pch=19)
abline(lm(EI ~ PBC, data = data), col="red")
