library(dplyr)
library(car)


########   SETTINGS    #############
setwd("/Users/lukasmohs/Desktop/Innovation-Analysis/") #adjust to directiory
data = read.csv("cleaned-responses.csv",sep=';')
summary(data)
data <- mutate(data, ATB = (X11a+X11b+X11c+X11d+X11e)/5)
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
#Modeling effect of SEBA on IE
fitSEBAonEI <- lm(EI ~ SEBA, data=data)
summary(fitSEBAonEI)
plot(data$SEBA, data$EI, axes=FALSE, main="SEBA vs. EI", 
     xlab="SEBA ", ylab="EI", pch=19)
axis(side=1, at=c(0:1))
axis(side=2, at=c(1:7))
abline(fitSEBAonEI, col="red")

#Modeling effect of SEBA on ATB
fitATB <- lm(ATB ~ SEBA, data=data)
summary(fitATB)
plot(data$SEBA, data$ATB, axes=FALSE, main="SEBA vs. ATB", 
     xlab="SEBA ", ylab="ATB", pch=19)
axis(side=1, at=c(0:1))
axis(side=2, at=c(1:7))
abline(fitATB, col="red")

#Modeling effect of SEBA on SN
fitSN <- lm(SN ~ SEBA, data=data)
summary(fitSN)
plot(data$SEBA, data$SN, axes=FALSE, main="SEBA vs. SN", 
     xlab="SEBA ", ylab="SN", pch=19)
axis(side=1, at=c(0:1))
axis(side=2, at=c(1:7))
abline(fitSN, col="red")

#Modeling effect of SEBA on PBC
fitPBC <- lm(PBC ~ SEBA, data=data)
summary(fitPBC)
plot(data$SEBA, data$PBC, axes=FALSE, main="SEBA vs. PBC", 
     xlab="SEBA ", ylab="PBC", pch=19)
axis(side=1, at=c(0:1))
axis(side=2, at=c(1:7))
abline(fitPBC, col="red")


########    EFFECT OF AJZEN ANTECEDENT    #############
#Modeling effect of Ajzens antecedents on IE
fitAjzen <- lm(EI ~ ATB + SN + PBC, data=data)
summary(fitAjzen)
#plot(fitAjzen)

#Plot effect of ATB on EI
plot(data$ATB,data$EI,  main="EI vs. ATB", 
     xlab="ATB ", ylab="EI", pch=19)
abline(lm(EI ~ ATB, data = data), col="red")

#Plot effect of SN on EI
plot(data$SN,data$EI,  main="EI vs. SN", 
     xlab="SN ", ylab="EI", pch=19)
abline(lm(EI ~ SN, data = data), col="red")

#Plot effect of PBC on EI
plot(data$PBC,data$EI,  main="EI vs. PBC",
     xlab="PBC ", ylab="EI", pch=19)
abline(lm(EI ~ PBC, data = data), col="red")

# Modeling Random to test
#data <- mutate(data, new = round(rnorm(66,4,2.75)))
#data <- mutate(data, new =  ifelse(new <  1, 1,new))
#data <- mutate(data, new =  ifelse(new >  7, 7,new))