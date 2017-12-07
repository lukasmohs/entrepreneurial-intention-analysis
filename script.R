library(dplyr)
library(car)

setwd("/Users/lukasmohs/Desktop/Innovation-Analysis/")
data = read.csv("cleaned-responses.csv",sep=';')
summary(data)
data <- mutate(data, PA = (X11a+X11b+X11c+X11d+X11e)/5)
data <- mutate(data, SN = (X13a+X13b+X13c)/3)
data <- mutate(data, PBC = (X15a + X15b + X15c + X15d + X15e + X15f)/6)
data <- mutate(data, EI = (X18a + X18b + X18c + X18d + X18e + X18f)/6)
data <- mutate(data, SEBA = ifelse(Attended.SEBA == 'yes', 1, 0))
data <- mutate(data, Entrepreneur = ifelse(Already.Entrepreneur == 'yes', 1, 0))

#Modeling effect of IE on Entrepreneur
fitIEonEntrpreneur <- lm(Entrepreneur ~ EI, data=data)
summary(fitIEonEntrpreneur)

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

#Modeling effect of Ajzens antecedents on IE
fitAjzen <- lm(EI ~ PA + SN + PBC, data=data)
summary(fitAjzen)
#plot(fitAjzen)

plot(data$PA,data$EI,  main="EI vs. PA", 
     xlab="PA ", ylab="EI", pch=19)

abline(lm(EI ~ PA, data = data), col="red") # regression line (y~x) 
