library(dplyr)

setwd("/Users/lukasmohs/Desktop/Innovation-Analysis/")
data = read.csv("cleaned-responses.csv",sep=';')
summary(data)
data <- mutate(data, PA = (X11a+X11b+X11c+X11d+X11e)/5)
data <- mutate(data, SN = (X13a+X13b+X13c)/3)
data <- mutate(data, PBC = (X15a + X15b + X15c + X15d + X15e + X15f)/6)
data <- mutate(data, IE = (X18a + X18b + X18c + X18d + X18e + X18f)/6)

#fit <- lm(IE ~ X11a+X11b+X11c+X11d+X11e + X13a + X13b + X13c + X15a + X15b + X15c + X15d + X15e + X15f, data=data)
fit <- lm(IE ~ PA + SN + PBC, data=data)
summary(fit) # show results