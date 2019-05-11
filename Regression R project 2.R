
# we are going to run different types of regression models using the orings dataset
#from the faraway library 


#install the package faraway and load it 

install.packages("faraway")

## Run
library(faraway)

#we will be using the orings data for this project 
data(orings)

 
orings

# running the different models:linear, probit and logit 

# linear model

linearModel = lm(damage/6 ~ temp,data=orings)
summary(linearModel)

# Probit 

probitModel = 
  glm(cbind(damage,6-damage) ~ temp,
      family=binomial(probit),data=orings)
summary(probitModel)


# Logit model

logitModel = 
  glm(cbind(damage,6-damage) ~ temp,
      family=binomial, data = orings) 
summary(logitModel)




6. Plots of fitted values

##Run

plot(damage/6~temp,orings,xlim=c(25,85),ylim=c(0,1),las=1,
     xlab="Temperature (°Fahrenheit)", ylab="Probability of Damage",
     pch=21,bg="blue",col="blue",
     main="O-ring Models: Linear:Black, Probit:Blue, Logit:Red")
#adding the regression line
tempGrid = 25:85
a=coef(linearModel)
lines(tempGrid,a[1]+a[2]*tempGrid,col="black",lwd=1)

a=coef(probitModel)
lines(tempGrid,pnorm(a[1]+a[2]*tempGrid),col="red",lwd=2)

a = coef(logitModel)
lines(tempGrid,ilogit(a[1]+a[2]*tempGrid),col="green",lwd=2)






























