install.packages("haven")                             
library("haven")
install.packages("moments")
library(moments)

data <- read_sav("Birthweight_data_SPSS.sav")
attach(data)
data
summary(data)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3)) 

#Length
crValues = data$length
hist(crValues,breaks=15,main="Length",prob = TRUE, xlab="Length",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#BirthWeight
crValues = data$Birthweight
hist(crValues,breaks=15,main="Birthweight",prob = TRUE, xlab="Birthweight",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Headcirumference
crValues = data$headcirumference
hist(crValues,breaks=15,main="Headcirumference",prob = TRUE, xlab="Headcirumference",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Gestation
crValues = data$Gestation
hist(crValues,breaks=15,main="Gestation",prob = TRUE, xlab="Gestation",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Smoker
crValues = data$smoker
hist(crValues,breaks=15,main="Smoker",prob = TRUE, xlab="Smoker",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Motherage
crValues = data$motherage
hist(crValues,breaks=15,main="Motherage",prob = TRUE, xlab="Motherage",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Mnocig
crValues = data$mnocig
hist(crValues,breaks=15,main="mnocig",prob = TRUE, xlab="mnocig",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Mheight
crValues = data$mheight
hist(crValues,breaks=15,main="Mheight",prob = TRUE, xlab="Mheight",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#mppwt
crValues = data$mppwt
hist(crValues,breaks=15,main="Mppwt",prob = TRUE, xlab="Mppwt",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Fage
crValues = data$fage
hist(crValues,breaks=15,main="Fage",prob = TRUE, xlab="Fage",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#fedyrs
crValues = data$fedyrs
hist(crValues,breaks=15,main="Fedyrs",prob = TRUE, xlab="Fedyrs",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Fnocig
crValues = data$fnocig
hist(crValues,breaks=15,main="Fnocig",prob = TRUE, xlab="Fnocig",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Fheihgt
crValues = data$fheight
hist(crValues,breaks=15,main="Fheight",prob = TRUE, xlab="Fheight",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#Lowbwt
crValues = data$lowbwt
hist(crValues,breaks=15,main="Lowbwt",prob = TRUE, xlab="Lowbwt",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)

#mage35
crValues = data$mage35
hist(crValues,breaks=15,main="Mage35",prob = TRUE, xlab="Mage25",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)