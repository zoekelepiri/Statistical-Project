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

#Histograms
par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3))
for (i in 1:ncol(data)){
  hist(data[[i]], main=paste("Histogram", names(data)[i]), xlab = paste("Values",names(data)[i]))
  box(lty = "solid")
}
# Log() Histograms
par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3))
for (i in 1:ncol(data)){
  hist(log(data[[i]]), main=paste("Log Histogram ", names(data)[i]), xlab = paste("Values L",names(data)[i]))  
  box(lty = "solid")
}
# Box Plots
par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3))
for (i in 1:ncol(data)){
  boxplot(data[[i]], main=paste("Box Plot ", names(data)[i]), xlab = paste("Values",names(data)[i])) 
  box(lty = "solid")
}
# Log() Box Plots
par(mfrow = c(5, 3),mai=c(0.3,0.3,0.3,0.3))
for (i in 1:ncol(data)){
  boxplot(log(data[[i]]), main=paste("Log Box Plot ", names(data)[i]), xlab = paste("Values",names(data)[i]))  
  box(lty = "solid")
}
