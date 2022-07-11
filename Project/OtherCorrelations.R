#Installations
install.packages("haven")                             
library("haven")
install.packages("moments")
library(moments)
install.packages("tree")
library(tree)

data <- read_sav("Birthweight_data_SPSS.sav")
attach(data)
data
summary(data)

#HEADCIRCUMFERENCE
#-----------------
plot(headcirumference) # έχουμε πρόβλημα με κάποια σημεία, βρίσκονται εκτός της γραφικής παράστασης
skewness(headcirumference)
qqnorm(headcirumference)
qqline(headcirumference)

#Multiple correlations
selectedCollumns<- c("headcirumference","mheight","length","motherage")
datas <- subset(data, select = selectedCollumns)
pairs(datas,panel=panel.smooth)
model<-tree(headcirumference~.,data=datas)
plot(model)
text(model)

#MODEL TO CREATE FOR LENGTH
#--------------------------
plot(length) # έχουμε πρόβλημα με κάποια σημεία, βρίσκονται εκτός της γραφικής παράστασης
skewness(length)
qqnorm(length)
qqline(length)
#MHEIGHT
#Linear
plot(mheight,length)
model1 <- lm(length~mheight)
summary.lm(model1)
summary.aov(model1) # αρκετά σημαντικό
abline(model1)
#Polynomial
mheight2 = mheight^2
model<- lm(length ~ mheight + mheight2)
summary(model)
anova(model,model1) 

#FHEIGHT
#Log
model2<-lm(log(length)~fheight)
summary(model2)
summary.aov(model2) # μη σημαντικό

#MPPWT
#Linear
plot(mppwt,length)
model1 <- lm(length~mppwt)
summary.lm(model1)
summary.aov(model1) # σημαντικό
abline(model1)
#Log in both variables
model3<-lm(log(length)~log(mppwt))
summary(model3) # σημαντικό 
summary.aov(model3) # σημαντική και η πιθανότητα

#MULTIPLE CORRELATIONS
selectedCollumns<- c("length","mheight","mppwt","Birthweight")
datas <- subset(data, select = selectedCollumns)
pairs(datas,panel=panel.smooth)
model<-tree(length~.,data=datas)
plot(model)
text(model)

#Multiple for length
model<- lm(length~mheight*Birthweight*mppwt+ I(mheight^2)  + I(Birthweight^2) + I(mppwt^2))
summary(model)
model2 <- update(model,~.-mheight:Birthweight:mppwt)
summary(model2)
model3 <- update(model2,~.-Birthweight:mppwt)
summary(model3)
model4 <- update(model3,~.-mheight:mppwt)
summary(model4)
model5<-update(model4,~.-I(mppwt^2))
summary(model5)
model6<-update(model5,~.-I(Birthweight^2))
summary(model6)
model7<- update(model6,~.-mppwt)
summary(model7)

#MODEL TO CREATE FOR GESTATION
#-----------------------------
#MPPWT
plot(mppwt,length,pch=16)
model0<-lm(log(Gestation)~mppwt)
summary(model0)
summary.aov(model0) # αρκετά σημαντικό

#FHEIGHT
plot(fheight,Gestation)
model1 <- lm(Gestation~fheight)
summary.lm(model1)
summary.aov(model1) # όχι σημαντικό
abline(model1)

#MHEIGHT
plot(mheight,Gestation)
model2 <- lm(Gestation~mheight)
summary.lm(model2)
summary.aov(model2) # όχι σημαντικό
abline(model2)

#MULTIPLE CORRELATIONS
selectedCollumns<- c("Gestation","fheight","mheight","mppwt")
datas <- subset(data, select = selectedCollumns)
pairs(datas,panel=panel.smooth)
model<-tree(Gestation~.,data=datas)
plot(model)
text(model)

#Multiple for Gestation
model<- lm(Gestation~mheight + I(mheight^2)+ fheight + I(fheight^2)+ mppwt + I(mppwt^2))
summary(model)
