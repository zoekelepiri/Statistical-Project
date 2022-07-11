#Installations
install.packages("haven")                             
library("haven")
install.packages("moments")
library(moments)
install.packages("DescTools")
library(DescTools)

data <- read_sav("Birthweight_data_SPSS.sav")
attach(data)
data
summary(data)

#CONVERTING DATA
headcirumference.categorical<-factor(headcirumference)
smoker.categorical<-factor(smoker)
lowbwt.categorical<-factor(lowbwt)
mage35.categorical<-factor(mage35)
fedyrs.categorical<-factor(fedyrs)

#ANOVA
#-----
#One Variable
#Example1
#Example1
plot(headcirumference.categorical,Birthweight,xlab="Headcirumference", ylab="Birtweight")
summary(aov(Birthweight~headcirumference.categorical)) 
model<-lm(Birthweight~factor(headcirumference))
summary(model) 
PostHocTest(aov(log(Birthweight)~headcirumference.categorical),method="hsd")

#Example2
summary(aov(Birthweight~fedyrs.categorical)) 
model<-lm(Birthweight~fedyrs.categorical)
summary(model) 
summary.lm(aov(Birthweight~fedyrs.categorical))

#Example3
summary(aov(Birthweight~lowbwt.categorical))
summary.lm(aov(Birthweight~lowbwt.categorical)) 
PostHocTest(aov(Birthweight~lowbwt.categorical),method="hsd")


# >=Two Variables
#Example1: Birthweight ~ lowbwt * headcirumference, 2 categorical features
model<-aov(Birthweight~lowbwt.categorical*headcirumference.categorical)
summary(model)
summary.lm(model)

#Example2: Birthweight ~ fedyrs * lowbwt, 2 categorical features
tapply(Birthweight,list(fedyrs.categorical,lowbwt.categorical),mean)
modelBFL0<-lm(Birthweight~fedyrs.categorical*lowbwt.categorical)
summary.aov(modelBFL0)
modelBFL1<-aov(Birthweight~fedyrs.categorical+lowbwt.categorical)
summary.lm(modelBFL1)
PostHocTest(modelBFL1,method="hsd")
labs<-c("Yes","No")
cols<-c("red","blue")
barplot(tapply(Birthweight,list(lowbwt.categorical,fedyrs.categorical),mean),ylim=c(0,30),beside=T,col=cols)
legend(6.5,28,labs,fill=cols)

#Example3: Birthweight ~ smoker * mage35, 2 categorical (T/F) features
modelBSF0<-lm(Birthweight~smoker.categorical*mage35.categorical)
modelBSF1<-lm(Birthweight~smoker.categorical+mage35.categorical)
summary.aov(modelBSF0)
summary.aov(modelBSF1)
anova(modelBSF0,modelBSF1)

#Example4: Birthweight ~ smoker *lowbwt * mage35, 3 categorical features
model<-aov(Birthweight~smoker.categorical*lowbwt.categorical*mage35.categorical)
summary(model)
summary.lm(model) 
model1<-aov(Birthweight~smoker.categorical+lowbwt.categorical+mage35.categorical)
summary(model1)
PostHocTest(model1,method="hsd")

#ANCOVA
#------
#Two Variables
#Example1
model<-lm(Birthweight~mheight*lowbwt.categorical)
summary.aov(model) 
model1<-lm(Birthweight~mheight+lowbwt.categorical)
anova(model,model1) 
summary.lm(model1)

#Example2
model<-lm(Birthweight~mnocig*smoker.categorical)
summary.aov(model)
model1<-lm(Birthweight~mnocig+smoker.categorical) #όχι σημαντικό
anova(model,model1) 
summary.lm(model1) 

#Example3
model<-lm(Birthweight~headcirumference*mage35.categorical)
summary.aov(model) # αλληλεπίδραση όχι στατιστικά σημαντική
model1<-lm(Birthweight~headcirumference+mage35.categorical)
anova(model,model1) 
summary.lm(model1) 

#Example4
model<-lm(Birthweight~Gestation*lowbwt.categorical)
summary.aov(model)
model1<-lm(Birthweight~Gestation+lowbwt.categorical)
anova(model,model1) 
summary.lm(model1)

#Example5
model<-lm(Birthweight~fage*fedyrs.categorical)
summary.aov(model) # η αλληλεπίδραση δεν είναι σημαντική
model1<-lm(Birthweight~fage+fedyrs.categorical)
anova(model,model1) 
summary.lm(model1) 

