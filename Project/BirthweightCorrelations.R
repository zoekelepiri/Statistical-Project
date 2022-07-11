#Installations
install.packages("haven")                             
library("haven")
install.packages("moments")
library(moments)
install.packages("tree")
library(tree)
install.packages("olsrr")
library("olsrr")

data <- read_sav("Birthweight_data_SPSS.sav")
attach(data)
data
summary(data)

#BASIC STEPS
par(mfrow=c(2,2))
plot(Birthweight)
boxplot(Birthweight,ylab="data values")
hist(Birthweight)
skewness(Birthweight)
qqnorm(Birthweight)
qqline(Birthweight)

# TWO SAMPLES
# SD
qf(0.975,83,83)
F.ratio<-var(fnocig)/var(mnocig)
F.ratio #Σύγκριση F.ratio με qf => Απόρριψη μηδενικής υπόθεσης F.ratio>qf
plot(x,df(x,83,83),type="l")
x.reject<-seq(qf(0.975,83,83),2.5,0.01)
y.reject<-df(x.reject,83,83)
polygon(c(qf(0.975,83,83),x.reject,2.5),
        c(0,y.reject,0),col="red")
abline(v=qf(0.975,83,83))

pf(F.ratio,83,83)
2*(1-pf(F.ratio,83,83))
x<-seq(0.5,2.5,0.01)
plot(x,df(x,83,83),type="l")
abline(v=F.ratio)

var.test(fnocig,mnocig)

# MEAN
qt(0.975,152)
x<-seq(-4,4,0.001)
plot(x,dt(x,152),type="l")
abline(v=qt(0.975,152))
abline(v=-qt(0.975,152))
x.reject.right<-seq(qt(0.975,152),4,0.01)
y.reject.right<-dt(x.reject.right,152)
polygon(c(qt(0.975,152),x.reject.right,4),c(0,y.reject.right,0),col="red")
x.reject.left<-seq(-4,-qt(0.975,152),0.01)
y.reject.left<-dt(x.reject.left,152)
polygon(c(-4,x.reject.left,-qt(0.975,152)),c(0,y.reject.left,0),col="red")
t.value<-(mean(fnocig)-mean(mnocig))/sqrt(var(fnocig)/84+var(mnocig)/84)
t.value

t.test(mnocig,fnocig)
#wilcox.test(mnocig,fnocig)

# DEPENDENCY
sm00=sum(smoker==0 & mage35==0)
sm01=sum(smoker==0 & mage35==1)
sm10=sum(smoker==1 & mage35==0)
sm11=sum(smoker==1 & mage35==1)
count1<-matrix(c(sm00,sm01,sm10,sm11),nrow=2)
count1
fisher.test(count1)

sl00=sum(smoker==0 & lowbwt==0)
sl01=sum(smoker==0 & lowbwt==1)
sl10=sum(smoker==1 & lowbwt==0)
sl11=sum(smoker==1 & lowbwt==1)
count2<-matrix(c(sl00,sl01,sl10,sl11),nrow=2)
count2
chisq.test(count2)

ml00=sum(mage35==0 & lowbwt==0)
ml01=sum(mage35==0 & lowbwt==1)
ml10=sum(mage35==1 & lowbwt==0)
ml11=sum(mage35==1 & lowbwt==1)
count3<-matrix(c(ml00,ml01,ml10,ml11),nrow=2)
chisq.test(count3)

# CORRELATION
cor(data)
cor(data,method="spearman")
cor.test(Birthweight,mheight)
cor.test(length,mheight)

#REGRESSION
#----------
# BIRTHWEIGHT ~ GESTATION , 1 arithmetic variable, 3 MODELS
plot(Gestation,Birthweight, pch=16)
#Linear Model
linear<-lm(Birthweight~Gestation)
linear
abline(linear)
fitted<-predict(linear)
residual = Birthweight-fitted
sum(residual)
for(i in 1:84) lines(c(Gestation[i],Gestation[i]), c(Birthweight[i],fitted[i]))
summary(linear) # το Birthweight με το gestation είναι αρκετά σημαντικά
summary(aov(linear))

#Polynomial model
Gestation2<-Gestation^2
model<-lm(Birthweight~Gestation+Gestation2)
model
summary(aov(model))
anova(model,linear) # Η διαφορά των υποδειγμάτων δεν είναι σημαντική, δεν μπορούμε να πούμε ότι δεν έχουμε γραμμικότητα

#Using log
exponential<-lm(log(Birthweight)~Gestation)
exponential
summary(exponential) # δεν είναι σημαντική σαν μέθοδος ωστόσο το χαρακτηριστικό gestation είναι αρκετά σημαντικό για τον καθορισμό του Birthweight


#BIRTHWEIGHT ~ MPPWT
#Linear
plot(mppwt,Birthweight, pch=16)
model0<-lm(Birthweight~mppwt)
summary(model0)
summary.aov(model0) # είναι σημαντικό  

#Using Log 
plot(mppwt,log(Birthweight))
model1 <- lm(log(Birthweight)~mppwt)
model1
summary.lm(model1)
summary.aov(model1)
abline(model1) # και εδώ τα αποτελέσματα δείχνουν ότι δεν είναι στατιστικά σημαντικά

# BIRTHWEIGHT ~ LENGTH , 1 arithmetic variable
#Linear
plot(length,Birthweight, pch=16)
model0<-lm(Birthweight~length)
summary(model0)
summary.aov(model0)
abline(model0) # είναι σημαντικό αλλά όχι και τόσο τόσο πολύ

#MODELS - PAIRS
selectedCollumns<- c("Birthweight","Gestation","fedyrs","mppwt","lowbwt","smoker")
datas <- subset(data, select = selectedCollumns)
pairs(datas,panel=panel.smooth)
model<-tree(Birthweight~.,data=datas)
plot(model)
text(model) #όλα αυτά μαζί πως επηρεάζουν την Birthweight

#Complex Model Correlations
#FIRST EXAMPLE
modeltest1<-lm(Birthweight~mppwt*fheight*length+I(mppwt^2)+I(fheight^2)+I(length^2))
summary(modeltest1)
modeltest2<-update(modeltest1,~.-mppwt:fheight:length)
summary(modeltest2)
modeltest3<-update(modeltest2,~.-fheight:length)
summary(modeltest3)
modeltest4<-update(modeltest3,~.-mppwt:length)
summary(modeltest4)
modeltest5<-update(modeltest4,~.-I(length^2))
summary(modeltest5)
modeltest6<-update(modeltest5,~.-I(fheight^2))
summary(modeltest6)
modeltest7<-update(modeltest6,~.-I(mppwt^2))
summary(modeltest7)
par(mfrow=c(2,2))
plot(modeltest7)
modeltest8<-lm(log(Birthweight) ~ mppwt + fheight + length + mppwt:fheight)
summary(modeltest8)
par(mfrow=c(2,2))
plot(modeltest8)
modeltest9<-lm(log(Birthweight) ~ mppwt + fheight + length + mppwt:fheight, subset=(1:length(Birthweight)!=10))
summary(modeltest9)
par(mfrow=c(2,2))
plot(modeltest9)
size= 1:length(Birthweight)
modeltest10<-lm(log(Birthweight) ~ mppwt + fheight + length + mppwt:fheight, subset=(size!=10 & size!=22))
summary(modeltest10)
par(mfrow=c(2,2))
plot(modeltest10)

#SECOND EXAMPLE 
#PartA
firstModel<-lm(Birthweight ~ Gestation + I(Gestation^2) + length + I(length^2) + headcirumference + I(headcirumference^2))
summary(firstModel) 
secondModel <- update(firstModel,~.-I(Gestation^2))
summary(secondModel)

#PartB
firstModel<-lm(Birthweight ~ Gestation*length*headcirumference  + I(Gestation^2) + I(length^2) + I(headcirumference^2))
summary(firstModel) 
secondModel <- update(firstModel,~.-Gestation:length:headcirumference)
summary(secondModel)
thirdModel <- update(secondModel,~.-Gestation:headcirumference)
summary(thirdModel)
fourthModel <- update(thirdModel,~.-I(Gestation^2))
summary(fourthModel)
fifth<- update(fourthModel,~.-length)
summary(fifth)

#THIRD EXAMPLE
#PartA
firstModel<-lm(Birthweight ~ fnocig + I(fnocig^2) + mnocig + I(mnocig^2) + headcirumference + I(headcirumference^2))
summary(firstModel)
secondModel <- update(firstModel,~.-I(fnocig^2))
summary(secondModel) 
thirdModel <- update(secondModel,~.-fnocig)
summary(thirdModel)  # έτσι έχουμε μόνο τους παράγοντες που είναι αρκετά σημαντικοί

#PartB
firstModel<-lm(Birthweight ~ fnocig* mnocig *headcirumference + I(fnocig^2) + I(mnocig^2) + I(headcirumference^2))
summary(firstModel) 
secondModel <- update(firstModel,~.-fnocig:mnocig:headcirumference)
summary(secondModel)
thirdModel <- update(secondModel,~.-mnocig:headcirumference)
summary(thirdModel)
fourthModel<-update(thirdModel,~.-I(fnocig^2))
summary(fourthModel)
fifthModel<-update(fourthModel,~.-I(mnocig^2))
summary(fifthModel)
sixthModel<-update(fifthModel,~.-mnocig)
summary(sixthModel)
finalModel<-update(sixthModel,~.-fnocig:mnocig)
summary(finalModel)
updatedModel<-lm( Birthweight ~ fnocig + headcirumference + I(headcirumference^2) + 
                    fnocig:headcirumference, subset=(1:length(Birthweight)!=10))
summary(updatedModel)
par(mfrow=c(2,2))
plot(updatedModel)

#FOURTH EXAMPLE
modelMom<-lm(Birthweight~length+I(length^2)+headcirumference+I(headcirumference^2)+
               Gestation+I(Gestation^2)+smoker+I(smoker^2)+motherage+I(motherage^2)+
               mnocig+I(mnocig^2)+mheight+I(mheight^2)+mppwt+I(mppwt^2)+
               lowbwt+I(lowbwt^2)+mage35+I(mage35^2))
summary(modelMom)

