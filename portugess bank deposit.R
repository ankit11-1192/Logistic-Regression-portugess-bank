library(dplyr)
library(ggplot2)
library(car)
library(ROCR)
library(tidyverse)
setwd("E:/")
ad=read.csv("bank1.txt",na.strings = c(" ","NA"),stringsAsFactors = FALSE)
str(ad)


# Cleaning dataset

ad$default = ifelse(ad$default == 'yes',1,0)
ad$housing = ifelse(ad$housing == 'yes',1,0)
ad$loan = ifelse(ad$loan == 'yes',1,0)
ad$y = ifelse(ad$y == 'yes',1,0)
ad$month = str_to_sentence(ad$month)
ad$month = match(ad$month,month.abb)
str(ad)
table(ad$poutcome)
ad=ad%>%
  mutate(FAIL=ifelse(poutcome=="failure",1,0),
         NON=ifelse(poutcome=="nonexistent",1,0),
         SUccess=ifelse(poutcome=="success",1,0))%>%
  select(-poutcome)
table(ad$marital)
ad=ad%>%
  mutate(Divorce=ifelse(marital=="divorced",1,0),
         Married=ifelse(marital=="married",1,0),
         Single=ifelse(marital=="single",1,0))%>%
  select(-marital)
str(ad)
ad=ad[,-5]
table(ad$default)
table(ad$education)

names(ad)
round(prop.table(table(ad$job,ad$y),1),2) 

ad=ad%>%
mutate(whitecollar=as.numeric(job %in% c("management","technician","unknown","admin.")),
        pinkcollar=as.numeric(job %in% c("housemaid","self-employed")),
        business=as.numeric(job %in% c("entrepreneur","services")),
       Unemployed=as.numeric(job %in% c("student","retired")))
table(ad$education)
round(prop.table(table(ad$education,ad$y),1),2)
ad=ad%>%
  mutate(Basic=as.numeric(education %in% c("basic.6y","basic.9y","basic.4y")),
         Professional=as.numeric(education %in% c("high.school","professional.course")),
         Graduate=as.numeric(education %in% c("university.degree","unknown")))
str(ad)
ad=ad%>%
  select(-education,-job,-contact,-month,-pdays)
#------------------remove outlier------------------------
summary(ad)
boxplot(ad$age)
x<- ad$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

caps[1]
caps[2]
ad$age[which(ad$age > 58)] <- 58

boxplot(ad$default)
boxplot(ad$housing)
boxplot(ad$loan)
boxplot(ad$duration)
x<- ad$duration
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

caps[1]
caps[2]
ad$duration[which(ad$duration > 650)] <- 650

boxplot(ad$campaign)
x<- ad$campaign
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)

x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

caps[1]
caps[2]
ad$campaign[which(ad$campaign > 6)] <- 6

boxplot(ad$previous)
x<- ad$previous
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
caps[1]
caps[2]
ad$previous[which(ad$previous > 0.5)] <- 0.5

boxplot(ad$emp.var.rate)
boxplot(ad$cons.price.idx)
boxplot(ad$cons.conf.idx)
x<- ad$cons.conf.idx
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
caps[1]
caps[2]
ad$cons.conf.idx[which(ad$cons.conf.idx > -30 )] <- -30 

boxplot(ad$euribor3m)
boxplot(ad$nr.employed)
boxplot(ad$y)
boxplot(ad$FAIL)
boxplot(ad$SUccess)
boxplot(ad$whitecollar)
boxplot(ad$pinkcollar)
boxplot(ad$business)
boxplot(ad$Basic)
boxplot(ad$Professional)
#---------------------data modelling-----------------------
summary(ad)
s=sample(1:nrow(ad),0.7*nrow(ad))
train=ad[s,]
test=ad[-s,]

fit=lm(y~ .,data = train)
dc=step(fit)
summary(dc)

formula(dc)

fit=glm(y ~ duration + previous + cons.price.idx + cons.conf.idx + euribor3m + 
          nr.employed + FAIL  + whitecollar + pinkcollar + 
          Unemployed + Basic + Professional,data = train)
summary(fit)
prop.table(table(ad$y))
train$predict=predict(fit,newdata=train,type="response")
view(train)
#---------------------cutoff---------------------------
ROCRpred = prediction(train$predict,train$y)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))





View(cutoff_data)
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
View(cutoffs)

for(cutoff in cutoffs){
  predicted=as.numeric(train$predict>cutoff)
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]            
View(cutoff_data)

cutoff_data$P=cutoff_data$FN+cutoff_data$TP
cutoff_data$N=cutoff_data$TN+cutoff_data$FP



cutoff_data=cutoff_data%>%
  mutate(Sn=TP/P,Sp=TN/N)%>%
  mutate(KS=abs(TP/P)-(FP/N)) %>%
  mutate(Accuracy =(TP + TN)/(P+N)) %>%
  #mutate(Lift=(TP/P)/(P+N)) %>%
  mutate(m=(8*FN+2+FP)/(P+N))
View(cutoff_data)


cutoff_data=cutoff_data%>%mutate(KS=as.numeric(KS))%>%na.omit()

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff

max(KS_cutoff)


train$score=as.numeric(train$predict>KS_cutoff)

table(train$y,train$score)

acc_train=(21058+2881)/(21058+4494+398+2881)
acc_train

summary(fit)

test$predict=predict(fit,newdata = test,type = "response")
view(train)

test$score=as.numeric(test$predict>KS_cutoff)

table(test$y,test$score)

Accuracy=(9142+1209)/(9142+1854+152+1209)
Accuracy
view(train)

test$finale=ifelse(0.2020202<test$predict,"Y","N")
view(test)

submission_portuges=data.frame(test$y,test$finale)

View(submission)
write.csv(submission,file="submission_portuges.csv")
getwd()
