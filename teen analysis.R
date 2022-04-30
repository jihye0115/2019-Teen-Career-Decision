youth_raw<-read.csv("C:\\Users\\JIHYE\\Desktop\\°ø¸ðÀü\\youth.csv",header=FALSE,na.strings = "NA")

#####data extract#####
youth1<-youth_raw[youth_raw$V1<19&13<=youth_raw$V1,]
youth2<-youth1[is.na(youth1$V147),]
youth<-youth2[,-c(146:151)]

#create variable 
for (i in 1:nrow(youth)){
  youth$q1[i]<-(youth$V2[i]+20-youth$V3[i]-youth$V4[i]+youth$V5[i]+youth$V6[i])/60
}
youth$sleep_time<-ifelse(youth$V7<10, youth$V7+24, youth$V7)
youth$wakeup_time<-youth$V11
youth$meal1<-(youth$V15-1)/3
youth$meal2<-(youth$V16-1)/3
youth$meal3<-(youth$V17-1)/3
youth$exercise_time<-ifelse(youth$V18==2,0,youth$V19)
youth$stress1<-(youth$V20-1)/3
youth$stress2<-(youth$V21-1)/3
youth$q6a<-(youth$V22-1)/3
youth$q6b<-(youth$V23-1)/3
youth$feeling<-apply(youth[,c("V24","V25","V26","V27","V28","V29","V30","V31","V32")]-1,1,mean)/3
youth[youth$V33==9,]$V33<-NA;youth[youth$V34==9,]$V34<-NA
youth[youth$V35==9,]$V35<-NA;youth[youth$V36==9,]$V36<-NA
youth$q8a<-apply(youth[,c("V33","V34")]-1,1,mean)/3
youth$q8b<-apply(youth[,c("V35","V36")]-1,1,mean)/3
youth$experience<-apply(youth[,c("V46","V47","V48","V49","V50","V51","V52","V53","V54")],1,sum,na.rm=TRUE)
youth$experience<-ifelse(youth$experience==576,NA,youth$experience)
youth$club_offline<-apply(youth[,c("V62","V64")],1,sum,na.rm=T)
youth$club_online<-ifelse(is.na(youth$V63),0,youth$V63)
youth$club_online<-ifelse(youth$club_online==50,NA,youth$club_online)
youth$facility<-apply(youth[,c("V72","V73","V74","V75","V76","V77","V78")],1,sum,na.rm=T)
youth$program_know<-apply(youth[,c(79:84)]-1,1,mean,na.rm=T)
youth[youth$V86==9,]$V86<-NA
youth$q13<-apply(youth[,c("V85","V86","V87","V88","V89","V90")]-1,1,mean)/4
youth$V91<-ifelse(!is.na(youth$V91) & youth$V91==9, NA, youth$V91)
youth$V92<-ifelse(!is.na(youth$V92) & youth$V92==9, NA, youth$V92)
youth$q14<-apply(youth[,c("V91","V92","V93")]-1,1,mean,na.rm=T)/4
youth$V94<-ifelse(!is.na(youth$V94) & youth$V94==9, NA, youth$V94)
youth$q15<-apply(youth[,c("V94","V95")]-1,1,mean,na.rm=T)/3
youth$family_satis<-(youth[,"V96"]-1)/3
youth$parents_help<-apply(youth[,c("V97","V98","V99")]-1,1,mean,na.rm=T)/2
for (i in 1:nrow(youth)){
  youth$marry[i]<-(4-youth$V100[i]+youth$V101[i]-1+youth$V102[i]-1)/9
}
youth$agree<-apply(youth[,c("V103","V104","V105","V106")]-1,1,mean)/9
youth$society1<-apply(youth[,c("V107","V108","V109")]-1,1,mean,na.rm=T)/3
youth$society2<-(youth[,"V110"])/10
youth$society3<-(youth[,"V111"])/10
youth$vote_age<-(youth[,"V112"]-1)/2
youth$multiculture<-apply(youth[,113:114]-1,1,mean)/3
youth$reunify<-apply(youth[,115:117]-1,1,mean)/3
youth$V121<-5-youth$V121
youth$school_satis<-apply(youth[,118:122]-1,1,mean)/3 
youth$school_score<-(youth[,123]-1)/4
youth[is.na(youth$V124),]$V124<-2
youth[is.na(youth$V125),]$V125<-0
youth$academy_time<-youth$V125
youth$self_study<-youth$V126
youth$alone_time<-(youth$V128-1)/3
youth$q31_1A<-ifelse(!is.na(youth$V133) & youth$V133==0, 0, 1)
youth$q31_2A<-ifelse(!is.na(youth$V134) & youth$V134==0, 0, 1)
youth$q31_3A<-ifelse(!is.na(youth$V135) & youth$V135==0, 0, 1)
youth$q31_4A<-ifelse(!is.na(youth$V136) & youth$V136==0, 0, 1)
youth$q31_5A<-ifelse(!is.na(youth$V137) & youth$V137==0, 0, 1)
youth$q31_6A<-ifelse(!is.na(youth$V138) & youth$V138==0, 0, 1)
youth$q31_7A<-ifelse(!is.na(youth$V139) & youth$V139==0, 0, 1)
youth$q31_8A<-ifelse(!is.na(youth$V140) & youth$V140==0, 0, 1)
youth$q32_1A<-ifelse(!is.na(youth$V141) & youth$V141==0, 0, 1)
youth$q32_2A<-ifelse(!is.na(youth$V142) & youth$V142==0, 0, 1)
youth$q32_3A<-ifelse(!is.na(youth$V143) & youth$V143==0, 0, 1)
youth$q32_4A<-ifelse(!is.na(youth$V144) & youth$V144==0, 0, 1)
youth$edu_want<-(youth$V145-1)/3
youth$career_plan<-ifelse(youth$V152==5|youth$V152==97|youth$V152==99,NA,youth$V152)
youth$parttime<-2-youth$V160
youth$job_training<-2-youth$V161
youth$sex<-youth$V165
youth$city_size<-4-youth$V171
youth$income<-youth$V172
youth$parents_edu<-youth$V173
youth<-youth[!is.na(youth$school_satis),]
youth$y1<-apply(youth[,c("V157","V158","V159")]-1,1,mean)/3
youth$y2<-2-youth$V153

youth_new<-youth[,-c(2:167)]
names(youth_new)[1]<-"age"
names(youth_new)

#as.factor
youth_new$career_plan<-as.factor(youth$career_plan)
youth_new$parttime<-as.factor(youth$parttime)
youth_new$job_training<-as.factor(youth$job_training)
youth_new$sex<-as.factor(youth_new$sex)
youth_new$city_size<-as.numeric(youth_new$city_size)    
youth_new$income<-as.numeric(youth_new$income)
youth_new$parents_edu<-as.numeric(youth_new$parents_edu)
youth_new$y2<-as.factor(youth_new$y2)
youth_new$q31_1A<-as.factor(youth_new$q31_1A)
youth_new$q31_2A<-as.factor(youth_new$q31_2A)
youth_new$q31_3A<-as.factor(youth_new$q31_3A)
youth_new$q31_4A<-as.factor(youth_new$q31_4A)
youth_new$q31_5A<-as.factor(youth_new$q31_5A)
youth_new$q31_6A<-as.factor(youth_new$q31_6A)
youth_new$q31_7A<-as.factor(youth_new$q31_7A)
youth_new$q31_8A<-as.factor(youth_new$q31_8A)
youth_new$q32_1A<-as.factor(youth_new$q32_1A)
youth_new$q32_2A<-as.factor(youth_new$q32_2A)
youth_new$q32_3A<-as.factor(youth_new$q32_3A)
youth_new$q32_4A<-as.factor(youth_new$q32_4A)
teen<-youth_new
summary(teen)
row.names(teen)<-1:nrow(teen)

#KNN for missing values
library(cluster)
dist.mtx <- as.matrix(daisy(teen[,1:58],stand=T))
which(!complete.cases(teen))

central.value <- function(x) {  
  if (is.numeric(x)) {median(x,na.rm=T)
  }  else if (is.factor(x)) {levels(x)[which.max(table(x))]
  } else {f <- as.factor(x)
  levels(f)[which.max(table(f))]}
}

for(r in which(!complete.cases(teen))) teen[r,which(is.na(teen[r,]))]<-
  apply(data.frame(teen[c(as.integer(names(sort(dist.mtx[r,])[2:11]))),which(is.na(teen[r,]))]),2,central.value)

#final data
summary(teen)
head(teen)

#train:test=7:3
set.seed(0)
random_index<-sample(nrow(teen),0.3*nrow(teen))
test<-teen[random_index,]
train<-teen[-random_index,]

#only numeric X
teen.num<-teen[,c(1:38,51,56:58)]
train.num<-train[,c(1:38,51,56:58)]
test.num<-test[,c(1:38,51,56:58)]

#data properties
symnum(cor(train.num,use="complete.obs")) 
summary(train)


#linear regression
lm.y1<-lm(y1~.,data=train[,-60])
summary(lm.y1)
step.y1<-step(lm.y1)
summary(step.y1)
pred.lm.y1<-predict(step.y1,test)
plot(jitter(test$y1),pred.lm.y1, main = 'Prediction of linear regression');abline(0,1,col="red")
mse.lm.y1<-mean((pred.lm.y1-test[,"y1"])^2);mse.lm.y1  #test error

#tree
library(tree)
tr.y1<-tree(y1~.,data=train[,-60])
tr.y1.cv<-cv.tree(tr.y1)
for(i in 2:100){
  tr.y1.cv$dev<-tr.y1.cv$dev+cv.tree(tr.y1)$dev
}
tr.y1.cv$dev<-tr.y1.cv$dev/100
plot(tr.y1.cv)
mysize<-tr.y1.cv$size[which.min(tr.y1.cv$dev)]
final.tr.y1<-prune.tree(tr.y1,best=mysize)
plot(final.tr.y1, main=c('Final Tree'));text(final.tr.y1, cex=1.2)
pred.tr.y1<-predict(final.tr.y1,test)
plot(jitter(test$y1),pred.tr.y1, main=c("Prediction of tree"));abline(0,1,col="red")
mse.tr.y1<-mean((pred.tr.y1-test[,"y1"])^2);mse.tr.y1 #test error


#random forest
library(randomForest)
set.seed(1)
rf.y1<-randomForest(y1~.,data=train[,-60],importance=T)
rf.y1
plot(rf.y1)
varImpPlot(rf.y1)
partialPlot(rf.y1,test[,-60],x.var=feeling)
partialPlot(rf.y1,test[,-60],x.var=society3)
partialPlot(rf.y1,test[,-60],x.var=q1)
pred.rf.y1<-predict(rf.y1,test)
plot(jitter(test$y1),pred.rf.y1, main='Prediction of randomForest'); abline(0,1,col="red")
mse.rf.y1<-mean((pred.rf.y1-test$y1)^2);mse.rf.y1  #test error


#LDA for y2
library(MASS)
train.num1<-train.num
train.num1[43]<-train$y2  #y2&numeric train
test.num1<-test.num
test.num1[43]<-test$y2  #y2$numeric test
names(train.num1)[43]<-"y2"
names(test.num1)[43]<-"y2"
lda.y2<-lda(y2~.,data=train.num1)
lda.y2
lda.y2.pred<-predict(lda.y2,test.num1)
plot(lda.y2,col = as.integer(train$y2))
t1<-table(test.num1$y2,lda.y2.pred$class);t1 #confusion matrix
prop.table(t1,1)
mean(ifelse(test.num1$y2!=lda.y2.pred$class,1,0)) 


#logistic regression for y2
glm.y2<-glm(y2~.,family=binomial,data=train[,-59])
glm.y2.step<-step(glm.y2)
summary(glm.y2.step)
glm.y2.pred<-predict(glm.y2.step,test,"response")
glm.y2.class<-ifelse(glm.y2.pred>0.5,1,0)
t2<-table(test$y2,glm.y2.class);t2 #confusion matrix
prop.table(t2,1)
mean(ifelse(glm.y2.class!=test$y2,1,0))
