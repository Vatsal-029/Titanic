train=read.csv(file = "C:/Users/Vatsal/Desktop/projects/R projects/Imarticus class practice/Excel Sheets/train.csv")
test=read.csv(file = "C:/Users/Vatsal/Desktop/projects/R projects/Imarticus class practice/Excel Sheets/test.csv")
test_1=read.csv(file = "C:/Users/Vatsal/Desktop/projects/R projects/Imarticus class practice/Excel Sheets/test_1.csv")
test=merge(test, test_1, by = "PassengerId")
names(train)
str(train)

train$Pclass=as.factor(train$Pclass)
test$Pclass=as.factor(test$Pclass)
train$Survived=as.factor(train$Survived)
test$Survived=as.factor(test$Survived)

str(train)
summary(train)
summary(train$Age)
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

train$Age[is.na(train$Age)]=mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)]=mean(test$Age, na.rm = TRUE)
names(train)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

test=test[!is.na(test$Fare),]

train1= subset(train, select = c(2,3,5,6,7,8,10))
test1= subset(test, select = c(2,4,5,6,7,9,11))

boxplot(train1$Fare)
summary(train1$Fare)
upper=31+1.5*IQR(train1$Fare); upper
train1$Fare [ train1$Fare > upper]= upper
boxplot(train1$Fare)
summary(train1$Fare)

boxplot(train1$Age)
summary(train1$Age)
upper=35+1.5*IQR(train1$Age); upper
train1$Age [ train1$Age>upper] = upper
lower=22.00-1.5*IQR(train1$Age); lower
train1$Age [train1$Age < lower] = lower
boxplot(train1$Age)
summary(train1$Age)

model= glm(Survived~., family = 'binomial', data = train1)
summary(model)

reg.model = step(glm(Survived~., family = 'binomial', data = train1), direction = 'both')
summary(reg.model)
anova(reg.model, test = 'Chisq')

table(train1$Pclass)
table(train1$Sex)

reg.model1= step(glm(Survived~ relevel(Pclass, ref = 2)+relevel(Sex,ref = 'female')
                     +Age+Fare+SibSp+Parch, family = 'binomial', data = train1)
                 , direction = 'both' )
summary(reg.model1)

reg.model1= step(glm(Survived~ relevel(Pclass, ref = 2)+relevel(Sex,ref = 'female')
                     +Age+Fare+SibSp, family = 'binomial', data = train1)
                 , direction = 'both' )
summary(reg.model1)

anova(reg.model1, test= 'Chisq')

# concordance and discordance 
Acc=function(model){
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}

Acc(logit)

Acc(reg.model1)

library(car)
vif(reg.model1) ##vif = collinearity

exp(coef(reg.model1)) 

test1$probs= predict(reg.model1, test1, type='response')
test1$Predict = as.factor(ifelse(test1$probs>0.70,1,0))
table(test1$Survived, test1$Predict)
library(caret)
confusionMatrix(test1$Survived, test1$Predict)

library(ROCR)
library(ggplot2)

predictTrain= predict(reg.model1, test1, type= 'response') 
ROCRpred = prediction(predictTrain, test1$Survived) ##prediction = used to calculate tpr and fpr

ROCRperf = performance(ROCRpred, 'tpr', 'fpr')  ##perfromance = fetch data
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)

pred = prediction(test1$probs, test1$Survived)
as.numeric(performance(pred, 'auc')@y.values)
