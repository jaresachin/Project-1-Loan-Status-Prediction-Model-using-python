
#Importing data set
library(readr)
bank <- read_csv("F:/Project/FST/Loan Status Prediction/data/train_data.csv")

#To see a na value in the data 
table(is.na(bank))

#It will return the column name along with the missing values
lapply(bank, function(bank) sum(is.na(bank)))


# remove na values :- remove rows - na.omit function
bank <- na.omit(bank) 


#to remove the columns
#install.packages("tidyverse")
#install.packages("dplyr")
library(dplyr)

#we have to remove column"loan id", which is not usefull for us
bank=select(bank, -Loan_ID)

#To get data type and structure of columns
str(bank)  
sum(is.na(bank))

#Label encoder 
bank$Gender=factor(bank$Gender,levels=c("Female","Male"),labels=c(0,1))

bank$Married=factor(bank$Married,levels=c("No","Yes"),labels=c(0,1))

bank$Dependents=factor(bank$Dependents,levels=c("0","1","2","3+"),labels=c(0,1,2,3))

bank$Education=factor(bank$Education,levels=c("Not Graduate","Graduate"),labels=c(0,1))

bank$Self_Employed=factor(bank$Self_Employed,levels=c("No","Yes"),labels=c(0,1))

bank$Property_Area=factor(bank$Property_Area,levels=c("Rural","Urban","Semiurban"),labels=c(0,1,2))

bank$Credit_History=factor(bank$Credit_History)

bank$Loan_Status=factor(bank$Loan_Status,levels=c("N","Y"),labels=c(0,1))
#-------------------------------------------------------------------
#Data Visualization:
xtabs(~Credit_History+Loan_Status,data=bank)

hist(bank$ApplicantIncome)

library(ggplot2)

#Loan_status wise income Boxplot
ggplot(data=bank,aes(x=Loan_Status,y=ApplicantIncome,fill=Loan_Status))+
geom_boxplot()+
ggtitle("Boxplot")

# Gender wise income Boxplot
ggplot(data=bank,aes(x=Gender,y=ApplicantIncome,fill=Gender))+
geom_boxplot()+
ggtitle("Boxplot")


# Gender wise income Density plot
ggplot(data=bank,aes(x=ApplicantIncome,fill=Gender))+
geom_density()+
ggtitle("Density Plot")


pairs(~ApplicantIncome+LoanAmount+Credit_History+Loan_Status,data=bank)  #pair plot


#-------------------------------------------------------------------
# Logistic regression model for classification for loan status

Logistic_model <- glm(bank$Loan_Status~ .,data=bank,family = "binomial")
Logistic_model
summary(Logistic_model)
   #p-value of married=0.04970 :-Means we are ( 1-0.04970)=0.9503 =95.03 % confident that the married variable going to significant for model.
Logistic_model$fitted.values
#AIC and residual deviance value less model is good 
prob <- predict(Logistic_model,data=bank,type="response");prob 
Logistic_pred <- ifelse(prob > 0.5, "1", "0")
confusion_matrix<-table(Actual =bank$Loan_Status,Logistic_pred);confusion_matrix

# Model Accuracy 
Accuracy<-sum(diag(confusion_matrix)/sum(confusion_matrix));Accuracy 

#Goodness of fit test.
with(Logistic_model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))
#p-value = 0.00000 < 0.05 we can conclude that the model is statistically significant.
#-----------------------------------------------------------------------
# Hypothesis Testing 
shapiro.test(bank$ApplicantIncome)
shapiro.test(log(bank$ApplicantIncome))


qqnorm(bank$ApplicantIncome)
qqnorm(log(bank$ApplicantIncome))

boxplot(log(bank$ApplicantIncome))


hist(bank$ApplicantIncome)

hist(log(bank$ApplicantIncome))

hist(bank$CoapplicantIncome)


hist(bank$LoanAmount)

hist(log(bank$LoanAmount))


table(bank$Loan_Amount_Term)

table(bank$Gender)




#Two sample t test
filterbymale=filter(bank,bank$Gender=="1")
loanAmount_male=filterbymale$LoanAmount;loanAmount_male


filterbyfemale=filter(bank,bank$Gender=="0")
loanAmount_female=filterbyfemale$LoanAmount;loanAmount_female



#to check equality of variance of loanAmount_male and loanAmount_female
#Ho: variance are same for male and female 
#H1: variance are not same for male and female 


var(loanAmount_male)
var(loanAmount_female)
?var.test
#var.test(x, y, ratio = 1,alternative = c("two.sided", "less", "greater"),conf.level = 0.95)
var.test(loanAmount_female,loanAmount_male)

F = var(loanAmount_female) /var(loanAmount_male);F
#++++++++++++++++++++++++++++++++++++++++++++++#
#Two sample t test

#t test is used to check mean loanAmoun of male and female are same or not

t.test(loanAmount_male,loanAmount_female)
n1=length(loanAmount_male);n1
n2=length(loanAmount_female);n2
m1=mean(loanAmount_male);m1
m2=mean(loanAmount_female);m2
sd1=sd(loanAmount_male);sd1
sd2=sd(loanAmount_male);sd2
#Conclusion:mean is not same beacause p value is less than 0.05


#++++++++++++++++++++++++++++++++++++++++#
#proportion test for to check proportion of male and female same or not for approval of loan status

table(bank$Gender)
n1= 394   # Total number of male who apply for loan
n2=86     # Total number of female who apply for loan

filterbymale=filter(bank,bank$Gender=="1")
loan_status_for_male=filterbymale$Loan_Status;loan_status_for_male
table(loan_status_for_male)

#X1 : Number of male who get loan
x1= 278

filterbyfemale=filter(bank,bank$Gender=="0")
loan_status_for_female=filterbyfemale$Loan_Status
loan_status_for_female
table(loan_status_for_female)
#X2 : Number of female who get loan
x2= 54
?prop.test
# Ho: Proportion is same for male and female for approving loan status
#H1:Proportion is same for male and female for approving loan status

prop.test(x=c(278,54), n=c(394,86), alternative ="two.sided",conf.level = 0.95, correct = TRUE)

#p-value=0.199 > 0.05 ,so do not reject null hypothesis.


#Proportion test using normal distribution
p1=x1/n1;p1
p2=x2/n2;p2
p=(x1+x2)/(n1+n2);p

z=(p1-p2)/(sqrt(p*(1-p)*((1/n1)+(1/n2))));z
pvalue=(1-pnorm(abs(z),0,1))*2;pvalue

#https://mathcracker.com/z-test-for-two-proportions


?chisq.test
#----------------------------------------------------------------
#chi sq test for propery_Area and loan status

p=table(bank$Property_Area,bank$Loan_Status);p
a=chisq.test(p,correct=T);a
attributes(a)

a$expected
a$statistic

fisher.test(p,conf.int=T,conf.level=0.99)  # used when observed frequency less than 5



#chi sq test for gender and loan status

w=table(bank$Gender,bank$Loan_Status);w
chisq.test(w,correct=T)



#gender and loan status is independent means no relation
#because p value is greater than 0.05

#-----------------------------------------------------------------------
#Normalizing numeric data

normalize <- function(x){ 
  return((x-min(x))/(max(x)-min(x))) }
#Once we run this code, we are required to normalize the numeric features in the data set.
#Instead of normalizing each of the 12 individual variables we use:

str(bank)
sapply(bank,class)
a<- as.data.frame(lapply(bank[6:9], normalize))
head(a)
 
bank=data.frame(cbind(bank[1:5],a[1:4],bank[10:12]))  # Normalized data of numerical variables
 

#split data into train and test data set 

set.seed(5)
indices=sample(nrow(bank),0.70*nrow(bank))
train=bank[indices, ]   #we put 70% data in train set and 30%data in test set
test=bank[-indices, ]
NROW(train)
NROW(test)

#what is logistic regression,mutinomial logistic regression and ordinal logistic regression:https://www.datacamp.com/community/tutorials/logistic-regression-R
#  Youtube= https://www.youtube.com/watch?v=Z5WKQr4H4Xk
logistic_model1=glm(Loan_Status~.,data=train,family = "binomial")
summary(logistic_model1)
#install.packages("caret")
library(caret)
#install.packages("caTools")

Logistic_prob=predict(logistic_model1,test,type="response");Logistic_prob
Logistic_pred <- ifelse(Logistic_prob > 0.50, "1", "0")

logistic_confusion_matrix=table(Actualvalue=test$Loan_Status,predictivalue=Logistic_pred)
logistic_confusion_matrix
Accuracy<-sum(diag(logistic_confusion_matrix)/sum(logistic_confusion_matrix));Accuracy



#To get the best threshold, we use ROC curve

library(ROCR)
#install.packages("ROCR")
res=predict(logistic_model1,train,type="response");res
ROCRpred=prediction(res,train$Loan_Status);ROCRpred
ROCRpref=performance(ROCRpred,"tpr","fpr");ROCRpref   #tpr=true positive rate and fpr=false positive rate
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.05))
plot(ROCRpref)

# We choose threshold value such that TPR large and FPR less.Also consider good AUC
Logistic_prob=predict(logistic_model1,test,type="response");Logistic_prob
Logistic_pred <- ifelse(Logistic_prob > 0.35, "1", "0")

logistic_confusion_matrix=table(Actualvalue=test$Loan_Status,predictivalue=Logistic_pred)
logistic_confusion_matrix
logistic_Accuracy<-sum(diag(logistic_confusion_matrix)/sum(logistic_confusion_matrix));logistic_Accuracy


# install.packages("caret")  #package used to calculate confusion matrix
library(caret)
#install.packages("e1071")
library(e1071)
confusionMatrix(table(Actualvalue=test$Loan_Status,predictivalue=Logistic_pred))

# Ctrl+Shift+C : multiple line comment
#Recall:It gives us an idea about when it's actual yes(1) ,how often does it predict yes
  
#(Sensitivity or TPR or Hit rate=TP/(TP+FN) =0.97
  
  # Specificity or TNR =TN/(TN+FP) =
  # 
  # >Precision: It tells us about when it predicts yes , how often is it correct
  # 
  # Positive Predictive Value(Precision) = TP/(TP+FP) =
  
  # Negative Predictive Value = TN/(TN+FN)=
  # 
  # >Accuracy : the proportion of the total number of predictions that were correct
  # 
  # Accuracy = (TN+TP)/(TN+FP+FN+TP)=
  # 
  # >Error rate = incorrect
  # >Error rate = incorrect predictions / total predictions
  # 
  # Error rate = (FP+FN)/(TN+FP+FN+TP) = 1-Accuracy=
  # 
  # >False Alarm
  # 
  # FP : Type 1 error
  # 
  # FN : Type 2 error
  # 
  # Note:
  #   
  #   High recall value, low precision value: This means that most of the positive examples are correctly recognized (low FN) but there are a lot of false positives.
  # 
  # Low recall value, high precision value: This shows that we miss a lot of positive examples (high FN) but those we predict as positive are indeed positive (low FP)
  # 
  # False)))
  # 
  # 
  
# prediction for new points
names(bank)


data.frame("s"=1,"w"=2)                      
                      
#--------------------------------------------------------------------------------------
#k-nearest neighbors classifier:-
# Youtube : https://www.youtube.com/watch?v=XSoau_q0kz8
 

# set.seed(123)
# indices=sample(nrow(normalizedata),0.70*nrow(normalizedata))
# train=normalizedata[indices, ]   #we put 70% data in train set and 30%data in test set
# test=normalizedata[-indices, ]

library(class)     #It is carries KNN fuction
knn_model=knn(train=train,test=test,cl=train$Loan_Status,k=3)
knn_model
attributes(knn_model)


knn_confusion_matrix=table(Actualvalue=test$Loan_Status,predictivalue=knn_model)
knn_confusion_matrix
knn_Accuracy<-sum(diag(knn_confusion_matrix)/sum(knn_confusion_matrix));knn_Accuracy


# Hyper Parameter tuning for K :-
i=1                      # declaration to initiate loop
k.optm=1                 #declaration to initiate loop
for (i in 1:28) {
  knn.mod=knn(train=train,test=test,cl=train$Loan_Status,k=i)
  
  knn_confusion_matrix=table(Actualvalue=test$Loan_Status,predictivalue=knn.mod)
  k.optm[i]=100*sum(diag(knn_confusion_matrix)/sum(knn_confusion_matrix))
  k=i
  cat(k,"=",k.optm[i],"\n")  # print accuarcy
}


plot(k.optm,type="b",xlab="k-value",ylab="Accuracy level")

k.optm
i=seq(1,28,1)
k_value=data.frame(i,k.optm)  # we can filter in ascending order

# For k = 3 or 5 we get accuracy 97.22222


library(class)
knn_model=knn(train=train,test=test,cl=train$Loan_Status,k=5)
knn_model
attributes(knn_model)


knn_confusion_matrix=table(Actualvalue=test$Loan_Status,predictivalue=knn_model)
knn_confusion_matrix
knn_Accuracy<-sum(diag(knn_confusion_matrix)/sum(knn_confusion_matrix));knn_Accuracy


library(caret)           #package used to calculate confusion matrix
#install.packages("e1071")
library(e1071)
confusionMatrix(table(test$Loan_Status,knn_model))
#------------------------------------------------------------------------------------

#Decision tree classifier
#Youtube link:https://www.youtube.com/watch?v=opQ49Xr748k&t=439s 
# https://www.youtube.com/watch?v=tU3Adlru1Ng


library(rpart)
library(tree)
library(rpart.plot)
?rpart
decision_tree=rpart(train$Loan_Status~.,data=train,control=rpart.control(3))
plot(decision_tree, use.n=TRUE,pretty =TRUE,margin=0.1) 
#margin use to adjust the size of the plot
text(decision_tree,cex=0.6)  
# cex used to adjust font size

str(decision_tree)
attributes(decision_tree)

decision_tree$variable.importance
pred=predict(decision_tree,test,type=c("class"))
pred
library(caret)
#install.packages("e1071")
library(e1071)
confusionMatrix(table(test$Loan_Status,pred))



####Hyper parameter tunning for DT :
i=1                      # declaration to initiate loop
minbucket.optm_train=1           #declaration to initiate loop for train data
minbucket.optm_test=1           #declaration to initiate loop for test data
for (i in 1:25) {
  decision_tree=rpart(train$Loan_Status~.,data=train,control=rpart.control(minbucket =i))
  pred1=predict(decision_tree,train,type=c("class"))
  pred2=predict(decision_tree,test,type=c("class"))
  DT_confusion_matrix1=table(Actualvalue=train$Loan_Status,predictivalue=pred1) # for train data
  DT_confusion_matrix2=table(Actualvalue=test$Loan_Status,predictivalue=pred2)  # for test data
  minbucket.optm_train[i]=100*sum(diag(DT_confusion_matrix1)/sum(DT_confusion_matrix1))
  minbucket.optm_test[i]=100*sum(diag(DT_confusion_matrix2)/sum(DT_confusion_matrix2))
  k=i
  cat(k,"=",minbucket.optm_train[i],minbucket.optm_test[i],"\n")  # print accuarcy
}


#plot(depth.optm_train,type="b",xlab="depth-value",ylab="Accuracy level")

i=seq(1,25,1)
minbucket_value=data.frame(i,minbucket.optm_train,minbucket.optm_test)  # we can filter in ascending order


##Using minbucket =7
decision_tree=rpart(train$Loan_Status~.,data=train,control=rpart.control(minbucket = 7))
rpart.plot(decision_tree,cex=0.6,extra=4,shadow.col="gray",box.palette="auto") 
           # cex used to adjust font size
           # extra=4 get probability of being 0 and 1

pred=predict(decision_tree,test,type=c("class"));pred
library(caret)
library(e1071)
confusionMatrix(table(test$Loan_Status,pred))



#Decision tree with party library
str(bank)
library(party)
decision_tree=ctree(train$Loan_Status~.,data=train,control=ctree_control(mincriterion=0.90,minsplit=30))
        #mincriterion = is confidence level for variable significant 
        #minsplit= branch will split further in 2 if sample size atleast 30
decision_tree
plot(decision_tree)
predict(decision_tree,test,type="prob")

pred=predict(decision_tree,test,type="response");pred
library(e1071)
confusionMatrix(table(test$Loan_Status,pred))

#------------------------------------------------------------------------------------
# Random Forest: 
# Random Forest in R - Classification and Prediction Example with Definition & Steps: https://www.youtube.com/watch?v=dJclNIN-TPo
library(randomForest)
RF=randomForest(Loan_Status~.,data=train);RF
    # where by default, ntree=500, ntry=3  -->( sqrt( no. of features))
    #OOB : out of bag error
    # prediction 0 error = 0.58653846 and prediction 1 error =0.05172

attributes(RF)
RF$type
RF$ntree
RF$mtry
RF$inbag
RF$classes
RF$confusion
RF$err.rate

RF_pred=predict(RF,test);RF_pred
library(caret)
confusionMatrix(test$Loan_Status,RF_pred)

# Number of nodes for the trees
hist(treesize(RF),main="Number of nodes for the trees",col="orange")
      # the frequency(no. of trees) of nodes between 65-70 is 120 and 90 tress have near to 3 nodes

# Variable important.
varImpPlot(RF)
    # Graph show how the pure node, Top 4 are important and more contribution for good accuracy. and bottom 4 variables namely Gender, Self Employed,Education and Married are less important 

varImpPlot(RF,sort=TRUE,n.var=10,main="Top 10 important features")
importance(RF)  # values 
varUsed(RF)  # find out which predictor variables are actually used in the RF
   # Gender variable used 1384 times in RF

#----------------------------------------------------------------------------------------
#Naive Bayes Classification : based on Bayes theorem 
# P(A/B) = (P(B/A)*P(A))/P(B)   # For numerical variable prob. calculated using Z
library(naivebayes)
library(dplyr)
library(psych)

str(bank)
names(bank)
NROW(train)
NROW(test)
naive_bayes_model=naive_bayes(Loan_Status~.,data=train);naive_bayes_model
plot(naive_bayes_model) #get many plots
   # if credit history 1 then there is more chances to get loan

naive_bayes_prob=predict(naive_bayes_model,test,type="prob")
tail(cbind(naive_bayes_prob,test[12]))

naive_bayes_pred=predict(naive_bayes_model,test)
naive_bayes_pred


library(caret)
confusionMatrix(test$Loan_Status,naive_bayes_pred)

#------------------------------------------------------------------------------------------------
# Neural Network for classification
library(neuralnet)
?neuralnet
dim(bank)   # Dimension of data
head(bank)
bank1=bank
#For NN all features required in numerical form
bank1$Gender=as.numeric(bank1$Gender)
bank1$Married=as.numeric(bank1$Married)
bank1$Dependents=as.numeric(bank1$Dependents)
bank1$Education=as.numeric(bank1$Education)
bank1$Self_Employed=as.numeric(bank1$Self_Employed)
bank1$Credit_History=as.numeric(bank1$Credit_History)
bank1$Property_Area=as.numeric(bank1$Property_Area)
  
str(bank1)
df_train=data.frame(train)
df_test=data.frame(test)
head(df_train)
names(df_train)
str(df_train)
nn=neuralnet(Loan_Status~ Credit_History+ApplicantIncome+LoanAmount,data=bank1, hidden = 1,act.fct = "logistic",linear.output = FALSE)
nn

plot(nn)
nn_pred=predict(nn,bank1);head(nn_pred)
response=nn_pred[,1];head(response)
y_pred <- ifelse(response > 0.5, "1", "0");head(y_pred)
y_pred1=as.factor(y_pred);head(y_pred1)
confusionMatrix(bank1$Loan_Status,y_pred1)

table(bank1$Loan_Status,y_pred1)



