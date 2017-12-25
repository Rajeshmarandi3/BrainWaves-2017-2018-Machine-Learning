
library(caret) #caret package
library(DMwR) #package for misisng value imputation
library(randomForest) #randomforest package
library(Dummies) # for making dummy variable

#loading trainig and test set 

traine <- read.csv("~/R_PROG/b0a982ac-d-BW2017Data/train.csv")
test <- read.csv("~/R_PROG/b0a982ac-d-BW2017Data/test.csv")

#Removing outlier from variable "return"
train=traine[-c(1666,1732,7097),]

#Converting relevant variable to date type

train$start_date=as.Date(as.character(train[['start_date']]),"%Y%m%d")
train$creation_date=as.Date(as.character(train[['creation_date']]),"%Y%m%d")
train$sell_date=as.Date(as.character(train[['sell_date']]),"%Y%m%d")

#Labeling relevant variable to 1 and 0 inplace of TRUE and FALSE

train$indicator_code[is.na(train$indicator_code)]<-0
train$indicator_code[train$indicator_code=="TRUE"]<-1

train$status[is.na(train$status)]<-0
train$status[train$status=="TRUE"]<-1

train$hedge_value[train$hedge_value=="TRUE"]<-1
train$hedge_value[train$hedge_value=="FALSE"]<-0

##As Imputing Missing value works only on numeric value
##Column with factor value are removed only for this process
tr=train[,-c(1,2,5,12,14)]
knnOutput1 <- knnImputation(tr[,names(tr)],3) #3Knn Imputation
tr=knnOutput1 #tr is a dataframe with misisng value Imputed

#Assigning imputed value of column "hedge_value" to 1 and 0
tr$hedge_value[tr$hedge_value>0.5]=1
tr$hedge_value[tr$hedge_value<=0.5]=0
train$sold=tr$sold
train$libor_rate=tr$libor_rate
train$bought=tr$bought
train$hedge_value=tr$hedge_value

##Repeating all the above procedure for test dataset

test$start_date=as.Date(as.character(test[['start_date']]),"%Y%m%d")
test$creation_date=as.Date(as.character(test[['creation_date']]),"%Y%m%d")
test$sell_date=as.Date(as.character(test[['sell_date']]),"%Y%m%d")

test$indicator_code[is.na(test$indicator_code)]<-0
test$indicator_code[test$indicator_code=="TRUE"]<-1
test$status[is.na(test$status)]<-0
test$status[test$status=="TRUE"]<-1

test$hedge_value[test$hedge_value=="TRUE"]<-1
test$hedge_value[test$hedge_value=="FALSE"]<-0

ts=test[,-c(1,2,5,12,14)]
knnOutput2 <- knnImputation(ts[,names(ts)],3) 
ts=knnOutput2
ts$hedge_value[ts$hedge_value>0.5]=1
ts$hedge_value[ts$hedge_value<=0.5]=0
test$sold=ts$sold
test$libor_rate=ts$libor_rate
test$bought=ts$bought
test$hedge_value=ts$hedge_value

#Now we me imputed our numeric column variable .
#Now lets make dummy variable for our factor variable.
#First of all we want all the column names that include.

nam=c("office_id","portfolio_id","country_code","type","currency")
dff <- dummy.data.frame(train, names=nam, sep="_") # Creating dummy dataframe
dff1 <- dummy.data.frame(test, names=nam, sep="_") # for test set

#Making new variable df1 and df2
df1=train$creation_set-train$start_date #differnce between this date
df2=train$sell_date-train$creation_set #differnce between this date

#Now drop the columns "start_date","creation_set","sell_date" 
 # I saved these dataset as train2 and test2 dataset
 
 #All the preprocessing is done 
 
 #Model fitting
 
train2 <- read.csv("~/R_PROG/b0a982ac-d-BW2017Data/train2.csv")
test2 <- read.csv("~/R_PROG/b0a982ac-d-BW2017Data/test2.csv")

#Using caret package for fitting random forest
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=12)
rf_gridsearch <- train(return~., data=tt[,-c(1,2)], method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)

pred=predict(rf_gridsearch,test2[,-c(1,2)])

#0.95352 online score for this competition

 
 

