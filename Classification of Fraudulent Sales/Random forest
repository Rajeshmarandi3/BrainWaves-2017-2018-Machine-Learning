#loading libraries

library(randomForest)

#loading modified datasets with dummy variable

df <- read.csv("~/R_PROG/df.csv") #training dataset
df1 <- read.csv("~/R_PROG/df1.csv") #test dataset

#Subsetting only numeric columns for fitting
tr=df[,c(1:6)]
tr$target=as.factor(df$target)

#Model fitting
rnf=randomForest(target~.,data=tr,ntree=70,importance=T)

#Predicting probabilities
predr=predict(rnf,df1[,c(1:6)],type = "prob")

#Accuracy 0.72
