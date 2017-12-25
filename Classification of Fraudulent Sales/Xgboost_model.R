#loading libraries

library(randomForest)
library(dummies)

#loading trainig and test dataset

train <- read.csv("~/R_PROG/f992303a-d-BW2017_2/train.csv")
test <- read.csv("~/R_PROG/f992303a-d-BW2017_2/test.csv")

#Xgboost wants that all its featured variable should be of numeric type.
#Thus I made dummy variable for all the factor-type column variable of two levels only.

nam=colnames(train[c(12,13,23:26)]) #nam contains names of factor variable with two factor levels
ttt=train[-c(1,4,9:11,14:22,32:50)] #removing factor variable because we will use their dummy variable inplace of them
df <- dummy.data.frame(ttt, names=nam, sep="_") #Creating dataframe "df" contaning dummy variable and other variable.

#Model fitting
#objective="multi:softprob" for classification
#num_class=2 as our target variable has only two levels
#eval_metric="merror" for classification

bstt <- xgboost(data = data.matrix(df[,-24]), label = train$target, max_depth = 9,
               eta = 0.005, nthread = 10, nrounds = 100,
               objective = "multi:softprob",num_class=2,
               eval_metric = "merror")

tts=test[-c(1,4,9:11,14:22,32:50)] making copy of test data with only relevant variable for prediction
df1 <- dummy.data.frame(tts, names=nam, sep="_")# making dummy variable for test data factor variables

#Predicting probabilities
pred=predict(bstt,data.matrix(df1))
prede=pred[seq(2,length(pred),2)] #Since pred variable contains probabilities of both FALSE and TRUE probabilities, I extracted probabilities at even place.

#0.68% online score in this competition.
