#loading libraries

library(randomForest)

#loading modified datasets with dummy variable
df <- read.csv("~/R_PROG/df.csv")
df1 <- read.csv("~/R_PROG/df1.csv")
tr=df[,c(1:6)]
tr$target=as.factor(df$target)
rnf=randomForest(target~.,data=tr,ntree=70,importance=T)

predr=predict(rnf,df1[,c(1:6)],type = "prob")
# head(predr[,2],20)

# sample_submissions <- read.csv("~/R_PROG/f992303a-d-BW2017_2/sample_submissions.csv")

sample_submissions$target=predr[,2]
write.csv(sample_submissions,"fr11.csv")

######################################################################3
