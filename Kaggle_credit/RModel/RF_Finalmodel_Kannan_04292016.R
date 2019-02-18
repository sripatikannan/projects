#Final Model Random FOrest
# Installing packages to be run on cluster

install.packages("mgcv",repos="http://cran.rstudio.com/", lib="/home/kannans/Rlibs")
library(mgcv,lib.loc = "/home/kannans/Rlibs")
install.packages("rpart",repos="http://cran.rstudio.com/",lib="/home/kannans/Rlibs")
library(rpart,lib.loc = "/home/kannans/Rlibs")
install.packages("earth",repos="http://cran.rstudio.com/",lib="/home/kannans/Rlibs")
library(earth,lib.loc = "/home/kannans/Rlibs")
install.packages("randomForest",repos="http://cran.rstudio.com/",lib="/home/kannans/Rlibs")
library(randomForest,lib.loc = "/home/kannans/Rlibs")
install.packages("foreign",repos="http://cran.rstudio.com/",lib="/home/kannans/Rlibs")
library(foreign,lib.loc = "/home/kannans/Rlibs")
install.packages("mice",repos="http://cran.rstudio.com/",lib="/home/kannans/Rlibs")
library(mice,lib.loc = "/home/kannans/Rlibs")

#converting excel to train and test dataset
train = read.csv("train.csv")
test = read.csv("test.csv")
hist(train$Response)
Response <- train$Response

#running mice to impute missing data 

full <- rbind( train[,-ncol( train )], test )
remove( train ); remove( test )
names <- names( full )
train_imp <- mice( full, m = 1 )

full <- complete( train_imp )
remove( train_imp )
names( full ) <- names
train <- full[1:length( Response ),]
train <- cbind( train, Response )
#saving data set as train and test again after imputation
full <- full[(1+length( Response )):nrow( full ),]
#changing factor variables for Random Forest
train$Product_Info_1<-as.factor(train$Product_Info_1)
train$Product_Info_3 <- factor( train$Product_Info_3 )
train$Product_Info_7 <- factor( train$Product_Info_7 )

train$Employment_Info_2 <- factor( train$Employment_Info_2 )

train$InsuredInfo_1 <-  factor( train$InsuredInfo_1 )
train$InsuredInfo_3 <-  factor( train$InsuredInfo_3 )

train$Insurance_History_2 <- factor( train$Insurance_History_2 )
train$Insurance_History_3 <- factor( train$Insurance_History_3 )
train$Insurance_History_4 <- factor( train$Insurance_History_4 )
train$Insurance_History_7 <- factor( train$Insurance_History_7 )
train$Insurance_History_8 <- factor( train$Insurance_History_8 )
train$Insurance_History_9 <- factor( train$Insurance_History_9 )

train$Family_Hist_1 <- factor(train$Family_Hist_1)

train$Medical_History_2 <- factor( train$Medical_History_2 )
train$Medical_History_3 <- factor( train$Medical_History_3 )
train$Medical_History_6 <- factor( train$Medical_History_6 )
train$Medical_History_8 <- factor( train$Medical_History_8 )
train$Medical_History_13 <- factor( train$Medical_History_13 )


#Random Forest Final Model
set.seed(1)
full<- train
#Converting train and test of full model into 50 50
train1<-train[sample(nrow(train), 29690),]
test<-train[!rownames(train) %in% rownames(train1),]
#Removing one row to make even training and test dataset otherwise RF gives an error
test<-test[-29691,]
y.train<- Response[rownames(train) %in% rownames(train1)]
train<-train1

#To run the same model, remove the following variables which were found out as less important from the Random forest
# vs <- c( 5,7,19,22,41,43,46,49,54,55,57,61,65,69,72,77,79,80,81,82,83,84,
#          85,87,88,89,91,92,93,94,95,96,97,99,101,102,103,104,105,106,107,
#          108,109,110,111,113,114,116,118,119,120,121,122 )
# 
# train <- train[,-vs ] # remove variable which importance in model is smaller than 50
# 
# vs <- c( 1,12,14,20,22,23,28,35,37,40,44,45,57,59,61,63,66,67,68,69,70 )
# train <- train[,-vs] # remove variable which importance in model is smaller than 100
# 
# vs <- c( 2,4,13,15,17,20,22,29,30,31,33,34,36,37,38,39,40,42,43,44,45,46,49 )
# train <- train[,-vs] # remove variable which importance in model is smaller than 200

#Random string function for Cross Validation

RandomString = function(percent,length)
{
  y = c()
  for(i in 1:length)
  {
    if(runif(1,0,1)<=percent)
    {
      y[i] = 1
    }
    else
    {
      y[i] = 0
    }
  }
  y
}
#assignments for performing CV
data=train #shortening name for ease of use
actual=train$Response#shortening name for ease of use
numHoldouts = 5 #number of holdouts to improve cross validation

vecMSErf1 = c() #Mean Square Error term
vecMAErf1 = c() #Mean Absolute Error term

for(i in 1:numHoldouts)
{
  y = RandomString(.25,nrow(data))
  tmp_data = cbind(data,y)
  tmp_actual = cbind(actual,y)
  holdout=subset(tmp_data,y==1)
  holdout_actual=subset(tmp_actual,y==1)
  leftover=subset(tmp_data,y==0)
  
  leftover<-data.frame(leftover)
  holdout<-data.frame(holdout)
  #model with ntree = 350 
  model.rf1<-randomForest(Response~ .,ntree=350, data=leftover)
  
  vecMSErf1[i] = mean((holdout_actual[,1]-predict(model.rf1,holdout, type="response"))^2)
  vecMAErf1[i] = (mean(abs(holdout_actual[,1]-predict(model.rf1,holdout, type="response"))))
}

save.image(file="rf_mice_1.Rdata")
mean(vecMAErf1)
mean(vecMSErf1)
save.image(file="rf_mice_1.Rdata")

varImpPlot(model.rf1,sort=TRUE,scale=TRUE,cex=1.2,pch=16,main="Variable Importance")
model.rf1
#check for variable influence
partialPlot(model.rf1,x.var='Wt',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='BMI',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Ins_Age',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Family_Hist_3',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Family_Hist_5',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Family_Hist_4',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Family_Hist_2',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Medical_History_2',pred.data=train, ylab='Response')
partialPlot(model.rf1,x.var='Ht',pred.data=train, ylab='Response')
#prediction for rf
pred<-round(predict(model.rf1,test,type="response"))
#predicted v/s observed for RF with categorical Response variable
library("ggplot2")
results <- data.frame(pred = predict(model.rf1, test),
                      obs = train1$Response)
plot(model.rf1)
p <- ggplot(results, aes(x = round(pred), y = obs))
p <- p + geom_jitter(position = position_jitter(width = 0.25, height = 0.25))
p
#Standard Deviation of Randomm Forest Model as requested in the report
sd(vecMAErf1)
sd(vecMSErf1)

