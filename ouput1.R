library("lmtest")
library("sandwich")
library("tidyverse")
library("party")

#reading data
source("http://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")
gse <- read.csv("http://bigblue.depaul.edu/jlee141/econdata/fannie_mae/Fannie_Mort_IL_2007.csv")
str(gse)

#creating indata by removing unecseassry columns
indata <- subset(gse,select=-c(fstimebuyer,state,orgyear,relo_flg,zip_3))

#converting categorical variables to factor variables
indata$purpose  <- as.factor(indata$purpose)
indata$occ_stat <- as.factor(indata$occ_stat)
str(gse)

#converting dummy variables
##install.packages('fastDummies')
library('fastDummies')
indata <- dummy_cols(indata, select_columns = c('purpose','occ_stat'),remove_selected_columns = TRUE)
str(indata)
zindata = min_max_nor(indata)

#splitting data into train and test
train_idx <-sample(nrow(indata),round(0.8*nrow(indata))) 
train <- indata[train_idx,]
test <- indata[-train_idx,] 
str(test)
str(train)
set.seed(2117335)

train$delinq    <- as.factor(train$delinq)

test$delinq <- as.factor(test$delinq)

#estimating nerual network models 
#install.packages("neuralnet")
library(neuralnet)

#running neural network with delinq as target variable for hidden=5
NN = neuralnet(delinq ~ orig_rt +orig_trm + oltv + num_bo + dti + cscore_b + num_unit, train, hidden = 5 , linear.output = T )

# plot neural network
plot(NN)

#neural network for hidden=c(5,2)
NN1 = neuralnet(delinq ~ orig_rt +orig_trm + oltv + num_bo + dti + cscore_b + num_unit, train, hidden = c(5,2) , linear.output = T )

# plot neural network
plot(NN1)

#neural network for hidden=c(7,2)
NN2 = neuralnet(delinq ~ orig_rt +orig_trm + oltv + num_bo + dti + cscore_b + num_unit, train, hidden = c(7,3) , linear.output = T )

# plot neural network
plot(NN2)

#neural network of own choice
NN3 = neuralnet(delinq ~ orig_rt +orig_trm + oltv + num_bo + dti + cscore_b + num_unit, train, hidden = c(8,4) , linear.output = T )

# plot neural network
plot(NN3)

#installing package for ROC AUC curve
#install.packages("dplyr")                     
library("dplyr") 
#install.packages("caTools")    # For Logistic regression 

#install.packages('pROC')       # For ROC curve to evaluate model 

library(caTools)

library(pROC)

#plotting ROC AUC curve
pred_test <- predict(NN1,test,type="response")

pred_test
test_prob = predict(NN1, test, type = "response")

test_roc = roc(test$delinq ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

#confusion matrix
##install.packages('caret')
library('caret')
test$delinq <- as.factor(test$delinq)
predicted <- predict(NN1, test, type="response")
predicted <- as.factor(predicted)
test$delinq <- ifelse(test$delinq=="Yes", 1, 0)

confusionMatrix(predicted, test$delinq)




#confusionmatrixlast try
predictionstrain <- predict(NN1, train)
table(train$delinq, predictionstrain)
(278+125)/nrow(train) 



#LAST
str(train)
matches.train.nn = train
matches.test.nn = test

str(matches.train.nn)
matches.train.nn$delinq = as.numeric(matches.train.nn$delinq)-1

str(matches.test.nn)
min <- apply(matches.train.nn, 2, min)
max <- apply(matches.train.nn, 2, max)
matches.train.nn <- scale(matches.train.nn, center = min, scale = max)
matches.test.nn  <- scale(matches.test.nn,  center = min, scale = max)

matches.train.nn = as.data.frame(matches.train.nn)
matches.test.nn = as.data.frame(matches.test.nn)

library(neuralnet)

set.seed(2117335)
matches.nn =  neuralnet(delinq ~ orig_rt +orig_trm + oltv + num_bo + dti + cscore_b + num_unit,
                        data = as.data.frame(matches.train.nn),
                        hidden = c(5,2), # number of neurons in hidden layers
                        linear.output = FALSE, # T for regression, F for classification
                        learningrate.limit = NULL)
