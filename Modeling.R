library(dplyr)
library(tidyr)
library(caret)
library(mice)
library(randomForest)
library(caTools)
library(rpart)
library(parallel)
library(doParallel)
library(pROC)
library(tree)
library(ggplot2)
library(tidyverse)
library(OptimalCutpoints)

impounds1920 <-read.csv("D:\\SMU\\6366\\Data\\final_impounds_v2.csv")
sapply(impounds1920, function(x) sum(is.na(x)))

# create our outcome variale 'repreat', it was based on prev_indata, 0 for not repreat,1 for repreat.
impounds1920$Repreat<-ifelse(is.na(impounds1920$Prev_InDate),0,1)
impounds1920$Repreat<-factor(impounds1920$Repreat)
levels(impounds1920$Repreat)<-c("No","Yes")
#impounds1920$Repreat
str(impounds1920)
impounds1920<-dplyr::filter(impounds1920, grepl('DOG|CAT', animal_type))
table(impounds1920$animal_type)
#write.csv(impounds1920, file = "D:\\SMU\\6366\\Data\\final_impounds_v3.csv")


#remove all the useless variables
impounds1920$In_Date<-NULL
impounds1920$crossing<-NULL
impounds1920$age_now<-NULL
impounds1920$Prev_Intake<-NULL
impounds1920$Prev_InDate<-NULL
impounds1920$Prev_InType<-NULL
impounds1920$Prev_ZipCode<-NULL
impounds1920$Prev_Addr<-NULL
impounds1920$day<-NULL
impounds1920$geoAddress<-NULL
impounds1920$ZipCode<-NULL
impounds1920$age_month<NULL
impounds1920$age_year<-NULL
impounds1920$age_week<-NULL
impounds1920$age_day<-NULL
impounds1920$age_num_days<-NULL
impounds1920$age_month<-NULL
impounds1920$Impound<-NULL
impounds1920$Animal<-NULL
impounds1920$X<-NULL

head(impounds1920)
str(impounds1920)
# change variable type
impounds1920$Type<-factor(impounds1920$Type)
impounds1920$sex<-factor(impounds1920$sex)
impounds1920$animal_type<-factor(impounds1920$animal_type)
impounds1920$year<-factor(impounds1920$year)
impounds1920$month<-factor(impounds1920$month)

# convert to dummmy variable and remove the original 
dmy<-dummyVars(" ~ Type + sex+animal_type+year+month", data = impounds1920)
data <- data.frame(predict(dmy, newdata = impounds1920))
data1<-cbind(impounds1920,data)
data1$Type<-NULL
data1$sex<-NULL
data1$animal_type<-NULL
data1$year<-NULL
data1$month<-NULL
data1$zipcode_new<-NULL
head(data1)
sapply(data1, function(x) sum(is.na(x)))

# impute the missing values
cl <- makePSOCKcluster(11)
registerDoParallel(cl)
imp1 <- mice(data1, method = c("pmm"), seed = 23109,maxit=30,m=10)
#plot(imp1)
#densityplot(imp1)
# stripplot(imp1)
data2 <- complete(imp1, 10)
sapply(data2, function(x) sum(is.na(x)))
# scale the numeric variable
data2[c(1,2,3)] <- scale(data2[c(1,2,3)])

stopCluster(cl)

#fdata$Repreat <- relevel(fdata$Repreat,ref = "No")
set.seed(470)

#Split the data
train.index <- createDataPartition(data2$Repreat, p = .7, list = FALSE)
train <- data2[ train.index,]
test  <- data2[-train.index,]
train_x<-train[1:34][,-4]
train_y<-as.factor(train$Repreat)
test_x<-test[1:34][,-4]
test_y<-as.factor(test$Repreat)

# set some parameters
glmnGrid <- expand.grid(alpha = c(0, 0.5, 1),lambda = seq(0.01, 0.2, length = 10))
ctrl <- trainControl(method = "repeatedcv",summaryFunction = twoClassSummary,classProbs = TRUE,repeats=5,savePredictions = TRUE)
model_weights <- ifelse(train$Repreat == "No",
                        (1/table(train$Repreat)[1]) * 0.5,
                        (1/table(train$Repreat)[2]) * 0.5)
# Random forest

library(MLmetrics)
cl <- makePSOCKcluster(11)
registerDoParallel(cl)

#loop the mytry
set.seed(476)
a=c()
i=3
for (i in 3:15) {
  rfFit<-randomForest(train$Repreat~ ., data=train)
  predValid <- predict(rfFit, test, type = "class")
  a[i-2] = mean(predValid == test$Repreat)
}
# The best mytry is 4
mytry<-which.max(a)+2

rfFit<-randomForest(train$Repreat~ .,mytry=mytry, data=train)
stopCluster(cl)
rfpred<-predict(rfFit,test)
rfCM <- confusionMatrix(table(rfpred,test_y),positive = "Yes",mode = "everything")
rfCM
# Sensitivity : 0.9942          
# Specificity : 0.2302
#  Accuracy : 0.8689  

#ROC anc AUC
rf_y<-as.numeric(test_y)
rf_predy<-as.numeric(rfpred)
rfROC <- roc(rf_y, predictor=rf_predy)
auc(rfROC)
plot(rfROC, legacy.axes = TRUE)

#variable importance 
varImpPlot(rfFit,scale=FALSE)

#try with optimal cutoff
m1_preds<- data.frame(prob = predict(rfFit, type = "prob")[,2])
m1_preds$pred <- predict(rfFit)
m1_preds$obs <- train$Repreat
optcut0 <- summary(optimal.cutpoints(X = "prob", status = "obs", data = m1_preds,
                                    tag.healthy = "No", methods = "MaxKappa"))


final_cut0 <- optcut0$MaxKappa$Global$optimal.cutoff$cutoff
final_cut0
#0.1271676
# refit with optimal
rfpred1<-data.frame(predict(rfFit,test,type = "prob"))
optimalrf<-ifelse(rfpred1$No>1-(final_cut0),"No","Yes")
head(optimalrf)
rfCMop <- confusionMatrix(table(optimalrf,test_y),positive = "Yes",mode = "everything")
rfCMop
#Accuracy : 0.8404 
#Sensitivity : 0.9079         
#Specificity : 0.4966         
#Pos Pred Value : 0.9019         
#Neg Pred Value : 0.5140 



#NN
cl <- makePSOCKcluster(11)
registerDoParallel(cl)
nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)
ctrl <- trainControl(method = "repeatedcv",summaryFunction = multiClassSummary,classProbs = TRUE,savePredictions = TRUE)
set.seed(476)
#without weights
nnetFit <- train(x = train_x,y = train_y,method = "nnet",metric = "Kappa",preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,trace = FALSE,maxit = 2000,
                 MaxNWts = 1*(maxSize * (ncol(train) + 1) + maxSize + 1),trControl = ctrl)
#with weights
nnetFit1 <- train(x = train_x,y = train_y,method = "nnet",metric = "Kappa",preProc = c("center", "scale"),weights = model_weights,
                  tuneGrid = nnetGrid,trace = FALSE,maxit = 2000,
                  MaxNWts = 1*(maxSize * (ncol(train) + 1) + maxSize + 1),trControl = ctrl)
#The final values used for the model were size = 10 and decay = 1
stopCluster(cl)

nnpred<-predict(nnetFit,test)
nnCM <- confusionMatrix(table(nnpred,test_y),positive = "Yes",mode = "everything")
nnCM
#Accuracy : 0.8599 
#Sensitivity : 0.9807          
#Specificity : 0.2442
#Neg Pred Value : 0.7133 

nnpred1<-predict(nnetFit1,test)
nnCM1 <- confusionMatrix(table(nnpred1,test_y),positive = "Yes",mode = "everything")
nnCM1 

#variable importance
nnimportance <- varImp(nnetFit, scale=FALSE)
plot(nnimportance)

# AUC and ROC
nn_predy<-as.numeric(nnpred)
nnROC <- roc(rf_y, predictor=nn_predy)
auc(nnROC)
plot(nnROC, legacy.axes = TRUE)

# try optimal without weights
m2_preds<- data.frame(prob = predict(nnetFit, type = "prob")[,2])
m2_preds$pred <- predict(nnetFit)
m2_preds$obs<- train$Repreat
head(m2_preds)
optcut2 <- summary(optimal.cutpoints(X = "prob", status = "obs", data = m2_preds[1:40000,], 
                                     tag.healthy = "No", methods = "MaxKappa"))
final_cut2 <- optcut2$MaxKappa$Global$optimal.cutoff$cutoff
final_cut2<-0.2920569
#0.2920569

#refit with optimal
nnpred1<-data.frame(predict(nnetFit,test,type = "prob"))
optimalnn<-ifelse(nnpred1$No>1-(final_cut2),"No","Yes")
head(optimalnn)
nnCMop <- confusionMatrix(table(optimalnn,test_y),mode = "everything")
nnCMop


# try optimal with weights
m2_preds_weight<- data.frame(prob = predict(nnetFit1, type = "prob")[,2])
m2_preds_weight$pred <- predict(nnetFit1)
m2_preds_weight$obs <- train$Repreat
head(m2_preds_weight)
optcut2_weight<- summary(optimal.cutpoints(X = "prob", status = "obs", data = m2_preds_weight[1:40000,], 
                                           tag.healthy = "No", methods = "MaxKappa"))
final_cut2_weight  <- optcut2_weight$MaxKappa$Global$optimal.cutoff$cutoff
final_cut2_weight <- 0.6797424
#0.6797424

#refit with optimal
nnpred1_weight<-data.frame(predict(nnetFit1,test,type = "prob"))
optimalnn_weight<-ifelse(nnpred1_weight$No>1-(final_cut2_weight),"No","Yes")
head(optimalnn_weight)
nnCMop_weight <- confusionMatrix(table(optimalnn_weight,test_y),mode = "everything")
nnCMop_weight

# Logistic
cl <- makePSOCKcluster(11)
registerDoParallel(cl)
ctrl2 <- trainControl(method = "repeatedcv",classProbs = TRUE,repeats=5,savePredictions = TRUE)
set.seed(476)
glmnGrid <- expand.grid(alpha = c(0, 0.5, 1),lambda = seq(0.01, 0.2, length = 10))
# #basic fit with tuneing 
glmnFit1 <- train(x = data.matrix(train_x),y = train_y,method = "glmnet",tuneGrid = glmnGrid,
                   metric = "Kappa",preProc = c("center", "scale"),family = "binomial",trControl = ctrl2)
# #weight and tuneing
glmnFit2 <- train(x = data.matrix(train_x),y = train_y,method = "glmnet",tuneGrid = glmnGrid,weights=model_weights,
                  metric = "Kappa",preProc = c("center", "scale"),family = "binomial",trControl = ctrl2)
stopCluster(cl)
#The final values used for the model were alpha = 0 and lambda = 0.01.
logisticpred1<-predict(glmnFit1,data.matrix(test))
logisticpred2<-predict(glmnFit2,data.matrix(test))

logisticCM1 <- confusionMatrix(table(logisticpred1,test_y),positive = "Yes",mode = "prec_recall")
logisticCM2 <- confusionMatrix(table(logisticpred2,test_y),positive = "Yes",mode = "prec_recall")

logisticCM1
#Sensitivity : 0.11987         
#Specificity : 0.98481 
#Accuracy : 0.8429  

logisticCM2
# Accuracy : 0.7619 
# Sensitivity : 0.53057 
# Specificity : 0.80728 


# #optimal without weight
m3_preds<- data.frame(prob = predict(glmnFit1, type = "prob")[,2])
m3_preds$pred <- predict(glmnFit1)
m3_preds$obs <- train$Repreat
head(m3_preds)
optcut3 <- optimal.cutpoints(X = "prob", status = "obs", data = m3_preds[1:40000,],
                                     tag.healthy = "No", methods = "MaxKappa")
final_cut3 <- optcut3$MaxKappa$Global$optimal.cutoff$cutoff
final_cut3<-0.2787286
# #0.2787286

#refit with optimal
logisticpred<-data.frame(predict(glmnFit1,test,type = "prob"))
optimallogistic<-ifelse(logisticpred$No>1-(final_cut3),"No","Yes")
head(optimallogistic)
logisticCMop <- confusionMatrix(table(optimallogistic,test_y),positive = "Yes",mode = "prec_recall")
logisticCMop

# #optimal with weight

m3_preds_weight<- data.frame(prob = predict(glmnFit2, type = "prob")[,2])
m3_preds_weight$pred <- predict(glmnFit2)
m3_preds_weight$obs <- train$Repreat
head(m3_preds_weight)
optcut3_weight <- optimal.cutpoints(X = "prob", status = "obs", data = m3_preds_weight[1:40000,],
                             tag.healthy = "No", methods = "MaxKappa")
final_cut3_weight <- optcut3_weight$MaxKappa$Global$optimal.cutoff$cutoff
final_cut3_weight<-0.5623562
#0.5623562

#refit with optimal
logisticpred_weight<-data.frame(predict(glmnFit2,test,type = "prob"))
optimallogistic_weight<-ifelse(logisticpred_weight$No>1-(final_cut3_weight),"No","Yes")
head(optimallogistic_weight)
logisticCMop_weight <- confusionMatrix(table(optimallogistic_weight,test_y),positive = "Yes",mode = "everything")
logisticCMop_weight


#variable importance 
logisticimportance <- varImp(glmnFit1, scale=FALSE)
plot(logisticimportance)

tmp_coeffs<-coef(glmnFit1$finalModel, glmnFit1$finalModel$lambdaOpt)
coefdata<-data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

