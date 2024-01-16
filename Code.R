


# Splitting the data into train and test
library(caret)
group_NPI$exclud_count = as.factor(group_NPI$exclud_count)
index <- createDataPartition(group_NPI$exclud_count, p = .80, list = FALSE)
train <- group_NPI[index, ]
test <- group_NPI[-index, ]
table(train$exclud_count)
#ROS(50:50)
library(smotefamily)
set.seed(111)
#50:50
train1 <- ovun.sample(exclud_count ~ .,
                      data = train,
                      p = 0.5,
                      N = nrow(train),
                      seed = 111)$data
table(train1$exclud_count)
#75:25
train2 <- ovun.sample(exclud_count ~ .,
                      data = train,
                      p = 0.25,
                      N = nrow(train), 
                      seed = 111)$data

table(train2$exclud_count)

#25:75
train3 <- ovun.sample(exclud_count ~ .,
                      data = train,
                      p = 0.75,
                      N = nrow(train))$data

table(train3$exclud_count)

#80:20
train_rose = ovun.sample(exclud_count~.,data=train, p = 0.20,  N = nrow(train),seed = 111)$data
train_rose$exclud_count = as.numeric(train_rose$exclud_cou)-1
table(train_rose$exclud_count)
#SMOTE

set.seed(111)
#SMOTE Balanced
train$exclud_count = as.numeric(train$exclud_count)
train.smote <- SMOTE(train,train$exclud_count, dup_size = 0)
train.smote <- train.smote$data 
train.smote$exclud_count <- as.factor(train.smote$exclud_count)
train.smote = train.smote[,-81]
train.smote$exclud_count = as.numeric(train.smote$exclud_count)-1
table(train.smote$exclud_count)

#Logistic Regression
library(caret)
library(ROCR)
library(ROSE)
library(smotefamily)
set.seed(100)
# Training the model
logist_original <- glm(exclud_count ~., data = train,family = "binomial")
logist_original

logist_50_50 <- glm(exclud_count ~., data = train1,family = "binomial")
logist_50_50

logist_75_25 <- glm(exclud_count ~., data = train2,family = "binomial")
logist_75_25

logist_25_75 <- glm(exclud_count ~., data = train3,family = "binomial")
logist_25_75

logist_rose <- glm(exclud_count ~., data = train_rose,family = "binomial")
logist_rose

logist_smote <- glm(exclud_count ~., data = train.smote,family = "binomial")
logist_smote

#Importance 
library(tibble)
importances_original <- varImp(logist_original) %>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall))
importances_original
importances_50_50 <- varImp(logist_o50_50)%>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall))
importances_50_50 
importances_75_25  <- varImp(logist_75_25)%>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall)) 
importances_75_25
importances_25_75 <- varImp(logist_25_75)%>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall))
importances_25_75
importances_rose <- varImp(logist_rose)%>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall))
importances_rose
importances_smote <- varImp(logist_smote1)%>% as.data.frame()%>% rownames_to_column() %>% arrange(desc(Overall)) 
importances_smote

importances_original= importances_original%>% arrange(Overall)
ggplot(importances_original,aes(x =reorder(rowname, Overall), y = Overall)) +  
  geom_bar(stat='identity') +
  labs(x="Features",y="Importance",title="Feature Imortance") +
  theme_bw() + coord_flip()

probs1 <- predict(logist_original, test, type = "response")
pred_original <- as.integer(probs1 > 0.5)
cm_logist1 = confusionMatrix(factor(pred_original), factor(test$exclud_count)) 
cm_logist1$byClass


probs2 <- predict(logist_50_50, test, type = "response")
pred_50_50 <- as.integer(probs2 > 0.5)
cm_logist2 = confusionMatrix(factor(pred_50_50), factor(test$exclud_count))
cm_logist2$byClass

probs3 <- predict(logist_75_25, test, type = "response")
pred_75_25 <- as.integer(probs3 > 0.5)
cm_logist3 = confusionMatrix(factor(pred_75_25), factor(test$exclud_count))
cm_logist3

probs4 <- predict(logist_25_75, test, type = "response")
pred_25_75 <- as.integer(probs4 > 0.5)
cm_logist4 = confusionMatrix(factor(pred_25_75), factor(test$exclud_count))
cm_logist4$byClass

probs5 <- predict(logist_rose, test, type = "response")
pred_rose <- as.integer(probs5 > 0.5)
cm_logist5 = confusionMatrix(factor(pred_rose), factor(test$exclud_count))
cm_logist5$byClass

probs6 <- predict(logist_smote, test, type = "response")
#probs6
pred_smote <- as.integer(probs6 > 0.5)
cm_logist6 = confusionMatrix(factor(pred_smote), factor(test$exclud_count))
cm_logist6$byClass

#accuracy
acc1 = cm_logist1$overall[1]
acc1
acc2 = cm_logist2$overall[1]
acc2
acc3 = cm_logist3$overall[1]
acc3
acc4 = cm_logist4$overall[1]
acc4
acc5 = cm_logist5$overall[1]
acc5
acc6 = cm_logist6$overall[1]
acc6

#Precision
precision_logist1 = cm_logist1$byClass[5]
precision_logist1
precision_logist2 = cm_logist2$byClass[5]
precision_logist2
precision_logist3 = cm_logist3$byClass[5]
precision_logist3
precision_logist4 = cm_logist4$byClass[5]
precision_logist4
precision_logist5 = cm_logist5$byClass[5]
precision_logist5
precision_logist6 = cm_logist6$byClass[5]
precision_logist6
# Recall
recall_logist1 = cm_logist1$byClass[6]
recall_logist1
recall_logist2 = cm_logist2$byClass[6]
recall_logist2
recall_logist3 = cm_logist3$byClass[6]
recall_logist3
recall_logist4 = cm_logist4$byClass[6]
recall_logist4
recall_logist5 = cm_logist5$byClass[6]
recall_logist5
recall_logist6 = cm_logist6$byClass[6]
recall_logist6

#Sensitivity
s.logist1 = cm_logist1$byClass[1]
s.logist1
s.logist2 = cm_logist2$byClass[1]
s.logist2
s.logist3 = cm_logist3$byClass[1]
s.logist3
s.logist4 = cm_logist4$byClass[1]
s.logist4
s.logist5 = cm_logist5$byClass[1]
s.logist5
s.logist6 = cm_logist6$byClass[1]
s.logist6
#F1
f11 = cm_logist1$byClass[7]
f11
f12 = cm_logist2$byClass[7]
f12
f13 = cm_logist3$byClass[7]
f13
f14 = cm_logist4$byClass[7]
f14
f15 = cm_logist5$byClass[7]
f15
f16 = cm_logist6$byClass[7]
f16
# Evaluation Curve
logist.pred1=prediction(probs1,test$exclud_count)
eval1= performance(logist.pred1,"acc")
plot(eval1)

roc1=performance(logist.pred1,"tpr","fpr")
plot(roc1,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

logist.pred2 = prediction( pred_50_50,test$exclud_count)
eval2= performance(logist.pred2,"acc")
plot(eval2)
roc2=performance(logist.pred2,"tpr","fpr")
plot(roc2,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

logist.pred3=prediction(probs3,test$exclud_count)
eval3= performance(logist.pred3,"acc")
plot(eval3)
roc3=performance(logist.pred3,"tpr","fpr")
plot(roc3,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

logist.pred4=prediction(probs4,test$exclud_count)
eval4= performance(logist.pred4,"acc")
plot(eval4)
roc4=performance(logist.pred4,"tpr","fpr")
plot(roc4,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

logist.pred5=prediction(probs5,test$exclud_count)
eval5= performance(logist.pred5,"acc")
plot(eval5)
roc5=performance(logist.pred5,"tpr","fpr")
plot(roc5,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

logist.pred6=prediction(probs6,test$exclud_count)
eval6= performance(logist.pred6,"acc")
plot(eval6)
roc6=performance(logist.pred6,"tpr","fpr")
plot(roc6,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

#AUC 
auc_logist1 = performance(logist.pred1, "auc")@y.values
auc_logist1
auc_logist2 = performance(logist.pred2, "auc")@y.values
auc_logist2
auc_logist3 = performance(logist.pred3, "auc")@y.values
auc_logist3
auc_logist4 = performance(logist.pred4, "auc")@y.values
auc_logist4
auc_logist5 = performance(logist.pred5, "auc")@y.values
auc_logist5
auc_logist6 = performance(logist.pred6, "auc")@y.values
auc_logist6


#XGBoost
library(xgboost)
library(caret)
library(ROSE)
set.seed(112)
train$exclud_count = as.numeric(train$exclud_count)-1
train1 = train1
train1$exclud_count = as.numeric(train1$exclud_count)
train2 = train2
train2$exclud_count = as.numeric(train2$exclud_count)
train3 = train3
train3$exclud_count = as.numeric(train3$exclud_count)
train_rose = rose_train
train_rose$exclud_count = as.numeric(train_rose$exclud_count)
train_smote = train.smote
train_smote$exclud_count = as.numeric(train_smote$exclud_count)
table(train$exclud_count)
#Train data
train.data = data.matrix(train[,-13])
train1.data = data.matrix(train1[,-13])
train2.data = data.matrix(train2[,-13])
train3.data = data.matrix(train3[,-13])
train_smote.data = data.matrix(train_smote[,-13])
train_rose.data = data.matrix(train_rose[,-13])
#Label
train.label = as.matrix(train[,13])
train1.label = as.matrix(train1[,13])
train2.label = as.matrix(train2[,13])
train3.label = as.matrix(train2[,13])
train_smote.label = as.matrix(train_smote[,13])
train_rose.label = as.matrix(train_rose[,13])
#Test data
test.data = data.matrix(test[,-13])
test.label = as.matrix(test[,13])

xgb_train = xgb.DMatrix(data = train.data, label = train.label)
xgb.train1 = xgb.DMatrix(data = train1.data,label = train1.label)
xgb.train2 = xgb.DMatrix(data = train2.data,label = train2.label)
xgb.train3 = xgb.DMatrix(data = train3.data,label = train3.label)
xgb.train_rose = xgb.DMatrix(data = train_rose.data,label = train_rose.label)
xgb.train_smote = xgb.DMatrix(data = train_smote.data,label = train_smote.label)
xgb_test = xgb.DMatrix(data = test.data, label = test.label)
#Training models
num_class = length(levels(exclud_count))
param = list(
  objective = "binary:logistic",
  eta = 0.01,
  gamma = 1,
  max_depth = 6,
  colsample_bytree = 0.5)
set.seed(112) 
xgbcv = xgb.cv(params = param,
               data = xgb_train,
               nrounds = 1000,
               nfold = 5,
               metrics = "auc",
               num_class=num_class,
               print_every_n = 10,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration

xgb.fit_original  = xgb.train(data = xgb_train,
                              params = param,
                              metrics = "auc",
                              nrounds = num_iterations)
xgb.fit_original

set.seed(112)
xgbcv = xgb.cv(params = param,
               data = xgb.train1,
               nrounds = 1000,
               nfold = 5,
               metrics = "auc",
               num_class=num_class,
               print_every_n = 10,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration
xgb.fit1 <- xgb.train(data = xgb.train1,
                      params = param,
                      metrics = "auc",
                      nrounds = num_iterations)
xgb.fit1

set.seed(112)
xgbcv = xgb.cv(params = param,
               data = xgb.train2,
               nrounds = 1000,
               nfold = 5,
               metrics = "auc",
               num_class=num_class,
               print_every_n = 10,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration
xgb.fit2 <- xgb.train(data = xgb.train2,
                      params = param,
                      metrics = "auc",
                      nrounds = num_iterations)
xgb.fit2

set.seed(112)
xgbcv = xgb.cv(params = param,
               data = xgb.train3,
               nrounds = 1000,
               nfold = 5,
               metrics = "auc",
               num_class=num_class,
               print_every_n = 10,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration

xgb.fit3 <- xgb.train(data = xgb.train3,
                      params = param,
                      metrics = "auc",
                      nrounds =num_iterations)
xgb.fit3

set.seed(112)
xgbcv = xgb.cv(params = param,
               data = xgb.train_rose,
               nrounds = 1000,
               nfold = 5,
               metrics = "auc",
               print_every_n = 10,
               num_class=num_class,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration

xgb.fit_rose <- xgb.train(data = xgb.train_rose,
                          params = param,
                          metrics = "auc",
                          nrounds = num_iterations)
xgb.fit_rose

set.seed(112)
xgbcv = xgb.cv(params = param,
               data = xgb.train_smote,
               nrounds = 1500,
               nfold = 5,
               metrics = "auc",
               num_class=num_class,
               print_every_n = 10,
               early_stopping_rounds = 30)
num_iterations = xgbcv$best_iteration
xgb.fit_smote <-xgb.train(data = xgb.train_smote,
                          params = param,
                          metrics = "auc",
                          nrounds = num_iterations)
xgb.fit_smote

#Importance
Importance.xgb <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb.fit_original) 
Importance.xgb

Importance.xgb1 <- xgb.importance(
  feature_names = colnames(xgb.train1), 
  model = xgb.fit1)
Importance.xgb1 

Importance.xgb2 <- xgb.importance(
  feature_names = colnames(xgb.train2), 
  model = xgb.fit2)
Importance.xgb2 

Importance.xgb3 <- xgb.importance(
  feature_names = colnames(xgb.train3), 
  model = xgb.fit3)
Importance.xgb3

Importance.xgb_rose <- xgb.importance(
  feature_names = colnames(xgb.train_rose), 
  model = xgb.fit_rose)
Importance.xgb_rose

Importance.xgb_smote <- xgb.importance(
  feature_names = colnames(xgb.train_smote), 
  model = xgb.fit_smote)
Importance.xgb_smote 
#Plot
xgb.plot.importance(Importance.xgb)

xgb.plot.importance(Importance.xgb1)

xgb.plot.importance(Importance.xgb2)

xgb.plot.importance(Importance.xgb3)

xgb.plot.importance(Importance.xgb_rose)

xgb.plot.importance(Importance.xgb_smote)

# Predict outcomes with the test data
xgb.pred1 = predict(xgb.fit_original,test.data)
xgb.prediction1 = ifelse(xgb.pred1 >= 0.09,1,0)
xgb.cm1 = confusionMatrix(factor(xgb.prediction1), factor(test$exclud_count))
xgb.cm1$byClass

xgb.pred2 = predict(xgb.fit1,test.data,reshape=T)
xgb.prediction2 = ifelse(xgb.pred2 >= 0.5,1,0)
xgb.cm2 <- confusionMatrix(factor(xgb.prediction2), factor(test$exclud_count))
xgb.cm2$byClass

xgb.pred3 = predict(xgb.fit2,test.data,reshape=T)
xgb.prediction3 = ifelse(xgb.pred3 >= 0.5,1,0)
xgb.cm3 <-confusionMatrix(factor(xgb.prediction3), factor(test$exclud_count))
xgb.cm3$byClass

xgb.pred4 = predict(xgb.fit3,test.data,reshape=T)
xgb.prediction4 = ifelse(xgb.pred4 >= 0.48,1,0)
xgb.cm4 <- confusionMatrix(factor(xgb.prediction4), factor(test$exclud_count))
xgb.cm4$byClass

xgb.pred5 = predict(xgb.fit_rose,test.data,reshape=T)
xgb.prediction5 = ifelse(xgb.pred5 >= 0.998,1,0)
xgb.cm5 <- confusionMatrix(factor(xgb.prediction5), factor(test$exclud_count))
xgb.cm5$byClass

xgb.pred6 = predict(xgb.fit_smote,test.data,reshape=T)
xgb.prediction6 = ifelse(xgb.pred6 >= 0.5,1,0)
xgb.cm6 <- confusionMatrix(factor(xgb.prediction6), factor(test$exclud_count))
xgb.cm6$byClass

#Confusion matrix

# xgb.cm1 <- confusionMatrix(xgb.prediction1, test)
xgb.cm1$byClass

# Evaluation Curve
xgb.pred1=prediction(xgb.pred1,test$exclud_count)
eval= performance(xgb.pred1,"acc")
plot(eval)

roc=performance(xgb.pred1,"tpr","fpr")
plot(roc,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

xgb.pred2=prediction(xgb.pred2,test$exclud_count)
eval= performance(xgb.pred2,"acc")
plot(eval)

roc=performance(xgb.pred2,"tpr","fpr")
plot(roc,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

xgb.pred3=prediction(xgb.pred3,test$exclud_count)
eval= performance(xgb.pred3,"acc")
plot(eval)

roc=performance(xgb.pred3,"tpr","fpr")
plot(roc,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

xgb.pred4=prediction(xgb.pred4,test$exclud_count)
eval= performance(xgb.pred4,"acc")
plot(eval)

roc=performance(xgb.pred4,"tpr","fpr")
plot(roc,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

xgb.pred5=prediction(xgb.pred5,test$exclud_count)
eval= performance(xgb.pred5,"acc")
plot(eval)

roc=performance(xgb.pred5,"tpr","fpr")
plot(roc,main="ROC curve",col="red", print.auc = TRUE, percent = TRUE)
abline(a=0,b=1)

xgb.pred6=prediction(xgb.pred6,test$exclud_count)
eval= performance(xgb.pred6,"acc")
plot(eval)

roc=performance(xgb.pred6,"tpr","fpr")
plot(roc,main="ROC curve",col="red", auc = TRUE, percent = TRUE)
abline(a=0,b=1)

#AUC 
auc_xgb1 = performance(xgb.pred1, "auc")@y.values
auc_xgb1
auc_xgb2 = performance(xgb.pred2, "auc")@y.values
auc_xgb2
auc_xgb3 = performance(xgb.pred3, "auc")@y.values
auc_xgb3
auc_xgb4 = performance(xgb.pred4, "auc")@y.values
auc_xgb4
auc_xgb5 = performance(xgb.pred5, "auc")@y.values
auc_xgb5
auc_xgb6 = performance(xgb.pred6, "auc")@y.values
auc_xgb6

#accuracy
acc1 = xgb.cm1$overall[1]
acc1
acc2 = xgb.cm2$overall[1]
acc2
acc3 = xgb.cm3$overall[1]
acc3
acc4 = xgb.cm4$overall[1]
acc4
acc5 = xgb.cm5$overall[1]
acc5
acc6 = logist_cm6$overall[1]
acc6

#Precision
precision_xgb1 = xgb.cm1$byClass[5]
precision_xgb1
precision_xgb2 = xgb.cm2$byClass[5]
precision_xgb2
precision_xgb3 = xgb.cm3$byClass[5]
precision_xgb3
precision_xgb4 = xgb.cm4$byClass[5]
precision_xgb4
precision_xgb5 = xgb.cm5$byClass[5]
precision_xgb5
precision_xgb6 = xgb.cm6$byClass[5]
precision_xgb6
# Recall
recall_xgb1 = xgb.cm1$byClass[6]
recall_xgb1
recall_xgb2 = xgb.cm2$byClass[6]
recall_xgb2
recall_xgb3 = xgb.cm3$byClass[6]
recall_xgb3
recall_xgb4 = xgb.cm4$byClass[6]
recall_xgb4
recall_xgb5 = xgb.cm5$byClass[6]
recall_xgb5
recall_xgb6 = xgb.cm6$byClass[6]
recall_xgb6

#Sensitivity
s.xgb1 =  xgb.cm1$byClass[1]
s.xgb1
s.xgb2 =  xgb.cm2$byClass[1]
s.xgb2
s.xgb3 =  xgb.cm3$byClass[1]
s.xgb3
s.xgb4 =  xgb.cm4$byClass[1]
s.xgb4
s.xgb5 =  xgb.cm5$byClass[1]
s.xgb5
s.xgb6 =  xgb.cm6$byClass[1]
s.xgb6
#F1
f1.xgb1 = xgb.cm1$byClass[7]
f1.xgb1
f1.xgb2 = xgb.cm2$byClass[7]
f1.xgb2
f1.xgb3 = xgb.cm3$byClass[7]
f1.xgb3
f1.xgb4 = xgb.cm4$byClass[7]
f1.xgb4
f1.xgb5 = xgb.cm5$byClass[7]
f1.xgb5
f1.xgb6 = logist_cm6$byClass[7]
f1.xgb6
#############
#Ligth GBM

library(lightgbm)
library(ROCR)
library(caret)
train1 = train1
train2 = train2
train2 = train3
train_rose = rose_train
train_smote = train.smote


train.data = as.matrix(train[,-13])
train1.data = as.matrix(train1[,-13])
train2.data = as.matrix(train2[,-13])
train3.data = as.matrix(train3[,-13])
train_smote.data = as.matrix(train_smote[,-13])
train_rose.datas = as.matrix(train_rose[,-13])

train.label = as.matrix(train[,13])
train1.label = as.matrix(train1[,13])
train2.label = as.matrix(train2[,13])
train3.label = as.matrix(train3[,13])
train_smote.label = as.matrix(train_smote[,13])
train_rose.label = as.matrix(train_rose[,13])

test.data = data.matrix(test[,-13])
test.label = as.matrix(test[,13])

lgb_train = lgb.Dataset(data = train.data, label= train.label)
lgb_train1 = lgb.Dataset(data = train1.data,label = train1.label)
lgb_train2 = lgb.Dataset(data = train2.data,label = train2.label)
lgb_train3 = lgb.Dataset(data = train3.data,label = train3.label)
lgb_train_rose = lgb.Dataset(data = train_rose.data,label = train_rose.label)
lgb.train_smote = lgb.Dataset(data = train_smote.data,label = train_smote.label)
lgb_test = lgb.Dataset(data = test.data, label= test.label)

valid = list(test = lgb_test)
params = list(max_bin = 5,
              learning_rate = 0.001,
              objective = "binary",
              metric = 'auc') 

lgb.original = lightgbm(params = params, 
                        lgb_train, 
                        valid, 
                        nrounds = 1000)
lgb.original

lgb.50_50 = lightgbm(params = params, 
                     lgb_train1, 
                     valid, 
                     nrounds = 1000)
lgb.50_50

lgb.75_25 = lightgbm(params = params, 
                     lgb_train2, 
                     valid, 
                     nrounds = 1000)
lgb.75_25

lgb.25_75 = lightgbm(params = params, 
                     lgb_train3, 
                     valid, 
                     nrounds = 1000)
lgb.25_75

lgb.rose = lightgbm(params = params, 
                    lgb_train_rose, 
                    valid, 
                    nrounds = 1500)
lgb.rose

lgb.smote = lightgbm(params = params, 
                     lgb.train_smote, 
                     valid, 
                     nrounds = 1000)
lgb.smote
#Importance 
Importance.lgb1 <- lgb.importance(lgb.original)
Importance.lgb1

Importance.lgb2 <- lgb.importance(lgb.50_50)
Importance.lgb2

Importance.lgb3 <- lgb.importance(lgb.75_25)
Importance.lgb3

Importance.lgb4 <- lgb.importance(lgb.25_75)
Importance.lgb4

Importance.lgb5 <- lgb.importance(lgb.rose)
Importance.lgb5

Importance.lgb6 <- lgb.importance(lgb.smote)
Importance.lgb6
#Importance Plots
lgb.plot.importance(Importance.lgb1)
lgb.plot.importance(Importance.lgb2)
lgb.plot.importance(Importance.lgb3)
lgb.plot.importance(Importance.lgb4)
lgb.plot.importance(Importance.lgb5)
lgb.plot.importance(Importance.lgb6)

#prediction & confusion matrix
lgb.pred1 = predict(lgb.original, test.data)
predicted1 = ifelse(lgb.pred1 > 0.003,1,0)
lgb.cm1 = confusionMatrix(factor(predicted1), factor(test$exclud_count))
lgb.cm1

lgb.pred2 = predict(lgb.50_50, test.data)
predicted2 = ifelse(lgb.pred2 > 0.5,1,0)
lgb.cm2 = confusionMatrix(factor(predicted2), factor(test$exclud_count))
lgb.cm2$byClass

lgb.pred3 = predict(lgb.75_25, test.data)
predicted3 = ifelse(lgb.pred3 > 0.5,1,0)
lgb.cm3 = confusionMatrix(factor(predicted3), factor(test$exclud_count))
lgb.cm3$byClass
lgb.pred4 = predict(lgb.25_75, test.data)
predicted4 = ifelse(lgb.pred4 > 0.5,1,0)
lgb.cm4 = confusionMatrix(factor(predicted4), factor(test$exclud_count))
lgb.cm4$byClass
lgb.pred5 = predict(lgb.rose, test.data)
predicted5 = ifelse(lgb.pred5 > 0.86,1,0)
lgb.cm5 = confusionMatrix(factor(predicted5), factor(test$exclud_count))
lgb.cm5$byClass
lgb.pred6 = predict(lgb.smote, test.data)
predicted6 = ifelse(lgb.pred6 > 0.6,1,0)
lgb.cm6 = confusionMatrix(factor(predicted6), factor(test$exclud_count))
lgb.cm6$byClass
# Evaluation Curve
pred1 =prediction(lgb.pred1,test$exclud_count)
eval1 = performance(pred1,"acc")
plot(eval1)

roc1=performance(pred1,"tpr","fpr")
plot(roc1, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred2 =prediction(lgb.pred2,test$exclud_count)
eval2 = performance(pred2,"acc")
plot(eval2)

roc2=performance(pred2,"tpr","fpr")
plot(roc2, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred3 =prediction(lgb.pred3,test$exclud_count)
eval3 = performance(pred3,"acc")
plot(eval3)

roc3=performance(pred3,"tpr","fpr")
plot(roc3, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred4 =prediction(lgb.pred4,test$exclud_count)
eval4 = performance(pred4,"acc")
plot(eval4)

roc4=performance(pred4,"tpr","fpr")
plot(roc4, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred5 =prediction(lgb.pred5,test$exclud_count)
eval5 = performance(pred5,"acc")
plot(eval5)

roc5=performance(pred5,"tpr","fpr")
plot(roc5, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred6 =prediction(lgb.pred6,test$exclud_count)
eval6 = performance(pred6,"acc")
plot(eval6)

roc6=performance(pred6,"tpr","fpr")
plot(roc6, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

library(pROC)
#accuracy
lgb.acc1 = lgb.cm1$overall[1]
lgb.acc1
lgb.acc2 = lgb.cm2$overall[1]
lgb.acc2
lgb.acc3 = lgb.cm3$overall[1]
lgb.acc3
lgb.acc4 = lgb.cm4$overall[1]
lgb.acc4
lgb.acc5 = lgb.cm5$overall[1]
lgb.acc5
lgb.acc6 = lgb.cm6$overall[1]
lgb.acc6

#Precision
precision_lgb1 = lgb.cm1$byClass[5]
precision_lgb1
precision_lgb2 = lgb.cm2$byClass[5]
precision_lgb2
precision_lgb3 =lgb.cm3$byClass[5]
precision_lgb3
precision_lgb4 = lgb.cm4$byClass[5]
precision_lgb4
precision_lgb5 = lgb.cm5$byClass[5]
precision_lgb5
precision_lgb6 = lgb.cm6$byClass[5]
precision_lgb6
# Recall
recall_lgb1 = lgb.cm1$byClass[6]
recall_lgb1
recall_lgb2 = lgb.cm2$byClass[6]
recall_lgb2
recall_lgb3 = lgb.cm3$byClass[6]
recall_lgb3
recall_lgb4 = lgb.cm4$byClass[6]
recall_lgb4
recall_lgb5 = lgb.cm5$byClass[6]
recall_lgb5
recall_lgb6 =lgb.cm6$byClass[6]
recall_lgb6

#Sensitivity
s.lgb1 =  lgb.cm1$byClass[1]
s.lgb1
s.lgb2 =  lgb.cm2$byClass[1]
s.lgb2
s.lgb3 =  lgb.cm3$byClass[1]
s.lgb3
s.lgb4 =  lgb.cm4$byClass[1]
s.lgb4
s.lgb5 =  lgb.cm5$byClass[1]
s.lgb5
s.lgb6 =  lgb.cm6$byClass[1]
s.lgb6
#F1
f1.lgb1 = lgb.cm1$byClass[7]
f1.lgb1
f1.lgb2 = lgb.cm2$byClass[7]
f1.lgb2
f1.lgb3 = lgb.cm3$byClass[7]
f1.lgb3
f1.lgb4 = lgb.cm4$byClass[7]
f1.lgb4
f1.lgb5 =lgb.cm5$byClass[7]
f1.lgb5
f1.lgb6 = lgb.cm6$byClass[7]
f1.lgb6
#auc(roc)
auc_lgb1 = performance(pred1, "auc")@y.values
auc_lgb1
auc_lgb2 = performance(pred2, "auc")@y.values
auc_lgb2
auc_lgb3 = performance(pred3, "auc")@y.values
auc_lgb3
auc_lgb4 = performance(pred4, "auc")@y.values
auc_lgb4
auc_lgb5 = performance(pred5, "auc")@y.values
auc_lgb5
auc_lgb6 = performance(pred6, "auc")@y.values
auc_lgb6

###########################
#Random Forest

library(caTools)
library(caret)
library(ROCR)
library(randomForest)
library(doParallel)
#all_cores <- parallel::detectCores(logical = FALSE)
#registerDoParallel(cores = all_cores)

group_NPI<-group_NPI[complete.cases(group_NPI),]
group_NPI$exclud_count <- as.factor(group_NPI$exclud_count)
train <- train
test <- test
train1 = train1
train2 = train2
train3 = train3
train_rose = train_rose
train_smote = train.smote

#Original data
library(caret)

set.seed(1201)
train$exclud_count = factor(train$exclud_count)
rf_original =  randomForest(factor(exclud_count) ~ ., 
                            data = train, 
                            importance = TRUE)
rf_original

set.seed(121) 
rf_50_50 = randomForest(factor(exclud_count) ~ ., 
                        data = train1, 
                        importance = TRUE)
rf_50_50

set.seed(122)
rf_75_25 <- randomForest(factor(exclud_count) ~ ., 
                         data = train2, 
                         importance = TRUE)
rf_75_25

set.seed(123)

rf_25_75 <- randomForest(factor(exclud_count) ~ ., 
                         data = train3, 
                         importance = TRUE)
rf_25_75


set.seed(124)
rf_rose <- randomForest(factor(exclud_count) ~ ., 
                        data = train_rose, 
                        importance = TRUE)
rf_rose
#gc(reset = TRUE)

set.seed(125)
rf_smote <- randomForest(factor(exclud_count) ~ ., 
                         data = train_smote, 
                         importance = TRUE)
rf_smote

rf_smote <- randomForest(factor(exclud_count) ~ ., 
                         data = train_smote)
rf_smote

rf.pred1 <- predict(rf_original, test[,-13], type = "prob")[,2]
r.pred1 = ifelse(rf.pred1 > 0.001,0,1)
cm_rf1 = confusionMatrix(factor(test$exclud_count), factor(r.pred1))
cm_rf1$byClass
rf_pred1 <- prediction(rf.pred1, test$exclud_count)
rf_auc1 <- performance(rf_pred1, measure = "auc")@y.values[[1]] 
rf_auc1

rf.pred2 <- predict(rf_50_50, test[,-13], type = "prob")[,2]
r.pred2 = ifelse(rf.pred2 >  0.00017,0,1)
cm_rf2 = confusionMatrix(factor(test$exclud_count), factor(r.pred2))
cm_rf2$byClass
rf_pred2 <- prediction(rf.pred2, test$exclud_count)
rf_auc2 <- performance(rf_pred2, measure = "auc")@y.values[[1]] 
rf_auc2

rf.pred3 <- predict(rf_75_25, test[,-13], type = "prob")[,2]
r.pred3 = ifelse(rf.pred3 > 0.00029,0,1)
cm_rf3 = confusionMatrix(factor(test$exclud_count), factor(r.pred3))
cm_rf3$byClass
rf_pred3 <- prediction(rf.pred3, test$exclud_count)
rf_auc3 <- performance(rf_pred3, measure = "auc")@y.values[[1]] 
rf_auc3

rf.pred4 <- predict(rf_25_75, test[,-13], type = "prob")[,2]
r.pred4 = ifelse(rf.pred4 > 0.00053,0,1)
cm_rf4 = confusionMatrix(factor(test$exclud_count), factor(r.pred4))
cm_rf4$byClass
rf_pred4 <- prediction(rf.pred4, test$exclud_count)
rf_auc4 <- performance(rf_pred4, measure = "auc")@y.values[[1]] 
rf_auc4

rf.pred5 <- predict(rf_rose, test[,-13], type = "prob")[,2]
r.pred5 = ifelse(rf.pred5 > 0.035,1,0)
cm_rf5 = confusionMatrix(factor(test$exclud_count), factor(r.pred5))
cm_rf5$byClass 
rf_pred5 <- prediction(rf.pred5, test$exclud_count)
rf_auc5 <- performance(rf_pred5, measure = "auc")@y.values[[1]] 
rf_auc5

library(pROC)
my_curve <- roc(test$exclud_count,rf.pred1)
my_curve
coords(my_curve, "best", ret = "threshold")

rf.pred6 <- predict(rf_smote, test[,-13], type = "prob")[,2]
r.pred6 = ifelse(rf.pred6 > 0.0001,0,1)
cm_rf6 = confusionMatrix(factor(test$exclud_count), factor(r.pred6))
cm_rf6
rf_pred6 <- prediction(rf.pred6, test$exclud_count)
rf_auc6 <- performance(rf_pred6, measure = "auc")@y.values[[1]] 
rf_auc6


fit.smote <- data.frame(actual = test$exclud_count,
                        predict(rf_smote, test, type = "class"))
#rf.pred6 = max.col(fit.smote)-1
cm_rf6 <- confusionMatrix(factor(rf.pred6), factor(test$exclud_count))
cm_rf6

pred1 =prediction(rf.pred1,test$exclud_count)
eval1 = performance(pred1,"acc")
plot(eval1)

roc1=performance(pred1,"tpr","fpr")
plot(roc1, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred2 =prediction(fit.pred2,test$exclud_count)
eval2 = performance(pred2,"acc")
plot(eval2)

roc2=performance(pred2,"tpr","fpr")
plot(roc2, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred3 =prediction(rf.pred3,test$exclud_count)
eval3 = performance(pred3,"acc")
plot(eval3)

roc3=performance(pred3,"tpr","fpr")
plot(roc3, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred4 =prediction(rf.pred4,test$exclud_count)
eval4 = performance(pred4,"acc")
plot(eval4)

roc4=performance(pred4,"tpr","fpr")
plot(roc4, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred5 =prediction(rf.pred5,test$exclud_count)
eval5 = performance(pred5,"acc")
plot(eval5)

roc5=performance(pred5,"tpr","fpr")
plot(roc5, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)

pred6 =prediction(rf.pred6,test$exclud_count)
eval6 = performance(pred6,"acc")
plot(eval6)

roc6=performance(pred6,"tpr","fpr")
plot(roc6, col="red",main="ROC curve", print.auc = TRUE)
abline(a=0,b=1)
# Importance 
varImp(rf_original)
varImp(rf_50_50)
varImp(rf_under)
varImp(rf_both)
varImp(rf_rose)
varImp(rf_smote)
# Variable importance plot
varImpPlot(rf_original)
varImpPlot(rf_over)
varImpPlot(rf_under)
varImpPlot(rf_both)
varImpPlot(rf_rose)
varImpPlot(rf_smote)


library(pROC)
#accuracy
rf.acc1 = cm_rf1$overall[1]
rf.acc1
rf.acc2 = cm_rf2$overall[1]
rf.acc2
rf.acc3 = cm_rf3$overall[1]
rf.acc3
rf.acc4 = cm_rf4$overall[1]
rf.acc4
rf.acc5 = cm_rf5$overall[1]
rf.acc5
rf.acc6 = cm_rf6$overall[1]
rf.acc6

#Precision
precision_rf1 = cm_rf1$byClass[5]
precision_rf1
precision_rf2 = cm_rf2$byClass[5]
precision_rf2
precision_rf3 =cm_rf3$byClass[5]
precision_rf3
precision_rf4 = cm_rf4$byClass[5]
precision_rf4
precision_rf5 = cm_rf5$byClass[5]
precision_rf5
precision_rf6 = cm_rf6$byClass[5]
precision_rf6
# Recall
recall_rf1 = cm_rf1$byClass[6]
recall_rf1
recall_rf2 =cm_rf2$byClass[6]
recall_rf2
recall_rf3 = cm_rf3$byClass[6]
recall_rf3
recall_rf4 =cm_rf4$byClass[6]
recall_rf4
recall_rf5 = cm_rf5$byClass[6]
recall_rf5
recall_rf6 = cm_rf6$byClass[6]
recall_rf6

#Sensitivity
s.rf1 =  cm_rf1$byClass[1]
s.rf1
s.rf2 =  cm_rf2$byClass[1]
s.rf2
s.rf3 = cm_rf3$byClass[1]
s.rf3
s.rf4 = cm_rf4$byClass[1]
s.rf4
s.rf5 = cm_rf5$byClass[1]
s.rf5
s.rf6 = cm_rf6$byClass[1]
s.rf6
#F1
f1.rf1 = cm_rf1$byClass[7]
f1.rf1
f1.rf2 = cm_rf2$byClass[7]
f1.rf2
f1.rf3 = cm_rf3$byClass[7]
f1.rf3
f1.rf4 = cm_rf4$byClass[7]
f1.rf4
f1.rf5 = cm_rf5$byClass[7]
f1.rf5
f1.rf6 =cm_rf6$byClass[7]
f1.rf6

#auc(roc)
auc_rf1 = performance(pred1, "auc")@y.values
auc_rf1
auc_rf2 = performance(pred2, "auc")@y.values
auc_rf2
auc_rf3 = performance(pred3, "auc")@y.values
auc_rf3
auc_rf4 = performance(pred4, "auc")@y.values
auc_rf4
auc_rf5 = performance(rf_pred5, "auc")@y.values
auc_rf5
auc_rf6 = performance(pred6, "auc")@y.values
auc_rf6