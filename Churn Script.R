## Load libraries for model

packages <- installed.packages()  
lapply(packages[1:500], require, character.only = TRUE)

## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")


active_router_dataset_original <- DBI::dbGetQuery(myconn,
"
SELECT *
FROM PROD_MGMT_DB.ML.MODEL_1to5_ECM_FEATURE_MONTHLY_PVT_VW v
")
str(active_router_dataset_original)

## Create dataset for models

active_router_dataset  <- active_router_dataset_original[-c(1,2,3,4,5,7)] ## this is data reduction and cleansing
active_router_dataset$CHURNED_ACCOUNT_FLAG <- factor(active_router_dataset$CHURNED_ACCOUNT_FLAG)
str(active_router_dataset)
summary(active_router_dataset)
attach(active_router_dataset)


## Descriptive statistics

ggplot(active_router_dataset, aes(x = CHURNED_ACCOUNT_FLAG)) +
  geom_histogram(binwidth = 1, fill = c("Blue", "Dark Red")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")



## Create Train/Test Dataset

inTrain <- createDataPartition(y = active_router_dataset$CHURNED_ACCOUNT_FLAG, p = .75, list = FALSE)
Active_train <- active_router_dataset[inTrain,]
Active_test <- active_router_dataset[-inTrain,]


# Model 1 Logistic Regression

Churn_model <- glm(CHURNED_ACCOUNT_FLAG ~ ., family = "binomial", Active_train)
summary(Churn_model)

# Identify best predictors

confint(Churn_model, level = .95)

# Model Performance

churn_prediction <- predict(Churn_model, Active_test, type ='response')

#Generate ROC curve


model_AUC <- colAUC(churn_prediction, Active_test$CHURNED_ACCOUNT_FLAG, plotROC = T)
abline(h=model_AUC,col="Green")
text(.2, .9, cex = .8, labels = paste("Optimal Cutoff:", round(model_AUC,4)))

# Convert probabilities to class

Churn_class <- ifelse(churn_prediction > 0.6557,1,0)

#Transform back to factors for Comparison

Churn_class <- factor(Churn_class)
Active_test$CHURNED_ACCOUNT_FLAG <-factor(Active_test$CHURNED_ACCOUNT_FLAG)

# Create a Confusion Matrix

confusionMatrix(Churn_class, Active_test$CHURNED_ACCOUNT_FLAG)

####### This is Model Two: Neural Networks

NNlog <- multinom(CHURNED_ACCOUNT_FLAG ~ ., data = Active_train)
summary(NNlog)


## Model Prediction


Churn_Prediction2 <-predict(NNlog, Active_test)
Prediction_Table <- table(Churn_Prediction2, Active_test$CHURNED_ACCOUNT_FLAG)
Prediction_Table

# Classification Rate

sum(diag((Prediction_Table))/ sum(Prediction_Table))

# Misclassification Rate

1 - sum(diag((Prediction_Table))/ sum(Prediction_Table))


# This next step is the decision tree model for churn


df <- active_router_dataset[-c(2)]
df_churn <- filter(df, CHURNED_ACCOUNT_FLAG < 1)
df_churn <- df_churn[-c(1,2)]

data_partition <- sample(2,nrow(df_churn), replace = TRUE, prob = c(0.75, 0.25))
data_train <- df_churn[data_partition == 1,]
data_test <- df_churn[data_partition == 2,]

tree_churn <- rpart(TOTAL_MINUTES  ~ ., data = data_train)
rpart.plot(tree_churn, type = 4, extra = 0, branch.lty = 3, box.palette = "RdYlGn")
summary(tree_churn)



tree_prediction <-predict(tree_churn, data_train, type = "class")


# This next step is the decision tree model for non-churn


df_nc <- active_router_dataset[-c(2)]
df_non_churn <- filter(df_nc, CHURNED_ACCOUNT_FLAG == 1)
df_non_churn <- df_non_churn[-c(1,2)]

data_partition_nc <- sample(2,nrow(df_non_churn), replace = TRUE, prob = c(0.75, 0.25))
data_train_nc <- df_non_churn[data_partition_nc == 1,]
data_test_nc <- df_non_churn[data_partition_nc == 2,]

tree_non_churn <- rpart(TOTAL_MINUTES  ~ ., data = data_train_nc)
rpart.plot(tree_non_churn, type = 4, extra = 0, branch.lty = 3, box.palette = "RdYlGn")
summary(tree_non_churn)

# data_partition
#
#
# str(active_router_dataset_dt)
#
# m.rpart <- rpart(quality ~ ., data = wine_train)

sapply(active_router_dataset, sd)
corMatrix <- round(cor(active_router_dataset), digits = 2)
corMatrix


# This splits the dataset into a 70/30 partition

set.seed(1234)
ind <- sample(2, nrow(active_router_dataset), replace = TRUE, prob=c(0.7, 0.3))
traindata <- active_router_dataset[ind==1,]
testdata <- active_router_dataset[ind==2,]


## this is a Logistic Regression model

mylogit <- glm(CHURNED_ACCOUNT_FLAG,  ~., data = traindata, family = "binomial")
summary(mylogit)

# This is an Influence Index Plot that displays the Cook and hat model

influenceIndexPlot(mylogit, vars=c("Cook", "hat"), id.n= 3)



confint(mylogit)
confint.default(mylogit)
exp(mylogit$coefficients)
exp(confint(mylogit))
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# This is a SVM Model


SVMModel <- svm(CHURNED_ACCOUNT_FLAG ~. , data = traindata, gamma = 0.1, cost = 1)
print(SVMModel)


str(traindata)
str(df2)
df2 <- traindata
df2$CHURNED_ACCOUNT_FLAG <- factor(df2$CHURNED_ACCOUNT_FLAG)


randomForestModel <- randomForest(CHURNED_ACCOUNT_FLAG ~., data = active_router_dataset)
print(randomForestModel)
importance(randomForestModel)

plot.new()
varImpPlot(randomForestModel, type = 1, pch = 19, col =1, cex = 1.0, main ="")
abline(v=45, col="blue")

plot.new()
varImpPlot(randomForestModel, type = 2, pch = 19, col =1, cex = 1.0, main ="")
abline(v=45, col="blue")



c50result <- C5.0(CHURNED_ACCOUNT_FLAG~., data = df2, rules = TRUE)
summary(c50result)
C5imp(c50result, metric = "usage")
C5imp(c50result, metric = "splits")



LogisticModel <- predict(mylogit, testdata, type = "response")
SVMResult <-predict(SVMModel, testdata, type = "response")


testdata$YHat1 <- predict(mylogit, testdata, type = "response")
testdata$Yhat2 <-predict(SVMModel, testdata, type = "response")

Predict1 <- function(t) ifelse(LogisticModel > t, 1,0)
Predict2 <- function(t) ifelse(SVMResult > t, 1,0)


confusionMatrix(Predict1(0.5), testdata$CHURNED_ACCOUNT_FLAG)
confusionMatrix(Predict2(0.5), testdata$CHURNED_ACCOUNT_FLAG)


pred1 <- prediction(testdata$YHat1, testdata$CHURNED_ACCOUNT_FLAG)
pred2 <- prediction(testdata$Yhat2, testdata$CHURNED_ACCOUNT_FLAG)

perf1 <-performance(pred1, "tpr", "fpr")
perf2 <-performance(pred2, "tpr", "fpr")
plot.new()
plot(perf1, col = "green", lwd = 2.5)
plot(perf2, add = TRUE, col = "blue", lwd = 2.5)
abline(0,1,col = "Red", lwd =2.5, lty = 2)

save(Churn_model, file = "ChurnModel.rda")
