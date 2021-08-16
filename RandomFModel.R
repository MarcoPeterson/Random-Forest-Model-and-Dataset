##################################################### Getting the Data and Data Pre-processing for various Models ###################################################

## Load libraries for model

library(dplyr)
library(dplyr.snowflakedb)
library(caret)
library(corrplot)
library(e1071)
library(ggcorrplot)
library(tibble)
library(randomForest)
library(tidyverse)
library(anytime)
library(party)
library(ggplot2)
library(odbc)
library(car)
library(olsrr)

## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "Snowflake", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")

## Original dataset

active_router_dataset_original <- DBI::dbGetQuery(myconn,
                                                  "
select *
from PROD_MGMT_DB.ML.CHURN_ML_DATASET_2021_06_JUN
where RANGE  = '1000+'
")

## Look at the columns in the original dataset

as.data.frame(colnames(active_router_dataset_original))

## This data reduction version one. This contains more features

active_router_dataset  <- select(active_router_dataset_original, 
c(
CHURNED_ACCOUNT_FLAG,
AGE_IN_MONTHS,                               
LOGIN_COUNT,                                  
TOTAL_MINUTES, 
Notifications,
Routers_NetCloud_OS_Menu,
Routers_Device_Dashboard_Link,
Routers_Configuration_Menu,
Routers_Commands_Menu, 
Routers_Configuration_Edit,
SUBSCRIBED_AVG_ROUTER_COUNT
# TERM_MAP,
# SWIMLANE_MAP
))

## This data reduction version two. Simple put, there are more variables

active_router_dataset  <- active_router_dataset_original[,c(3,5,7,8,12,16,9:64)]
active_router_dataset  <- active_router_dataset[,-c(8:14)]

## This is to transforming certain features to fit the classification model.

active_router_dataset$SWIMLANE_MAP <- as.factor(active_router_dataset$SWIMLANE_MAP)
active_router_dataset$TERM_MAP <- as.factor(active_router_dataset$TERM_MAP)
active_router_dataset$YEAR_MONTH <- anydate(active_router_dataset$YEAR_MONTH)
active_router_dataset$CHURNED_ACCOUNT_FLAG <- as.factor(active_router_dataset$CHURNED_ACCOUNT_FLAG)

## This is to make sure the columns look correctly after the data transformation

as.data.frame(colnames(active_router_dataset))

## Descriptive Statistics on Churn Data

churn_plot <- active_router_dataset %>% 
        group_by(CHURNED_ACCOUNT_FLAG) %>% 
        summarise(Count = n())%>% 
        mutate(percentage = prop.table(Count)*100)
ggplot(churn_plot, aes(reorder(CHURNED_ACCOUNT_FLAG, -percentage), percentage), fill = CHURNED_ACCOUNT_FLAG)+
        geom_col(fill = c("339", "red"))+
        geom_text(aes(label = sprintf("%.2f%%", percentage)))+
        xlab("Non Churn (Left) vs Churn (Right)") + 
        ylab("Percent")+
        ggtitle("Churn vs Non Churn Percentage: 1000 + Account Sizes") + theme(plot.title = element_text(hjust = 0.5))

##################################################### The next step involves a look at churn and non churn datasets ###################################################
glimpse(active_router_dataset)

## Create dataset that is only churn

data_frame_churn <- active_router_dataset_original %>% filter(active_router_dataset$CHURNED_ACCOUNT_FLAG == 1)

## Create dataset that is only non_churn

data_frame_non_churn <- active_router_dataset_original %>% filter(active_router_dataset$CHURNED_ACCOUNT_FLAG == 0)

## In this step create a correlation plot for churn

corr_churn <- select(data_frame_churn, c(
        AGE_IN_MONTHS,                               
        SUBSCRIBED_ROUTER_COUNT,                 
        LOGIN_COUNT,                                  
        TOTAL_MINUTES, 
        Account_Subscriptions, 
        # Routers_NetCloud_OS_Menu,
        Routers_Device_Dashboard_Link, 
        Routers_Configuration_Menu, 
        Routers_Commands_Unregister, 
        Routers_Commands_Menu,    
        Notifications
))

nv <- sapply(corr_churn, is.numeric)
cormat <- cor(corr_churn[,nv])
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE,
           ggtheme = ggplot2:: theme_dark (),
           title = "Correlation Plot of Numeric Variables on Churned Accounts")

## In this step create a correlation plot for non-churn

corr_non_churn <- select(data_frame_non_churn, c(
        AGE_IN_MONTHS,                               
        SUBSCRIBED_ROUTER_COUNT,                 
        LOGIN_COUNT,                                  
        TOTAL_MINUTES, 
        Account_Subscriptions, 
        # Routers_NetCloud_OS_Menu,
        Routers_Device_Dashboard_Link, 
        Routers_Configuration_Menu, 
        Routers_Commands_Unregister, 
        Routers_Commands_Menu,    
        Notifications
))

nv_2 <- sapply(corr_non_churn  , is.numeric)
cormat_2 <- cor(corr_non_churn [,nv_2])
ggcorrplot(cormat_2, hc.order = TRUE, type = "lower",
           lab = TRUE,
           ggtheme = ggplot2:: theme_dark (),
           title = "Correlation Plot of Numeric Variables on Non Churned Accounts")


########################################################## The next step involves creating samples for date specificity######################################################


## This is a way to partition the dataset based on YEAR_MONTH. It is a form of stratified sampling
set.seed(123)
dates <- unique(active_router_dataset$YEAR_MONTH) #already in order
slices <- createTimeSlices(dates,
                           initialWindow = 10,
                           horizon = 5,
                           skip = 4,
                           fixedWindow = TRUE)

slices <- lapply(slices, function(x){
        lapply(x, function(k){
                active_router_dataset %>%
                        mutate(n = 1:n()) %>%
                        filter(YEAR_MONTH %in% dates[k]) %>%
                        pull(n)
        })
})
train <- active_router_dataset[slices$train[[1]],]
test <- active_router_dataset[slices$test[[1]],]
tr <- trainControl(returnData = TRUE, 
                   classProbs = TRUE,
                   index = slices$train,
                   indexOut = slices$test)


## Make sure the data looks okay with glimpse


glimpse(test)
glimpse(train)


# If need be, this is another method for sampling the dataset, if you are not concerned about the sequence of time

set.seed(123)
ind <- sample(2, nrow(active_router_dataset), replace = TRUE, prob = c(0.7, 0.3))
train <- active_router_dataset[ind==1,]
test <- active_router_dataset[ind==2,]
str(train)


################################################################ Model 1 Random Forest: Classification Model################################################################


## Seed for non-randomization

set.seed(222)

## Create a basic, general model before you tune it

rf_model <- randomForest(CHURNED_ACCOUNT_FLAG~. - SFDC_ACCOUNT_ID - YEAR_MONTH, data=train)

## See what it looks like, and look at the attributes

print(rf_model)
attributes(rf_model)

# Prediction & Confusion Matrix - train data

p1 <- predict(rf_model, train)
confusionMatrix(p1, train$CHURNED_ACCOUNT_FLAG)


# # Prediction & Confusion Matrix - test data

p2 <- predict(rf_model, test)
confusionMatrix(p2, test$CHURNED_ACCOUNT_FLAG)

# Error rate of Random Forest

plot(rf_model) 

# Tune the model according to the errors you just found. Also the [,-7] represents the churned column

t <- tuneRF(train[,-7], train[,7],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

# This is the more tuned and specific version 

rf_model <- randomForest(CHURNED_ACCOUNT_FLAG~. - SFDC_ACCOUNT_ID - YEAR_MONTH, data=train,
                         ntree = 500,
                         mtry = 14,
                         importance = TRUE,
                         proximity = TRUE)

# Prediction & Confusion Matrix - train data again

p1 <- predict(rf_model, train)
confusionMatrix(p1, train$CHURNED_ACCOUNT_FLAG)


# # Prediction & Confusion Matrix - test data again

p2 <- predict(rf_model, test)
confusionMatrix(p2, test$CHURNED_ACCOUNT_FLAG)

# No. of nodes for the trees

hist(treesize(rf_model),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance

varImpPlot(rf_model,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


importance(rf_model)
varUsed(rf_model)

# Partial Dependence Plot

partialPlot(rf_model, train,Routers_Commands_Unregister, "1")
partialPlot(rf_model, train, Routers_Configuration_Menu, "1")
partialPlot(rf_model, train,Routers_Commands_Menu , "1")
partialPlot(rf_model, train,Help_Panel_Button, "1")
partialPlot(rf_model, train,LOGIN_COUNT, "1")
partialPlot(rf_model, train,Routers_Move, "1")
partialPlot(rf_model, train,TOTAL_MINUTES, "1")
partialPlot(rf_model, train,Routers_Device_Dashboard_Link, "1")
partialPlot(rf_model, train,Notifications , "1")
partialPlot(rf_model, train,Account_Subscriptions, "1")


# Extract Single Tree
getTree(rf_model, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix. This is for knowledge discovery and visual representation

table(train$SWIMLANE_MAP)

MDSplot(rf_model, train$SWIMLANE_MAP)
MDSplot(rf_model, test$SWIMLANE_MAP)


plot(randomForest(CHURNED_ACCOUNT_FLAG~.- SFDC_ACCOUNT_ID - YEAR_MONTH, data=train))


############################################################# Model 2 Random Forest: Regression Model ###############################################################

active_router_dataset_original  <- active_router_dataset_original[,-c(1)]

## Given that we know the results from the last model, we can re-run everything for a linear regression


active_router_dataset  <- select(active_router_dataset_original, c( CHURNED_ACCOUNT_FLAG,
                                                                    YEAR_MONTH,                                   
                                                                    SWIMLANE_MAP,                                
                                                                    AGE_IN_MONTHS,                               
                                                                    SUBSCRIBED_ROUTER_COUNT,                 
                                                                    TERM_MAP,                                    
                                                                    LOGIN_COUNT,                                  
                                                                    TOTAL_MINUTES, 
                                                                    Account_Subscriptions, 
                                                                    Routers_NetCloud_OS_Menu,
                                                                    Routers_Device_Dashboard_Link, 
                                                                    Routers_Configuration_Menu, 
                                                                    Routers_Commands_Unregister, 
                                                                    Routers_Commands_Menu,    
                                                                    Notifications,
                                                                    SFDC_ACCOUNT_ID
))

## This data reduction version two. Simple put, there are more variables

active_router_dataset_original  <- active_router_dataset_original[,-c(1)]
active_router_dataset  <- active_router_dataset_original[,c(3,5,7,8,12,16,9:64)]
active_router_dataset  <- active_router_dataset[,-c(8:14)]

## This is to transforming certain features to fit the regression model

active_router_dataset$SWIMLANE_MAP <- as.factor(active_router_dataset$SWIMLANE_MAP)
active_router_dataset$TERM_MAP <- as.factor(active_router_dataset$TERM_MAP)
active_router_dataset$YEAR_MONTH <- anydate(active_router_dataset$YEAR_MONTH)
active_router_dataset$CHURNED_ACCOUNT_FLAG <- as.numeric(active_router_dataset$CHURNED_ACCOUNT_FLAG)


## Look at the underlying data with glimpse

glimpse(active_router_dataset)


## This is to make sure the columns look correctly after the data transformation

as.data.frame(colnames(active_router_dataset))

## This is a way to partition the dataset based on YEAR_MONTH. It is a form of stratified sampling
set.seed(123)
dates <- unique(active_router_dataset$YEAR_MONTH) #already in order
slices <- createTimeSlices(dates,
                           initialWindow = 10,
                           horizon = 5,
                           skip = 4,
                           fixedWindow = TRUE)

slices <- lapply(slices, function(x){
        lapply(x, function(k){
                active_router_dataset %>%
                        mutate(n = 1:n()) %>%
                        filter(YEAR_MONTH %in% dates[k]) %>%
                        pull(n)
        })
})
train <- active_router_dataset[slices$train[[1]],]
test <- active_router_dataset[slices$test[[1]],]
tr <- trainControl(returnData = TRUE, 
                   classProbs = TRUE,
                   index = slices$train,
                   indexOut = slices$test)

## Look at the underlying data with glimpse

glimpse(train)

## Set the seed to non-randomization, and create a regression model with what was learned with the classification model

set.seed(222)
rf_model_probability <- randomForest(formula = CHURNED_ACCOUNT_FLAG~. - SFDC_ACCOUNT_ID - YEAR_MONTH, data=train,
                                     ntree = 200,
                                     mtry = 14,
                                     importance = TRUE,
                                     proximity = TRUE,
                                     type = regression)


################################################################## Evaluate Model Performance Using Accuracy ###############################################################

## Look at the table of your predictions

predrf <- predict(rf_model_probability , data = "train", type = "response")
table(predrf, train$CHURNED_ACCOUNT_FLAG)
acc_rf_train <- (2827+1458)/nrow(train)
acc_rf_train


## If the data and models look fine, append the new data


active_router_dataset$ChurnPrediction <- predict(rf_model, newdata = active_router_dataset, type = "class")
active_router_dataset$Churn_Probability <- predict(rf_model_probability , newdata = active_router_dataset, type = "class")

## Export it out as the following

write.csv(active_router_dataset, file = "1000+JUN.csv", row.names = TRUE)


## Look at results from a regression model standpoint, just to get a different perspective

test_model <- lm(formula = CHURNED_ACCOUNT_FLAG ~ .,  data = active_router_dataset)

## The lower the mean the better

mean(test_model$residuals)

## Look at the histogram of the residuals 

ols_plot_resid_hist(test_model)

## Look at the outliers in the data

ols_plot_resid_stud_fit(test_model)

## Look at the durbinWatsonTest

durbinWatsonTest(test_model)

## Look at the cooks chart and find the outliers manually

ols_plot_cooksd_chart(test_model)

## Look for multi-collinearity

ols_vif_tol(test_model)

## scan original data for outliers

active_router_dataset[1, ]


########################################################################## Saving Model to a file ###########################################################################

# Classification

save(rf_model, file = "rf_model_250_450.rda")

# Regression 

save(rf_model_probability, file = "rf_model_probability_250_450.rda")

















