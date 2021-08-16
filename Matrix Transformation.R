## Load libraries

library(glmnet)
library(Matrix)
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
library(plyr)
library(pROC)

## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")

## Get the original dataset

active_router_dataset_original <- DBI::dbGetQuery(myconn,
                                                  "
select *
from PROD_MGMT_DB.ML.MM_LMC_MODEL_6TO9_ECM_FEATURE_MONTHLY_PVT_VW
order by SFDC_ACCOUNT_ID,YEAR_MONTH
")

## This data reduction version two. Simple put, there are more variables

active_router_dataset  <- active_router_dataset_original[,c(3,5,7,8,12,16,9:64)]
active_router_dataset  <- active_router_dataset[,-c(8:14)]
active_router_dataset  <- active_router_dataset[,-c(1:6)]

## This is to transforming certain features to fit the classification model.

active_router_dataset$SWIMLANE_MAP <- as.factor(active_router_dataset$SWIMLANE_MAP)
active_router_dataset$TERM_MAP <- as.factor(active_router_dataset$TERM_MAP)
active_router_dataset$YEAR_MONTH <- anydate(active_router_dataset$YEAR_MONTH)
active_router_dataset$CHURNED_ACCOUNT_FLAG <- as.factor(active_router_dataset$CHURNED_ACCOUNT_FLAG)

## Percent of zero's

per <- lapply(active_router_dataset, function(x){ length(which(x==0))/length(x)})

## Count of zero's

nonzero <- function(x) sum(x != 0)

cou <- numcolwise(nonzero)(active_router_dataset)

## Transform into dataframe

per <- as.data.frame(per)
cou <- as.data.frame(cou)

## Combine the rows with each dataset

combined_data <- rbind(per, cou)

## This is how transpose the datafame to a vector

tranpose_df <- t(combined_data)

## Use the vector as a new dataframe

final_df <- data.frame(r1= row.names(tranpose_df), tranpose_df, row.names=NULL) 

## Name columns in final dataframe

colnames(final_df) <- c("Feature", "Zero_Percentage", "Frequency_Zeros")

## reorder the dataframe

final_df <- final_df[c(1,3,2)]


## Write out to an csv 


write.csv(final_df, file = "Sparsity.csv", row.names = TRUE)



############################################################## Sparse Matrix and GLMNET: Machine Learning with R #################################################


some_dataframe <- active_router_dataset

some_matrix <- data.matrix(some_dataframe[2:49])


Matrix(some_matrix, sparse = TRUE)



set.seed(2)


split <- sample(nrow(some_dataframe), floor(0.7*nrow(some_dataframe)))

split


train <-some_dataframe[split,]
test <- some_dataframe[-split,]

train_sparse <- sparse.model.matrix(~.,train[2:49])
test_sparse <- sparse.model.matrix(~.,test[2:49])


fit <- glmnet(train_sparse,train[,1])


cv <- cv.glmnet(train_sparse,train[,1],nfolds=20)
pred <- predict(fit, test_sparse,type="response", s=cv$lambda.min)

pred

auc = roc(test[,1], pred)
print(auc$auc)















