## Load libraries for visualization

library(mice)
library(VIM)
library(Hmisc)
library(naniar)
library(Amelia)
library(gridExtra)

## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")

## Original dataset

active_router_dataset_original <- DBI::dbGetQuery(myconn,
                                                  "
select *
from PROD_MGMT_DB.ML.MM_LMC_MODEL_250TO499_ECM_FEATURE_MONTHLY_PVT_VW
order by SFDC_ACCOUNT_ID,YEAR_MONTH
")

## Data reduction 

active_router_dataset  <- active_router_dataset_original[,c(3,5,7,8,12,16,9:64)]
active_router_dataset  <- active_router_dataset[,-c(8:14)]
active_router_dataset  <- active_router_dataset[,-c(1,2,3,6,59)]
attach(active_router_dataset)
as.data.frame(colnames(active_router_dataset_original))
## Data transformation version one

active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == 0] <- "Zero"
active_router_dataset[active_router_dataset == 0] <- NA

## Data transformation version two

active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == 0] <- "Non-Churn"
active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == 1] <- "Churn"
active_router_dataset[active_router_dataset == 0] <- NA

## Graph of missing values/zero entities

gg_miss_var(active_router_dataset) + labs(title = "Number of Zero's Per Feature in Dataset", x = "NCM Features", y = "Frequency of Zero's") + 
theme(plot.title = element_text(hjust = 0.5))
# gg_miss_upset(active_router_dataset)
# aggr(active_router_dataset,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)


## This shows ggplots in ggarange 

a <- ggplot(active_router_dataset,
       aes(x = TOTAL_MINUTES,
           y = Routers_Commands_Unregister)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG) 

b <- ggplot(active_router_dataset,
            aes(x = TOTAL_MINUTES,
                y = Routers_Configuration_Menu)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG)

c <- ggplot(active_router_dataset,
            aes(x = TOTAL_MINUTES,
                y = Routers_Commands_Menu)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG)

d <- ggplot(active_router_dataset,
            aes(x = TOTAL_MINUTES,
                y = Routers_Device_Dashboard_Link)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG)

e <- ggplot(active_router_dataset,
            aes(x = TOTAL_MINUTES,
                y = Routers_Move)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG)

f <- ggplot(active_router_dataset,
            aes(x = TOTAL_MINUTES,
                y = Notifications)) +
  geom_miss_point(show.legend = FALSE) +
  facet_wrap(~CHURNED_ACCOUNT_FLAG)

## Combine them together and create similiar title

gridExtra::grid.arrange(a, b, c, d, e, f, top = "Churn vs Non-Churn Feature Usage Difference: 6 Most Important Factors")

## Percent of missing data and values

pct_miss(active_router_dataset)
n_miss(active_router_dataset)
n_complete(active_router_dataset)

## Data Impuration with mean used per column calculation

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
active_router_dataset <- replace(active_router_dataset, TRUE, lapply(active_router_dataset, NA2mean))

## Transform Data back to where it originally was

active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == "Zero"] <- 0
active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == 1] <- 0
active_router_dataset$CHURNED_ACCOUNT_FLAG[active_router_dataset$CHURNED_ACCOUNT_FLAG == 2] <- 1


summary(active_router_dataset)


