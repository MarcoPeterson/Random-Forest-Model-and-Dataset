## Step One: Get the data

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
mydata <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")
mydata <- DBI::dbGetQuery(myconn,
                          "select  *
from PROD_MGMT_DB.ML.POLL_DATASET_REGRESSION")

original_ds <- DBI::dbGetQuery(myconn,
                               "select  *
from PROD_MGMT_DB.ML.POLL_DATASET_REGRESSION")


# Make sure the data looks good

head(mydata)
head(original_ds)

# Change the names and create two other data sets which have different features


cradle <- data.frame(mydata$ACCOUNT_AGE_IN_MONTHS, mydata$POLL_COUNT, mydata$NUM_ROUTERS, mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, mydata$NUM_EVENTS, mydata$NUM_HOURS, mydata$TOTAL_LOGINS)
names(cradle)  <- c("ACCOUNT_AGE_IN_MONTHS", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL", "NUM_EVENTS", "NUM_HOURS", "TOTAL_LOGINS")

cradle2 <- data.frame(mydata$ACCOUNT_AGE_IN_MONTHS, mydata$INDUSTRY <- as.numeric(as.factor(mydata$INDUSTRY)),  mydata$POLL_COUNT, mydata$NUM_ROUTERS, round(mydata$AVERAGE_POLL), mydata$NUM_EVENTS, round(mydata$NUM_HOURS/24), mydata$TOTAL_LOGINS)
names(cradle2)  <- c("ACCOUNT_AGE_IN_MONTHS", "INDUSTRY", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL", "NUM_EVENTS", "NUM_DAYS", "TOTAL_LOGINS")

mydata$INDUSTRY <- as.numeric(as.factor(mydata$INDUSTRY))
mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT <- round(mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)



as.numeric(as.factor(cradle2$INDUSTRY))
mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT <- round(mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)
cradle2[is.na(cradle2)] <- 0
mydata$INDUSTRY[is.na(mydata$INDUSTRY)] <-0

# See the results at hand
str(original_ds)
str(mydata)
str(cradle)
str(cradle2)

head(original_ds)
head(mydata)
head(cradle)
head(cradle2)

# See the correlations below

cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$TOTAL_LOGINS)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$ACCOUNT_AGE_IN_MONTHS)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$NUM_ROUTERS)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$POLL_COUNT)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$NUM_EVENTS)
cor(cradle$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, cradle$NUM_HOURS)

#  This is to create the regression models

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

head(cradle)

#  This is the regression

reg(y = cradle$AVERAGE_POLL, x = cradle2[1:3])
reg(y = cradle2$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, x = cradle2[1:4])

#  create correlation plots

cor(cradle2[c("ACCOUNT_AGE_IN_MONTHS", "INDUSTRY", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL", "NUM_EVENTS", "NUM_DAYS", "TOTAL_LOGINS")])
cor(mydata[c("ACCOUNT_AGE_IN_MONTHS", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL_RESPONSE_FOR_ACCOUNT", "NUM_EVENTS", "NUM_HOURS", "TOTAL_LOGINS")])
pairs(mydata[c("ACCOUNT_AGE_IN_MONTHS", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL_RESPONSE_FOR_ACCOUNT", "TOTAL_LOGINS")])
pairs.panels(mydata[c("ACCOUNT_AGE_IN_MONTHS", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL_RESPONSE_FOR_ACCOUNT", "TOTAL_LOGINS")])
pairs.panels(cradle2[c("ACCOUNT_AGE_IN_MONTHS", "INDUSTRY", "POLL_COUNT", "NUM_ROUTERS", "AVERAGE_POLL","TOTAL_LOGINS")])


# M <-cor(cradle2)
# corrplot(M, type="upper", order="hclust",
#          col=brewer.pal(n=8, name="RdYlBu"))
# corrplot(M, method="number")
# ggpairs(cradle2, title="correlogram with ggpairs()")
# corr <- abs(cor(cradle2))
# colors <- dmat.color(corr)
# order <- order.single(corr)
# cpairs(cradle2,                    # Data frame of variables
#        order,                   # Order of the variables
#        panel.colors = colors,   # Matrix of panel colors
#        border.color = "grey70", # Borders color
#        gap = 0.45,              # Distance between subplots
#        main = "Ordered variables colored by correlation", # Main title
#        show.points = TRUE,      # If FALSE, removes all the points
#        pch = 21,                # pch symbol
#        bg = rainbow(3)[iris$Species])



# histogram


hist(cradle2$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)
hist(cradle2$POLL_COUNT)
hist(cradle2$ACCOUNT_AGE_IN_MONTHS)
hist(cradle2$NUM_ROUTERS)
hist(cradle2$TOTAL_LOGINS)
hist(cradle2$INDUSTRY)

hist(cradle2$AVERAGE_POLL, 
     main=" Histogram of Average Poll Response", 
     xlab="Score of Poll Distribution", 
     border="black", 
     col="lightblue")

#  this is a 5 point summary

ggsummarystats(
  mydata, x = "AVERAGE_POLL", y = "POLL_COUNT", 
  ggfunc = ggboxplot, add = c("jitter"), 
  color = "blue", palette = "jco",
  facet.by = c("SFDC_ACCOUNT_NAME"), labeller = "label_both", 
  free.panels = TRUE,
  ggtheme = theme_bw(), legend = "top"
)

#  training dataset and test dataset

cradle_train <- cradle2[1:30, ]
cradle_test <- cradle2[31:106, ]

mydata_train <- mydata[1:30, ]
mydata_test <- mydata[31:106, ]

head(mydata)


#  models for decision tree graph


ins_model4 <- lm(AVERAGE_POLL ~ ., data = cradle2)
ins_model4

ins_model5 <- lm(AVERAGE_POLL ~., data = mydata)
ins_model5

summary(ins_model4)
summary(ins_model5)

#  this is how to show the models

m.rpart <- rpart(AVERAGE_POLL_RESPONSE_FOR_ACCOUNT ~ ., data = mydata)
rpart.plot(m.rpart, digits = 1, fallen.leaves = TRUE, type = 5, extra = 101)
rpart.plot(m.rpart, digits = 1)


data(ptitanic)
tree <- rpart(AVERAGE_POLL ~., data = mydata)
rpart.plot(tree, type = 5, extra = 0, branch.lty = 1, box.palette = "RdYlGn")


par(mfrow = c(4,3))
for(iframe in 1:nrow(tree1$frame)) {
  cols <- ifelse(1:nrow(tree1$frame) <= iframe, "black", "gray")
  prp(tree1, col = cols, branch.col = cols, split.col = cols)
  
}

p.rpart <- predict(m.rpart, mydata_test)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)

# compare the correlation
cor(p.rpart, mydata_test$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, mydata_test$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)

# mean absolute error between actual values and mean value
mean(mydata_test$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT) # result = 5.87
MAE(8.236842, mydata_test$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)

mydata

m.cubist <- cubist(x = mydata_train[5], y = mydata_train$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)


# display basic information about the model tree
m.cubist

# display the tree itself
summary(m.cubist)

# generate predictions for the model
p.cubist <- predict(m.cubist, mydata_test)

# summary statistics about the predictions
summary(p.cubist)

# correlation between the predicted and true values
cor(p.cubist, mydata_test$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(mydata$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT, p.cubist)




