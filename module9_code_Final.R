##Build a logistic regression model
##Discuss the output


###  install packages
install.packages("caTools")
install.packages("MASS")
install.packages("pROC")

###   call packages
library(caTools)  ## for testing/training split

library(MASS) ### for stepwise regression
# note that MASS will mask dplyr::select() - they learned about masked
# functions in Mod 2 but haven't had to deal with any yet

library(pROC)  ## for generating ROC curve

##  Remember to set your directory where you saved the data and change the direction of the slashes
### Using the data as set up from the EDA module.

setwd("C:/Users/kkehrer/Desktop/Business Venures/Data Moves Her/Emeritus/Intro to Data Science")

# Get the Data
logistic_income_data <- read.csv("income_7.csv")


str(logistic_income_data)
glimpse(logistic_income_data)
#

## Take a look at your data
head(logistic_income_data)

summary(logistic_income_data)


## drop the variables EDA that are:
###   originial income variable (we'll keep income_recoded)
###   the grouped countries, we don't want two measures of the same thing
drops <- c("income", "country", "education", "government_work", "marital_status", "occupation_grouped", "gov_work_grouped")

income_reduced <- logistic_income_data %>%
  dplyr::select(-income, -country, -education, -government_work,
         -marital_status, -occupation_grouped, -gov_work_grouped)
#

###  we'll be using income_recoded because otherwise the default is alphabetical order

###   we have NA's in the data that we need to remove so that the lengths will be the same for prediction
###  This is because logistic regression removes NAs while modeling

income_reduced <- na.omit(income_reduced)
#

str(income_reduced)

###   set up testing and training sets
set.seed(20)  ###  making sure this is reproducible
sample <- sample.split(income_reduced$income_recoded, SplitRatio = .50)
train <- filter(income_reduced, sample == TRUE)
test <- filter(income_reduced, sample == FALSE)
#


names(train)

###  let's build a simpler model to discuss output

reg1 <- glm(income_recoded ~ 
            grouped_marital +
            grouped_gov_work +
            grouped_education +
            grouped_country +
            age +
            gender,
            family = binomial(), train)
summary(reg1)

### Null deviance shows how well the model performs with just an intercept (naive model)
### Residual deviance is the goodness of fit of the entire model

reg2 <- glm(income_recoded ~ 
            grouped_marital +
            grouped_gov_work +
            grouped_education +
            age +
            gender,
            family= binomial(), train)
summary(reg2)


### We can calculate the z-scores directly from the output
###  Calculate Z value for frequent business travel

###  calculating armed-forces z-score
zscore <- reg2$coefficients[3]/0.057455
zscore
### Calculate p-value for a Z-score
2*pnorm(zscore)

## confidence intervals for coefficients:
confint(reg2)

## Let's continue with reg2
## Look at output in terms of odds:

exp(reg2$coefficients)

##another way to do this
exp(coef(reg2))

## exponentiated confidence intervals
exp(confint(reg2))

###  probabilities
summary(reg2$fitted.values)


##Fitting a logistic regression model, here we're using a full model
reg <- glm(income_recoded ~ ., family=binomial(), train)
summary(reg)


## Using stepwise regression
##  There are backwards and forwards methods.  Here we're using both
step.model <- stepAIC(reg, direction = "both", 
                      trace = FALSE)
summary(step.model)



summary(step.model$fitted.values)


count(coefficients(step.model))


## drop ethnicity
step.model1 <- update(step.model, . ~ . - ethnicity)
summary(step.model1)

## only a very small change in the residual deviance



table(train$occupation, train$income_recoded)



###  Create a table that includes the probabilites and then inspect it.
income_train_pred <- train %>%
  mutate(fitted.values = step.model1$fitted.values)
#

###   everyone with a capital gain of 99999 has a probability of 1 of making greater than 50k (obviously)
### it's quite odd looking that so many inputs had the exact same capital gain number
### This would raise questions about accuracy.

###  generate ROC curve
roc_curve <- plot(roc(train$income_recoded, step.model1$fitted.values))

###  actual area under the curve:
roc(train$income_recoded, step.model1$fitted.values)


####  Let's see how we'll we can predict for people in the test set:

###  Make predictions 
p <- predict(step.model1, type = "response", data = test)

class(p)
dim(p)  ## We have a 736x2 matrix

p <- data.frame(p)

names(p)
summary(p)


###  classify the prediction
p$prediction <- ifelse(p > .5, 1, 0)


### Create a table of predictions vs. actuals:
## In this scenario it's called a confusion matrix.
table(test$income_recoded, p$prediction)

## we can recode to make it easier to read:
true_value <- ifelse(test$income_recoded == 1, "High Income", "Low Income")
predicted_value <- ifelse(p$prediction == 1, "Predicted High", "Predicted Low")


confusion <- table(true_value, predicted_value)

confusion[1]
confusion[2]
confusion[3]
confusion[4]

accuracy <- (confusion[1] + confusion[4])/(confusion[1] + confusion[2] + confusion[3] + confusion[4])

##  precision and recall are two measures that help you understand the quality of your binary classifier
precision <- confusion[1]/(confusion[1] + confusion[2])
recall <- confusion[1]/(confusion[1] + confusion[3])



###  classify the prediction (round 2)
p$prediction2 <- ifelse(p$p > .2, 1, 0)

##  recode to read easier:
predicted_value2 <- ifelse(p$prediction2 == 1, "Predicted High", "Predicted Low")



### Create a table of predictions vs. actuals:
confusion2 <- table(true_value, predicted_value2)

confusion2[1]
confusion2[2]
confusion2[3]
confusion2[4]

accuracy2 <- (confusion2[1] + confusion2[4])/(confusion2[1] + confusion2[2] + confusion2[3] + confusion2[4])

precision2 <- confusion2[1]/(confusion2[1] + confusion2[2])
recall2 <- confusion2[1]/(confusion2[1] + confusion2[3])
