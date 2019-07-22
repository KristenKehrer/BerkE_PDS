# Introduction
#This project explores EDA using the UCI income_data Income data set.
# The goal will eventually be to to predict pay category, but first we need to clean and inspect the data.

###  Install packages/libraries
install.packages("tidyverse") # pro: comes with dplyr and ggplot2
# con: takes a long time to install if students are doing it on the fly
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("forcats")

###   call packages/libraries
library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(forcats)


##  Remember to set your directory where you saved the data and change the direction of the slashes
setwd("C:/Users/kkehrer/Desktop/Business Venures/Data Moves Her/Emeritus/Intro to Data Science")

# Get the Data
income_data <- read.csv("income.csv")
str(income_data)


### Let's quickly look at some summary stats
summary(income_data)



#  We have both continous and discrete variables.
# I'm going to put them in separate vectors so we can loop through them later

categorical_names <- c("government_work", "education", "marital_status",
                       "occupation", "relationship", "ethnicity",
                       "gender", "country", "income")

continuous_names <- c("age", "capital_gain", "capital_loss", "hours_per_week")


continuous_data <- income_data %>%
  select(-categorical_names)

categorical_data <- income_data %>%
  select(categorical_names)
#

##  First we'll look at univariate plots of continuous variables
hist(income_data$age)

# Still a histogram but easier to extend:
ggplot(income_data) +
  geom_histogram(aes(x = age))


## Although the summary helps, let's look at histograms of our continous variables
for (i in continuous_names) {
  x <- continuous_data[,i]
  hist(x,
       main = paste("Histogram of ", i)
  )
}
# similar but using ggplot
for(i in continuous_names) {
  g <- ggplot(continuous_data) +
    geom_histogram(aes_string(x = i)) +
    ggtitle(paste("Histogram of ", i))
  print(g)
}
# aes_string; it allows the value of i to be interpreted 
# as a variable name in loops


## In EDA we typically look at boxplots for another view of our data

for (i in continuous_names) {
  x <- continuous_data[,i]
  boxplot(x, range = 0,
          main = paste("Boxplot of ", i)
  )
}



###  without the range set, default is 1.5 IQR
for (i in continuous_names) {
  x <- continuous_data[,i]
  boxplot(x, 
          main = paste("Boxplot of ", i)
  )
}
# In ggplot
for(i in continuous_names) {
  g <- ggplot(continuous_data) +
    geom_boxplot(aes_string(y = i)) +
    ggtitle(paste("Boxplot of ", i))
  print(g)
}
#


###  Capital loss and capital gain have many zeros.
# Let's create new data frames with non zero amounts to get a better picture.

capital_gain_not_zero <- income_data %>%
  filter(capital_gain != 0)

capital_loss_not_zero <- income_data %>%
  filter(capital_loss != 0)
#

## Now lets take a look at these variables.
ggplot(capital_gain_not_zero) +
  geom_boxplot(aes(y = capital_gain)) +
  ggtitle("Non Zero Capital Gain")

ggplot(capital_loss_not_zero) +
  geom_boxplot(aes(y = capital_loss)) +
  ggtitle("Non Zero Capital Loss")
#

summary(capital_gain_not_zero)
summary(capital_loss_not_zero)




###  We could also consider recoding capital gain/loss as binary variables.  
###  This is considered "feature engineering" and we could see what features work best in our model.
###  Although with this recoding we'd lose information.
for (i in categorical_names) {
  x <- categorical_data[,i]
  plot(x,
       main = paste("Plot of ", i)
  )
}



####  Bivariate plots (These are scatterplots)
str(income_data)

ggplot(income_data) + 
  geom_point(aes(x = capital_gain, y = age)) + 
  ggtitle("Age by Capital Gains")
#

ggplot(income_data) + 
  geom_point(aes(x = capital_gain, y = hours_per_week)) + 
  ggtitle("Age by Capital Gains")
#

summary(income_data$income)

## recode income so we can create a table with it:
income_data <- income_data %>%
  mutate(income_recoded = ifelse(income == ">50K", 1, 0))
#

summary(income_data$income_recoded)

str(income_data)
glimpse(income_data)
#

## Table of income by country
country_income_table <- income_data %>% 
  group_by(country) %>%
  summarise(visitors = n(),
            high_salary = sum(income_recoded)/visitors)
#


### correlation we can use regular pearson correlation here
cor.income_data <- cor(continuous_data)

corrplot(cor.income_data, method = "number")
corrplot(cor.income_data, method = "circle")

## correlation between the different continous variables is quite low

###  Education could be recoded and we could use a rank correlation for ordinal variables
###  note that the other discrete variables are not ordinal,

# Data Cleaning
# We've noticed some of these variables have a large number of factors, we can reduce the number
# of factors by grouping like things.


###  Assess potential groupings for country
north_america <- c("Canada", "Cuba", "Dominican Republic", "El-Salvador",
                    "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico",
                    "Nicaragua", "Outlying US", "Puerto-Rico",
                    "Trinadad and Tobago", "United States")
income_data <- income_data %>%
  mutate(grouped_country = ifelse(country %in% north_america,
                                  "North America", "other"))
#

income_data$grouped_country  <- as.factor(income_data$grouped_country )

summary(income_data$grouped_country)                                             


###   recoding education
table(income_data$education)


post_high <- c("Assoc to acdm", "Assoc to voc", "Bachelors",
               "Doctorate", "HS to grad", "Masters",
               "Prof to school", "Some to college")
income_data <- income_data %>%
  mutate(grouped_education = ifelse(education %in% post_high,
                                    "finished high", "not finished high"))
#

###  making sure things are grouped correctly
table(income_data$grouped_education, income_data$education)


ggplot(income_data) + 
  geom_bar(aes(x = grouped_education)) + 
  ggtitle("Grouped Education")



## possible recoding for government work
table(income_data$government_work)


gov_cat <- c("Federal gov", "Local gov" , "State gov")
income_data <- income_data %>%
  mutate(grouped_gov_work = ifelse(government_work %in% gov_cat,
                                   "gov", "non gov"))

# check out the results to make sure it worked correctly
table(income_data$grouped_gov_work, income_data$government_work)

###  Look at marital status to see how we might want to recode this.
table(income_data$marital_status)

marriage_cat <- c("Divorced", "Never married", "Widowed")
income_data <- income_data %>%
  mutate(grouped_marital = ifelse(marital_status %in% marriage_cat,
                                  "not married", "married"))
#

table(income_data$grouped_marital, income_data$marital_status)



####  Handling missing values
### above we recoded missing values into an "other" category, 
# we also didn't know exactly what "south" meant.
table(income_data$government_work)
income_data[income_data == " ?"] <- NA


###  Tables do not show NA values but NA values cannot be used in modeling.
table(income_data$government_work)

##  Checking for NAs
sum(is.na(income_data$age))
sum(is.na(income_data$government_work))



# Counting NAs in every column
income_data %>% 
  summarise_all(list(name = ~sum(is.na(.))))
#


names(income_data)
## It's important when determining whether or not to 
###  remove NAs from the dataset, are you introducing any bias?
##  We have missing countries, we could either exclude them for modeling, or we could 
##  enter them as something like "unknown"

##  I bet missing occupation and missing "government_work" overlap heavily.  Let's check
income_data$occupation_grouped <- ifelse(is.na(income_data$occupation), "naOCC", "NOTnaOCC")

income_data$gov_work_grouped <- ifelse(is.na(income_data$government_work), "naGOV", "NOTnaGOV")

table(income_data$occupation_grouped, income_data$gov_work_grouped)

7/32561

###   The non matches are less than 0.02% of our data, so I won't look into it further

table(income_data$income, income_data$gov_work_grouped)


###  let's export our current dataset so that we have all of our newly created variables
###  When we start logistic regression

write.csv(income_data, file = "Income_7.csv", row.names = FALSE)
#
