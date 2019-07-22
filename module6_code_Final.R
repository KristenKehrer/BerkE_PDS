install.packages(pwr)

library(pwr)  ###   sample size and power calculator
library(dplyr)


###  Set working directory, please update with your own file path:
##  Remember backslashes need to be changed to forward slashes
setwd("C:/Users/kkehrer/Desktop/Business Venures/Data Moves Her/Emeritus/Intro to Data Science")


# header = TRUE by default
web_test <- read.csv("hypothesis_test.csv")


str(web_test)

web_test$test_assignment <- as.factor(web_test$test_assignment)

class(web_test$test_assignment)


# Look at relevant metrics
web_test_table <- web_test %>% 
  group_by(test_assignment) %>%
  summarise(visitors = n(),
            distinct_visitors = n_distinct(customer_id),
            num_conversions = sum(Conv),
            percent_convert = num_conversions/visitors,
            average_sales_per_visitor = mean(sales),
            average_RPV = mean(sales - cost_of_goods),
            average_sale_per_conv = sum(sales)/num_conversions,
            average_quantity = sum(quantity)/num_conversions,
            dupes = visitors - n_distinct(customer_id)
            )
#

###   the n_distinct brings up an important issue of certain customers being counted twice 
###   Or potentially seeing both test and control experiences.  These will need to be removed.


web_test_no_dupes <- web_test %>%
  filter(!duplicated(customer_id))
#

class(web_test_no_dupes)


web_test_table2 <- web_test_no_dupes %>% 
  group_by(test_assignment) %>%
  summarise(visitors = n(),
            distinct_customers = n_distinct(customer_id),
            num_conversions = sum(Conv),
            percent_convert = num_conversions/visitors,
            average_sale_per_visitor = mean(sales),
            average_RPV = mean(sales - cost_of_goods),
            average_sale_per_conv = sum(sales)/num_conversions,
            average_quantity = sum(quantity)/num_conversions,
            dupes = visitors - distinct_customers)
#

###  Chi-squared test of proportions (you'll notice we only needed to give the numerator and denominator)

numerator <- c(4282, 5144)
denominator <- c(21971, 21840)
test<- prop.test(numerator, denominator)


###   calculating the difference in the 2 proportions
test$estimate[1] - test$estimate[2]


### calculating confidence intervals for the proportions
n1 <- 21971
n2 <- 21840

p1 <- 4282/n1
p2 <- 5144/n2

z_alpha_2 <- 1.96
SE_p1 <- sqrt((p1 * (1 - p1))/n1)
SE_p2 <- sqrt((p2 * (1 - p2))/n2)

upper_bound_p1 <- p1 + z_alpha_2 * SE_p1
lower_bound_p1 <- p1 - z_alpha_2 * SE_p1

upper_bound_p1
lower_bound_p1 


upper_bound_p2 <- p2 + z_alpha_2 * SE_p2
lower_bound_p2 <- p2 - z_alpha_2 * SE_p2

upper_bound_p2
lower_bound_p2 

###  Conclusion:  Using the cross out pricing increases conversion.  This increase in conversion will
##  lead to an increase in sales due to the product offering in this example.


## t.test() would be used to test the difference in means if we were interested in calculating the difference
## in sales amounts (had the test or hypothesis been different).  In this scenario, we'd give the t.test() function
## two vectors.  A t-test example will be covered in the homework.

?t.test

########################  Sample Size ##########################################

pwr.t.test(n = NULL,###   we leave this blank because we want "n" as an output
           d = 0.02,  ## effect size, difference between the means divided by the pooled standard deviation
           sig.level = 0.05, 
           power = .75,
           type = "paired")
#


######################### Power  ####################################################
pwr.t.test(n =  40000 ,
           d = 0.02,  ## effect size, difference between the means divided by the pooled standard deviation
           sig.level = 0.05,
           power = NULL, ###   leave this blank to calculate the power of our test
           type = "paired")
#