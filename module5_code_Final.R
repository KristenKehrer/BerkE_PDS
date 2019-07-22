# ---- Simple Random Sample ----
library(dplyr)
library(caTools)
library(ggplot2)
library(knitr)


cardata <- read.csv("car data.csv")
#

head(cardata, 20)

# look at the dimensions
dim(cardata)


# structure
str(cardata)
glimpse(cardata)
#

names(cardata)

########## simple random sample:
set.seed(123) ## for reproducibility

sample <- sample.split(cardata$Selling_Price, SplitRatio = .05)  ## Taking a sample using a single variables
# Generate a vector of TRUE and FALSE, with 95% being FALSE
sample <- sample.split(cardata$Selling_Price, SplitRatio = .05)
sample
# Filter the cardata to only include the rows that correspond to positions where sample is TRUE
full_sample <- filter(cardata, sample == TRUE)
#

############ stratified sample:
set.seed(1)
sample_cars <- cardata %>%
  group_by(Transmission) %>%
  sample_n(10)

# working with the normal distribution
pnorm(1.96, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(10, mean = 0, sd = 1)

###### Working with the Student's t-distribution
pt(1.96, 10000000000, lower.tail = TRUE, log.p = FALSE)
qt(.025, 10000000000, lower.tail = TRUE, log.p = FALSE)

## smaller degrees of freedom (hence smaller sample size)
rt(10, 12)

## larger degrees of freedom (hence larger sample size)
rt(10, 1200)

####   Using our car dataset again
ggplot(cardata) +
  geom_histogram(aes(x = Selling_Price))
#

###  Random sample by ratio
set.seed(1234)
sample <- sample.split(cardata$Selling_Price, SplitRatio = .05)
full_sample <- filter(cardata, sample == TRUE)
#


### sample size problem
p <- 0.2
standard_deviation <- sqrt(p*(1-p))
Z_alpha_2 <- qnorm(.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

n <- (Z_alpha_2^2)*(standard_deviation/.005)^2
n