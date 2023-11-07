## Sales volumes predictive analysis 

### 1. Load and explore the data

# Install and import Tidyverse and Dplyr.
library(tidyverse)
library(dplyr)

# Import the data set.
setwd(dir='/Users/FrancescaGalli/Documents/Course 3 Python:R LSE/LSE_DA301_assignment_files')
sales <- read.csv(file.choose(), header=T)

# Print the data frame.
print(sales)
glimpse(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales <- subset(sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
print(sales)

# Check if there are any null values
na_counts <- colSums(is.na(sales))
na_counts

# View the descriptive statistics.
summary(sales)

# Import the necessary libraries for data manipulation.
install.packages("DataExplorer")
library(DataExplorer)
install.packages("stringr")
library(dplyr)

# Create a summary report.
create_report(sales)

################################################################################

### 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_pro <- sales %>% group_by(Product) %>% 
  summarise(Global_Sales=sum(Global_Sales),
            NA_Sales=sum(NA_Sales),
            EU_Sales=sum(EU_Sales))

# Alternative view: Group data based on Platform and determine the sum per Platform.
sales_pla <- sales %>% group_by(Platform) %>% 
  summarise(Global_Sales=sum(Global_Sales),
            NA_Sales=sum(NA_Sales),
            EU_Sales=sum(EU_Sales))

head(sales_pla)
dim(sales_pla)

# View the Product data frame.
head(sales_pro)
dim(sales_pro)

# Explore the data frame.
summary(sales_pro)

## 2b) Determine which plot is the best to compare game sales.

# Filter out outliers (individual product sales above £17M) to better understand relationship:
sales_pro1 <- filter(sales_pro, NA_Sales<'17')
summary(sales_pro1)

# Create scatterplots.
ggplot(sales_pro1, mapping=aes(x=EU_Sales, y=NA_Sales)) + geom_point() +
  labs(title="NA and EU sales by individual product ID",
       x="EU Sales (£ million)", y="NA Sales (£ million)") + theme_linedraw() + geom_smooth(method = lm)

ggplot(sales_pro1, mapping=aes(x=EU_Sales, y=Global_Sales)) + geom_point() +
  labs(title="Global and EU sales by individual product ID",
       x="EU Sales (£ million)", y="Global Sales (£ million)") + theme_linedraw() + geom_smooth(method = lm)

ggplot(sales_pro1, mapping=aes(x=NA_Sales, y=Global_Sales)) + geom_point() +
  labs(title="Global and NA sales by individual product ID",
       x="NA Sales (£ million)", y="Global Sales (£ million)") + theme_linedraw() + geom_smooth(method = lm)

# Create histograms.
ggplot(sales_pro, mapping=aes(x=EU_Sales)) + geom_histogram(binwidth = 3) +
  labs(title="Products sold in EU by total sales amount",
       x="EU Sales (£ million)", y="Products sold") + theme_linedraw()

ggplot(sales_pro, mapping=aes(x=NA_Sales)) + geom_histogram(binwidth = 3) +
  labs(title="Products sold in NA by total sales amount",
       x="NA Sales (£ million)", y="Products sold") + theme_linedraw()

ggplot(sales_pro, mapping=aes(x=Global_Sales)) + geom_histogram(binwidth = 2) +
  labs(title="Products sold globally by total sales amount",
       x="Global Sales (£ million)", y="Products sold") + theme_linedraw()

## We can see that the majority of individual product global sales are below the £10M mark.

# Create boxplots.
ggplot(sales_pro, mapping=aes(Global_Sales)) + geom_boxplot()
## Boxplots are not appropriate for comparing quantitative vs quantitative data.
## They could be used instead to look at sales by platform:
qplot(NA_Sales, Platform, data=sales, geom='boxplot',
      main='North America sales across platforms', 
      xlab='NA sales (£ million)')
qplot(EU_Sales, Platform, data=sales, geom='boxplot',
      main='European Union sales across platforms', 
      xlab='EU sales (£ million)')


###############################################################################

### 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plot for Global Sales:
qqnorm(sales_pro$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')
qqline(sales_pro$Global_Sales,
       col='red',
       lwd=2)

# Create Q-Q Plot for NA Sales:
qqnorm(sales_pro$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')
qqline(sales_pro$NA_Sales,
       col='red',
       lwd=2)

# Create Q-Q Plot for EU Sales:
qqnorm(sales_pro$EU_Sales,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')
qqline(sales_pro$EU_Sales,
       col='red',
       lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk tests.
shapiro.test(sales_pro$Global_Sales)
shapiro.test(sales_pro$NA_Sales)
shapiro.test(sales_pro$EU_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_pro$Global_Sales)
kurtosis(sales_pro$Global_Sales)

skewness(sales_pro$NA_Sales)
kurtosis(sales_pro$NA_Sales)

skewness(sales_pro$EU_Sales)
kurtosis(sales_pro$EU_Sales)

# All three Sales columns are skewed towards higher values (skewness>2.8)
# and present "fat" tails or big outliers (kurtosis>15.6). This supports 
# the decision to exclude outliers when creating the sales_pro1 data frame to
# compare the Sales plots against each other.

## 3d) Determine correlation

# Insall packages required to plot a pairplot
install.packages("ggplot2")
install.packages("GGally")
library(GGally)

# Use a pairplot to investigate correlations between NA, EU and Global sales
ggpairs(sales_pro)


###############################################################################

### 4. Create a simple linear regression model

## 4a) Create two linear regression models

# Create two linear regression models on the original data.
# Global sales and NA sales
model1 <- lm(Global_Sales~NA_Sales, data=sales_pro)
# Global sales and EU sales
model2 <- lm(Global_Sales~EU_Sales, data=sales_pro)

# View the models
summary(model1)
summary(model2)

# View residuals on plot
plot(model1$residuals)
plot(model2$residuals)
# The residuals show a pattern which makes interpretation of coefficients unreliable.

## 4b) Create a plot (simple linear regression)

# Basic visualisation of model 1.
plot(sales_pro$Global_Sales, sales_pro$NA_Sales)
coefficients(model1)
# Add line-of-best-fit.
abline(coefficients(model1))

# Basic visualisation of model 2.
plot(sales_pro$Global_Sales, sales_pro$EU_Sales)
coefficients(model2)
# Add line-of-best-fit.
abline(coefficients(model2))

###############################################################################

### 5. Create a multiple linear regression model

# Multiple linear regression model.
model3 <- lm(Global_Sales ~ NA_Sales + EU_Sales, data=sales_pro)

summary(model3)

# Plot the residuals.
plot(model3$residuals)

###############################################################################

### 6. Predictions based on given values

# Create a dataframe to hold test values
sales_test <- data.frame (NA_Sales  = c(34.02, 3.93, 2.73, 2.26, 22.08),
                  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Obtain the global sales test values manually
for (i in 1:nrow(sales_test)) {sales_test$Global_Sales[i] <- 
  sales_test$NA_Sales[i] + sales_test$EU_Sales[i]}

# View the test data
sales_test

# Predict values using model3
predict_test = predict(model3, newdata = sales_test, interval = 'confidence')

# Compare predictions agains the test data
predict_test

###############################################################################
###############################################################################




