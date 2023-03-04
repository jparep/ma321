#Install Packages
#install.packages("corrplot") #v0.92 (under R version 4.2.2)
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("VIM")
#install.packages("magrittr")

library("magrittr")
library("VIM")
library("dplyr")
library("tidyverse")
library("corrplot")

#############################################################################
### Q1 Statistical & Visual DEscriptive Aanalysis
#############################################################################
# Load the dataset
data <- read.csv("house_data.csv")
attach(data)
# Viewing the first few rows of the dataset
head(data)
# Viewing the names of the variables
names(data)
#An overview of the dataset's structure
dim(data) 
str(data)
#Numerical summaries of the variables in the dataset
summary(data)


# Identifying and displaying numerical variables
num_var <- sapply(data, function(x) is.numeric(x))
dim(data[, num_var])
names(data[, num_var])

#checking for missing variables 
is.na(data)

install.packages("mice")
library(mice)
md.pattern(data) #We can see that only 10 variables have missing values(they have red boxes)
#No. of missing values in the data frame
sum(is.na(data))
#No. of missing values in each column of the data frame
missing_var<-sapply(data, function(x) sum(is.na(x)))
#Counting variables with missing values 
missing_counts <- colSums(is.na(data))
#Display names of variables with missing values 
missing_var_names <- names(missing_counts[missing_counts > 0])
missing_var_names


#GRadphical Description of Missing Data
#build function that counts missing values
count_nas <- function(data){
  
  variable_name_column <- c()
  number_missing_column <- c()
  
  for (i in 2:ncol(data)){
    variable_name <- colnames(data[i])
    number_missing <- sum(is.na(data[i]))
    variable_name_column <- c(variable_name_column,variable_name)
    number_missing_column <- c(number_missing_column,number_missing)
  }
  
  missing_table <- data.frame(variable_name_column,number_missing_column)
  missing_table <- missing_table %>% mutate(percentage=round(number_missing_column*100/nrow(data),2)) %>% arrange(desc(percentage))
  missing_table
}

#chart for missing values
aggr(data[-1], prop = T, numbers = T, cex.axis=.5, cex.numbers = 0.1,
     ylab=c("Proportion of missingness","Missingness Pattern"),
     labels=names(data[-1]))


###########################################
# Create a histogram of a variable using ggplot2
library(ggplot2)
ggplot(data, aes(x = variable)) + geom_histogram()

# Create a boxplot of a variable by group using ggplot2
ggplot(data, aes(x = group, y = variable)) + geom_boxplot()

# Add initial comments about the dataset
# For example, you may comment on the distribution of the variables, any outliers or missing values, and any interesting patterns or relationships that you observe.


#Correlation with SalePrice
numericVars <- which(sapply(data, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- data[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables


#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


#IMPUTATE DATA
#mpute missing data - In this case, the random forest mice function is used. Random seed set to 5
imputed_data <- mice(data, m=5, method = "rf")
compplete_imputed_data <- complete(imputed_data,3) # use 3rd cycle complete imputed dataset
summary(complete(compplete_imputed_data)) ##Summary for Descriptive Statistical Analysis
stripplot(imputed_data, pch = 20, cex = 1.2) #illustrate imputed data

# Check for any NA after imputation?
sapply(imputed_data, function(x) sum(is.na(x))) 
compplete_imputed_data$imp #Checking on the imputed values to diagnose any issues

#############################################################################
###  QUESTION 2: Logistic Regression to Classifiy Overall House Condition
#############################################################################
#Group houses based on Overall condition
unique(data$OverallCond)

data$HouseCondition <- with(data, ifelse(OverallCond >6, "High",
                                         ifelse(OverallCond >3, "Midium", "Low")))
df <- data %>%
  select("OverallCond", "HouseCondition")

df
  

#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################





#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################










