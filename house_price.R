#MA321 ASSIGNMENT
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




###########################################
# Create a histogram of a variable using ggplot2
library(ggplot2)
ggplot(data, aes(x = variable)) + geom_histogram()

# Create a boxplot of a variable by group using ggplot2
ggplot(data, aes(x = group, y = variable)) + geom_boxplot()

# Add initial comments about the dataset
# For example, you may comment on the distribution of the variables, any outliers or missing values, and any interesting patterns or relationships that you observe.
