#library("magrittr")
library("VIM")
library("dplyr")
library("tidyverse")
library("corrplot")
library("mice")
library("ggplot2")
library("VIM")


#############################################################################
### Q1 Statistical & Visual DEscriptive Aanalysis
#############################################################################
# Load the dataset
df <- read.csv("house_data.csv")

attach(df)
head(df)  # Viewing the first few rows of the dataset
names(df) # Viewing the names of the variables
dim(df)   #An overview of the dataset's structure
glimpse(df)
str(df)
summary(df) #Numerical summaries of the variables in the dataset

#checking for missing variables 
sum(is.na(df)) # 5,910 total NAs
df_naCols <- which(colSums(is.na(df))>0) # Identify variables with NAs
sort(colSums(sapply(df[df_naCols],is.na)), decreasing = TRUE) # 10 variables with NAs (Total NAs ordered in Dec)

# V1 - NA graphical description
md.pattern(df) #

# V1 - NA graphical description
aggr(df[-1], prop = T, numbers = T, cex.axis=.5, cex.numbers = 0.1,
     ylab=c("Proportion of missingness","Missingness Pattern"),
     labels=names(df[-1]))

# V3 - NA graphical description 
aggr_plot <- aggr(df, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(df),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))


# Identifying and displaying numerical variables
#Correlation with SalePrice
numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
all_numVar <- df[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#IMPUTATE DATA
#mpute missing data - In this case, the random forest mice function is used. Random seed set to 5
imputed_data <- mice(df, m=5, method = "rf")
compplete_imputed_data <- complete(imputed_data,3) # use 3rd cycle complete imputed dataset
summary(complete(compplete_imputed_data)) ##Summary for Descriptive Statistical Analysis
stripplot(imputed_data, pch = 20, cex = 1.2) #illustrate imputed data

# Check for any NA after imputation?
sapply(imputed_data, function(x) sum(is.na(x))) 
compplete_imputed_data$imp #Checking on the imputed values to diagnose any issues

############ Q1 TEMP ########################################################
#### Handled NA and outliers aid my work in the interim #####################

# Drop variables with 80% missing data (4 variables here have NA > 80%)
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence))

# Before imputing remaining NAs, factor categorical variables first
# Factor all char variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)
str(df1) # Show char variables converted to factor

# Subset numerical variables that require factoring (7 num variabels require factoring here)
num_var_factor <-  c("OverallQual", "OverallCond", "FullBath", "BedroomAbvGr",
                     "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")

# Factor numerical variables
df1[num_var_factor] <- lapply(df1[num_var_factor], factor)
str(df1) # Show numerical variables converted to factor

# Graphically illustrate remaining NAs to be imputed
md.pattern(df1)

#impute NAs - In this case, the random forest mice function is used. Random m set to 5
imp_df <- mice(df1, seed = 123, m=5, method = "rf")
comppleted_imp_df <- complete(imp_df,3) # use 3rd cycle complete imputed dataset
summary(complete(comppleted_imp_df)) ##Summary for Descriptive Statistical Analysis
stripplot(imp_df, pch = 20, cex = 1.2) #illustrate imputed data

# Check for any NA after imputation?
sapply(comppleted_imp_df, function(x) sum(is.na(x))) # good to go!
View(comppleted_imp_df)

##############  Q1 END #########################################################

#############################################################################
###  QUESTION 2: Logistic Regression to Classifiy Overall House Condition
#############################################################################
#Group houses based on Overall condition
unique(comppleted_imp_df$OverallCond)

comppleted_imp_df$HouseCondition <- with(df, ifelse(OverallCond >6, "High",
                                         ifelse(OverallCond >3, "Midium", "Low")))
df2 <- comppleted_imp_df %>%
  select("OverallCond", "HouseCondition")

df2
  

#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################





#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################










