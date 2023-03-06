#library("magrittr")
library("VIM")
library("dplyr")
library("tidyverse")
library("corrplot")
library("mice")
library("ggplot2")
library("VIM")


# Load the dataset
df <- read.csv("house_data.csv")

############# STATISTICAL Descriptive ANALYSIS ##############################
attach(df)
head(df)  # Viewing the first few rows of the dataset
names(df) # Viewing the names of the variables
dim(df)   #An overview of the dataset's structure
view(df)
glimpse(df)
str(df)
summary(df) #Numerical summaries of the variables in the dataset
introduce(df) #Get more detail about row, columns and NAs
##check duplicate
duplicated(df)
df[duplicated(ddf),]
### Missing Value Analysis - check NAs
sum(is.na(df)) # 5,910 total NAs
df_naCols <- which(colSums(is.na(df))>0) # Identify variables with NAs
sort(colSums(sapply(df[df_naCols],is.na)), decreasing = TRUE) # 10 variables with NAs (Total NAs ordered in Dec)

################ GRAPHICAL DESCRIPTIVE ANALYSIS ###############################
# V1 - NA graphical description
plot_histogram(df) #Histogram for all the numberical variables

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


########## CORRELATION ########################################################
#Correlation with 
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


################### CREATE SUBSET & DESELECT VARIABLES #########################
# Drop variables with 80% missing data (4 variables here have NA > 80%)
# Also, deselecting variables like month Sold var which does not add any value in House Condition
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence, MoSold, YrSold))

# Factor all char variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)
str(df1) # Show char variables converted to factor

# Subset numerical variables that require factoring (9 num variabels require factoring here)
num_var_factor <-  c("OverallQual", "OverallCond", "FullBath", "BedroomAbvGr",
                     "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")

# Factor numerical variables
df1[num_var_factor] <- lapply(df1[num_var_factor], factor)
str(df1) # Show numerical variables converted to factor

################ IMPUTAE REMAINING MISSING VALUES #############################
# Graphically illustrate remaining NAs to be imputed
md.pattern(df1)
#impute NAs - In this case, the random forest mice function is used. Random m set to 5
imp_df <- mice(df1, seed = 123, m=5, method = "rf")
comppleted_imp_df <- complete(imp_df,3) # use 3rd cycle complete imputed dataset
summary(complete(comppleted_imp_df)) ##Summary for Descriptive Statistical Analysis
stripplot(imp_df, pch = 20, cex = 1.2) #illustrate imputed data

# Check for any NA after imputation?
sapply(comppleted_imp_df, function(x) sum(is.na(x))) # good to go!
sum(is.na(comppleted_imp_df))
View(comppleted_imp_df)

#############################################################################
############################################
##### CHECKING AND HANDLING OUTLIERS #######
############################################
length(select_if(df1, is.numeric)) # 14 numberical variables
names(select_if(df1, is.numeric))

diagnose_numeric(comppleted_imp_df) %>% 
  filter(minus > 0 | zero > 0) %>% 
  select(variables, median, zero:outlier) %>% 
  flextable()

# Take average for the outliers to check the influence on variables
diagnose_outlier(comppleted_imp_df) %>% flextable()
# Get descriptive statistics after imputed
describe(comppleted_imp_df) %>% flextable()

###Normality Test
normality(comppleted_imp_df) %>% flextable()

# Check outliers graphically for all the numerical variables 
# Since outliers in these numerical var contains important info, they are retained
comppleted_imp_df %>% select(SalePrice) %>%  plot_outlier()   
comppleted_imp_df %>% select(LotFrontage) %>%  plot_outlier() 
comppleted_imp_df %>% select(LotArea) %>%  plot_outlier()     
comppleted_imp_df %>% select(YearBuilt) %>%  plot_outlier()   
comppleted_imp_df %>% select(MasVnrArea) %>%  plot_outlier()
comppleted_imp_df %>% select(TotalBsmtSF) %>%  plot_outlier() 
comppleted_imp_df %>% select(X2ndFlrSF) %>%  plot_outlier()   
comppleted_imp_df %>% select(LowQualFinSF) %>%  plot_outlier()
comppleted_imp_df %>% select(GrLivArea) %>%  plot_outlier()   
comppleted_imp_df %>% select(GarageArea) %>%  plot_outlier()
comppleted_imp_df %>% select(PoolArea) %>%  plot_outlier()    
comppleted_imp_df %>% select(MiscVal) %>%  plot_outlier()     t

########################################
######  Collinearity 
###########################################
#install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(na.omit(comppleted_imp_df), maxcat = 5L)

df_scaled <- comppleted_imp_df %>% mutate_if(is.numeric, scale)

summary(df_scaled)


##############  Q1 END #########################################################

#############################################################################
###  QUESTION 2: Logistic Regression to Classifiy Overall House Condition
#############################################################################
#Group houses based on Overall condition
unique(comppleted_imp_df$OverallCond)

comppleted_imp_df$OverallCond <- with(df, ifelse(OverallCond >6,"Good",
                                         ifelse(OverallCond >3, "Average", "Poor")))

###### SELECT NUMERICAL VARIABLES ########
num_df <- unlist(lapply(comppleted_imp_df, is.numeric))
num_df <- comppleted_imp_df[, num_df]

#Remove ID Column
num_df <- num_df %>% select(-Id)
str(num_df)

# Find 0's in num var
colSums(num_df == 0)

######### SUBSET CATEGORICAL VARIABLEA ########
cat_df <- unlist(lapply(comppleted_imp_df, is.factor))
cat_df <- comppleted_imp_df[, cat_df]
str(cat_df)



# TO DO - NEED TO SELECT FEATURES FIRST
count(comppleted_imp_df, OverallCond) #Check the classification distribution

############## Training Data ###############################
#create a list of random number ranging from 1 to number of rows from actual data 
#and 80% of the data into training data 
sample_df = sort(sample(nrow(comppleted_imp_df), nrow(comppleted_imp_df)*.8))

#creating training data set by selecting the output row values
train <- comppleted_imp_df[sample_df,]

#creating test data set by not selecting the output row values
test <- comppleted_imp_df[-sample_df,]
 
dim(df) 
dim(train)
dim(test)
names(df)

#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################





#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################

library("car")

mod <- glm(SalePrice ~ LotFrontage +  LotArea + OverallQual + YearBuilt + MasVnrArea + TotalBsmtSF + 
  X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + FullBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + 
  Fireplaces + GarageArea + PoolArea + MiscVal + SalePrice ,data = num_df, family = binomial(link = "logit"))


