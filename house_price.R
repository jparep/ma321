#Assignment Mell Pull
library("VIM")
library("dplyr")
library("tidyverse")
library("corrplot")
library("mice")
library("ggplot2")
library("VIM")


################################################################################
####  Q1: STATISTICAL Descriptive ANALYSIS
################################################################################
# Load the dataset
df <- read.csv("house_data.csv")

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
# CHeck unique values in var
sapply(df, function(x) length(unique(x)))

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


############## FACTOR VARIABLEs - BOTHE CAT & NUM ##########################################
#comppleted_imp_df[] <- lapply(comppleted_imp_df, factor)
#factor_df <- comppleted_imp_df

#############################################################
#Group houses based on Overall condition
# Drop variables with 80% missing data (4 variables here have NA > 80%)
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence))
# Also, deselecting irrelevant variables
df1 <- subset(df1, select = -c(Id, LowQualFinSF, PoolArea, MiscVal, MoSold))

unique(df$OverallCond)
#OverallCon btween 7-10 is classified as 1 (Good), btwn 4-6 is classifed as 2 (Average),
# between 1-3 is classified as 3 (Poor) condition
df1$OverallCond <- with(df1, ifelse(OverallCond <=3, "Poor", ifelse(OverallCond <=6, "Average", "Good")))

# Factor all categorical Variables variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)

# Subset numerical variables that require factoring (9 num variabels require factoring here)
#num_fac <-  c("OverallQual", "FullBath", "BedroomAbvGr",
#                     "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")
#
# Factor numerical variables
#sub_df[num_fac] <- lapply(sub_df[num_fac], factor)

# Select all the categorical variables
#cat_df <- unlist(lapply(sub_df, is.factor))
#cat_df <- sub_df[cat_df]

num_df <- unlist(lapply(sub_df, is.numeric))
num_df <- sub_df[num_df]
##############################################################################
################ HANDLING NAs - IMPUTAE REMAINING MISSING VALUES ##############
#impute NAs - In this case, the random forest mice function is used. Random m set to 5
imp_df <- mice(df1, seed = 123, m=5, method = "rf")
df0 <- complete(imp_df,3) # use 3rd cycle complete imputed dataset

summary(complete(df0)) ##Summary for Descriptive Statistical Analysis

class(df0)
unique(df0$OverallCond)

# Check for any NA after imputation?
sapply(df0, function(x) sum(is.na(x))) # good to go!
sum(is.na(df0))

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


# calulate the correlations in numerical variables
r <- cor(num_df, use="complete.obs")
round(r,2)
library(ggplot2)
install.packages("ggcorrplot")
library("ggcorrplot")
ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

#############################################################################
############################################
##### CHECKING AND HANDLING OUTLIERS #######
############################################
length(select_if(df1, is.numeric)) # 14 numberical variables
names(select_if(df1, is.numeric))

#install.packages("flextable")
library(flextable) # for beautifying tables
library(dlookr)    # for the main event of the evening
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

###############################################################################
######  COLLINEARITy #
###############################################################################
#install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(na.omit(df0), maxcat = 5L)

library(corrplot)
data.corr <- as.data.frame(sapply(df, as.numeric))

correlations = cor(data.corr, method = "s")
# Show variables that have strong correlations with price, focus on coefficient > 0.5 or < -0.5
corr.price = as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
corr.id = names(which(apply(corr.price, 1, function(x) (x > 0.05 | x < -0.50))))
corrplot(as.matrix(correlations[corr.id,corr.id]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = 1,cl.cex = 1, number.cex=1)


sapply(df, function(x) length(unique(x)))


#############################################################################
###  QUESTION 2: Logistic Regression to Classifiy Overall House Condition
#############################################################################

###### SELECT NUMERICAL VARIABLES ########
num_df <- unlist(lapply(comppleted_imp_df, is.numeric))
num_df <- comppleted_imp_df[, num_df]

# Find 0's in num var
colSums(num_df == 0)

######### SUBSET CATEGORICAL VARIABLEA ########
cat_df <- unlist(lapply(df0, is.factor))
cat_df <- df0[, cat_df]
str(cat_df)

# TO DO - NEED TO SELECT FEATURES FIRST
count(comppleted_imp_df, OverallCond) #Check the classification distribution


############## Training Data ###############################

###### Train 1 #####
set.seed(1)
index <- sample(nrow(df0),nrow(df0)*0.80)
train = df0[index,]
test = df0[-index,]

## Train 2 #####
#install.packages("caTools")
library(caTools)
set.seed(12)
split <- sample.split(OverallCond, SplitRatio = 0.8)
train <- subset(df0, split == T)
test <- subset(df0, split == F)
# check output Class distributiion
table(test$OverallCon)
pre <- predict(mod3, newdata=train, type = "response",)
#table(test$OverallCond, pre > 0.5)
y_pred_num <- ifelse(pre > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$OverallCond
table(train$OverallCond, pre > 0.5)

########### TRain 3 #####################
#install.packages("caret")
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=1)  # prevents printing scientific notations.
set.seed(100)
split <- createDataPartition(df0$OverallCond, p=0.8, list = F)
train <- df0[split, ]
test <- df0[-split, ]

# Class distribution of train data
table(train$OverallCond)

# Down Sample
set.seed(100)
down_train <- downSample(x = train[, colnames(train) %ni% "OverallCon"],
                         y = train$OverallCond)
table(down_train$OverallCond)

# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = train[, colnames(train) %ni% "OverallCon"],
                     y = train$OverallCond)

table(up_train$OverallCond)



############## FEATURE SELECTION ##############################
# FEATURE SELECTION - for Overall Condition rating classification
fullMod1 <- glm(OverallCond~., family="binomial", data=train)

mod1 <- glm(OverallCond ~ Condition1 + HouseStyle + YearBuilt + Exterior1st + Exterior1st +
              MasVnrArea + Foundation + TotalBsmtSF + GrLivArea + Functional +GarageArea  +
              YrSold + SaleType + SalePrice, family="binomial", data=df0)

# Backwards selection is the default
step1 = step(fullMod1) 
### Fianl MOdel from step - AIC  1083.33
stepMod1 <- glm(OverallCond ~ Street + YearBuilt + MasVnrArea + ExterCond + Foundation + 
                  BsmtQual + BsmtCond + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + 
                  FullBath + BedroomAbvGr + KitchenQual + Fireplaces + GarageArea + 
                  GarageCond + YrSold + SalePrice,  family="binomial", data=df0)


mod2 <- glm(OverallCond ~ Condition1 + YearBuilt + BsmtQual + GrLivArea +
              Functional + GarageArea + SalePrice + SaleCondition + SaleType + PavedDrive +
              Fireplaces + GrLivArea + BldgType, family="binomial", data=train)

# Select model with stepAIC
library("MASS")
stepAIC <- stepAIC(fullMod1)

mod3 <- glm(OverallCond ~ YearBuilt + Foundation + BsmtQual + TotalBsmtSF + BsmtCond + BsmtQual + X1stFlrSF +
              X2ndFlrSF + FullBath + BedroomAbvGr + KitchenQual + Fireplaces + YrSold + SalePrice,family="binomial", data=train )

# Build Logistic Model
#logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
logitmod <- glm(OverallCond ~  Condition1 + YearBuilt + BsmtQual + GrLivArea +
                  Functional + GarageArea + SalePrice + SaleCondition + SaleType + PavedDrive +
                  Fireplaces + BldgType, family="binomial", data=down_train)

pred <- predict(logitmod, newdata = test, type = "response")
# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$OverallCond
table(pred)
# Accuracy
mean(y_pred == y_act)  # 94%

pred <- predict(logitmod, newdata = testData, type = "response")

summary(fullMod1) # AIC 1,165.9
summary(step)   #AIC  1,083.33
summary(stepMod)#AIC  1,083.33
summary(mod1)    #AIC 1,203.9
summary(mod2)     # AIC = 963.84.62 (removed TotRmsAbvGrd correlated to GrLivArea)
summary(stepAIC) #AIC = 1083.3
summary(mod3) # AIC=1083.33
summary(logitmod) #AIC 94.594


######Prediction #######
#install.packages('caTools')
library(caTools)
set.seed(88)
split <- sample.split(df0$OverallCond, SplitRatio = 0.80)

#get training and test data
dresstrain <- subset(df0, split == TRUE)
dresstest <- subset(df0, split == FALSE)

summary(logitmod)
predict <- predict(logitmod, type = 'response')
#confusion matrix
table(dresstrain$OverallCond, predict > 0.5)

#ROCR Curve
#install.packages("ROCR")
library(ROCR)
data(ROCR.simple)
ROCRpred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)

#library(ROCR)
#ROCRpred <- prediction(predict, dresstrain$OverallCond)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))




#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################

#SCALE Numerical variables 
df_scaled <- df0 %>% mutate_if(is.numeric, scale)
summary(df_scaled)
dfCom <- cbind(cat_df, df_scaled)



#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################









