library("VIM")
library("dplyr")
library("tidyverse")
library("mice")
library("ggplot2")
library("ggcorrplot")
library("corrplot")
library("VIM")
library("DataExplorer")
library("dlookr")
library("flextable")
library('caTools')
library("caret")
library("nnet")
library("e1071") #for SVM model

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

# Statistical description in transpose form
num_df <- unlist(lapply(df, is.numeric))
num_df <- df[, num_df]
t(summary(num_df))

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
# Visual 1 - NA graphical description
plot_histogram(df) #Histogram for all the numberical variables

md.pattern(df) #

# Visual 2 - NA graphical description
aggr(df[-1], prop = T, numbers = T, cex.axis=.5, cex.numbers = 0.1,
     ylab=c("Proportion of missingness","Missingness Pattern"),
     labels=names(df[-1]))

# Visual 3 - NA graphical description 
aggr_plot <- aggr(df, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(df),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))


############## Removing variables with NA > 80%  ##########################################
# Drop variables with 80% missing data (4 variables here have NA > 80%)
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence))
# Also, deselecting irrelevant variables
df1 <- subset(df1, select = -c(Id, LowQualFinSF, PoolArea, MiscVal, MoSold))

unique(df$OverallCond)

#Group houses based on Overall condition
#OverallCon btween 7-10 is classified as 1 (Good), btwn 4-6 is classifed as 2 (Average),
# between 1-3 is classified as 3 (Poor) condition
df1$OverallCond <- with(df1, ifelse(OverallCond <=3, "Poor", ifelse(OverallCond <=6, "Average", "Good")))

# Factor all categorical Variables variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)

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
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3 | x < -0.3)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


# calulate the correlations in numerical variables
r <- cor(num_df, use="complete.obs")
round(r,2)

ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


##### OUTLIERS  ####################
length(select_if(df1, is.numeric)) # 14 numberical variables
names(select_if(df1, is.numeric))

diagnose_numeric(df1) %>% 
  filter(minus > 0 | zero > 0) %>% 
  select(variables, median, zero:outlier) %>% 
  flextable()

# Take average for the outliers to check the influence on variables
diagnose_outlier(df1) %>% flextable()
# Get descriptive statistics after imputed
describe(df1) %>% flextable()

# Plot data with outliers.
par(mfrow=c(1, 2))
plot(df$GrLivArea, df$SalePrice, xlim=c(0, 6000), ylim=c(0,700000), main="With Outliers", xlab="GrLive Area", ylab="Sale Price", pch="*", col="red", cex=2)
abline(lm(SalePrice ~ GrLivArea, data=df), col="blue", lwd=3, lty=2)
# Plot of original data without outliers. Note the change in slope (angle) of best fit line.

# Check outliers graphically for all the numerical variables 
# Since outliers in these numerical var contains important info, they are retained
df0 %>% select(SalePrice) %>%  plot_outlier()   
df0 %>% select(LotFrontage) %>%  plot_outlier() 
df0 %>% select(LotArea) %>%  plot_outlier()     
df0 %>% select(YearBuilt) %>%  plot_outlier()   
df0 %>% select(MasVnrArea) %>%  plot_outlier()
df0 %>% select(TotalBsmtSF) %>%  plot_outlier() 
df0 %>% select(X2ndFlrSF) %>%  plot_outlier()   
df0 %>% select(LowQualFinSF) %>%  plot_outlier()
df0 %>% select(GrLivArea) %>%  plot_outlier()   
df0 %>% select(GarageArea) %>%  plot_outlier()
df0 %>% select(PoolArea) %>%  plot_outlier()    
df0 %>% select(MiscVal) %>%  plot_outlier() 

###Normality Test
normality(df1) %>% flextable()

######  COLLINEARITy #############################
plot_correlation(na.omit(df1), maxcat = 5L)

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
############## Training Data ###############################
set.seed(123) #set seed
split <- sample.split(df0$OverallCond, SplitRatio = 0.80) #Plit dataset
train <- subset(df0, split == TRUE) #Training dataset
test <- subset(df0, split == FALSE) # Testing dataset

############## MODEL  - FEATURE SELECTION ##############################
# Full Model - All features selected
full_mod1 <- multinom(OverallCond~., family="binomial", data=train)

# Features selected using P-value < 0.5 from full model output
reduced_mod2 <- multinom(OverallCond ~ Condition1 + HouseStyle + YearBuilt + Exterior1st + Exterior1st +
              MasVnrArea + Foundation + TotalBsmtSF + GrLivArea + Functional +GarageArea  +
              YrSold + SaleType + SalePrice, data=train)

# Features selected from  highly correlation matrix
corr_mod3 <- multinom(OverallCond ~ Condition1 + YearBuilt + BsmtQual + GrLivArea +
              Functional + GarageArea + SalePrice + SaleCondition + SaleType + PavedDrive +
              Fireplaces + GrLivArea + BldgType, data=train)

# Full Step model
step = step(full_mod1)

# Features selected from Full Step Model
step_mod4 <- multinom(OverallCond ~ Street + Neighborhood + Condition1 + HouseStyle + 
                   YearBuilt + RoofMatl + Exterior1st + ExterQual + ExterCond + 
                   Foundation + BsmtQual + BsmtCond + TotalBsmtSF + GrLivArea + 
                   FullBath + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageArea + 
                   GarageCond + YrSold + SaleType + SaleCondition + SalePrice, data=train)


summary(full_mod1)        # AIC = 1135.28  
summary(reduced_mod2)        # AIC = 1080.597  
summary(corr_mod3)        # AIC = 1096.734  
summary(step_mod4)# AIC = 977.3645  use this model since few features with low AIC value

######Prediction ########################
#2-tailed z test
z <- summary(step_mod4)$coefficients/summary(step_mod4)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

################################################
# Logistic Regression Prediction -> TEST DATASET
pred1_train <- predict(step_mod4, train)
#Confusion Matrix
tab1_train <- table(pred1_train, train$OverallCond)
confusionMatrix(tab1_train)

# Accuracy test on Training Dataset
sum(diag(tab1_train))/sum(tab1_train) # 89.6% classification accuracy based on testing data
1 - sum(diag(tab1_train))/sum(tab1_train) #10.4% overall miss classification

# Prediction and Model Assessment
# Accuracy & Sensitivity for Testing Data
n1 <-table(train$OverallCond)
n1
n1/sum(n1)
tab1_train/colSums(tab1_train) #Average classification accuraccy is performing better than Poor and better relatively


################################################
# Logistic Regression Prediction -> TEST DATASET
pred2_test <- predict(step_mod4, test)
#Confusion Matrix
tab2_test <- table(pred2_test, test$OverallCond)
confusionMatrix(tab2_test)

#Accuracy test on Testing Dataset
sum(diag(tab2_test))/sum(tab2_test) # 80.1% classification accuracy based on testing data
1 - sum(diag(tab2_test))/sum(tab2_test) #19.9% overall miss classification

# Accuracy & Sensitivity for Testing Data
n2 <-table(test$OverallCond)
n2
n2/sum(n2)
tab2_test/colSums(tab2_test) #Average classification accuraccy is performing better than Poor and better relatively


###### SVM MODEL ############################################################
# Features selected from Full Step Model
svm_mod <- svm(OverallCond ~ Street + Neighborhood + Condition1 + HouseStyle + 
                                YearBuilt + RoofMatl + Exterior1st + ExterQual + ExterCond + 
                                Foundation + BsmtQual + BsmtCond + TotalBsmtSF + GrLivArea + 
                                FullBath + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageArea + 
                                GarageCond + YrSold + SaleType + SaleCondition + SalePrice, data=train)

summary(svm_mod)

#####################################
# SVM prediction -> TRAINING DATASET
pred3_train <- predict(svm_mod, train)
#Confusion Matrix
tab3_train <- table(pred3_train, train$OverallCond)
confusionMatrix(tab3_train)

# SVM Accuracy test -> on Training Dataset
sum(diag(tab3_train))/sum(tab3_train) # 83.3% classification accuracy based on testing data
1 - sum(diag(tab3_train))/sum(tab3_train) #16.7% overall miss classification

# SVM Prediction and Model Assessment
n3 <-table(train$OverallCond)
n3
n3/sum(n3)
tab3_train/colSums(tab3_train)

#####################################
# SVM prediction -> TESTING DATASET
pred4_test <- predict(svm_mod, test)
#Confusion Matrix
tab4_test <- table(pred4_test, test$OverallCond)
confusionMatrix(tab4_test)

#SVM Accuracy test -> on Testing Dataset
sum(diag(tab4_test))/sum(tab4_test) # 80.1% classification accuracy based on testing data
1 - sum(diag(tab4_test))/sum(tab4_test) #19.9% overall miss classification

# Accuracy & Sensitivity for Testing Data
n4 <-table(test$OverallCond)
n4
n4/sum(n4)
tab4_test/colSums(tab4_test) #Average classification accuraccy is performing better than Poor and better relatively

#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################

#SCALE Numerical variables 
df_scaled <- num_df %>% mutate_if(is.numeric, scale)
t(summary(df_scaled))
dfCom <- cbind(cat_df, df_scaled)


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
       
##Models:
no_id_df <- df0[,-c(1)]
indeces <- sample(nrow(no_id_df), 0.8*nrow(no_id_df))
indeces
length(indeces)

train =no_id_df[indeces,]
test = no_id_df[-indeces,]
dim(train)
dim(test)


#Random forest
require(randomForest)
#install.packages('randomForest')
library(randomForest)
library(ipred)

forest.df <- randomForest(SalePrice ~ ., data=train)
forest.df
plot(forest.df)

pred<-predict(forest.df, newdata=test)

errorest(SalePrice ~ ., data=test, model=randomForest,
         estimator = "cv", predict = pred)

plot(x=pred, y=test[,50],
     xlab='Predicted Values',
     ylab='Actual Values', col='blue',
     main='Predicted vs. Actual Values')

#abline(a=0,b=1, col='red')
#abline shows a 'perfect' gradient

#SVM 
library(e1071)
#new df with high correlation to sale price
high_corr_df <- no_id_df[,c(12,29,38,24,26,30,34,14,50)]
names(high_corr_df)

#removing 'X1stFlrSF' as it is highly correlated with 'TotalBsmntSF'
#removing 'GrLivArea' as it is highly correlated with 'TotRmsAbvGrd'
new_df <- high_corr_df[,-c(2,5)]
names(new_df)

indeces = sample(nrow(new_df), 0.8*nrow(new_df))
indeces
length(indeces)

training=new_df[indeces,]
testing= new_df[-indeces,]
dim(training)
dim(testing)

SVM_model<- svm(SalePrice~., data=training)
print(SVM_model)

svm_pred <- predict(SVM_model, testing)
str(training)
str(testing)

sapply(lapply(testing, unique), length)

x <- 1:length(test$SalePrice)

plot(x, testing$SalePrice, pch=18, col="red")
lines(x, svm_pred, lwd="1", col="blue")

errorest(SalePrice ~ ., data=testing, model=svm,
         estimator = "cv", predict = svm_pred)



#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################









