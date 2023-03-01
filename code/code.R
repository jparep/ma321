#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("purrr")
library("ggplot2")
library("tidyvers")
library("dplyr")
library("purrr")

#Task (1.A) - Load Data
data <- read.csv("./raw_data/house_data.csv")
attach(data)

#Task 1(B) - Statistical Analysis Description
head(data)
names(data)
dim(data)
summary(data)
View(data)


#Task (1.C) - graphically Description
ggplot(data, aes(SaleType, SalePrice, color=SalePrice)) +
  geom_point(aes(color=SaleType)) +
  scale_color_viridis_d() +
  theme_minimal()

ggplot(data=data, aes(x=SalePrice)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Price Values")


#Task (1.D) - checking NA
which(is.na(data))
sum(is.na(data)) #5910
sapply(data, function(x) sum(is.na(x))) #Missing data for each column

#Task (1.E) - Handle missing values

#Task (1.F) - Check Outliers
boxplot(data$SalePrice,
        ylab = "SalePrice"
)
