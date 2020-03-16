# Houce Prices Prediction - Prerit Saxena

rm(list=ls(all=TRUE))

setwd("C:/Users/Prerit/Desktop/Datatest/House Prices")

# Reading in data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Looking at the datasets

str(train)
str(test)

summary(train)
summary(test)

# Cleaning

test$SalePrice <- 500
full_data <- rbind(train, test)

str(full_data)

# NA's

sort(colSums(is.na(full_data)), decreasing = TRUE)

library(DMwR)
full_data2 <- centralImputation(full_data)

str(full_data2)

sort(colSums(is.na(full_data2)), decreasing = TRUE)

# Splittings

train2 <-  subset(full_data2, SalePrice != 500)
test2 <- subset(full_data2, SalePrice == 500)

str(train2)
str(test2)

# Training and validation split

library(caTools)
set.seed(123)
spl <- sample.split(train2, 0.7)
train3 <- subset(train2, spl == TRUE)
valid1 <- subset(train2, spl == FALSE)

# Baseline model

model_lm1 <- lm(SalePrice~., data = train3)
summary(model_lm1)

model_lm2 <- step(model_lm1)
summary(model_lm2)

# MSE

MSE <- mean(model_lm2$residuals^2)
MSE

# predicting on validattion dataset


levels(valid1$Condition2) = levels(train3$Condition2)
pred_lm <- predict(model_lm2, newdata = valid1)

library(rpart)
model_cart <- rpart(SalePrice~., data = train3, method = "anova")
summary(model_cart)

pred_cart <- predict(model_cart, newdata = valid1)

library(rpart.plot)
prp(model_cart)
hist(pred_cart)

# MSE 

RMSE_valid <- sqrt(mean((log(pred_cart)-log(valid1$SalePrice))^2))
RMSE_valid

pred_cart_test <- predict(model_cart, newdata = test)

# write to file

output <- data.frame(test$Id, pred_cart_test)
colnames(output) <- c("Id", "SalePrice")

write.csv(output, "submission1.csv", row.names = FALSE)











