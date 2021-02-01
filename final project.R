library(corrplot)
library(ggplot2)
library(MASS)
library(leaps)
library(glmnet) 
library(caret)
library(dplyr)
library(readr)
library(nnet)
library(VGAM)
library(pROC)


wine_data <-read.csv("winequality-red.csv", TRUE, ";")
class(wine_data)
names(wine_data)
head(wine_data)
dim(wine_data)
str(wine_data)
summary(wine_data)

#correlation
#fixed.acidity & citric.acid
plot(fixed.acidity, citric.acid, main="Scatterplot", xlab="fixed.acidity", ylab="citric.acid", 
     ylim = c(0, 1.2), xlim = c(4, 16), cex=0.5, col=8)
abline(lm(citric.acid~fixed.acidity), col=2)

plot(fixed.acidity, density, main="Scatterplot", xlab="fixed.acidity", ylab="density", 
     xlim = c(4, 16), cex=0.5, col=8)
abline(lm(density~fixed.acidity), col=2)

plot(fixed.acidity, pH, main="Scatterplot", xlab="fixed.acidity", ylab="pH", col=8)
abline(lm(pH~fixed.acidity), col=2)

plot(free.sulfur.dioxide, total.sulfur.dioxide, main="Scatterplot", xlab="free.sulfur.dioxide", ylab="total.sulfur.dioxide", col=8)
abline(lm(total.sulfur.dioxide~free.sulfur.dioxide), col=2)

plot(citric.acid, volatile.acidity, main="Scatterplot", xlab="citric.acid", ylab="volatile.acidity", cex=0.6, col=8)
abline(lm(volatile.acidity~citric.acid), col=2)

attach(wine_data)
barplot(table(quality), col=c("indianred4", "indianred3", "indianred2", "indianred1", "indianred", "indianred4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.5)

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(fixed.acidity, col="indianred3", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(volatile.acidity, col="indianred3", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(citric.acid, col="indianred3", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)
boxplot(residual.sugar, col="indianred3", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(chlorides, col="indianred3", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(fixed.acidity, col="indianred3", pch=19)

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(free.sulfur.dioxide, col="indianred3", pch=19)
mtext("free.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(total.sulfur.dioxide, col="indianred3", pch=19)
mtext("total.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(density, col="indianred3", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(pH, col="indianred3", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(sulphates, col="indianred3", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)
boxplot(alcohol, col="indianred3", pch=19)
mtext("alcohol", cex=0.8, side=1, line=2)

attach(wine_data)
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
hist(fixed.acidity, col="indianred3")
hist(volatile.acidity, col="indianred3")
hist(citric.acid, col="indianred3")
hist(residual.sugar, col="indianred3")
hist(chlorides, col="indianred3")
hist(free.sulfur.dioxide, col="indianred3")
hist(total.sulfur.dioxide, col="indianred3")
hist(density, col="indianred3")
hist(pH, col="indianred3")
hist(sulphates, col="indianred3")
hist(alcohol, col="indianred3")

wine_data$rating <- ifelse(wine_data$quality < 5, 'poor', ifelse(
  wine_data$quality < 7, 'normal', 'excellent'))
wine_data$rating <- ordered(wine_data$rating,
                      levels = c('poor', 'normal', 'excellent'))
summary(wine_data$rating)

wine_data$quality <- factor(wine_data$quality, ordered = T)
qplot(data = wine_data, rating, fill = quality, 
      xlab = "Rating", ylab = "count", main = 'Distribution of Red Wine by Rating')


plot(wine_data)
corrplot(cor(wine_data)) #alcohol has the strongest correlation with wine quality

# least squares estimation  coef(model)

#
set.seed(6)
test.set <- seq(1,1599,2)
train_data <- wine_data[-test.set,]
test_data <- wine_data[test.set,]

# multiple linear regression; check multicollinearity
model1 <- lm(quality ~., data = train_data)
summary(model1)
vif(model1)
BIC(model1)

# remove fixed.acidity; high vif
model2 <- lm(
  quality ~ volatile.acidity + citric.acid + residual.sugar + chlorides + 
    free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = train_data)
summary(model2)
vif(model2)
AIC(model2)

# remove density
model2 <- lm(
  quality ~ volatile.acidity + citric.acid + residual.sugar + chlorides + 
    free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = train_data)
summary(model2)
vif(model2)
AIC(model2)
pred_model<- predict(model_ls, test_fat)
pred_model
mean((test_data$quality - pred_model) ** 2)

# outlier robust regression
model4 <- rlm(quality ~ volatile.acidity + citric.acid + residual.sugar + chlorides + 
                free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = train_data)
summary(model4)
plot(model4)

#best subset regression
bsr <- regsubsets(quality~., data = train_data)
bsr <- summary(bsr)
bsr
bsr$bic
model3 <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol, data = train_data)
summary(model3)

pred_model <- predict(model3, test_data)

mean((test_data$quality - pred_model) ** 2)

#ridge regression
model_ridge <- cv.glmnet(
  as.matrix(train_data[, 1 : 11]),
  train_data$quality,
  alpha = 0)
pred_ridge <- predict(
  model_ridge, 
  as.matrix(test_data[, 1 : 11]),
  s = "lambda.min")
mean((test_data$quality - pred_ridge) ** 2)

#lasso regression
model_lasso <- cv.glmnet(
  as.matrix(train_data[, 1 : 11]),
  train_data$quality,
  alpha = 1)
pred_lasso <- predict(
  model_lasso, 
  as.matrix(test_data[, 1 : 11]),
  s = "lambda.min")
mean((test_data$quality - pred_lasso) ** 2)

#logistic regression
wine_data$good.wine <- ifelse(wine_data$quality > 6, 1, 0)
wine_data$good.wine <- as.factor(wine_data$good.wine)
set.seed(12345)
test.set <- seq(1,1599,2)
train_data <- wine_data[-test.set,]
test_data <- wine_data[test.set,]
str(wine_data)
head(wine_data)
model <- glm(good.wine ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol, family = binomial, data = train_data)
summary(model)
predictions <- predict(model, test_data, type = "response")
predicted_values <- ifelse(predictions > 0.5, 1, 0)
predicted_values <- as.factor(predicted_values)
test_data$good.wine <- as.factor(test_data$good.wine)
confusionMatrix(predicted_values, test_data$good.wine)

## ROC curve
y <- as.numeric(test_data$good.wine)
g <- roc(y ~ predicted_values)
plot(g, grid = TRUE, print.auc = TRUE)
