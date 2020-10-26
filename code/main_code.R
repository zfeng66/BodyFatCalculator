## Contributions
## Zijun Feng & Yiqun Xiao: Model Selection Part
## Xiangyu Wang: Statistical Analysis Part
## Mengqi Li: Model Diagnosis Part


rm(list=ls())

if (!require("leaps")) {
  install.packages("leaps")
  library(leaps)
}
if (!require("caret")) {
  install.packages("caret")
  library(caret)
}
if (!require("car")) {
  install.packages("car")
  library(car)
}


data = read.csv("../data/BodyFat_clean.csv", header=T)
x <- as.matrix(data[, -1])
y <- data$BODYFAT
n <- length(y)


## Model Selection

model1 = lm(y~x) # consider main effects
model2 = lm(y~poly(x, 2)) # consider main effects and second order terms
anova(model1, model2) # second order terms are not significant

# Best subsets regression
leaps <- regsubsets(x=x, y=y, nbest = 8)
leaps_summary <- summary(leaps)
leaps_all_vars <- leaps_summary$which[,-1]

# Criterion1: adjusted R^2
(vars1 <- leaps_all_vars[which.max(leaps_summary$adjr2), ])

# Criterion2: Mallow's Cp
(vars2 <- leaps_all_vars[which.min(leaps_summary$cp), ])

# Criterion3: AIC
(vars3 <- leaps_all_vars[which.min(leaps_summary$bic+(2-log(length(y)))*as.numeric(rowSums(leaps_all_vars))), ])

# Criterion4: BIC
(vars4 <- leaps_all_vars[which.min(leaps_summary$bic), ])

leaps_summary$bic[which.min(leaps_summary$bic)]
par(mfrow=c(1, 1))
png("../image/BIC_TopModels.png", width=1200, height=1200, res=150)
plot(leaps, main="Top Models under BIC criterion")
graphics.off()

# adjusted R^2 returns a model with 8 variables
# Mallow's Cp returns a model with 6 variables
# AIC returns a model with 7 variables
# BIC returns a model with 3 variables

# Leave one out cross validation
train_ctrl <- trainControl(method = "LOOCV")

loocv1 <- train(y~.,
                data = data.frame(y, x[,vars1]),
                trControl = train_ctrl,
                method = "lm")
loocv1

loocv2 <- train(y~.,
                data = data.frame(y, x[,vars2]),
                trControl = train_ctrl,
                method = "lm")
loocv2

loocv3 <- train(y~.,
                data = data.frame(y, x[,vars3]),
                trControl = train_ctrl,
                method = "lm")
loocv3

loocv4 <- train(y~.,
                data = data.frame(y, x[,vars4]),
                trControl = train_ctrl,
                method = "lm")
loocv4

# RMSE almost the same, so choose the simplest model which is under BIC criterion

## Statistical Analysis

model = lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data = data)
summary(model)
predict(model, data.frame(WEIGHT=154, ABDOMEN=85, WRIST=17), se.fit=TRUE, interval="prediction", level=0.95)

## Model Diagnosis

# Multicollinearity
car::vif(model)


# Normality
png("../image/QQ.png", width=800, height=800, res=150)
qqnorm(rstandard(model), pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
       main="Normal QQ Plot of the Residuals")
abline(a=0, b=1, col="red", lwd=2)
graphics.off()
shapiro.test(rstandard(model))

# Linearity and Homoscedasticity
png("../image/StdResidual.png", width=800, height=800, res=150)
plot(predict(model),rstandard(model), pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Predicted Value", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(h=0, col="red", lwd=2)
graphics.off()

# Leverage points and Influential points
png("../image/Leverage.png", width=800, height=800, res=150)
plot(1:n,hatvalues(model), type="p", pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index", ylab="hii", main="Leverage Points")
graphics.off()

# Cook's distance
png("../image/CooksDistance.png", width=800, height=800, res=150)
plot(1:n,cooks.distance(model), type="p", pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index", ylab="Cook's Distance", main="influential Points")
graphics.off()

# DFFITS
png("../image/DIFFITS.png", width=800, height=800, res=150)
plot(1:n,dffits(model), type="p", pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index", ylab="DFFITS", main="influential Points")
abline(h=0, col="red", lwd=2)
graphics.off()


# DFBETAS
dfbeta = dfbetas(model)
png("../image/DFBETAS.png", width=800, height=1200, res=150)
par(mfrow = c(3,1))
plot(1:n,dfbeta[,2], type="p",pch=18, cex=1.2, cex.lab=1.5, cex.main=1.5,
     xlab="Index",ylab="DFBETAS:WEIGHT",main="influential Points")
abline(h=0, col="red", lwd=2)
plot(1:n,dfbeta[,3], type="p", pch=18, cex=1.2, cex.lab=1.5,
     xlab="Index",ylab="DFBETAS:ABDOMEN")
abline(h=0, col="red", lwd=2)
plot(1:n,dfbeta[,4], type="p", pch=18, cex=1.2, cex.lab=1.5,
     xlab="Index",ylab="DFBETAS:WRIST")
abline(h=0, col="red", lwd=2)
graphics.off()

# Remove Rplot.pdf
fn <- "Rplot.pdf"
if (file.exists(fn))
  file.remove(fn)
