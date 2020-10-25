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

# best subsets regression
leaps <- regsubsets(x=x, y=y, nbest = 8)
leaps_summary <- summary(leaps)
leaps_all_vars <- leaps_summary$which[,-1]

# Criterion1: adjusted R^2
vars1 <- leaps_all_vars[which.max(leaps_summary$adjr2), ]  

# Criterion2: Mallow's Cp
vars2 <- leaps_all_vars[which.min(leaps_summary$cp), ]

# Criterion3: AIC
vars3 <- leaps_all_vars[which.min(leaps_summary$bic+(2-log(length(y)))*as.numeric(rowSums(leaps_all_vars))), ] 

# Criterion4: BIC
vars4 <- leaps_all_vars[which.min(leaps_summary$bic), ]  

leaps_summary$bic[which.min(leaps_summary$bic)]
par(mfrow=c(1, 1))
png("../image/BIC_TopModels.png", width=1200, height=1200, res=150)
plot(leaps)
graphics.off()

# adjusted R^2 returns a model with 8 variables
# Mallow's Cp returns a model with 6 variables
# AIC returns a model with 7 variables
# BIC returns a model with 3 variables

(re <- cbind(adjusted_R = leaps_summary$adjr2[which.min(leaps_summary$bic)], cp = leaps_summary$cp[which.min(leaps_summary$bic)], 
             aic = leaps_summary$bic[which.min(leaps_summary$bic)]+(2-log(length(y)))*sum(leaps_all_vars[which.min(leaps_summary$bic),]), bic = leaps_summary$bic[which.min(leaps_summary$bic)]))

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

model = lm(y~x[,vars4])
summary(model)

## Model Diagnosis

# Multicollinearity
car::vif(lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data = data))

# Linearity and Homoscedasticity
png("../image/StdResidual.png", width=800, height=800, res=150)
plot(predict(model),rstandard(model),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main=" Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)
graphics.off()

# Normality
png("../image/QQ.png", width=800, height=800, res=150)
qqnorm(rstandard(model),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
graphics.off()
shapiro.test(rstandard(model))


# Leverage points and Influential points
pii = hatvalues(model)
png("../image/Leverage.png", width=800, height=800, res=150)
plot(1:n,pii,type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Pii",main="Leverage Values (Pii)")
graphics.off()

# Cook's distance
png("../image/CooksDistance.png", width=800, height=800, res=150)
cooki = cooks.distance(model)
plot(1:n,cooki,type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")
graphics.off()

# DFFITS
png("../image/DIFFITS.png", width=800, height=800, res=150)
diff = dffits(model)
plot(1:n,diff,type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="DFFITS",main="Influence Values (DFFITS)")
abline(a=0,b=0,col="black",lwd=3)
graphics.off()


# DFBETAS
dfbeta = dfbetas(model)
png("../image/DFBETAS.png", width=800, height=1200, res=150)
par(mfrow = c(3,1))
plot(1:n,dfbeta[,2],type="p",pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="DFBETAS:WEIGHT",main="Influence Values (DFBETAS)")
abline(a=0,b=0,col="black",lwd=3)
plot(1:n,dfbeta[,3],type="p",pch=19,cex=1.2,cex.lab=1.5,
     xlab="Index (Each Observation)",ylab="DFBETAS:ABDOMEN")
abline(a=0,b=0,col="black",lwd=3)
plot(1:n,dfbeta[,4],type="p",pch=19,cex=1.2,cex.lab=1.5,
     xlab="Index (Each Observation)",ylab="DFBETAS:WRIST")
abline(a=0,b=0,col="black",lwd=3)
graphics.off()

