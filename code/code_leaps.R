if (!require("leaps")) {
  install.packages("leaps")
}
if (!require("caret")) {
  install.packages("caret")
}

library(leaps)
library(caret)


data_bf0 <- read.csv("../data/BodyFat.csv", header = T)[, -1]

png( paste0( "../image/BodyFatHist.png" ) )
hist(data_bf0$BODYFAT)
graphics.off()

Calculated_BMI <- data_bf0$WEIGHT*0.45359237 / (data_bf0$HEIGHT*0.0254)^2
plot(data_bf0$ADIPOSITY, Calculated_BMI)  # BMI
plot(data_bf0$BODYFAT, data_bf0$DENSITY)  # Density-Bodyfat

data_bf <- data_bf0[, -2]  # remove the Density colum
data_bf <- data_bf[c(-182,  # Bodyfat=0
                     -42,  # Height too low
                     -216,  # Bodyfat too high
                     -39, -163, -221, # unmatched BMI
                     -48, -76, -96), ]  # unmatched Density-Bodyfat

## Anova test for all interaction terms
m0 <- lm(BODYFAT~., data=data_bf)
m_int <- lm(BODYFAT~.^2, data=data_bf)
anova(m0, m_int)  # No interaction term is significant

## Use leaps package to do model selection
x <- as.matrix(data_bf[, -1])
y <- data_bf$BODYFAT

leaps_summary <- summary(regsubsets(x=x, y=y, nbest=8))
leaps_all_vars <- leaps_summary$which[,-1]
vars1 <- leaps_all_vars[which.min(leaps_summary$rss), ]  # criterion1: RSS
vars2 <- leaps_all_vars[which.max(leaps_summary$adjr2), ]  # criterion2: adjR^2
vars3 <- leaps_all_vars[which.min(leaps_summary$cp), ]  # criterion3: Cp
vars4 <- leaps_all_vars[which.min(leaps_summary$bic+(2-log(length(y)))*as.numeric(rowSums(leaps_all_vars))), ]  # criterion4: AIC
vars5 <- leaps_all_vars[which.min(leaps_summary$bic), ]  # criterion5: BIC

## criterion1,2 return the same model with 8 variables
## criterion3,4 return the same model with 7 variables
## criterion5 returns a simpler model with 3 variables
leaps_result<-list(vars1,vars2,vars3,vars4,vars5)

## Anova test to compare these models
m1<-lm(y~x[,vars1])
m2<-lm(y~x[,vars3])
m3<-lm(y~x[,vars5])
anova(m2, m1)
anova(m3, m2)

## Leave one out cross validation for m2 and m3
train_ctrl <- trainControl(method = "LOOCV")
loocv1 <- train(y~.,
                data = data.frame(y, x[,vars3]),  # same model by criterion1,2,3,4
                trControl = train_ctrl,
                method = "lm")
loocv1

loocv2 <- train(y~.,
                data = data.frame(y, x[,vars5]),
                trControl = train_ctrl,
                method = "lm")
loocv2

