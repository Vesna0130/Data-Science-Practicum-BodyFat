source('code/clean.R')
attach(bodyfat.clean)

par(mfrow = c(1, 1))
    
#bodyfat.clean <- scale(bodyfat.clean)
#bodyfat.clean=bodyfat[-c(42,39),-1]

### Simple linear model

#bodyfat.clean <- bodyfat.clean[,-1]
#bodyfat.clean[,c(2:15)] <- scale(bodyfat.clean[,c(2:15)])

# Consider all variables and fit a simple linear model, there is a relative large R-square.
# But most of the variables are not significant.

library(car)
vif(lm.fat)

### Lasso

## sampling
x <- as.matrix(bodyfat.clean[,-c(1,2)])
y <- as.vector(BODYFAT)

## ridge regression
library(glmnet)
grid <- 10^seq(10,-2,length = 100)
#ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
#plot(ridge.mod, main = "The ridge")

par(mfrow = c(1, 1))

## the lasso
library(lars)
(laa <- lars(x, y))
summary(laa)





lasso.mod<-glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.mod, main = "The lasso")

summary(lm.lasso <- lm(formula = BODYFAT ~ HEIGHT + ABDOMEN + WRIST + AGE + NECK,
                  data = bodyfat.clean[, -2]))
summary(lm.lasso <- lm(formula = BODYFAT ~ HEIGHT + ABDOMEN + WRIST + AGE,
                       data = bodyfat.clean[, -2]))

library(glmnet)

## cross-validation
set.seed(1000)
cv.out <- cv.glmnet(as.matrix(bodyfat.clean[,-c(1,2)]), as.vector(BODYFAT), alpha = 1, nfolds = 10)
plot(cv.out)
(best.lambda <- cv.out$lambda.min)

lasso.pred <- predict(lasso.mod, s = best.lambda, newx = x)
mean((lasso.pred - y)^2)

##
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out,type = "coefficients",s = best.lambda)
lasso.coef





# Fit linear model

lm.backward <- lm(formula = BODYFAT ~ ., data = bodyfat.clean[, -2])
summary(step(lm.backward, direction = "backward", k = log(250) ))

lm.backward <- lm(formula = BODYFAT ~ ABDOMEN + WRIST + AGE,
                  data = bodyfat.clean[, -2])
plot(lm.backward, which = 1)


summary(step(lm.lasso, direction = "backward", k = log(250) ))
lm.lasso.step <- lm(formula = BODYFAT ~ HEIGHT + ABDOMEN + WRIST, data = bodyfat.clean[, -2])

finaldata <- as.data.frame(cbind(BODYFAT, HEIGHT, ABDOMEN, FOREARM, WRIST))
plot(finaldata)

lm.full <- lm(formula = BODYFAT ~ ., data = bodyfat.clean[, -2])

### Summary
Adj.r.squared <- round(c(summary(lm.lasso)$adj.r.squared, summary(lm.backward)$adj.r.squared,
                         summary(lm.lasso.step)$adj.r.squared, summary(lm.full)$adj.r.squared) , 4)# Adjusted R^2
AIC <- round(c(AIC(lm.lasso), AIC(lm.backward), AIC(lm.lasso.step), AIC(lm.full)) , 4)# AIC
BIC <- round(c(BIC(lm.lasso), BIC(lm.backward), BIC(lm.lasso.step), BIC(lm.full)) , 4)# BIC
MSE <- round(c(mean((lm.lasso$fitted.values - y)^2), mean((lm.backward$fitted.values - y)^2),
               mean((lm.lasso.step$fitted.values - y)^2), mean((lm.full$fitted.values - y)^2)) , 4) # MSE
Variable <- c('HEIGHT, ABDOMEN, WRIST, AGE','HEIGHT, ABDOMEN, WRIST','ABDOMEN, WRIST, AGE','All')
summary <- rbind(Adj.r.squared, AIC, BIC, MSE, Variable)
colnames(summary) <- c('Lasso','Backward','Lasso.step','Full')
rownames(summary) <- c('Adjusted R^2', 'AIC', 'BIC', 'MSE','Variables')
summary

summary.lasso <- summary(lm.lasso)
CI.upper <- summary.lasso$coefficients[,1] + summary.lasso$coefficients[,2]*qnorm(0.975)
CI.lower <- summary.lasso$coefficients[,1] - summary.lasso$coefficients[,2]*qnorm(0.975)
cbind(round(summary.lasso$coefficients, 4), 'Confidence Intervals' = paste0('(',round(CI.lower,4), ', ', round(CI.upper,4),')'))
