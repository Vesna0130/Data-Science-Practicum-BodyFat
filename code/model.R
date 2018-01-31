source('code/clean.R')

#bodyfat.clean=bodyfat[-c(42,39),-1]

### Simple linear model

#bodyfat.clean <- bodyfat.clean[,-1]
#bodyfat.clean[,c(2:15)] <- scale(bodyfat.clean[,c(2:15)])
model.clean=lm(DENSITY~.,data = bodyfat.clean[,-1])
summary(step(model.clean,direction = "both"))

#     The results before and after scaling doesn't have a significant difference.
#X <- scale(model.matrix(model.clean)[,-1])
X <- model.matrix(model.clean)[,-c(1,2)]
Y <- bodyfat.clean[,2]

library(leaps) # for leaps()
library(faraway) # for Cpplot()
g <- leaps(X, Y)
layout(1)
Cpplot(g)

g <- leaps(X, Y,nbest = 1)
Cpplot(g)

g<-leaps(X,Y, nbest = 1, method = "adjr2")
plot(g$adjr2)

(g$which)[which(g$adjr2 == max(g$adjr2)),]
r2.choice=c(2,3,5:10,12:15)
summary(model.r2 <- lm(DENSITY ~ ., data=bodyfat.clean[,c(1,r2.choice)]))


summary(lm.density <- lm(DENSITY ~ AGE + ADIPOSITY + CHEST + ABDOMEN + HIP + BICEPS + WRIST,
                 data = bodyfat.clean))
attach(bodyfat.clean)
den <- 1/DENSITY
fat <- BODYFAT
summary(lm(fat~den))
