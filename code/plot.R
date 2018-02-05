source('code/clean.R')
source('code/model.R')

plot(bodyfat.clean)

# Explained Variable
par(mfrow = c(2, 2))

hist(bodyfat$BODYFAT)
boxplot(bodyfat$BODYFAT)
qqnorm(bodyfat$BODYFAT)
qqline(bodyfat$BODYFAT)
plot(bodyfat$BODYFAT)

hist(bodyfat$DENSITY)
boxplot(bodyfat$DENSITY)
qqnorm(bodyfat$DENSITY)
qqline(bodyfat$DENSITY)
plot(bodyfat$DENSITY)

hist(bodyfat$AGE)
boxplot(bodyfat$AGE)
qqnorm(bodyfat$AGE)
qqline(bodyfat$AGE)
plot(bodyfat$AGE)

hist(bodyfat$WRIST)
boxplot(bodyfat$WRIST)
qqnorm(bodyfat$WRIST)
qqline(bodyfat$WRIST)
plot(bodyfat$WRIST)

hist(bodyfat$HEIGHT)
boxplot(bodyfat$HEIGHT)
qqnorm(bodyfat$HEIGHT)
qqline(bodyfat$HEIGHT)
plot(bodyfat$HEIGHT)

hist(bodyfat$ABDOMEN)
boxplot(bodyfat$ABDOMEN)
qqnorm(bodyfat$ABDOMEN)
qqline(bodyfat$ABDOMEN)
plot(bodyfat$ABDOMEN)

#plot(lm.lasso)
#plot(lm.backward)
#plot(lm.lasso.step)
#plot(lm.full)

par(mfrow = c(1, 1))
BD <- lm(BODYFAT~DENSITY, data = bodyfat)
plot(BD, which = 1)
plot(BODYFAT, 1/DENSITY)



