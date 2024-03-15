#MLR model1

iqsize <- read.table("iqsize.txt", header=T)
attach(iqsize)

pairs(cbind(PIQ, Brain, Height, Weight))

model <- lm(PIQ ~ Brain + Height + Weight)
summary(model)

anova(model) 


model <- lm(PIQ ~ Brain + Height)

summary(model)

anova(model)

confint(model) #95% confidence

predict(model, interval="confidence", se.fit=T,
        newdata=data.frame(Brain=90, Height=70))

predict(model, interval="confidence", se.fit=T,
        newdata=data.frame(Brain=79, Height=62))

predict(model, interval="prediction",
        newdata=data.frame(Brain=90, Height=70))

plot(x=fitted(model), y=residuals(model),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=Brain, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=Height, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

hist(residuals(model), main="")

qqnorm(residuals(model), main="", datax=TRUE)
qqline(residuals(model), datax=TRUE)

plot(x=Weight, y=residuals(model),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

detach(iqsize)

#MLR with Categorical Predictors
#Additive effect

birthsmokers <- read.table("birthsmokers.txt", header=T)
attach(birthsmokers)

pairs(cbind(Wgt, Gest, Smoke))

model <- lm(Wgt ~ Gest + Smoke)

plot(x=Gest, y=Wgt, ylim=c(2300, 3700), 
     col=ifelse(Smoke=="yes", "red", "blue"),
     panel.last = c(lines(sort(Gest[Smoke=="no"]),
                          fitted(model)[Smoke=="no"][order(Gest[Smoke=="no"])],
                          col="blue"),
                    lines(sort(Gest[Smoke=="yes"]),
                          fitted(model)[Smoke=="yes"][order(Gest[Smoke=="yes"])],
                          col="red")))

summary(model)

confint(model)


predict(model, interval="confidence",
        newdata=data.frame(Gest=c(38, 38), Smoke=c(1, 0)))


model.0 <- lm(Wgt ~ Gest, subset=Smoke==0)
summary(model.0)

predict(model.0, interval="confidence",
        newdata=data.frame(Gest=38))

model.1 <- lm(Wgt ~ Gest, subset=Smoke==1)
summary(model.1)

predict(model.1, interval="confidence",
        newdata=data.frame(Gest=38))

Smoke2 <- ifelse(Smoke=="yes", 1, -1)
model.3 <- lm(Wgt ~ Gest + Smoke2)
summary(model.3)

detach(birthsmokers)

#MLR with Categorical Predictors
#Interaction Effect
depression <- read.table("depression.txt", header=T)
attach(depression)

plot(x=age, y=y)
legend("topleft", col=1:3, pch=1,
       inset=0.02, x.intersp = 1.5, y.intersp = 1.8,
       legend=c("Trt A", "Trt B", "Trt C"))

age.x2 <- age*x2
age.x3 <- age*x3

model.1 <- lm(y ~ age + x2 + x3 + age.x2 + age.x3)
summary(model.1)

plot(x=age, y=y, ylim=c(20, 80), col=as.numeric(TRT),
     panel.last = c(lines(sort(age[TRT=="A"]),
                          fitted(model.1)[TRT=="A"][order(age[TRT=="A"])],
                          col=1),
                    lines(sort(age[TRT=="B"]),
                          fitted(model.1)[TRT=="B"][order(age[TRT=="B"])],
                          col=2),
                    lines(sort(age[TRT=="C"]),
                          fitted(model.1)[TRT=="C"][order(age[TRT=="C"])],
                          col=3)))
legend("topleft", col=1:3, pch=1, lty=1,
       inset=0.02, x.intersp = 1.5, y.intersp = 1.8,
       legend=c("Trt A", "Trt B", "Trt C"))

plot(x=fitted(model.1), y=rstudent(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

anova(model.1)

model.2 <- lm(y ~ age)
anova(model.2, model.1)

model.3 <- lm(y ~ age + x2 + x3)
anova(model.3, model.1)

detach(depression)

#Data Transformation -- Natural Logarithm
wordrecall <- read.table("wordrecall.txt", header=T)
attach(wordrecall)

model.1 <- lm(prop ~ time)
summary(model.1)

plot(x=time, y=prop, ylim=c(-0.1, 0.9),
     panel.last = lines(sort(time), fitted(model.1)[order(time)]))

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

lntime <- log(time)

model.2 <- lm(prop ~ lntime)
summary(model.2)

plot(x=lntime, y=prop,
     panel.last = lines(sort(lntime), fitted(model.2)[order(lntime)]))

plot(x=fitted(model.2), y=residuals(model.2),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.2), main="", datax=TRUE)
qqline(residuals(model.2), datax=TRUE)

prop1.25 <- prop^-1.25

model.3 <- lm(prop1.25 ~ time)
summary(model.3)

plot(x=time, y=prop1.25,
     panel.last = lines(sort(time), fitted(model.3)[order(time)]))

plot(x=fitted(model.3), y=residuals(model.3),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.3), main="", datax=TRUE)
qqline(residuals(model.3), datax=TRUE)

predict(model.2, interval="prediction",
        newdata=data.frame(lntime=log(1000)))

confint(model.2)[2,]*log(10) # 95% CI for 10-fold increase in time


detach(wordrecall)

#Stepwise Regression
cement <- read.table("cement.txt", header=T)
#cement<-cement[complete.cases(cement),]
attach(cement)

pairs(cement)

model.0 <- lm(y ~ 1)
add1(model.0, ~ x1 + x2 + x3 + x4, test="F")

model.4 <- lm(y ~ x4)
add1(model.4, ~ . + x1 + x2 + x3, test="F")

model.14 <- lm(y ~ x1 + x4)
drop1(model.14, ~ ., test="F")

add1(model.14, ~ . + x2 + x3, test="F")

model.124 <- lm(y ~ x1 + x2 + x4)
drop1(model.124, ~ ., test="F") 

model.12 <- lm(y ~ x1 + x2)
add1(model.12, ~ . + x3 + x4, test="F")

summary(model.12)

detach(cement)

#Best Subsets Regression
cement <- read.table("cement.txt", header=T)
attach(cement)

library(leaps)

subset <- regsubsets(y ~ x1 + x2 + x3 + x4, method="exhaustive", nbest=2, data=cement)
cbind(summary(subset)$outmat, round(summary(subset)$adjr2, 3), round(summary(subset)$cp, 1))


model.1234 <- lm(y ~ x1 + x2 + x3 + x4)
model.12 <- lm(y ~ x1 + x2)

SSE.k <- sum(residuals(model.12)^2) # SSE_k = 57.90448
MSE.all <- summary(model.1234)$sigma^2 # MSE_all = 5.982955
params <- summary(model.12)$df[1] # k+1 = 3
n <- sum(summary(model.1234)$df[1:2]) # n = 13
SSE.k/MSE.all + 2*params - n # Cp = 2.678242

model.14 <- lm(y ~ x1 + x4)

SSE.k <- sum(residuals(model.14)^2) # SSE_k = 74.76211
params <- summary(model.14)$df[1] # k+1 = 3
SSE.k/MSE.all + 2*params - n # Cp = 5.495851

model.124 <- lm(y ~ x1 + x2 + x4)
library(car)
vif(model.124)

model.123 <- lm(y ~ x1 + x2 + x3)
vif(model.123)

summary(model.12)

vif(model.12)

plot(x=fitted(model.12), y=rstandard(model.12),
     panel.last = abline(h=0, lty=2))

qqnorm(rstandard(model.12), main="", datax=TRUE)
qqline(rstandard(model.12), datax=TRUE)

detach(cement)
