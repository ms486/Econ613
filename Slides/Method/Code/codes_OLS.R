
library(quantreg)
library(AER)
library(texreg)

set.seed(123)
nobs  = 100
const = 2
par   = 0.1
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
summary(lm(ysim~xvec))
summary(rq(ysim~xvec))


#========================
# crime in chicago
#========================

data("Guns")

## visualization
library("lattice")
xyplot(log(violent) ~ as.numeric(as.character(year)) | state, data = Guns, type = "l")

## Stock & Watson (2007), Empirical Exercise 10.1, pp. 376--377
fm1 <- lm(log(violent) ~ law, data = Guns)
summary(fm1)

fm2 <- lm(log(violent) ~ law + prisoners + density + income + 
            population + afam + cauc + male, data = Guns)
summary(fm2)

fm3 <- lm(log(violent) ~ law + prisoners + density + income + 
            population + afam + cauc + male + state, data = Guns)
summary(fm3)

fm4 <- lm(log(violent) ~ law + prisoners + density + income + 
            population + afam + cauc + male + state + year, data = Guns)
summary(fm4)

texreg(list(fm1,fm2,fm3,fm4))

#========================
# ols
#========================
set.seed(123)
const = 2
par   = 0.1

nobs  = 10
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
reg1  = lm(ysim~xvec)
summary(reg1)

nobs  = 100
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
reg2  = lm(ysim~xvec)
summary(reg2)

nobs  = 1000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
reg3  = lm(ysim~xvec)
summary(reg3)

nobs  = 10000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
reg4  = lm(ysim~xvec)
summary(reg4)

nobs  = 100000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + rnorm(nobs)
reg5  = lm(ysim~xvec)
summary(reg5)

texreg(list(reg1,reg2,reg3,reg4,reg5),model.name = c("n=10","n=100","n=1000","n=10000","n=100000"))

#========================
# ols - misspecification
#========================
set.seed(123)
const = 2
par   = 0.1

nobs  = 10
xvec  = rnorm(nobs)
ysim  = const + par*xvec + runif(nobs,0,5)
reg1  = lm(ysim~xvec)
summary(reg1)

nobs  = 100
xvec  = rnorm(nobs)
ysim  = const + par*xvec + runif(nobs,0,5)
reg2  = lm(ysim~xvec)
summary(reg2)

nobs  = 1000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + runif(nobs,0,5)
reg3  = lm(ysim~xvec)
summary(reg3)

nobs  = 10000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + runif(nobs,0,5)
reg4  = lm(ysim~xvec)
summary(reg4)

nobs  = 100000
xvec  = rnorm(nobs)
ysim  = const + par*xvec + runif(nobs,0,5)
reg5  = lm(ysim~xvec)
summary(reg5)

texreg(list(reg1,reg2,reg3,reg4,reg5),model.name = c("n=10","n=100","n=1000","n=10000","n=100000"))

#========================
# ols - misspecification (2)
#========================
set.seed(123)
const = 2
par   = 0.1

nobs  = 10
xvec  = rnorm(nobs)
ysim  = const + par*exp(xvec) + runif(nobs,0,5)
reg1  = lm(ysim~xvec)
summary(reg1)

nobs  = 100
xvec  = rnorm(nobs)
ysim  = const + par*exp(xvec) + runif(nobs,0,5)
reg2  = lm(ysim~xvec)
summary(reg2)

nobs  = 1000
xvec  = rnorm(nobs)
ysim  = const + par*exp(xvec) + runif(nobs,0,5)
reg3  = lm(ysim~xvec)
summary(reg3)

nobs  = 10000
xvec  = rnorm(nobs)
ysim  = const + par*exp(xvec) + runif(nobs,0,5)
reg4  = lm(ysim~xvec)
summary(reg4)

nobs  = 100000
xvec  = rnorm(nobs)
ysim  = const + par*exp(xvec) + runif(nobs,0,5)
reg5  = lm(ysim~xvec)
summary(reg5)

texreg(list(reg1,reg2,reg3,reg4,reg5),model.name = c("n=10","n=100","n=1000","n=10000","n=100000"))

