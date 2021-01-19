
set.seed(123)

nind = 1000
nper = 10

xMat = matrix(runif(nind*nper,0,10),nind,nper)
eps  = matrix(rnorm(nind*nper,0,.1),nind,nper)
beta = runif(1)
fixE = runif(nind,0,0.25)
yMat = mat.or.vec(nind,nper)

for (i in 1:nper)
{
  yMat[,i] = fixE + beta*xMat[,i] + eps[,i]
}

reg1 = lm(c(yMat)~c(xMat))
summary(reg1)
texreg::texreg(reg1)

qplot(0.13+0.087*c(xMat),c(yMat),xlab = "Fitted Values",ylab="True Values")
#=============================
# ff
#=============================

xMat = matrix(runif(nind*nper,0,10),nind,nper)
eps  = matrix(rnorm(nind*nper,0,1),nind,nper)
beta = runif(1)
fixE = runif(nind,-10,10)
yMat = mat.or.vec(nind,nper)

for (i in 1:nper)
{
  yMat[,i] = fixE + beta*xMat[,i] + eps[,i]
}

reg2 = lm(c(yMat)~c(xMat))
summary(reg2)
texreg::texreg(reg2)


qplot(-0.25 + 0.39*c(xMat),c(yMat),xlab = "Fitted Values",ylab="True Values")

#=============================
# ff
#=============================

xMat = matrix(runif(nind*nper,0,10),nind,nper)
eps  = matrix(rnorm(nind*nper,0,1),nind,nper)
beta = runif(1)
fixE = runif(nind,-10,10)
yMat = mat.or.vec(nind,nper)

for (i in 1:nper)
{
  yMat[,i] = fixE + beta*xMat[,i] + eps[,i]
}

for (i in 1:nind)
{
  yMat[i,] 
}


reg2 = lm(c(yMat)~c(xMat))
summary(reg1)


qplot(0.93*c(xMat),c(yMat))
