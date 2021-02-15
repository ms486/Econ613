set.seed(123)

# simulate independent variables
nobs = 10000
nvar = 3
x1   = runif(nobs)
x2   = runif(nobs)
x3   = runif(nobs)

true_par = runif(nvar+1) 
# simulate latent variable
yhat = true_par[1] + true_par[2]*x1 + true_par[3]*x2 + true_par[4]*x3 + rnorm(nobs);
# simulate yhat
yvar = as.numeric(yhat>0)

table(yvar)

#==========================
# probit in R 
#==========================

reg1 = glm(yvar~x1+x2+x3,family = binomial(link = "probit"))
summary(reg1)

#==========================
# Programming the likelihood
#==========================

flike = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
#  pr              = exp(beta)/(1+exp(beta)) logit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

# test if the function is correct
test_par = reg1$coefficients
flike(test_par,x1,x2,x3,yvar)
logLik(reg1)

ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
start    = runif(4,-10,10)
#res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
out[i0,] = res$par
}

#===========================================
start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)

fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est = cbind(true_par,summary(reg1)$coefficients[, 1],summary(reg1)$coefficients[, 2],res$par,prop_sigma)
colnames(est) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est

