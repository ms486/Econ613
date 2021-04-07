#=============================
# examples
# GMM and MM
#=============================
library(ggplot2)
library(moments)
library(tidyverse)

#========================================
# estimate the mean of a distribution
#========================================

set.seed(123)
xx = runif(10000,2,3)

# suppose I observe a realization of a random distribution
# I want to find parameters to match the realization to a normal distribution
# moment estimator

mean(xx)
sd(xx)

xx1 = rnorm(10000,mean(xx),sd(xx))

df = data.frame(data=xx,sim=xx1) %>% gather()
df

table(df$key)

pf = ggplot(df,aes(value,fill=key)) + geom_density(alpha=0.6)
pf

#==========================================
# fit is not good 
# use a mixture of normals
#==========================================
set.seed(123)
mom = all.moments(xx,order.max=5)
mom[-1]

# method of moments with simulations here
# no variance/covariance matrix
mm_sim = function(param,emp_mom)
{
  nsim    = 50
  sim_mom = mat.or.vec(5,nsim)
  for (iS in 1:nsim)
  {
  draw1    = rnorm(1000,param[1],exp(param[2]))
  draw2    = rnorm(1000,param[3],exp(param[4]))
  lambda   = exp(param[5])/(1+exp(param[5]))
  # form the mixture
  draw         = lambda*draw1 + (1-lambda)*draw2
  # calculate the moments
  sim_mom[,iS] = all.moments(draw,order.max = 5)[-1]
  }
  sim_mom = apply(sim_mom,1,mean)
  like = sum((sim_mom - emp_mom)^2)
  return(like);
}

start = c(3.7524794,-1.1654526,0.5193245,-5.2488761,0.4979038)
res  = optim(start,fn=mm_sim,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),emp_mom=mom[-1])

param    = res$par
draw1    = rnorm(1000,param[1],exp(param[2]))
draw2    = rnorm(1000,param[3],exp(param[4]))
lambda   = exp(param[5])/(1+exp(param[5]))
# form the mixture
draw         = lambda*draw1 + (1-lambda)*draw2

df = data.frame(data=xx,sim=draw) %>% gather()

pf = ggplot(df,aes(value,fill=key)) + geom_density(alpha=0.6)
pf

mom = all.moments(draw,order.max=5)
mom[-1]

mom = all.moments(xx,order.max=5)
mom[-1]

#============================================
# including the matrix of variance covariance
#============================================

# method of moments with simulations here
# calculate the variance of the moments by boostrap.
nboot   = 49
mom_mat = mat.or.vec(5,nboot)
for (iN in 1:nboot)
{
  xs           = sample(xx,10000,replace=T)
  mom          = all.moments(xs,order.max=5)
  mom_mat[,iN] = mom[-1]
}

vs = apply(mom_mat,1,var)

mm_sim = function(param,emp_mom,vs)
{
  nsim    = 50
  sim_mom = mat.or.vec(5,nsim)
  for (iS in 1:nsim)
  {
    draw1    = rnorm(1000,param[1],exp(param[2]))
    draw2    = rnorm(1000,param[3],exp(param[4]))
    lambda   = exp(param[5])/(1+exp(param[5]))
    # form the mixture
    draw         = lambda*draw1 + (1-lambda)*draw2
    # calculate the moments
    sim_mom[,iS] = all.moments(draw,order.max = 5)[-1]
  }
  sim_mom = apply(sim_mom,1,mean)
  crit    = (sim_mom - emp_mom)*(1/vs)*(sim_mom - emp_mom)
  return(sum(crit));
}

start = c(-0.2029413,0.6773643,2.4927606,-1.4250393,-5.7520823)
res  = optim(start,fn=mm_sim,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),emp_mom=mom[-1],vs=vs)

param    = res$par
draw1    = rnorm(1000,param[1],exp(param[2]))
draw2    = rnorm(1000,param[3],exp(param[4]))
lambda   = exp(param[5])/(1+exp(param[5]))
# form the mixture
draw3     = lambda*draw1 + (1-lambda)*draw2

df = data.frame(data=xx,sim_ncov=draw,sim_cov=draw3) %>% gather()
df

pf = ggplot(df,aes(value,fill=key)) + geom_density(alpha=0.6)
pf

#============================================
# linear model 
#============================================
