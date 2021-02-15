#=====================================
# illustration of bootstrap
#=====================================

library(AER)

data("CPS1985")

reg = lm(log(wage) ~ gender*married + education + experience + I(experience^2),data = CPS1985)
summary(reg)


#=====================================
# first example of bootstrap..
#=====================================
R    = 999;                      # number of bootstrap
nind = nrow(CPS1985);            # number of individuals
nvar = length(reg$coefficients)  # number of variables

outs = mat.or.vec(R,nvar)
set.seed(123)

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = CPS1985[samp,]
  reg1     = lm(log(wage) ~ gender*married + education + experience + I(experience^2),data = dat_samp)
  outs[i,] = reg1$coefficients
}

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

est = cbind(summary(reg)$coefficients[,1],
            summary(reg)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")
est

#==========================================
# numerical gradient
#==========================================

fn = function(x)
{
  out = x*x + 5*x
  return(out)
}

dfn = function(x)
{
  out = 2*x+5
  return(out)
}

num_gradient = function(x)
{
  nx  = length(x)
  h   = 0.00001
  out = NULL
  ef1 =  fn(x+h)
  ef0 =  fn(x-h)
  dfn = (ef1-ef0)/(2*h)
  return(dfn)
}

xval = 2:10
fn(xval)  
dfn(xval)
num_gradient(xval)

