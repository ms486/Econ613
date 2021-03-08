#======================
# libraries
#======================
library(tidyverse)
library(data.table)
library(stringr)
library(mfx)

#===================
# paths
#===================

mainpath = "~/Dropbox/Teaching/2020/Methods/Econ613/Assignments/A1"
datapath = "/dat"

#===================
# open the datasets
#===================

datstu = fread(paste0(mainpath,datapath,"/datstu.csv"))
datjss = fread(paste0(mainpath,datapath,"/datjss.csv"))
datsss = fread(paste0(mainpath,datapath,"/datsss.csv"))

names(datsss)
names(datjss)
names(datstu)

#==========================================
# exercise 1
#==========================================


#================================
# number of students
#================================

nrow(datstu)

#================================
# Number of schools
#================================

names(datsss)
set_unique = datsss %>% dplyr::select(schoolname,schoolcode) %>% distinct()
nrow(set_unique)

#================================
# Number of choices (school,program)
#================================

names(datstu)
# first, I create a choice variables
datstu = datstu %>% mutate(choice1=paste0(schoolcode1,choicepgm1),
                           choice2=paste0(schoolcode2,choicepgm2),
                           choice3=paste0(schoolcode3,choicepgm3),
                           choice4=paste0(schoolcode4,choicepgm4),
                           choice5=paste0(schoolcode5,choicepgm5),
                           choice6=paste0(schoolcode6,choicepgm6))

# then I am creating a vector for all choices
dat_choice = datstu %>% dplyr::select(choice1,choice2,choice3,choice4,choice5,choice6) %>% gather('key','value') %>% dplyr::select(value) %>% distinct()
length(unique(dat_choice$value))

#================================
# Number of missing score
#================================

table(is.na(datstu$score))

#================================
# apply to the same school
#================================

# first I create a subset of the data with schools only - then I count the number of unique by individuals
dat_school = data.matrix(datstu %>% dplyr::select(contains('schoolcode')))
ndistinct  = apply(dat_school,1,function(x)length(unique(x)))
table(ndistinct)

# 207,155 students apply to 6 disctinct choices.. then 340823 - 207155 apply to the same school..
# 88,154 apply to one school twice...
# 195 apply to a single school

#================================
# apply to less than 6 choices
# first i create a dataset with schoolcode and program choices.. then I drop the missing and count the remaining students
#===============================

dat_choice = datstu %>% dplyr::select(contains('schoolcode')|contains('choicepgm')) %>% drop_na()
dat_choice

# 323089 out of 340 823 students do not have any missing


#=====================#=====================
# exercise 2
#=====================#=====================

dat_choice = datstu %>% dplyr::select(choice1,choice2,choice3,choice4,choice5,choice6) %>% gather('key','choice') %>% dplyr::select(choice)
dat_choice = dat_choice %>% mutate(schoolcode= gsub('\\D','', choice),pgm=gsub('\\d','', choice)) %>% distinct()
dim(dat_choice)

# create a code for matching.. thats better..
lev        = dat_choice$choice
dat_choice = dat_choice %>% mutate(ch = as.numeric(factor(choice,levels=lev)))
dat_choice

#========================================================================
# I will match with the school data to find the district 
#========================================================================

datsss_work = datsss %>% dplyr::select(schoolcode,sssdistrict,ssslong,ssslat) %>% mutate(schoolcode=as.character(schoolcode)) %>% drop_na() %>% distinct()
dat_choice  = dat_choice %>% left_join(datsss_work)
dat_choice
dim(dat_choice)

#===================================================
# need to create a sub data for assigned students
#===================================================

dat_sub = datstu %>% dplyr::select(score,choice1,choice2,choice3,choice4,choice5,choice6,rankplace) %>% filter(rankplace<7)
dat_sub = dat_sub %>% mutate(choice = ifelse(rankplace==1,choice1,
                                                  ifelse(rankplace==2,choice2,
                                                         ifelse(rankplace==3,choice3,
                                                                ifelse(rankplace==4,choice4,
                                                                       ifelse(rankplace==5,choice5,choice6))))))
dat_sub = dat_sub %>% dplyr::select(choice,score) %>% group_by(choice) %>% summarise(cutoff=min(score),quality=mean(score),size=n())
dat_sub

dat_choice = dat_choice %>% left_join(dat_sub)
dat_choice

#==========================================
# exercise 3: Distance
#==========================================

dat_student = datstu %>% left_join(datjss %>% dplyr::select(-1)) %>% mutate(ch1 = as.numeric(factor(choice1,levels=lev)),
                                                                     ch2 = as.numeric(factor(choice2,levels=lev)),
                                                                     ch3 = as.numeric(factor(choice3,levels=lev)),
                                                                     ch4 = as.numeric(factor(choice4,levels=lev)),
                                                                     ch5 = as.numeric(factor(choice5,levels=lev)),
                                                                     ch6 = as.numeric(factor(choice6,levels=lev)))
dat_student

# distance for each choice...
dat_temp  = dat_student %>% dplyr::select(ch1,ch2,ch3,ch4,ch5,ch6,point_x,point_y,score) 
dat_sch   = dat_choice %>% dplyr::select(ch,ssslong,ssslat,cutoff,quality)

#==========================================
# combining with exercise 4
#==========================================


dat_dist = NULL
for (iX in 1:6)
{
  # i select the coordination and rename choice iX into choice to merge it with the school data
  dd = dat_temp %>% dplyr::select(iX,7,8,9) %>% rename(ch = 1) 
  dd = dd %>% left_join(dat_sch) %>% mutate(dist=sqrt((69.172*(ssslong-point_x)*cos(point_y/57.3))^2+
                                                                   (69.172*(ssslat-point_y))^2))
  dat_dist = cbind(dat_dist,dd %>% dplyr::select(1,4,7,8,9))
  # then calculate distance 
}
colnames(dat_dist) = rep(paste0(rep(c("ch","score","cutoff","quality","dist"),6),rep(1:6,each=5)))
dat_dist

#==========================================
# exercise 4:  
#==========================================
datf = data.frame(dat_dist)
datf = datf %>% mutate(score_quant=cut_number(score1,n=4))
datf

table(datf$score_quant)

datf %>% dplyr::select(contains("cutoff")) %>% summarise_all(funs(mean,sd),na.rm=TRUE)
datf %>% dplyr::select(contains("cutoff"),score_quant) %>% group_by(score_quant) %>% summarise_all(funs(mean,sd),na.rm=TRUE)

datf %>% dplyr::select(contains("quality")) %>% summarise_all(funs(mean,sd),na.rm=TRUE)
datf %>% dplyr::select(contains("quality"),score_quant) %>% group_by(score_quant) %>% summarise_all(funs(mean,sd),na.rm=TRUE)

datf %>% dplyr::select(contains("dist")) %>% summarise_all(funs(mean,sd),na.rm=TRUE)
datf %>% dplyr::select(contains("dist"),score_quant) %>% group_by(score_quant) %>% summarise_all(funs(mean,sd),na.rm=TRUE)


#==========================================
# part 2
# exercise 5:
#==========================================
set.seed(123)
ndraw = 10000
x1    = runif(ndraw,1,3)
x2    = rgamma(ndraw,scale=2,shape=3)
x3    = rbinom(ndraw,size=1,prob=0.3)
eps   = rnorm(ndraw,2,1)

Y     = 0.5 + 1.2*x1 -0.9*x2 + 0.1*x3 + eps
ydum  = as.numeric(Y>mean(Y))
table(ydum)

#==========================================
# exercise 6:
#==========================================

# correlation
cor(Y,x1)

# x mat
X    = cbind(1,x1,x2,x3)
nvar = ncol(X)

# beta 
beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y
beta_hat

# standard errors
eps_hat     = Y - X %*% beta_hat
sigma_hat   = t(eps_hat) %*% eps_hat / (ndraw-nvar-1)
sigma_hat


vcov = sigma_hat[1,1] * solve(t(X)%*%X)
vcov

## Estimate of standard errors
sd_beta =sqrt(diag(vcov))
sd_beta

#============================
# check with lm
#============================
reg1 = lm(Y~X)
cbind(beta_hat,summary(reg1)$coefficients[,1])
cbind(sd_beta,summary(reg1)$coefficients[,2])


#==========================================
# exercise 7:
#==========================================

# the likelihood of a linear model is the product of densities

flike = function(par,x1,x2,x3,yvar,model)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  if (model=="plm")
  {
    xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
    like            = log(dnorm(yvar-xbeta))
  }
  if (model=="probit")
  {
  pr              = pnorm(xbeta) # probit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  }else 
  {
  pr              = exp(xbeta)/(1+exp(xbeta)) #logit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  }
  return(-sum(like))
}

# test if the function is correct
test_par = runif(4)
flike(test_par,x1,x2,x3,ydum,model="plm")
flike(test_par,x1,x2,x3,ydum,model="probit")
flike(test_par,x1,x2,x3,ydum,model="logit")

reg1 = glm(ydum~x1+x2+x3,family = binomial(link = "probit"))
test_par = reg1$coefficients
flike(test_par,x1,x2,x3,ydum,model="probit")
logLik(reg1)


# optimization
# recheck with different starting
# need to do it many times to make sure it converges
start       = runif(4,-10,10)
res_probit  = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=ydum,model="probit",hessian=TRUE)
res_logit   = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=ydum,model="logit",hessian=TRUE)
res_plm     = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=ydum,model="plm",hessian=TRUE)


#==========================================
# compute sd errors
#==========================================


fisher_info = solve(res_probit$hessian)       # standard formula is -res$hessian but flike is return -like
var_probit  = sqrt(diag(fisher_info))
var_probit


#==========================================
# marginal effects
#==========================================
#library(mfx)
#dd = data.frame(ydum=ydum,x1=x1,x2=x2,x3=x3)
#probitmfx(ydum~x1+x2+x3,data=dd,atmean = TRUE)

# at the compute marginal effect at the mean 
par = res_probit$par

margeff_eval = function(par,x1,x2,x3,yvar,model)
{
  xbeta   = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr      = ifelse(model=="probit",
                   dnorm(mean(xbeta)),dlogis(mean(xbeta)))
  marg            = par[-1]*pr
  return(marg)
}

#====================
# delta method 
#====================
# var(g(x)) = g'(E(x))^2 var(x)
# finite difference to evaluate g'(x)
heps = 0.00001
gprime = NULL
out = NULL
out_bench = margeff_eval(par,x1,x2,x3,yvar,"probit")
for (i in 2:length(par))
{
  parh = par
  parh[i] = par[i] + heps 
  res = margeff_eval(parh,x1,x2,x3,yvar,"probit")
  out[i-1] = res[i-1]
}

#==================
#derivative
#==================

gprime = (out - out_bench)/heps
gprime

#==================
# sds
#==================
var_marg = gprime*var_probit[-1]
var_marg

# checking 
dd = data.frame(ydum=ydum,x1=x1,x2=x2,x3=x3)
probitmfx(ydum~x1+x2+x3,data=dd,atmean = TRUE)

# close enough .. may need the second type of finite difference
tabf = cbind(out_bench,var_marg)
tabf

# checking 
dd = data.frame(ydum=ydum,x1=x1,x2=x2,x3=x3)
probitmfx(ydum~x1+x2+x3,data=dd,atmean = TRUE)

#====================
# boostrap version
#====================

margeff = function(start,x1,x2,x3,yvar,model)
{
  res  = optim(start,fn=flike,method="BFGS",control=list(maxit=1000),x1=x1,x2=x2,x3=x3,yvar=ydum,model=model,hessian=TRUE)
  par  = res$par
  xbeta   = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr      = ifelse(model=="probit",
                   dnorm(mean(xbeta)),dlogis(mean(xbeta)))
  marg            = par[-1]*pr
  return(marg)
}

margeff(par,x1,x2,x3,ydum,"probit")


nboot =99
ni   = length(x1)
parf = mat.or.vec(3,nboot)
for (i in 1:nboot)
{
indices = sample(1:ni,ni,replace=T)
parf[,i] = margeff(par,x1[indices],x2[indices],x3[indices],ydum[indices],"probit")
}
  
tabf = cbind(out_bench,apply(parf,1,sd))
tabf

# checking 
dd = data.frame(ydum=ydum,x1=x1,x2=x2,x3=x3)
probitmfx(ydum~x1+x2+x3,data=dd,atmean = TRUE)
