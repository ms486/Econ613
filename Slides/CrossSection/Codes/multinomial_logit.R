rm(list=ls())


#========================================================================================================
# examples of likelihood based estimation for multinomial choices
#========================================================================================================

library(snow)
library(nloptr)
library(boot)
library(data.table)

savepath = "/Tex/New"
datapath = "/Data"
modpathl  = "/home/ms486/Dropbox/papers/education/Ghana Structural/New"
modpath  = "/Users/ms486/Dropbox/papers/education/Ghana Structural/New"
codpath  = "/Codes/Descriptive"
estcpath  = "/Codes/Estim/Main"
outpath  = "/Codes/Output"
sourcepath = "/Users/ms486/Box/Schools"
tabout = "/tab_paper"

options(xtable.floating = FALSE)

if (dir.exists("/home/ms486/Dropbox")) {
  setwd(modpathl)
}else if (dir.exists("/Users/ms486/Dropbox")){
  setwd(modpath)
}else{
  setwd("/net/storage-01/econ/home/m/ms486/Schools")
}

mainpath = getwd()

#===============================================
# load the data
#===============================================

setwd(paste0(mainpath,datapath))
x1   = as.matrix(fread(paste0(mainpath,datapath,"/x1f.csv")))
x2   = as.matrix(fread(paste0(mainpath,datapath,"/x2f.csv")))
df   = as.matrix(fread(paste0(mainpath,datapath,"/df.csv")))

d1   = df[,1] 
d2   = df[,2] 

#========================================================
# estimation
# probit example
# bootstrap and clustering
#========================================================
npar  = ncol(x1)
param = runif(npar)

like =function(param, x1, x2, d1, d2){
  out         = pnorm((x1%*%param-d1)-(x2%*%param-d2));                        
  out[out==0] = 0.00001;
  return(-sum(log(out)));
}

like(param,x1,x2,d1,d2)
system.time(like(param,x1,x2,d1,d2))

lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar) 

res_probit_sub   = nloptr(start,eval_f=like, lb=lower,ub=upper,
                opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                x1=x1,x2=x2,d1=d1,d2=d2)

res_probit_bobyqa  = nloptr(start,eval_f=like, lb=lower,ub=upper,
                   opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                   x1=x1,x2=x2,d1=d1,d2=d2)

pp = cbind(res_probit_sub$solution,res_probit_bobyqa$solution)
cbind(res_probit_sub$objective,res_probit_bobyqa$objective)

#========================================
# standard error of the estimation
#========================================

out         = res_probit_bobyqa$solution
nboot       = 50
nq          = nrow(x1)
nn          = mat.or.vec(npar,nboot)

bt          = function(i,start,x1,x2,d1,d2)
{
  indices = sample(1:nq,nq,replace=T)
  res     = nloptr(start,eval_f=like, lb=lower,ub=upper,
                   opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                   x1=x1[indices,],x2=x2[indices,],d1=d1[indices],d2=d2[indices])  
   return(res$solution)  
}

ncores = 50;
cl     = makeCluster(ncores)
clusterExport(cl,c("like","x1","x2","d1","d2"))
clusterEvalQ(cl, library(nloptr)) 
parf = parSapply(cl, 1:nboot, bt,start=out,x1=x1,x2=x2,d1=d1,d2=d2)
stopCluster(cl)

save.image("out_probit.RDATA")

#===============================================
# load the data
#===============================================
dat_indiv   = fread(paste0(mainpath,datapath,"/dat_indiv.csv"))
dat_school  = fread(paste0(mainpath,datapath,"/dat_school.csv"))
dat_choices = fread(paste0(mainpath,datapath,"/dat_choices.csv"))


dat_indiv[1:10,]
dat_school[1:10,]
dat_choices[1:10,]

library(dplyr)
dat_est = dat_indiv %>% left_join(dat_choices) %>% dplyr::select(1:8,10) %>% rename("ch"=ch1) %>% left_join(dat_school)
dat_est = dat_est %>% mutate(choice=as.numeric(factor(ch,ordered=TRUE))) %>% drop_na()
df_est  = data.matrix(dat_est) 

#==========================================
# multinomial logit 
#==========================================

like_fun = function(param,dat_est,dat_school)
{
  avgscore08 =  dat_est$avgscore08
  male       =  dat_est$male
  jqual      =  dat_est$jqual
  colonial   =  dat_school$colonial
  board      =  dat_school$board
  ch         =  dat_school$choice
  ni = nrow(df_est)
  nj = length(unique(df_est[,32]))
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  pn3    = param[(2*nj+1):(3*nj)]
  pn4    = param[(3*nj+1):(4*nj)]
  
  for (j in 1:nj)
  {
    # conditional logit
    ut[,j] = param[1] + param[2]*avgscore08[j]+ param[3]*colonial[j] + param[4]*board[j]
    # conditional logit
    ut[,j] = param[1] + param[2]*avgscore08[j] + param[3]*colonial[j] + param[4]*male*jqual[j]
                      + param[5]*male*colonial[j];   
    # multinomial logit
    ut[,j] = male*pn1[j] + jqual*pn2[j] 
    ut[,j] = male*pn1[j] + jqual*pn2[j] + avgscore08[j]*male*pn3[j] + colonial[j]*jqual*pn4[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}


