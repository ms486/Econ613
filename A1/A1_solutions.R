#================================================================
# Solution for assignment 1
#================================================================
rm(list=ls())

library(foreign)
library(weights)
library(ggplot2)
library(gridExtra)
library(reshape)
library(gdata)
library(Rmisc)
library(pROC)
library(grid)
library(readstata13)
library(xtable)
library(Rmisc)
library(dplyr)
library(nnet)
library(fastDummies)

datapath = "/dat"
modpath  = "/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/Econ613/A1"

options(xtable.floating = FALSE)

#========================================================
# load the data
#========================================================
datstu = read.csv(paste0(modpath,datapath,"/","datstu.csv"))
datjss = read.csv(paste0(modpath,datapath,"/","datjss.csv"))
datsss = read.csv(paste0(modpath,datapath,"/","datsss.csv"))


#========================================================
# Exercise 1 
#========================================================
str(datstu) # provide a good overview of the data 

#====================
#number of students 
#====================
nrow(datstu)

#====================
#number of schools
#====================
# here i find all the variables with schoolcode(grep(schoolcode)), vectorize it, 
# then find unique set of schools, and then length it 
# get rid of the missing observations
length(unique(c(as.matrix(datstu[,grep("schoolcode",names(datstu))])),na.rm=T))

#number of programs
length(unique(c(as.matrix(datstu[,grep("pgm",names(datstu))])),na.rm=T))

#number of choices
datstu$choice1  = paste0(datstu$schoolcode1,datstu$choicepgm1)
datstu$choice2  = paste0(datstu$schoolcode2,datstu$choicepgm2)
datstu$choice3  = paste0(datstu$schoolcode3,datstu$choicepgm3)
datstu$choice4  = paste0(datstu$schoolcode4,datstu$choicepgm4)
datstu$choice5  = paste0(datstu$schoolcode5,datstu$choicepgm5)
datstu$choice6  = paste0(datstu$schoolcode6,datstu$choicepgm6)

nc = ncol(datstu)
length(unique(c(as.matrix(datstu[,(nc-5):nc])),na.rm=T))

#missing test score
table(is.na(datstu$score))

#apply to the same school
schools = apply(as.matrix(datstu[,grep("schoolcode",names(datstu))]),1,function(x)length(unique((x))))
table(schools)

#apply to less than 6 choices
table(is.na(dat08$schoolcode1))
table(is.na(dat08$schoolcode2))
table(is.na(dat08$schoolcode3))
table(is.na(dat08$schoolcode4))
table(is.na(dat08$schoolcode5))
table(is.na(dat08$schoolcode6))


#========================================================
# Exercise 2
#========================================================

#====================================================================
# first, I create a database that records only choices
# then, i create the school where the student is admitted
#====================================================================

datchoice = datstu[,(nc-5):nc]
placement = NULL
for (iter in 1:nrow(datstu))
{
  if (!is.na(datstu$rankplace[iter])&datstu$rankplace[iter]<7)
  {
  placement[iter] = datchoice[iter,datstu$rankplace[iter]]
  }
}

#====================================================================
# Second, i create dataframe indicating for each student
# the test score and the school he got admitted to..
# then I cast that database into a dataframe indicating the cutoff and quality
# then I extract the first 6 characteristics of the choice to get the school code
#====================================================================

temp        = data.frame(placement,datstu$score)
names(temp) = c("school","score")
dat_prog  =  temp %>% 
            group_by(school) %>% 
            summarise(cutoff  = min(score,na.rm=T),
                      quality = mean(score,na.rm=T))

datfinal            = data.frame(dat_prog)
datfinal$schoolcode = substr(datfinal$school,1,6)
datfinal[1:10,]

#====================================================================
# Finally, i match this dataframe to sss by schoolcode
#====================================================================

datf          = merge(datfinal,datsss,by="schoolcode")
dat_aggregate = subset(datf,select=c("school","cutoff","quality","ssslong","ssslat"))


#======================================================================
# exercise 4/5
#======================================================================


merger = function(dat_ind,dat_agg,ind)
{
  nvar   = ncol(dat_agg) 
  fdat08 = merge(dat_ind,dat_agg,by.y="school",by.x=paste0("choice",ind),all.x=TRUE)
  nf     = ncol(fdat08)
  names(fdat08)[(nf-nvar+2):nf] = paste(names(fdat08)[(nf-nvar+2):nf],paste0("ch",ind),sep="_")
  return(fdat08)
}

dat_temp = subset(datstu,select=c("score","jssdistrict","choice1","choice2","choice3","choice4","choice5","choice6"))

dat1 = merger(dat_temp,dat_aggregate,1)
dat2 = merger(dat1,dat_aggregate,2)
dat3 = merger(dat2,dat_aggregate,3)
dat4 = merger(dat3,dat_aggregate,4)
dat5 = merger(dat4,dat_aggregate,5)
dat6 = merger(dat5,dat_aggregate,6)

#=====================
# merge with gps data
# for jss location
#=====================

datc = subset(datsss,select=c("sssdistrict","ssslong","ssslat"))
names(datc) = c("district","jsslong","jsslat")

datfinal = merge(dat6,datc,by.x="jssdistrict",by.y="district")

datfinal$dist_ch1 = sqrt((69.172*(datfinal$ssslong_ch1-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch1-datfinal$jsslat))^2)

datfinal$dist_ch2 = sqrt((69.172*(datfinal$ssslong_ch2-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch2-datfinal$jsslat))^2)

datfinal$dist_ch3 = sqrt((69.172*(datfinal$ssslong_ch3-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch3-datfinal$jsslat))^2)

datfinal$dist_ch4 = sqrt((69.172*(datfinal$ssslong_ch4-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch4-datfinal$jsslat))^2)

datfinal$dist_ch5 = sqrt((69.172*(datfinal$ssslong_ch5-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch5-datfinal$jsslat))^2)

datfinal$dist_ch6 = sqrt((69.172*(datfinal$ssslong_ch6-datfinal$jsslong)*cos(datfinal$jsslat/57.3))^2+
                           (69.172*(datfinal$ssslat_ch6-datfinal$jsslat))^2)


#===========================
# characteristics
#===========================

var = paste0(c("cutoff","quality","dist"),"_ch")

tabler = function(dat,number)
{
  dat1 = subset(dat,select=paste0(var,number)) 
  out  = c(apply(dat1[,1:3],2,function(x)c(mean(x),sd(x))))
  out
}


tabchoice = cbind(tabler(datfinal,1),tabler(datfinal,2),tabler(datfinal,3),tabler(datfinal,4),
                  tabler(datfinal,5),tabler(datfinal,6))



#=====================================
# how diversified are the portfolios
#=====================================
names(datfinal)[grep("cutoff",names(datfinal))]

deciles = quantile(dat$cutoff,probs=seq(0,1,by=0.25),na.rm=T)

dat_cut   = datfinal[,grep("cutoff",names(datfinal))]
for (i in 1:6)
{
  dat_cut[,i] = cut(dat_cut[,i],deciles)
}

tab11 = cbind(prop.table(table(dat_cut[,1])),
              prop.table(table(dat_cut[,2])),
              prop.table(table(dat_cut[,3])),
              prop.table(table(dat_cut[,4])),
              prop.table(table(dat_cut[,5])),
              prop.table(table(dat_cut[,6])))

tab11
