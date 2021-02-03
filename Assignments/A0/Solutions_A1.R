#=========================================================================
# A1
#=========================================================================

#=========================================================================
# Exercise 1: Introduction
#=========================================================================

# 2. list of packages then install
list_packages = c("Hmisc","gdata","boot","MASS","moments","snow","mvtnorm")
install.packages(list_packages)

# 3. set a working directory
setwd("C:/Users/ms486/Dropbox/Teaching/2020/Methods/Assignment/A0")

# 4. Content of the directory and environment
dir()
ls()

# 5. is 678 a multiple of 9
678%%9==0

# 6.
save.image()

# 7.
?mean
?cut2    # may not work if the package that contains cut2 is not loaded
??cut2

#8. An operation that returns NaN
log(-1)

#=========================================================================
# Exercise 2: Titanic
#=========================================================================

#1 - using sum
# find the names of each dimension of the array
dimnames(Titanic)
#a # Total population
sum(Titanic)
#b # Total adults
sum(Titanic[,,"Adult",])
#c Total crew
sum(Titanic["Crew",,,])
#d 3rd-class children
sum(Titanic["3rd",,"Child",])
#e # 2nd class adult female
sum(Titanic["2nd","Female","Adult",])
#f # 1st class children male
sum(Titanic["1st","Male","Child",])
#g # Female crew survivor
sum(Titanic["Crew","Female",,"Yes"])
#h #1st class male adult survivor
sum(Titanic["1st","Male", ,"Yes"])



# 2 
#a #Proportion of survivors among first class male adult
prop.table(Titanic["1st","Male","Adult",])
#b #Proportion of survivors among first class female adult
prop.table(Titanic["1st","Female","Adult",])
#c #Proportion of survivors among first class male children
prop.table(Titanic["1st","Male","Child",])
#d #Proportion of survivors among third class female adult
prop.table(Titanic["3rd","Female","Adult",])



#=========================================================================
# Exercise 3: Vectors
#=========================================================================

#1.a 
1:50
seq(1,50,by=1)
rev(50:1)

#1.b
50:1
seq(50,1,by=-1)
rev(1:50)

#2.a
rep(c(10,19,7),15)

#2.b
rep(c(1,2,5,6),8)

#3
x = seq(3.1,6,by=0.1)
log(x)*sin(x)

#4
x = c(0:100)
#sampling without replacement
mean(sample(x, 90, replace=F))
#sampling with replacement
mean(sample(x, 90, replace=T))

#5a : avoiding a loop
a = 1:20
b = t(c(1:15))
sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b)))

#5b : looping only once
a    = 1:20
curr = 0
for (i in a) {
  b = t(c(1:i))
  curr = curr + sum(exp(sqrt(a))*log(a^5)/(5+exp(a%*%b)*cos(a)%*%sin(b)))
}

#6
x = seq(3,6,0.1)
exp(x)*cos(x)

#=============================================
# Exercise 4:  VECTORS - ADVANCED #
#=============================================

# create xVec and yVec 
xVec = sample(0:999,1000,replace = T)
yVec = sample(0:999,1000,replace = T)
#2a
zVec = yVec[-1]-xVec[-length(xVec)]
#2b
wVec = yVec[1:length(yVec)-1]/xVec[2:length(xVec)]
#2c
subX = xVec[xVec >= 200]
#2d
which(yVec >= 600)

#=============================================
# Exercise 5:  Matrix
#=============================================

A = matrix(c(1,1,3,5,2,6,-2,-1,-3), nrow = 3, ncol = 3, byrow = T)

# alternatively
# A = cbind(c(1,5,-2),c(1,2,-1),c(3,6,-3))

#1a
A^3 == 0
#1b
cbind(A,A[,1]+A[,3])
#1c
A[3,] = A[1,] + A[2,]
#1d
rowMeans(A)
colMeans(B)

#2
mat = matrix(c(2,1,3,10,1,1,1,6,1,3,2,13),nrow = 3,ncol = 4,byrow = T)

#3 Solve(A,b) to solve A(X) = b
solve(mat[1:3,1:3],mat[,4])


#===================================
# Exercice 6 FUNCTIONS #
#===================================

fun1 = function(a,n) {
  myVec = 1:n
  myA = a^myVec
  return(sum(myA/myVec))
}

fun2 = function(x) {
  if (x<0) {
    return(x^2 + 2*x + abs(x))
  } else if (0 <= x & x < 2) {
    return(x^2 + 3 + log(1+x))
  } else {
    return(x^2 + 4*x - 14)
  }
}

fun2(-3) #6
fun2(0) #3
fun2(3) #7

#===================================
# Exercice 7 INDEXES #
#===================================
#1
v1 = sample(c(1:20),36,replace = T)
#2
x[2:length(x)]
x[-1]
#3
v2 = v1>5
as.integer(v2)
#4
m1 = matrix(v1,nrow=6,ncol=6,byrow=T)
#5
x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
#6
condition = is.na(x) + is.infinite(x)
x[!condition]

#===================================
# Exercise 8 : DATA MANIPULATION 
#===================================
#1
library(AER)
data("GSOEP9402", package = "AER")
dat = GSOEP9402
#2
typeof(dat)  # list
class(dat)   # data.frame
nrow(dat)    # 675
ncol(dat)    # 12
colnames(dat)
# school, birthyear, gender, kids, parity, income, size, state, marital, meducation, memployment, year
#3
require(dplyr)
require(ggplot2)

ggplot(dat %>% group_by(year) %>% summarize(mean_income = mean(income)) , aes(x=year,y=mean_income)) + geom_line() + ylab("Average Income")

#4
gender = dat %>% group_by(gender) %>% summarize(mean_income=mean(income))
school = dat %>% group_by(school) %>% summarize(mean_income=mean(income))
memployment = dat %>% group_by(memployment) %>% summarize(mean_income=mean(income))
incomes = c('male-female'=gender$mean_income[[1]]-gender$mean_income[[2]], 'gymnasium-hauptschule'=school$mean_income[[3]]-school$mean_income[[1]], 'realschule-hauptschule'=school$mean_income[[2]]-school$mean_income[[1]],'none-fulltime'=memployment$mean_income[[3]]-memployment$mean_income[[1]], 'none-parttime'= memployment$mean_income[[2]]-memployment$mean_income[[1]])

#===================================
# Exercise 9: FIRST REGRESSION #
#===================================

#1
data("CASchools", package = "AER")
data=CASchools
#2
data$school= factor(data$school)
data$district= factor(data$district)

reg1 = lm(read ~ .-school, data)
summary(reg1)

#3
formula = y~x.lm(formula)
reg2 = lm(read ~ .-school, data[1:200,])
summary(reg2)

# note that sd are not calculated. this regression does not make much sense..

#===================================
# Exercise 10 ADVANCED INDEXING #
#===================================

#1
require(actuar)
lu = rpareto(200, 1, 1)  
length(lu[lu>10]) #17
lu[which(lu>10)] = rlogis(length(lu[lu>10]),6.5,0.5)
#2
require(truncnorm)
de = rnorm(200,1,2)
de = log(de)
de[which(is.nan(de))] = rtruncnorm(length(which(is.nan(de))))
de[which(de<0)] = rtruncnorm(length(which(de<0)), a=0)
#3
orig = runif(200,0,1)
dest = runif(200,0,1)
#4
hist = matrix(runif(200*200,0,1),nrow=200,ncol=200)
dist = matrix(runif(200*200,0,1),nrow=200,ncol=200)
#5,6
int = outer(orig, dest, "+")+dist
su = log(int)/(1+log(int))
se = exp(int)/(1+exp(int))
#7
r = 0.05
getQ = function(w) {
  frac = outer(r+de, r+de, "/") 
  one = frac * w
  two = lu * log(w)
  three = lu * (1+log(w))
  mid = outer(two,three,"-")
  four = frac * sum(su) - sum(su)
  five = frac * sum(se) - sum(se)
  return(frac * w + mid + four + five)
}
getQ(9245)
#8
require(pracma)
gridw = seq(9100,55240,50)
#9
system.time(sapply(gridw, FUN=getQ, simplify = F))


###########################
# [11] TESTS AND INDEXING #
###########################
#1 
vec = c(1,2,3)
is.array(vec) #FALSE
is.vector(vec) #TRUE
is.matrix(vec) #FALSE
#2
x0=rnorm(1000)
table(x0>0)[[2]]
table(x0>1)[[2]]
table(x0>2)[[2]]
table(x0>0.5)[[2]]
table(x0<0)[[2]]
table(x0>-1)[[2]]
#3
require(Hmisc)
x1 = cut2(runif(100,0,1),g=10)
levels(x1)= paste("q",1:10,sep = "")
#4
is.factor(x1) #TRUE
#5
table(x1=="q1")[[2]] == 10 
#6
as.numeric(x1) #the levels get converted into integers
#7
rand=rnorm(1000)
#8
which(rand>0)
#9
w = rand[which(rand>0)]
w = subset(rand,rand>0)
w = rand[rand>0]

####################
# [12] PROGRAMMING #
####################
#recursion
u = function(N) {
  if (N==0|N==1) {return(1)}
  return(u(N-1)+u(N-2))
}
#1
sum(c(1:400)^2)
#2
sum(c(1:249) * c(2:250))
#3
crra = function(c,theta) {
  op = c^(1-theta)/(1-theta)
  if (0.97 <= theta & theta <=1.03) {return(log(op))}
  return(op)
}
#4
fact = function(n) {
  if (n==0|n==1) {return(1)}
  return(prod(n)) 
}

#######################
# [13] APPLY FUNCTIONS#
#######################
#1
m = matrix(c(rnorm(20,0,10),rnorm(20,-1,10)),nrow=20,ncol=2)
#mean by row
apply(m, MARGIN=1, FUN=mean)
#mean by col
apply(m, MARGIN=2, FUN=mean)
#min by row
apply(m, MARGIN=1, FUN=min)
#min by col
apply(m, MARGIN=2, FUN=min)
#max by row
apply(m, MARGIN=1, FUN=max)
#max by col
apply(m, MARGIN=2, FUN=max)
#sd by row
apply(m, MARGIN=1, FUN=sd)
#sd by col
apply(m, MARGIN=2, FUN=sd)
#2
library(datasets)
data(iris)
iris %>% group_by(Species) %>% summarise(mean_sepal_length = mean(Sepal.Length))
tibble = iris %>% group_by(Species) %>% summarise(mean_sepal_width = mean(Sepal.Width))
sum(log(tibble$mean_sepal_width))
#3a
y1=NULL; for (i in 1:100) {y1[i]=exp(i)}
exp(1:100)
sapply(1:100,exp)
#3b
y1=NULL; system.time(for (i in 1:100) {y1[i]=exp(i)})
system.time(exp(1:100))
system.time(sapply(1:100,exp))

#################################
# [14] SIMULATING AND COMPUTING #
#################################
#1
x = rnorm(10000)
summary(x)
#2
dsummary = function(vec) { 
  min = summary(vec)[[1]]
  dec1 = quantile(vec,prob=c(0.1,0.9))[[1]]
  qt1 = summary(vec)[[2]]
  med = summary(vec)[[3]]
  mean = summary(vec)[[4]]
  qt3 = summary(vec)[[5]]
  dec9 = quantile(vec,prob=c(0.1,0.9))[[2]]
  max = summary(vec)[[6]]
  return(c(min,dec1,qt1,med,mean,qt3,dec9,max))
}
#3
dnorm(0.5, mean=2,sd=0.05)
pnorm(2.5,mean=2,sd=0.05)
qnorm(0.95,mean=2,sd=0.05)
#4
dt(0.5, df=5)
pt(2.5, df=5)
qt(0.95, df=5)
#5
dpareto(0.5, 3,1)
ppareto(2.5, 3,1)
qpareto(0.95, 3,1)

################
# [15] MOMENTS #
################
V = rnorm(100,-2,5)
#1
n = length(V)
#2
mean(V)
#3
var(V)
#4
require(moments)
skewness(V)
#5
kurtosis(V)

############
# [16] OLS #
############
#1

# number of individuals
ni = 1000
# number of variables
nvar = 10

X              = matrix(,nrow=1000,ncol=10)
dat            = rbeta(1000*10, 2,1)
X[1:1000,1:10] = dat
length(X[X<0]) == 0
#2
sigmasq = 0.5
beta    = rgamma(10,2,1)
#3
eps     = rnorm(1000)
#4
Y = X%*% beta + sqrt(sigmasq)*eps
#5
solve(t(X)%*%X)%*%t(X)%*%Y
#6
Yhat = X %*% beta
epsHat = Yhat - Y
hist(epsHat, col="gray")
plot(density(epsHat))
#7
s = t(epsHat) %*% epsHat / (1000-10-1)
v = s[1,1] * solve(t(X)%*%X)
#8
param = cbind(beta,sqrt(v)) 
fit0 = lm(Y~0+X)
summary(fit0)
#9
confint(fit0)

#10
sigmasq = 0.01
Y = X%*% beta + sqrt(sigmasq)*eps
fit1 = lm(Y~0+X)
confint(fit1)

