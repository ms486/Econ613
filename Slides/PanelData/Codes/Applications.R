library(AER)
library(ggplot2)
library(texreg)

data("Guns")
names(Guns)
Guns$group = substr(Guns$state,1,1)
table(Guns$group)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/Econ613/Slides/PanelData/Plot")

pp = ggplot(data=Guns,aes(x=as.numeric(as.character(year)),y=log(violent))) + geom_point() + xlab("Year") + ylab("Violent Crime")
pp

pdf("Time.pdf")
pp
dev.off()

pdf("State.pdf")
pp = ggplot(data=Guns,aes(x=state,y=log(violent))) + geom_point() + ylab("Violent Crime") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pp
dev.off()

pdf("State_wrap.pdf")
pp = ggplot(data=Guns,aes(x=as.numeric(as.character(year)),y=log(violent))) + theme(axis.text = element_blank()) + geom_point() + xlab("Year") + ylab("Violent Crime") + facet_wrap(~state) 
pp
dev.off()


vm1 = lm(log(violent) ~ law, data = Guns)
coeftest(fm1, vcov = sandwich)

vm2 <- lm(log(violent) ~ law + prisoners + density + income +
            population + afam + cauc + male, data = Guns)
coeftest(fm2, vcov = sandwich)

rm1 = lm(log(robbery) ~ law, data = Guns)
coeftest(rm1, vcov = sandwich)

rm2 <- lm(log(robbery) ~ law + prisoners + density + income +
            population + afam + cauc + male, data = Guns)
coeftest(rm2, vcov = sandwich)


fm31 <- lm(log(violent) ~ law + prisoners + density + income +
            population + afam + cauc + male + state, data = Guns)
printCoefmat(coeftest(fm31, vcov = sandwich)[1:9,])

fm32 <- lm(log(violent) ~ law + prisoners + density + income +
            population + afam + cauc + male + year, data = Guns)
printCoefmat(coeftest(fm32, vcov = sandwich)[1:9,])

fm33 <- lm(log(violent) ~ law + prisoners + density + income +
            population + afam + cauc + male + state + year, data = Guns)
printCoefmat(coeftest(fm33, vcov = sandwich)[1:9,])

texreg(list(fm31,fm32,fm33))

#========================
# application 2
# empl UK
#========================

data("EmplUK", package="plm")
dat = EmplUK

stargazer(dat)

spec1 = lm(log(emp)~log(wage)+log(capital)+log(output),data=dat)
spec2 = lm(emp~wage+capital+output,data=dat)
spec4 = pggls(emp~wage+capital+as.factor(sector), data=EmplUK, model="pooling")

summary(spec2)
summary(spec1)
summary(spec3)
summary(spec4)

texreg::texreg(list(spec1,spec2))

spec4 = pggls(log(emp)~log(wage)+log(capital), data=EmplUK, model="within")
spec3 = pggls(log(emp)~log(wage)+log(capital), data=EmplUK, model="pooling")

summary(spec3)
summary(spec4)

stargazer::stargazer(spec3,spec4)

#========================
# application 3
# Us States production
#========================

data("Produc", package="plm")

formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

model1 = plm(formula, data=Produc, model="within")
model2 = plm(formula, data=Produc, model="between")
model3 = plm(formula, data=Produc, model="fd")

texreg::texreg(list(model1,model2,model3))


#========================
# application 4
# estimating fixed effects 
#========================

set.seed(123)

ni   = 10000
nt   = 10
nvar = 3

alph = runif(ni,-10,10)

ymat = mat.or.vec(ni,nt)
xmat = array(runif(ni*nt*nvar),c(ni,nt,nvar))
bt   = runif(3)

for (i in 1:nt)
{
  ymat[,i] = alph + bt[1]*xmat[,i,1] + bt[2]*xmat[,i,2] + bt[3]*xmat[,i,3] + rnorm(ni)
}
  
dat = data.frame(yy=c(ymat),ind=rep(1:ni,nt),time=rep(1:nt,ni),x1=c(xmat[,,1]),x2=c(xmat[,,2]),x3=c(xmat[,,3]))

reg1 = lm(yy~x1+factor(ind),data=dat)

