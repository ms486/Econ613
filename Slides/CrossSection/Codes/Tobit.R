#=======================
# simulation 
# following cameron triverdi 16.2
#=======================


#=============================
# case 1
#=============================

set.seed(123)
lw    = rnorm(1000,2.75,(0.6^2))
eps   = rnorm(1000,0,10000)
ystar = -2500 + 1000*lw + eps

ystar_c = ystar
ystar_c[ystar_c<0] = 0

indext = which(ystar>0)

# uncensored mean
lm1 = lm(ystar~lw)
# censored mean
lm2 = lm(ystar_c~lw)
# truncated mean
lm3 = lm(ystar[indext]~lw[indext])

texreg::texreg(list(lm1,lm2,lm3))

#=============================
# case 2
#=============================

set.seed(123)
lw    = rnorm(1000,2.75,(0.6^2))
eps   = rnorm(1000,0,1)
ystar = -2500 + 1000*lw + eps

ystar_c = ystar
ystar_c[ystar_c<0] = 0

indext = which(ystar>0)

# uncensored mean
lm1 = lm(ystar~lw)
# censored mean
lm2 = lm(ystar_c~lw)
# truncated mean
lm3 = lm(ystar[indext]~lw[indext])

texreg::texreg(list(lm1,lm2,lm3))

#=============================
# case 2
#=============================

set.seed(123)
lw    = rnorm(1000,2.75,(0.6^2))
eps   = rnorm(1000,0,10)
ystar = -25 +10*lw + eps

ystar_c = ystar
ystar_c[ystar_c<0] = 0

indext = which(ystar>0)

# uncensored mean
lm1 = lm(ystar~lw)
# censored mean
lm2 = lm(ystar_c~lw)
# truncated mean
lm3 = lm(ystar[indext]~lw[indext])

texreg::texreg(list(lm1,lm2,lm3))

dat = data.frame(ystar = ystar,lnw = lw, ystarc = ystar_c,
                 ystart = c(ystar[indext],rep(NA,1000-length(indext))),
                 lnwt = c(lw[indext],rep(NA,1000-length(indext))))

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/Econ613/Slides/DiscreteChoice/Plot")

pp1 = ggplot(dat,aes(lnw,ystar)) + geom_point()
pp2 = pp1 + geom_smooth(aes(lnw,ystar),method="lm")
pp3 = pp2 + geom_smooth(aes(lnw,ystarc),method="lm")
pp4 = pp3 + geom_smooth(aes(lnwt,ystart),method="lm")
pp4

ggsave("scatter1.pdf",width = 15, height = 15, units = "cm",pp1)
ggsave("scatter2.pdf",width = 15, height = 15, units = "cm",pp2)
ggsave("scatter3.pdf",width = 15, height = 15, units = "cm",pp3)
ggsave("scatter4.pdf",width = 15, height = 15, units = "cm",pp4)

