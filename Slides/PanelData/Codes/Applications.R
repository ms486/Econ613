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
