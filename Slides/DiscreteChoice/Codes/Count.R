
library(AER)
library(stargazer)
library(xtable)
library(texreg)
library(MASS)

data("Affairs")

affairs = Affairs
names(affairs)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/Econ613/Slides/DiscreteChoice/Tables")
# 
# sink("desc.tex")
# stargazer(affairs)
# sink()

affairs$yaff = as.numeric(affairs$affairs>0)
names(affairs)
table(affairs$yaff)
prop.table(table(affairs$yaff))

est_linear = lm(affairs ~ as.factor(gender) + as.factor(education) + as.factor(age) + as.factor(children) ,data = affairs)
summary(est_linear)

est_probit4 = glm(yaff ~ as.factor(gender) + as.factor(education) + as.factor(age) + as.factor(children) , family = binomial(link = "probit"), 
                  data = affairs)

est_poisson = glm(affairs ~ as.factor(gender) + as.factor(education) + as.factor(age) + as.factor(children) ,data = affairs, family="poisson")

est_nb       = glm.nb(affairs ~ as.factor(gender) + as.factor(education) + as.factor(age) + as.factor(children) ,data = affairs)

sink("count.tex")
texreg(list(est_probit4,est_linear,est_poisson,est_nb), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()


