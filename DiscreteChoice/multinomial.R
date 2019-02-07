#=====================
library(bayesm)
library(mlogit)
library(stargazer)
#=====================

data("Car", package = "mlogit")

data(Car)
head(Car)

car = mlogit.data(Car, varying = 5:70, shape = "wide", sep = "",
                   choice = "choice", alt.levels = 1:6)
head(car)
names(car)

cars <- car %>% group_by(alt) %>% summarise_all(funs(mean))
cars

summary(car[,6:7])

stargazer(car[,c(2:5,8:ncol(car)-1)])

model01 <- mlogit(choice ~ type + fuel + acc, data = car)
model02 <- mlogit(choice ~ type + fuel + acc   + price + cost, data = car)
model03 <- mlogit(choice ~ type + fuel + price + cost + range + acc, data = car)
model04 <- mlogit(choice ~ type + fuel + price + cost + range + acc + speed + pollution, data = car)
model05 <- mlogit(choice ~ type + fuel + price + cost + range + acc + speed + pollution + size + space + station, data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("clogit.tex")
summary(model05)
sink()

## a pure "multinomial model"

model11 <- mlogit(choice ~ 0 | college, data = car)
model12 <- mlogit(choice ~ 0 | college + hsg2, data = car)
model13 <- mlogit(choice ~ 0 | college + hsg2 + coml5, data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("mlogit.tex")
summary(model13)
sink()

## mixed logit 
modelf <- mlogit(choice ~ type + fuel + price + cost + range + acc + speed + pollution + size + space + station | college + hsg2 + coml5, data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("mixedlogit.tex")
summary(modelf)
sink()

