#=====================
library(mlogit)
library(stargazer)
library(texreg)
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


#============================================================================================
# start with something simple to think about the interpretation of variables
#============================================================================================
model01 <- mlogit(choice ~ type + price + cost, data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/Econ613/DiscreteChoice")

sink("condlogit.tex")
texreg(model01)
sink()

#============================================================================================
# specification search and interpretation of interactions
#============================================================================================

model021 <- mlogit(choice ~ fuel + range + price, data = car)
model022 <- mlogit(choice ~ fuel + range + price + cost, data = car)
model023 <- mlogit(choice ~ fuel + range + I(price*cost), data = car)
model024 <- mlogit(choice ~ fuel + range + price + I(price*cost), data = car)
model025 <- mlogit(choice ~ fuel + range + cost + I(price*cost), data = car)
model026 <- mlogit(choice ~ fuel + range + price + cost + I(price*cost), data = car)

sink("condlogit2.tex")
texreg(list(model021,model022,model023,model024,model025,model026))
sink()

#============================================================================================
# specification search and the effect of size
#============================================================================================

model031 <- mlogit(choice ~ type + size, data = car)
model032 <- mlogit(choice ~ type + size + I(size^2) , data = car)
model033 <- mlogit(choice ~ type + as.factor(size), data = car)
model034 <- mlogit(choice ~ type + size*price, data = car)

sink("condlogit3.tex")
texreg(list(model031,model032,model033,model034))
sink()


#============================================================================================
# specification search and the effect of size
#============================================================================================

model081 <- mlogit(choice ~ price+cost+size, data = car)
model082 <- mlogit(choice ~ price+cost+size + I(size^2) , data = car)
model083 <- mlogit(choice ~ price+cost+as.factor(size), data = car)

sink("condlogit8.tex")
texreg(list(model081,model082,model083))
sink()

#============================================================================================
# Trying to kill off this effect
#============================================================================================

car$size1 = car$size + 1
car$size2 = car$size -10

model071 <- mlogit(choice ~ price+cost+size, data = car)
model072 <- mlogit(choice ~ price+cost+(size1), data = car)
model073 <- mlogit(choice ~ price+cost+(size2), data = car)
model074 <- mlogit(choice ~ price+cost+log(size1), data = car)
model075 <- mlogit(choice ~ price+cost+as.numeric(size>2), data = car)

sink("condlogit7.tex")
texreg(list(model071,model072,model073,model074,model075))
sink()

#============================================================================================
# specification search and interpretation of interactions with observed attributes
#============================================================================================

model041 <- mlogit(choice ~ fuel + price + cost +  range, data = car)
model042 <- mlogit(choice ~ fuel + price + cost +  range + I(range*coml5) , data = car)
model043 <- mlogit(choice ~ fuel + price + cost +  range + I(cost*coml5) , data = car)
model044 <- mlogit(choice ~ fuel + price + cost +  range + I(cost*coml5) + I(range*coml5) , data = car)
summary(model042)

sink("condlogit4.tex")
texreg(list(model041,model042,model043,model044))
sink()

#============================================================================================
# specification search and interpretation of interactions with observed attributes
#============================================================================================

model051 <- mlogit(choice ~ type + price + cost + I(price*college) , data = car)
model052 <- mlogit(choice ~ type + price + cost + I(price*college) , data = car)
model053 <- mlogit(choice ~ type + price + cost + I(price*college) + I(cost*college) , data = car)

sink("condlogit5.tex")
texreg(list(model051,model052,model053))
sink()


#============================================================================================
## a pure "multinomial model"
#============================================================================================

model11 <- mlogit(choice ~ 0 | college, data = car)
model12 <- mlogit(choice ~ 0 | college + I(college*cost), data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("multlogit.tex")
texreg(list(model11,model12))
sink()


model21 <- mlogit(choice ~ 0 | coml5, data = car)
model22 <- mlogit(choice ~ 0 | coml5 + I(coml5*size), data = car)

sink("multlogit1.tex")
texreg(list(model21,model22))
sink()


## mixed logit 
modelf1 = mlogit(choice ~ price + cost + range + acc,data = car)
modelf2 = mlogit(choice ~ price + cost + range + acc|  hsg2, data = car)
modelf3 = mlogit(choice ~ price + cost + range + acc + speed + pollution | hsg2 + coml5, data = car)
modelf4 = mlogit(choice ~ price + cost + range + acc + speed + pollution | college + hsg2 + coml5, data = car)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("mixedlogit.tex")
texreg(list(modelf1,modelf2,modelf3,modelf4))
sink()

