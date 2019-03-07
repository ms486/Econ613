
library(AER)
library(stargazer)
library(xtable)
library(texreg)

data("Affairs")

affairs = Affairs
names(affairs)

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

sink("desc.tex")
stargazer(affairs)
sink()

affairs$yaff = as.numeric(affairs$affairs>0)
names(affairs)
table(affairs$yaff)
prop.table(table(affairs$yaff))

options(digits=3)
df1 = rbind(prop.table(table(affairs$gender,affairs$yaff),2),
           prop.table(table(affairs$age,affairs$yaff),2),
           prop.table(table(affairs$yearsmarried,affairs$yaff),2),
           prop.table(table(affairs$yaff)))
df1 = round(df1,2)
df1

first1 = c("\\multirow{2}{*}{Gender}",
           "  "  ,
           "\\multirow{9}{*}{Age}",
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           "\\multirow{8}{*}{Years}",
           " Married " ,
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           " " ,
           "Share")

second1 =  c(levels(affairs$gender),levels(factor(affairs$age)),levels(factor(affairs$yearsmarried)),
             "")

df1 = cbind(first1,second1,df1)
df1

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- nrow(df1)

addtorow$command <- c(paste0('\\toprule \n',
                             ' & & No Affair & Affair \\\\ \n'),
                      '\\bottomrule \n')

print(xtable(df1, align="lllll", digits = 3), include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL,
      add.to.row = addtorow, sanitize.text.function = function(x) {x})

print(xtable(df1, align="lllll", digits = 3), include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL,
      add.to.row = addtorow, sanitize.text.function = function(x) {x},file="tab1.tex")


df2 = rbind(prop.table(table(affairs$children,affairs$yaff),2),
           prop.table(table(affairs$religiousness,affairs$yaff),2),
           prop.table(table(affairs$education,affairs$yaff),2))
df2 = round(df2,3)

first2 = c("\\multirow{2}{*}{Children}",
          "  "  ,
          "\\multirow{5}{*}{Religiousness}",
          " " ,
          " " ,
          " " ,
          " " ,
          "\\multirow{7}{*}{Education}",
          " " ,
          " " ,
          " " ,
          " " ,
          " " ,
          " " )

second2 = c(levels(affairs$children),levels(factor(affairs$religiousness)),levels(factor(affairs$education)))


df2 = cbind(first2,second2,df2)
df2

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- nrow(df2)

addtorow$command <- c(paste0('\\toprule \n',
                             ' & & No Affair & Affair \\\\ \n'),
                      '\\bottomrule \n')

print(xtable(df2, align="lllll", digits = 3), include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL,
      add.to.row = addtorow, sanitize.text.function = function(x) {x},file="tab2.tex")


#===================================================
# regressions
#===================================================

est_probit1 = glm(yaff ~ as.factor(gender), family = binomial(link = "probit"), 
                  data = affairs)

est_probit2 = glm(yaff ~ as.factor(gender) + as.factor(age), family = binomial(link = "probit"), 
               data = affairs)

est_probit3 = glm(yaff ~ as.factor(gender) + as.factor(age) + as.factor(children), family = binomial(link = "probit"), 
                  data = affairs)

est_probit4 = glm(yaff ~ as.factor(gender) + as.factor(age) + as.factor(children) + as.factor(yearsmarried), family = binomial(link = "probit"), 
                  data = affairs)

#================================

est_logit1 = glm(yaff ~ as.factor(gender), family = binomial, 
                  data = affairs)

est_logit2 = glm(yaff ~ as.factor(gender) + as.factor(age), family = binomial, 
                 data = affairs)

est_logit3 = glm(yaff ~ as.factor(gender) + as.factor(age) + as.factor(children), family = binomial, 
                 data = affairs)

est_logit4 = glm(yaff ~ as.factor(gender) + as.factor(age) + as.factor(children) + as.factor(yearsmarried), family = binomial, 
                  data = affairs)


sink("probit.tex")
texreg(list(est_probit1, est_probit2,est_probit3,est_probit4), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()

sink("logit.tex")
texreg(list(est_logit1, est_logit2,est_logit3,est_logit4), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()

sink("discrete.tex")
texreg(list(est_probit4,est_logit4), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()


