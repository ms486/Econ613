
library(AER)
library(ggplot2)
library(xtable)
library(texreg)

data("SwissLabor")

swiss = SwissLabor
summary(swiss)

swiss$age = 10*swiss$age

mtlong <- reshape2::melt(swiss[,1:4])

setwd("/Users/ms486/Dropbox/Teaching/2019/AppliedEconometrics/DiscreteChoice")

pdf("char.pdf")
ggplot(mtlong, aes(value)) + facet_grid(participation~variable, scales = 'free_x') +
  geom_histogram()
dev.off()

tab = rbind(prop.table(table(swiss$youngkids,swiss$participation),2),
            prop.table(table(swiss$oldkids,swiss$participation),2))
tab = round(tab,4)

first =  c("\\multirow{4}{*}{Number of young kids}",
                   "  "  ,
                   "  "  ,
                   "  "  ,
           "\\multirow{7}{*}{Number of old kids}",
           "  "  ,
           "  "  ,
           "  "  ,
           "  "  ,
           "  "  ,
           "  "  )
           
df = cbind(first,c(0:3,0:6),tab)
df


addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- -1
addtorow$pos[[2]] <- nrow(df)

addtorow$command <- c(paste0('\\toprule \n',
                             ' &  &  \\multicolumn{2}{c}{Participation} \\\\ \n',
                             ' &  & No  & Yes \\\\ \n'),
                      '\\bottomrule \n')

print(xtable(df, align="lllll", digits = 3), include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL,
      add.to.row = addtorow, sanitize.text.function = function(x) {x})

print(xtable(df, align="lllll", digits = 3), include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL,
      add.to.row = addtorow, sanitize.text.function = function(x) {x},file="tab3l.tex")

probit1 = glm(participation ~ income, data = swiss, family = binomial(link = "probit"))
probit2 = glm(participation ~ income + I(income^2), data = swiss, family = binomial(link = "probit"))
probit3 = glm(participation ~ income + I(income^2) + I(income^3), data = swiss, family = binomial(link = "probit"))

sink("probit_inc.tex")
texreg(list(probit1, probit2,probit3), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()

probit1 = glm(participation ~ age, data = swiss, family = binomial(link = "probit"))
probit2 = glm(participation ~ age + I(age^2), data = swiss, family = binomial(link = "probit"))
probit3 = glm(participation ~ age + I(age^2) + I(age^3), data = swiss, family = binomial(link = "probit"))

sink("probit_age.tex")
texreg(list(probit1, probit2,probit3), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()

probit1 = glm(participation ~ income + age + education + youngkids + oldkids, data = swiss, family = binomial(link = "probit"))
probit2 = glm(participation ~ income + age + education + youngkids + oldkids + I(age^2), data = swiss, family = binomial(link = "probit"))
probit3 = glm(participation ~ income + age + education + youngkids + oldkids + I(age^2) + I(income^2), data = swiss, family = binomial(link = "probit"))

sink("probit_spec.tex")
texreg(list(probit1, probit2,probit3), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()


fav = mean(dnorm(predict(probit3, type = "link")))
marg =  as.matrix(fav * coef(probit3))
xtable(marg)

xtable(table(data = as.numeric(swiss$participation)-1,model= round(fitted(probit3))))

swiss$score<-predict.glm(probit3, type="response" )
pred = prediction(swiss$score,swiss$participation)

pdf("roc.pdf")
par(mfrow=c(1,2))
plot(performance(pred, "acc"))
plot(performance(pred, "tpr", "fpr"))
dev.off()


#=========================
# different model by status


probit31 = glm(participation ~ income + age + education + youngkids + oldkids + I(age^2) + I(income^2), data = subset(swiss,foreign=="no"), family = binomial(link = "probit"))
probit32 = glm(participation ~ income + age + education + youngkids + oldkids + I(age^2) + I(income^2), data = subset(swiss,foreign=="yes"), family = binomial(link = "probit"))

sink("probit_foreign.tex")
texreg(list(probit31, probit32), dcolumn = FALSE, booktabs = TRUE,use.packages = FALSE, caption = "Probit Models",float.pos = "hb!")
sink()


#========================
data("MurderRates")
murder = MurderRates

murder_logit <- glm(I(executions > 0) ~ time + income + noncauc + lfp + southern, data = MurderRates, family = binomial)
summary(murder_logit)
