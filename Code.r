
#load packages
library(ltm)
library(psych)
library(knitr)
library(pander)
library(knitr)
library(psych)


#load data
setwd("A:/Fall 2017/Stats 154/ICPSR_34871/DS0001/")
load("34871-0001-Data.rda")

#exploratory factor analysis


df1 <- da34871.0001
df1 <- df1[,c(2:19)]
df2 <- as.data.frame(lapply(df1,as.integer))





#remove NA
df2 <- na.omit(df2)

eigen <- eigen(cov(df2))
eigen$value

sum(eigen$value)

binary <- function(df){
  for(i in  1:18){
    df[,i][df[,i] < 3] <- 0L
    df[,i][df[,i] >= 3] <- 1L
  }
  return(df)
}

#rekeying
rekey <- function(item){
  item[item == 1] <- 8L
  item[item == 2] <- 7L
  item[item == 3] <- 6L
  item[item == 4] <- 5L
  item <- item - 4L
  return(item)
}
df2$Q9 <- rekey(df2$Q9)
df2$Q15 <- rekey(df2$Q15)

df2 <- binary(df2)
write.table(df2, "Final data.txt")
df2 <- read.table(file = "A:/Fall 2017/Stats 154/ICPSR_34871/DS0001/Final data.txt")
#polychoric(df2)
#polyserial(df2,df2)


summary(df2)

poly <- polychoric(df2)
pander(poly$rho, digits = 3)


eigen <- eigen(poly$rho)

eigen$value

fa0 <- fa(df2, nfactors = 5, rotate = "none", fm="ols")
print(fa0$loadings, cutoff = 0)

#kable(cor(df2))
#pander(describe(df2)[3:4])

fa1 <- fa(df2, nfactors = 2, rotate = "varimax", fm="ols", cor = "poly")
fa2 <- fa(df2, nfactors = 2, rotate = "oblimin", fm="ols", cor = "poly")

fa1
fa2
fa2$Phi

print(fa1$loadings, cutoff = 0)


print(fa2$loadings, cutoff = 0)

#Ordinary least squares

df3 <- df2[c(3:8)]
fa3 <- fa(df3, nfactors = 1, fm="ols", cor = "poly")
print(fa3$loadings, cutoff = 0)

fa0 <- fa(df2, nfactors = 1, rotate = "none", fm="ols", cor = "poly")
print(fa0$loadings, cutoff = 0)

eigen(cor(df2))$value

eigen(cor(df3))$value


round(eigen$value, digit = 10)

eigen$value[1] / sum(eigen$value)

poly2 <- polychoric(df3)

eigen(poly2$rho)$value[1] / sum(eigen(poly2$rho)$value)



kable(head(df3, 20))

psych::describe(df3)

#The sum of all the item variances 
sapply(df3,var)
sum_si <- sum(sapply(df3,var))
sum_si

#Variance of the total score
total <- df3$Q3 + df3$Q4 + df3$Q5 + df3$Q6 + df3$Q7 + df3$Q8
total_var <- var(total)
total_var

#Number of Items
k <- length(df3)
k
#Cronbach's Alhpa

alhpa_value <-  (k/(k-1)) * (1 - (sum_si/var(total)))

alhpa_value

alpha(df3)




#IRT section

#rasch 

rasch_m1 <- rasch(df3)
coefficients(rasch_m1)
plot(rasch_m1, legend = TRUE)
ltm::factor.scores(rasch_m1)
information(rasch_m1, c(-4,4))

#unidimTest(rasch_m1)





#tutorial link
#https://www.youtube.com/watch?v=VtWsyUCGfhg  


#2PL

#IRT.param keeps a, b value in traditional format

ltm_m1 <- ltm(df3~z1, IRT.param = TRUE)
#out <- unidimTest(ltm_m1)
#out
#plot(out, type = "b", pch = 1:2)

summary(ltm_m1)
coef(ltm_m1)

#below 0 = easy
# a = Dscrmn   b = Dffclt

#plot
plot(ltm_m1, type = "ICC", legend = TRUE)
#Test information 
plot(ltm_m1, type = "IIC", legend = TRUE)

## The Standard Error of Measurement can be plotted by
vals <- plot(ltm_m1, type = "IIC", items = 0, plot = FALSE)
par(mar = c(4,4,3,4))

plot(vals[, "z"],vals[, "info"], type = "l", lwd = 2,
     xlab = "Ability", ylab = "information", 
     main = "Test Information and Standard Error of Measurement")
par(new = TRUE)
plot(vals[, "z"], 1 / sqrt(vals[, "info"]), type = "l", lwd = 2, lty = 2, col = "red",
     xlab = "", ylab = "", axes = FALSE)
mtext("Standard Error", side = 4, line = 2.5, col = "red")
axis(4, ylim = c(0:14), col = "red", col.axis = "red")
legend(1.8, 14, legend = c("Information","Standard Error"), 
       lwd=c(2,2), lty = c(1,2), col = c("black","Red"), text.col = c("black","Red"))



pander(ltm::factor.scores(ltm_m1), digits = 3)
person.fit(ltm_m1)

item.fit(ltm_m1)

#3PL
options(scipen=999)

tpm_m1 <- tpm(df3, type = "latent.trait", IRT.param = TRUE)
summary(tpm_m1)
coef(tpm_m1)

#below 0 = easy
# a = Dscrmn   b = Dffclt

#plot
plot(tpm_m1, type = "ICC", legend = TRUE)
#test information 
plot(tpm_m1, type = "IIC")
plot(tpm_m1, type = "IIC", item = 0)

ltm::factor.scores(tpm_m1)
person.fit(tpm_m1)





