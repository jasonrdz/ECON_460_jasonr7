# script file SI Model calculations
#
#
# Comments:
# Data for the lab are in the Excel file singleIndexPrices.csv, which contains monthly prices on Boeing, Nordstrom, Starbucks and Microsoft stocks as 
# well as returns on the S&P 500 index.
#
#Download the R codes and change the directory as you did before
# 

options(digits=4, width=70)
# load zoo package
library(zoo)

# setwd("Your Own Directory")

singleIndexPrices.df = read.csv(file="jason.csv", 
                   header=TRUE, stringsAsFactors=FALSE)

colnames(singleIndexPrices.df)


# Create zoo object from data and dates in singleIndexPrices.df
#
td = seq(as.Date("1998-01-01"), as.Date("2003-01-01"), by="months")
singleIndexPrices.z = zoo(singleIndexPrices.df[,-1], td)

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}                              
plot(singleIndexPrices.z, lwd=2, col="blue")
#
# create returns
#
si.z = diff(log(singleIndexPrices.z))
lab8.df = as.data.frame(si.z)
head(lab8.df)

# returns excluding market
ret.mat = as.matrix(lab8.df[,-1])

# plot returns over full sample
plot(si.z, panel=my.panel, lwd=2, col="blue")

#
# compute alphas and betas using descriptive statistics
#

ret.mat = as.matrix(lab8.df)
muhat.vals = colMeans(lab8.df)
cov.mat = var(ret.mat)
beta.vals = cov.mat[,1]/cov.mat[1,1]
alpha.vals = muhat.vals[-1] - beta.vals[-1]*muhat.vals["sp500"]
beta.vals
alpha.vals
#
#
# Create timePlots of data
#


# Analysis of Single Index Model
#

# estimate SI model for Microsoft
gm.fit = lm(gm~sp500,data=lab8.df)
class(gm.fit)
names(gm.fit)

cat.fit = lm(cat~sp500,data=lab8.df)
class(cat.fit)
names(cat.fit)

tmus.fit = lm(tmus~sp500,data=lab8.df)
class(tmus.fit)
names(tmus.fit)

dis.fit = lm(dis~sp500,data=lab8.df)
class(dis.fit)
names(dis.fit)

# show regression output using print and summary methods

#for gm
gm.fit
summary(gm.fit)
# 95% confidence intervals
confint(gm.fit, level=0.95)

#for cat 
cat.fit
summary(cat.fit)
# 95% confidence intervals
confint(cat.fit, level=0.95)

#for t-mobile
tmus.fit
summary(tmus.fit)
# 95% confidence intervals
confint(tmus.fit, level=0.95)

#for disney
dis.fit
summary(dis.fit)
# 95% confidence intervals
confint(dis.fit, level=0.95)


#
# Analysis of Single Index Model
#
 
# create scatterplot with regression line
plot(lab8.df$sp500,lab8.df$dis,main="SI model for DIS", pch=16, col="blue", lwd=2,
     xlab="SP 500 returns", ylab="disreturns")	# create scatterplot
abline(dis.fit, lwd=2, col="orange")							# add regression line
abline(h=0,v=0)

# do the same thing as above for the other assets: sbux, boeing and nord: these will correspond to question 1
# note Residual standard error is the estimation of the squar root of error variance (see 1.(d))

gm.fit = lm(gm~sp500,data=lab8.df)
tmus.fit = lm(tmus~sp500,data=lab8.df)
dis.fit = lm(dis~sp500,data=lab8.df)
cat.fit = lm(dis~sp500,data=lab8.df)

#
# For the above, repeat the summary and graphical analysis you did for msft
#


#
# Estimate SI model for equally weighted portfolio
#

# create equally weighted portfolio 
# and add to data frame lab8.df
port = (lab8.df$cat + lab8.df$tmus + lab8.df$dis + lab8.df$gm)/4
lab8.df$port = port
colnames(lab8.df)

# estimate SI model for equally weighted portfolio
port.fit = lm(port~sp500,data=lab8.df)

# show regression output
summary(port.fit)
confint(port.fit, level=0.95)

# create scatterplot with regression line
plot(lab8.df$sp500,lab8.df$port,main="SI model for port",
     pch=16, lwd=2, col="blue", xlab="S&P 500", ylab="Portfolio")	
abline(port.fit, lwd=2, col="orange")									# add regression line
abline(h=0,v=0)

#
# compute covariance matrix implied by single index model
#

# Ex: use coef extractor function to get regression coefficients
coef(cat.fit)

# extract estimated betas for each stock
gm.beta = coef(gm.fit)[2]
tmus.beta = coef(tmus.fit)[2]
dis.beta = coef(dis.fit)[2]
cat.beta = coef(cat.fit)[2]

# create vector of betas
beta.vec = c(gm.beta,tmus.beta,dis.beta,cat.beta)
names(beta.vec) = c("GM","TMUS","DIS","CAT")
beta.vec

# compute variance of market

sig2.sp500 = var(lab8.df$sp500)
sig2.sp500

# compute var(Rm)*beta*beta'

cov.market = sig2.sp500*(beta.vec%*%t(beta.vec))
cov.market

# compute residual variances
# note: use residuals extractor function to get OLS residuals
residuals(gm.fit)
residuals(cat.fit)
residuals(tmus.fit)
residuals(dis.fit)



sig2e.gm = var(residuals(gm.fit))
sig2e.cat = var(residuals(cat.fit))
sig2e.tmus = var(residuals(tmus.fit))
sig2e.dis = var(residuals(dis.fit))

D.mat = diag(c(sig2e.gm,sig2e.cat,sig2e.tmus,sig2e.dis))
D.mat

# compute si covariance matrix

cov.si = cov.market + D.mat

# compare with sample covariance matrix
cov.hat = var(lab8.df[,3:6])
cov.si
cov.hat
cov.si - cov.hat

# compute si correlation matrix

cor.si = cov2cor(cov.si)

# compare with sample correlation matrix
cor.hat = cor(lab8.df[,3:6])
cor.si
cor.hat
cor.si - cor.hat
