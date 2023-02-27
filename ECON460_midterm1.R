# portfolio_exercises.r				script file for portfolio theory exercises
#
# 
#
# Comments:
# Data for the lab are in the Excel file singleIndexPrices.csv, which contains monthly prices on Boeing, Nordstrom, Starbucks and Microsoft stocks as 
# well as returns on the S&P 500 index.
#
# Download the R codes and change the directory 
# Put your code and csv file in the same folder

options(digits=4, width=70)
library("zoo")
# load the data into a zoo object using the zoo function read.csv
source("portfolio_noshorts.r")
source("portfolio.r")

# load data
setwd("/Users/jasonrodriguez/Desktop/ECON_460")

singleIndexPrices.df = read.csv(file="jason.csv", 
                                header=TRUE, stringsAsFactors=FALSE)

colnames(singleIndexPrices.df)

# Create zoo object from data and dates in singleIndexPrices.df
#
td = seq(as.Date("2015/01/02"), as.Date("2023/01/02"), by="months")
singleIndexPrices.z = zoo(singleIndexPrices.df[,-1], td)

#
# create returns
#
si.z = diff(log(singleIndexPrices.z))
lab8.df = as.data.frame(si.z)
head(lab8.df)

# returns excluding market
ret.mat = as.matrix(lab8.df[,-1])


# Create zoo object from data and dates in return matrix

lab7.z = ret.mat
start(lab7.z)
end(lab7.z)
colnames(lab7.z)

#
# Compute pairwise scatterplots
#

pairs(coredata(lab7.z), col="blue", pch=16)

#
# Compute estimates of CER model parameters
#

muhat.vals = apply(lab7.z, 2, mean)
muhat.vals
sigma2hat.vals = apply(lab7.z, 2, var)
sigma2hat.vals
sigmahat.vals = apply(lab7.z, 2, sd)
sigmahat.vals
cov.mat = var(lab7.z)
cov.mat
cor.mat = cor(lab7.z)
cor.mat

#
# portfolio theory calculations using functions in portfolio.r
#

# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(muhat.vals, cov.mat)
gmin.port
plot(gmin.port, col="blue")

# compute efficient portfolio with target return equal to highest average return
mu.target = max(muhat.vals)
e1.port = efficient.portfolio(muhat.vals, cov.mat, mu.target)
e1.port
plot(e1.port, col="blue")

# compute covariance b/w min var portfolio and efficient port
t(gmin.port$weights)%*%cov.mat%*%e1.port$weights

# compute efficient frontier of risky assets and plot
e.frontier = efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, alpha.max=1)
summary(e.frontier)
plot(e.frontier, plot.assets=T, col="blue", pch=16, cex=2)

 


