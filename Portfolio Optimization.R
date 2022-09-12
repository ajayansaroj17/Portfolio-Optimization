library("tseries")
library("PortfolioAnalytics")
library("quantmod")
library("Quandl")
library("DEoptim")
library('stats')



#Data reading
Mydata = read.csv("My_data.csv", header=T)
Mydata$ï..date = as.Date(Mydata$ï..date)
Mydata = xts(Mydata[,2:5], order.by = Mydata$ï..date)
class(Mydata)
View(Mydata)

#Bonds
mybonds = Mydata[,1]
mbonds=monthlyReturn(mybonds, type='arithmetic')
colnames(mbonds) = "mbonds"
table.AnnualizedReturns(mbonds)
charts.PerformanceSummary(mbonds)

#Stocks
Mystocks = Mydata[,2]
mbse = monthlyReturn(Mystocks, type='arithmetic')
colnames(mbse)='mbse'
table.AnnualizedReturns(mbse)
charts.PerformanceSummary(mbse)

#International_bonds
My_ibonds = Mydata[,3]
mibonds = monthlyReturn(My_ibonds, type='arithmetic')
colnames(mibonds)='mibond'
table.AnnualizedReturns(mibonds)
charts.PerformanceSummary(mibonds)

#International_stock (NAsdeq)
My_istocks = Mydata[,4]
mistocks = monthlyReturn(My_istocks, type='arithmetic')
colnames(mistocks)='mistocks'
table.AnnualizedReturns(mistocks)
charts.PerformanceSummary(mistocks)

#Comparision
Mycomp1=cbind(mbonds,mbse,mibonds,mistocks)
table.AnnualizedReturns(Mycomp1)
charts.PerformanceSummary(Mycomp1)


#### ******************************************************************************************
#Normality test
Mymean_1 = mean(mbonds)
Mymedian_1 = median(mbonds)
Mymret1 = cbind(Mymean_1,Mymedian_1)
Mymret1

Mymsd_1 = sd(mbonds)
Mymad_1 = MeanAbsoluteDeviation(mbonds)

Mymriskcomp1 = cbind(Mymsd_1,Mymad_1)
Mymriskcomp1

Mymean_2 = mean(mbse)
Mymedian_2 = median(mbse)
Mymret2 = cbind(Mymean_2,Mymedian_2)
Mymret2

Mymsd_2 = sd(mbse)
Mymad_2 = MeanAbsoluteDeviation(mbse)

Mymriskcomp2 = cbind(Mymsd_2,Mymad_2)
Mymriskcomp2

Mymean_3 = mean(mistocks)
Mymedian_3 = median(mistocks)
Mymret3 = cbind(Mymean_3,Mymedian_3)
Mymret3

Mymsd_3 = sd(mistocks)
Mymad_3 = MeanAbsoluteDeviation(mistocks)

Mymriskcomp3 = cbind(Mymsd_3,Mymad_3)
Mymriskcomp3

Mymean_4 = mean(mibonds)
Mymedian_4 = median(mibonds)
mret4 = cbind(Mymean_4,Mymedian_4)
mret4

Mymsd_4 = sd(mibonds)
Mymad_4 = MeanAbsoluteDeviation(mibonds)

Mymriskcomp4 = cbind(Mymsd_4,Mymad_4)
Mymriskcomp4
#another property of -ve skewed : msd > mmad


#*risk normality
#skewness
Mymskew_bonds = skewness(mbonds)
Mymskew_bonds  #negative skewed

#kurtosis
Mymkurto_bonds = kurtosis(mbonds)
Mymkurto_bonds #k<3 not normal

#JB tets
Mymjb_bonds = jarque.bera.test(mbonds)
Mymjb_bonds  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal


#skewness_stock
Mymskew_stocks = skewness(mbse)
Mymskew_stocks  #negative skewed

#kurtosis_stock
Mymkurto_stocks = kurtosis(mbse)
Mymkurto_stocks #k<3 not normal

#JB tets_stock
Mymjb_stocks = jarque.bera.test(mbse)
Mymjb_stocks  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal


#skewness_int-bond
Mymskew_ibonds = skewness(mibonds)
Mymskew_ibonds  #negative skewed

#kurtosis_int-bond
Mymkurto_ibonds = kurtosis(mibonds)
Mymkurto_ibonds #k<3 not normal

#JB tets_int-bond
Mymjb_ibonds = jarque.bera.test(mibonds)
Mymjb_ibonds  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal

#skewness_int-stock
Mymskew_istocks = skewness(mistocks)
Mymskew_istocks  #negative skewed

#kurtosis_int-stock
Mymkurto_istocks = kurtosis(mistocks)
Mymkurto_istocks #k<3 not normal

#JB tets_int-stock
Mymjb_istocks = jarque.bera.test(mistocks)
Mymjb_istocks  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal

Mycomp2 = cbind(Mymskew_bonds,Mymskew_stocks,Mymskew_ibonds,Mymskew_istocks,Mymkurto_bonds,Mymkurto_stocks,Mymkurto_ibonds,Mymkurto_istocks)
Mycomp2


#Value at risk
Mymvar_bonds = VaR(p=0.99,mbonds,method='modified')
colnames(Mymvar_bonds)=c('mvar_bonds')
Mymvar_stocks = VaR(p=0.99,mbse,method='modified')
colnames(Mymvar_stocks)=c('Mymvar_stocks')

Mymvar_ibonds = VaR(p=0.99,mibonds,method='modified')
colnames(Mymvar_ibonds)=c('Mymvar_ibonds')
Mymvar_istocks = VaR(p=0.99,mistocks,method='modified')
colnames(Mymvar_istocks)=c('Mymvar_istocks')

Factor = c("MymVAR_bonds","MymVAR_stocks","mVAR_int-bonds","mVAR_int-stocks")
Value_at_Risk=c(Mymvar_bonds,Mymvar_stocks,Mymvar_ibonds,Mymvar_istocks)
Mydf_2 = data.frame(Factor,Value_at_Risk)
print(Mydf_2)
## *********************************************************************************************************************
#Risk and Return
#covariance
Mymain = cbind(mbonds,mbse,mibonds,mistocks)
Mymacov = cov(Mymain)
Mymacov

#corr
Mymacor=cor(mmain)
Mymacor

#coefficient of determination
Mymar2 = macor^2
Mymar2

# ********************************************************************************************************************
#portfolio optimization
Mymport = cbind(mbonds,mbse,mibonds,mistocks)

# 4.2.2. Naive Global Portfolio (Monthly Rebalancing)
Mymnaivew = as.numeric(t(c(0.25,0.25,0.25,0.25)))
names(Mymnaivew) = c("mbonds","mstocks","mibonds","mistocks")
Mymnaive = Return.portfolio(R=mport,weights=Mymnaivew,geometric=F,rebalance_on="months")
colnames(Mymnaive) = "Mymnaive"

# 4.2.3. Roche Global Portfolio (Monthly Rebalancing)
Mymrochew = as.numeric(t(c(0.24,0.18,0.33,0.25)))
names(mrochew) = c("mbonds","mstocks","mibonds","mistocks")
Mymroche = Return.portfolio(R=mport,weights=Mymrochew,geometric=F,rebalance_on="months")
colnames(Mymroche) = "Mymroche"

# 4.2.4. Bogle U.S. Portfolio (Monthly Rebalancing)
Mymboglew = as.numeric(t(c(0.40,0.60,0.00,0.00)))
names(Mymboglew) = c("mbonds","mstocks","mibonds","mistocks")
Mymbogle = Return.portfolio(R=mport,weights=Mymboglew,geometric=F,rebalance_on="months")
colnames(Mymbogle) = "Mymbogle"

Mybenchcomp = cbind(Mymnaive,Mymroche,Mymbogle)
View(Mybenchcomp)
# 4.2.6. Benchmark Portfolios Returns Comparison
table.AnnualizedReturns(Mybenchcomp)
charts.PerformanceSummary(Mybenchcomp)


# 4.3. Portfolio Optimization 

# 4.3.1. Mean Maximization 

# Portfolio Specifications
Mymport1c = portfolio.spec(assets = colnames(mport))

# Portfolio Constraints
Mymport1c = add.constraint(Mymport1c,type="weight_sum",min_sum=0.99,max_sum=1.01)
Mymport1c = add.constraint(Mymport1c,type="long_only")

# Portfolio Objectives
Mymport1c = add.objective(Mymport1c,type="return",name="mean")

# Portfolio Optimization
Mymportopt1 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=Mymport1c,optimize_method="DEoptim",)
chart.Weights(Mymportopt1)

####
Mymport1w = as.numeric(Mymportopt1$weights)
names(Mymport1w) = c("mbonds","mstocks","mibonds","mistocks")
Mymport1 = Return.portfolio(R=mport["2015-01-31::"],weights=Mymport1w,geometric=F,rebalance_on="months")
colnames(Mymport1) = "mport1"

####

# 4.3.2. Standard deviation minimization

# Portfolio Specifications
Mymport2c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
Mymport2c = add.constraint(Mymport2c,type="weight_sum",min_sum=0.99,max_sum=1.01)
Mymport2c = add.constraint(Mymport2c,type="long_only")

# Portfolio Objectives
Mymport2c = add.objective(Mymport2c,type="risk",name="StdDev")

# Portfolio Optimization
Mymportopt2 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=Mymport2c,optimize_method="DEoptim")
chart.Weights(Mymportopt2)

####
Mymport2w = as.numeric(Mymportopt2$weights)
names(Mymport2w) <- c("mbonds","mstocks","mibonds","mistocks")
Mymport2 <- Return.portfolio(R=Mymport["2015-01-31::"],weights=Mymport2w,geometric=F,rebalance_on="months")
colnames(Mymport2) <- "Mymport2"

####

# 4.3.3. Mean Maximization and Standard Deviation Minimization Portfolio

# Portfolio Specifications
Mymport3c = portfolio.spec(assets=colnames(Mymport))

# Portfolio Constraints
Mymport3c = add.constraint(Mymport3c,type="weight_sum",min_sum=0.99,max_sum=1.01)
Mymport3c = add.constraint(Mymport3c,type="long_only")

# Portfolio Objectives
Mymport3c = add.objective(Mymport3c,type="return",name="mean")
Mymport3c = add.objective(Mymport3c,type="risk",name="StdDev")

# Portfolio Optimization
Mymportopt3 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=Mymport3c,optimize_method="DEoptim")
chart.Weights(Mymportopt3)

####
Mymport3w = as.numeric(Mymportopt3$weights)
names(Mymport3w) = c("mbonds","mstocks","mibonds","mistocks")
Mymport3 = Return.portfolio(R=mport["2015-01-31::"],weights=Mymport3w,geometric=F,rebalance_on="months")
colnames(Mymport3) = "Mymport3"

####

# 4.3.4. Mean Maximization Value at Risk (VaR) Minimization Portfolio

# Portfolio Specifications
Mymport4c = portfolio.spec(assets=colnames(Mymport))

# Portfolio Constraints
Mymport4c = add.constraint(Mymport4c,type="weight_sum",min_sum=0.99,max_sum=1.01)
Mymport4c = add.constraint(Mymport4c,type="long_only")

# Portfolio Objectives
Mymport4c = add.objective(Mymport4c,type="return",name="mean")
Mymport4c = add.objective(Mymport4c,type="risk",name="VaR",arguments=list(p = 0.99,method="modified"))

# Portfolio Optimization
Mymportopt4 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=Mymport4c,optimize_method="DEoptim")
chart.Weights(Mymportopt4)


#####
Mymport4w = as.numeric(Mymportopt4$weights)
names(Mymport4w) <- c("mbonds","mstocks","mibonds","mistocks")
Mymport4 <- Return.portfolio(R=mport["2015-01-31::"],weights=Mymport4w,geometric=F,rebalance_on="months")
colnames(Mymport4) <- "Mymport4"

#####
#Comparision
Mycomp3 = cbind(Mymnaivew,Mymport1w,Mymport2w,Mymport3w,Mymport4w)
Mycomp3

# 4.3.5. Optimized Portfolios Backtesting Comparison
Mymportcomp = cbind(Mymnaive["2015-01-31::"],Mymport1,Mymport2,Mymport3,Mymport4)
table.AnnualizedReturns(Mymportcomp)
charts.PerformanceSummary(Mymportcomp)
tgc
