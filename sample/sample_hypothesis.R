
# for the distributions
# d stands for density, p stands for cdf, q(cdf) finds the value x with cdf,
# r stands for generating random variable following dist
rnorm(n = 10)
plot(density(rnorm(n=100000)))
dnorm(x = 0)
pnorm(q = 0) # returns cdf
qnorm(p = 0.975); qnorm(p = 0.5)  

# similar functions
# http://www.cyclismo.org/tutorial/R/probability.html
dchisq()
pchisq()
qchisq()
rchisq()

dlnorm()
plnorm()
qlnorm()
rlnorm()

# use chi square stats to check the flip coin example
chisq.stats = (10 - 5)^2 / 5 + (0 - 5)^2 /5
1 - pchisq(chisq.stats, df = 1)

chisq.test(c(10,0), p = c(1/2, 1/2))
# above should give same result of p value

train <- read.csv("train.csv", stringsAsFactors = FALSE)
table(train$OverallQual)
train$highQual <- with(train, ifelse(OverallQual > 5, 1, 0))
table(train$BldgType)
with(train, table(highQual, BldgType)) # Same distribution of high/low quality in each building type
with(train, chisq.test(highQual, BldgType))
1 - pchisq(101.43, df=4)

# calculate the chi square stats manually
# expected value should be: 
apply(with(train, table(highQual, BldgType)), 1, sum) # row sum
# 0 
# 538
# 1
# 922 
apply(with(train, table(highQual, BldgType)), 2, sum) # col sum
# 1Fam  2fmCon Duplex  Twnhs TwnhsE 
# 1220     31     52     43    114  
# expected value in cells should be:
#         BldgType
# highQual 1Fam                       
#        0  1220 *(538/1460) = 449   
#        1  1220*(922/1460) = 770      

qqnorm(train$SalePrice)
qqline(train$SalePrice)
qqnorm(log(train$SalePrice))
qqline(log(train$SalePrice))

# Recall CLT 
#### Check the impact of non-normality RV on sample mean ####
# generate lognormal variable
set.seed(3)
ln <- rlnorm(10000, 0, 1)
plot(density(ln))
plot(density(log(ln)))
sample_size <- 100 # 3, 10, 100
mean <- 0
for (i in c(1:1000)) {
  ln_sample <- sample(ln, sample_size)
  mean[i] <- mean(ln_sample)
}
plot(density(mean))

# generate normal distribution
set.seed(3)
normal <- rnorm(10000, 0, 1)
plot(density(normal))
sample_size <- 100 # 3 10, 100
normal_mean <- 0
for (i in c(1:1000)) {
  normal_sample <- sample(normal, sample_size)
  normal_mean[i] <- mean(normal_sample)
}
plot(density(normal_mean))

# preprocessing, feature engineering
# 1. Simplifying the features, e.g., categorical variables with too many levels
table(train$OverallCond)
train$SimOverallCond <- with(train, 
                             ifelse(OverallCond <= 3, "low",
                                    ifelse(OverallCond <= 6, "med", "high")))

# 2. Combine some of the features
# area related: '1stFlrSF' + '2ndFlrSF' to give us combined Floor Square Footage
# 1stflr+2ndflr+lowqualsf+GrLivArea = All_Liv_Area

# Total SF for house (incl. basement)
train$AllSF <- with(train, GrLivArea + TotalBsmtSF)
with(train, cor(log(SalePrice), AllSF))
with(train, cor(log(SalePrice), GrLivArea))
with(train, cor(log(SalePrice), TotalBsmtSF))

# Total number of bathrooms
train$TotalBath <- with(train, BsmtFullBath + 0.5 * BsmtHalfBath + FullBath + 0.5 * HalfBath)

# date related: sold - remodel, remodel - built, sold - built

# Explore whether feature could make a difference in SalePrice
train$years <- with(train, YrSold - YearBuilt)
table(train$years)
train$new <- with(train, ifelse(years <= 10, 1, 0))
boxplot(log(subset(train, new == 1)$SalePrice),
        log(subset(train, new == 0)$SalePrice))
with(subset(train, new == 0), plot(density(log(SalePrice))))
with(subset(train, new == 1), lines(density(log(SalePrice)), col = 'red'))

# let's check t test result
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
# mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, ...)
# Welch t-test (var.equal = FALSE) and student t-test(var.equal = TRUE)
with(train, t.test(log(SalePrice) ~ new))
newHZ <- subset(train, new == 1)
oldHZ <- subset(train, new == 0)
sqrt(var(log(newHZ$SalePrice))/dim(newHZ)[1] +
       var(log(oldHZ$SalePrice))/dim(oldHZ)[1])
# t score = ( 11.78882 - 12.19914) / above stderr

# install library and load it
library(pwr)
mean_diff <- mean(log(newHZ$SalePrice)) - 
  mean(log(oldHZ$SalePrice))
sample_stderr <- sqrt(var(log(newHZ$SalePrice))/dim(newHZ)[1] +
                        var(log(oldHZ$SalePrice))/dim(oldHZ)[1])
effect_size <- mean_diff / sample_stderr

pwr.t2n.test(n1 = dim(newHZ)[1], 
             n2 = dim(oldHZ)[1], 
             d = mean_diff,
             sig.level = 0.05, 
             alternative = 'two.sided')

# pwr.t2n.test(n1 = dim(newHZ)[1], 
             # n2 = dim(oldHZ)[1], 
             # d = effect_size,
             # sig.level = 0.05, 
             # alternative = 'two.sided')

# learn pwr.t2n.test
pwr.t2n.test(n1 = dim(newHZ)[1], 
             n2 = dim(oldHZ)[1],
             sig.level = 0.05,
             power = 1, 
             alternative = 'two.sided')

pwr.t2n.test(n1 = dim(newHZ)[1], 
             d = 0.148,
             sig.level = 0.05,
             power = 0.8, 
             alternative = 'two.sided')