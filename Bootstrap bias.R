#Bootstrap bias
## Sample variance
set.seed(1979)
n= 25 # small sample
x = rnorm(n) # random sample from the standard normal
varx = var(x) * (n-1)/n # Biased estimator for variance
c(varx, varx-1, -1/n) # sample variance and bias relative
to the true value 1 and expected value of the bias
B = 1000 # Number of required bootstrap samples
bvarx = NULL
for(i in 1: B){
  xstar = sample(x, n, replace = TRUE)
  bvarx[i]= var(xstar)*(n-1)/n
}
thetastar = mean(bvarx)
c(thetastar,thetastar-varx) # resample variance and bias estimate

install.packages("boot")
install.packages("bootstrap")
library(boot)
library(bootstrap)
theta.hat=function(d,i)
{ cor(d[i,1],d[i,2]) }
#Perform bootstrapping using the boot function.
set.seed(1979)
corr_bs=boot(data=law,statistic=theta.hat,R=5000)
corr_bs$t0
#The bias of theta.hat is estimated to be
mean(corr_bs$t)-corr_bs$t0
#The bias corrected estimate is
corr_bs$t0-(mean(corr_bs$t)-corr_bs$t0)
#In this case, our biased corrected estimate is further from the true theta than the uncorrected estimate.
cor(law82[,2],law82[,3])
corr_bs$t0




set.seed(1) #for reproducibility
#deformation stress required to puncture test specimens for 40 lots of surimi
x<-c(41.28, 45.16, 34.75, 40.76, 43.61, 39.05, 41.20, 41.02, 41.33, 40.61, 40.49, 41.77, 42.07,
        44.83, 29.12, 45.59, 41.95, 45.78, 42.89, 40.42, 49.31, 44.01, 34.87, 38.60, 39.63, 38.52, 38.52, 43.95,
        49.08, 50.52, 43.85, 40.64, 45.86, 41.25, 50.35, 45.18, 39.67, 43.89, 43.89, 42.16)
mu0<- mean(x) #mean of original sample
Shat<- sd(x)/sqrt(n)
thetas<- NULL
tstar<- NULL
for (i in 1:1000) {
  x_sample<- sample(x, n, replace = TRUE) #new resample
  mu<- mean(x_sample) #estimate
  thetas[i]<-mu  #save
  tstar[i]<-(mu-mu0)/(sd(x_sample)/sqrt(n))} #pivotal quantity

c(mu0, mean(thetas)) #compare sample mean to mean of bootstrap sampling distribution
c(Shat, sd(thetas)) #compare standard error from sample to standard error estimatefrom bootstrap distribution
quantile(tstar, probs = c(0.025,0.975)) #quantiles from bootstrap percentile t
qt(c(0.025,0.975), n - 1) #quantiles from student t distribution
mu0 + quantile(tstar, probs = c(0.025,0.975))*Shat #bootstrap percentile t confidenceinterval
mu0 + qt(c(0.025,0.975), n - 1) * Shat #student t confidence interval for comparison


set.seed(1) #reproducibility
library(bootstrap)
boott(x, theta = mean, sdfun =function(x,nbootsd,theta)
{sqrt(var(x)/length(x))},+ nboott =1000, perc = c(0.025,0.975)) #bootstrap percentile t 95% C.I.$confpoints

