# Bootstrapping with hypothesis testing.
## using Law data
install.packages("boot")
install.packages("bootstrap")

library(boot)
library(bootstrap)

attach(law)

#To test if the correlation coefficient is significantly different from zero. 

plot(law$LSAT, law$GPA)

theta.hat = cor(law$LSAT, law$GPA) #Sample correlation coefficient
theta.hat


n = length(law$LSAT)
nboot = 10000
theta.star = double(nboot) #Vector to store the bootstrap correlation coeff

for(i in 1: nboot)
{
  k = sample(1:n, replace = TRUE)
  theta.star[i] = cor(law$LSAT[k], law$GPA[k])
}

##lower tail test of theta = 0
ltpvalue = mean(theta.star <= 0)

##Upper tail p-value of theta = 0
utpvalue = 1-ltpvalue

## Two sided p-value
pvalue = 2 * min(ltpvalue, utpvalue)

pvalue


#One sample Hypothesis Testing. The Geyser data both times in minutes. To check if the 
#waiting time is 1 hour = 60 minutes. H0: mu = 60
library(MASS)
data(geyser)
mean(geyser$waiting)
mean(geyser$duration)

waiting_new=geyser$waiting - mean(geyser$waiting)+60
mean(waiting_new) #Now the mean of this variable should be exactly 60. So the data is now centered at 60 rather than the 72.

par(mfrow =c(1,2))
hist(geyser$waiting)
hist(waiting_new)
length(waiting_new)

bstrap = c()
for(i in 1:1000){
  newsample=sample(waiting_new,299, replace=T )
  bstrap = c(bstrap, mean(newsample))
}
hist(bstrap)
#From the histogram getting 72 seems to have small probability

N= length(geyser$waiting) 
N

mu0=73
sample_mean= mean(geyser$waiting) #storing the sample mean
sample_sd=sd(geyser$waiting)
Obs_value=(sample_mean-mu0)/(sample_sd/sqrt(N))
Obs_value

B = 10000 #number of bootstrap samples we are interested in
waiting_new=geyser$waiting - mean(geyser$waiting)+ mu0
bss= sample(waiting_new, B*N, replace = T) #choosing the samples
bss_matrix= matrix(bss, nrow= B) #storing in a matrix
bs_mean=rowMeans(bss_matrix) #computing the bootstrap mean for each of the sample
bs_sd= apply(bss_matrix, 1, sd) # to obtain the sd of each boot strap sample and store in bs_sd
bs_t= (bs_mean-mu0)/(bs_sd/sqrt(N)) #to compute the test statistics t for each bootstrap sample
ltp=mean(bs_t <= Obs_value)
utp = 1-ltp
two_sided_p=2 * min(ltp, utp)
two_sided_p

t.test(geyser$waiting, mu = mu0, alternative = "two.sided")

