---
  title: "Bootstrap worksheet"
author: "Anish Raviraj"
date: "2/8/2020"
output:
  word_document: default
html_document: default
---
  Ques 1:
  a) How many possible bootstrap resamples of these data are there?
  ```{r}
choose(23,12) #Perfor nCr and enter "k" as 23 from (2*N-1)
```
# 1352078 possible bootstrap resamples are there

b)Using R and the sample() function, or a random number table or generator,
generate five resamples of the integers from 1 to 12.
```{r}
Org_data<-c(4.94,5.06,4.53,5.07,4.99,5.16,4.38,4.43,4.93,4.72,4.92,4.96)
Sample1<-sample(Org_data,1:12,replace = TRUE)
Sample2<-sample(Org_data,1:12,replace = TRUE)
Sample3<-sample(Org_data,1:12,replace = TRUE)
Sample4<-sample(Org_data,1:12,replace = TRUE)
Sample5<-sample(Org_data,1:12,replace = TRUE)
```

c)For each of the resamples in b, find the mean of the corresponding elements of
the aflatoxin data set. Print out the 5 bootstrap means.
```{r}
mean(Sample1)
mean(Sample2)
mean(Sample3)
mean(Sample4)
mean(Sample5)
```

d)Find the mean of the resample means. Compare this with the mean of the
original data set.
```{r}
ResampleMean<-c(Sample1,Sample2,Sample3,Sample4,Sample5)
mean(ResampleMean)
mean(Org_data)
```

e)Find the minimum and the maximum of the five resample means. This a crude
bootstrap confidence interval on the mean. 
```{r}
min(ResampleMean)
max(ResampleMean)
```

Ques2:
  a)For the sample data, compute the mean and its standard error and the median
```{r}
##Storing a variable "AirlineAccident" with the sample data.
AirlineAccident<-c(23, 16, 21, 24, 34, 30,28, 24, 26, 18, 23, 23, 36, 37, 49, 50, 51, 56, 46, 41, 54, 30, 40,31)
mean(AirlineAccident)
sd(AirlineAccident)
median(AirlineAccident)
```
b) Compute bootstrap estimates of the mean, median and 25% trimmed
mean with estimates of their standard errors, using B = 1000 resamples. 
```{r}
bs_mean=NULL
bs_median=NULL
B=1000
set.seed(1)
for (i in 1:B) {
  bs_AirlineAccident=sample(AirlineAccident,1:24,replace = TRUE) #Bootstrap setup for variable
  bs_mean[i]=mean(bs_AirlineAccident) #Bootstrap mean
  bs_median[i]=median(bs_AirlineAccident) #Bootstrap median
}
mean(bs_mean)
median(bs_median)
mean(bs_mean, trim = .25) # Trimmed value of 25% mean

sd(bs_mean) #Standard deviation
sd(bs_median)

par(mfrow=c(1,1))
hist(bs_mean,xlab = "No.of AirlineAccident", ylab = "Frequency",col = "red")
hist(bs_median,xlab = "No.of AirlineAccident", ylab = "Frequency",col = "blue")
```

c) Compare parts a and b. How do the estimates compare?
  # In part a), it returns the average and median of the airline accidents, whereas in part b) apart from the average airline accident and median value, it also shows the 25% trimmed mean of the accident.
  
  Ques3: Consider a population that has a normal distribution with mean µ = 36, standard
  deviation ?? = 8.
  a) The sampling distribution of ?????? for samples of size 500 will have what
distribution, mean and standard error?
  
  ans: For a quantitative variable with population mean µ and std. deviation ?? with sample mean ???, the sampling distribution spread will be  ??/sq. root(n) and the distribution will be normal as long as the dataset population in normal. For this question, it will be normally distributed with a sample of size 500, mean = 36, and std. error = 8/sq.root(500).

b)Use R to draw a random sample of size 500 from the population. Conduct
exploratory data analysis on your sample
```{r}
set.seed(4) #set seed for bootstrap simulation
a=rnorm(500,36,8) 
mean(a)
sd(a,na.rm = FALSE)
sd(a)/sqrt(500) #For finding the std.error
hist(a)
abline(v=mean(a), col="red", lty=2)
```

c)Compute the bootstrap distribution for your sample and note the bootstrap
mean and standard error
```{r}
B=10^4
bs_1=NULL

set.seed(1000) #set seed for bootstrap simulation
# For looping the bootstrap mean
for (i in 1:B) {
  bs1=sample(a,500, replace = TRUE)
  bs_1[i]= mean(bs1)
}
mean(bs_1)
sd(bs_1)
hist(bs_1)
abline(v=mean(bs_1), col="red", lty=2)
```
d) #Dist            #Mean    #std.dev
#Population           36        8
#Sampling dist        36        8
#Sample               35.76     7.75
#Bootstrap sample     35.76     0.34

e)Repeat for sample of sizes n = 50 and n = 10. Carefully describe your
observations about the effects of sample size on the bootstrap distribution.
```{r}
bs_50=NULL
set.seed(100)
for (i in 1:B) {
  bs50=sample(a, 50, replace=T)
  bs_50[i]=mean(bs50)
}
mean(bs50)
sd(bs50)
hist(bs50)
```
```{r}
#Repeat with sample of size 10.
bs_10=NULL
set.seed(11)
for (i in 1:B) {
  bs10=sample(a, 10, replace=T)
  bs_10[i]=mean(bs10)
}
mean(bs10)
sd(bs10)
hist(bs10)
```


Ques4:
  a) 
```{R}
(6/8+5/8+5/8+5/8+7/8+4/8)/6
```
b)Write out the R code to generate data of 100 parametric bootstrap samples and
compute an 80% percentile confidence interval for ??.
```{r}
set.seed(17)
x_binomial=rbinom(100,8,(2/3))
bs_binomial=NULL

for (i in 1:B) {
  bs_sample=sample(x_binomial,100, replace = TRUE)
  bs_binomial[i]=mean(bs_sample)
}

quantile(bs_binomial,c(0.1,0.8))
```
80% percentile confidence interval for ?? is between 5.179 and 5.432

Ques 5:
  a)Propose a parametric approach to answer this question. Mention clearly all
assumptions for such an approach

Ans: We can approach this by the hypothesis test  
where  Ho= Mean time of full time prof and graduate student are equal.
Ha= The mean time of full time prof and graduate student are not equal.

We can first check for normal distribution and conduct a T-test to find if there is a significant difference by evaluating the value of P. We arrive at a p-value>0.05. Hence, fail to reject the hypothesis.
```{r}
Full_time_prof<-c(28, 23, 18, 16, 15, 15, 13, 31, 31)
Grad_student<-c(9, 11, 14, 14, 16, 19, 37)
t.test(Full_time_prof, Grad_student , var.equal = T)
```

b) The value we get after conducting a bootstrap instead of a T-test also results in not much difference. We fail to reject the hypothesis.
```{r}
B=10^4
diff_mean=numeric(B) #Same as using NULL
for(i in 1:B)
{
  fl.sample=sample(Full_time_prof, 9, T)
  gt.sample=sample(Grad_student, 7, T)
  diff_mean[i]=mean(fl.sample)- mean(gt.sample)
}
hist(diff_mean)
abline(v=mean(Full_time_prof)-mean(Grad_student), col="red", lty=2)
quantile(diff_mean, c(0.025, 0.975))
```

