# statistical inference finds out the difference of
# measurement and compare are reoroducible or not

# review the p-value and test confidence interval concepts
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

# split data into two weight datasets
bwt.nonsmoke <- filter(babies, smoke==0)
bwt.nonsmoke <- bwt.nonsmoke[,c("bwt")] 
bwt.smoke <- filter(babies, smoke==1)
bwt.smoke <- bwt.smoke[,c("bwt")]

# check the population differences and statistics
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke) # difference between population mean
popsd(bwt.nonsmoke) # population sd of the nonsmoking mom baby weight
popsd(bwt.smoke) # population sd of the smoking mom baby weight

# try to use the sample as inference of the population
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke,N)
dat.s <- sample(bwt.smoke,N)
se <- sqrt(sd(dat.ns)^2/N+sd(dat.s)^2/N)
obs <- abs(mean(dat.ns)-mean(dat.s))
tval <- obs/se
cat("The t-statisic of samples is:",tval)
# find out how unusual the t value would be if null hypothesis were true
2 * (1 - pnorm(tval)) # the sum AUC of two tails beyond t values from mean
2 * pnorm(-tval) # a simpler way to calculate the same result above
qnorm(0.995) * se # 99% confidence interval

# confidence interval for the population mean
dat <- read.csv("mice_pheno.csv")
chowPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]
mu_chow <- mean(chowPopulation)
cat("the population mean of parameter in interest is:",mu_chow)
# estimate the population mean from sample mean
set.seed(1)
N <- 30
chow <- sample(chowPopulation,N)
cat("one sample mean of the paremeter in interest is:",mean(chow))
# sample mean is normally distributed with mean equals to mu_chow
# and standard deviation (known as standard error) of:
se <- sd(chow)/sqrt(N)
# take 95% confidence interval for example to calculate
Q <- qnorm(1-0.05/2) # convert interval value to sd size from mean
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se) # get interval values
interval[1]<mu_chow & interval[2]>mu_chow # verify interval is correct
# the result should be 95% ture, as the confidence interval of 95% means
# take more samples to comfirm the result and plot out
plot_ci<-function(N){
  library(rafalib)
  mypar()
  B <- 250 # take 250 examples
  count <- 0 # initialize a counter for outliers
  plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
       xlab="weight",ylab="interval",ylim=c(1,B))
  abline(v=mean(chowPopulation))
  for (i in 1:B) {
    chow <- sample(chowPopulation,N)
    se <- sd(chow)/sqrt(N)
    interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
    covered <- 
      mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
    color <- ifelse(covered,1,2) # values out of interval are in red
    if (!covered) count = count + 1
    lines(interval, c(i,i),col=color)
  }
  cat("There are",count,"results out of interval, actual confidenc interval is", 100-count/2.5,"%.")
}
N <- 30 # set sample size to 30
plot_ci(N)
N <- 5 # set sample size to 5
plot_ci(N) # confidence interval degrade to less than 90% due to small sample size
# use t-distribution for small example size
Q <- qt(1-0.05/2,df=4) # df equals sample size minus one
N <- 5
plot_ci(N) # now confidence interval recovered to about 95%
