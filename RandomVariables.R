#update R to latest version if necessary
#this code below checks the latest version
#while update should be implemented under GUI of R
install.packages("installr")
library(installr)
updateR()

#prepare the libs
library("downloader")
library("dplyr")
library("UsingR")
library("rafalib")
library("gapminder")
library("magrittr")

#install the packages if not yet
install.packages("downloader")
install.packages("dplyr")
install.packages("UsingR")
install.packages("rafalib")
install.packages("gapminder")
install.packages("magrittr")

#specify a seed to get the same result when use reandom
set.seed(1)

#download the raw data if not yet and read it into variable
library(downloader)
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
if (!file.exists(filename)) download(url, destfile=filename)
datLocal <- read.csv("femaleMiceWeights.csv")

#or read the raw data from url directly into variable
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
datUrl <- read.csv(url)

#check the head of data
head(datLocal)
head(datUrl)

#to find out if the mice on the hf diet is heavier
library(dplyr)
control <- filter(datLocal,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(datLocal,Diet=="hf") %>% select(Bodyweight) %>% unlist
print( mean(treatment) )
print( mean(control) )
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)

#sample the mice three times to see the change
library(downloader)
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleControlsPopulation.csv"
url <- paste0(dir, filename)
##check if file exists and if it does not, download it:
if (!file.exists(filename)) download(url,destfile=filename)
population <- read.csv("femaleControlsPopulation.csv")
##use unlist to turn it into a numeric vector
population <- unlist(population)
set.seed(1)
control <- sample(population,5)
meanOfAll <- mean(population)
meanOfSample5 <- mean(control)
result <- abs(meanOfAll - meanOfSample5)
cat("The difference between mean of the population and a sample is:", result, "\n")
set.seed(5)
control <- sample(population,5)
meanOfSample5 <- mean(control)
result <- abs(meanOfAll - meanOfSample5)
cat("The difference between mean of the population and another sample is:", result)
#average values of the next three samples are all different
control <- sample(population,12)
mean(control)
control <- sample(population,12)
mean(control)
control <- sample(population,12)
mean(control)

#calculate the p-value by following "null hypothesis" which we assume that
#treatment and control groups have no difference
n <- 10000
nulls <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulls[i] <- mean(treatment) - mean(control)
}
mean(abs(nulls) > obsdiff)

library(rafalib)
mypar()
qqnorm(nulls)
qqline(nulls)

#instead of sampling, use cumulative distribution to describe data
data(father.son,package="UsingR")
x <- father.son$fheight
round(sample(x,10),1)
#empirical cumulative distribution function (ECDF)
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest,len=300)
heightecdf <- ecdf(x)
plot(values, heightecdf(values), type="l",
 xlab="a (Height in inches)",ylab="Pr(x <= a)")

#use histgrams to describe data
hist(x) #a basic histgram of heights
bins <- seq(smallest,largest)
hist(x,breaks=bins,xlab="Height (in inches)",main="Adult men heights")

#probability distribution
n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
totals <- vector("numeric",11)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
}

hist(null,freq=TRUE)
abline(v=obsdiff,col="red",lwd=2)

#Null Distributions Exercises
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

#1
#Set the seed at 1, random sample 5 mice 1,000 times and caculate average.
#What proportion of these averages is more than 1 gram away from x average?
meanOfPopulation <- mean(x)
set.seed(1)
n <- 1000
count <- 0
for (i in 1:n) {
  meanOfSample <- mean(sample(x, 5))
  if (abs(meanOfSample - meanOfPopulation) > 1) {
    count = count + 1
  }
}
count/1000 #0.498

#2 do it 10,000times, what is the result now?
meanOfPopulation <- mean(x)
set.seed(1)
n <- 10000
count <- 0
for (i in 1:n) {
  meanOfSample <- mean(sample(x, 5))
  if (abs(meanOfSample - meanOfPopulation) > 1) {
    count = count + 1
  }
}
count/10000 #0.4976

#3 what is the result when sample 50 instead of 5
meanOfPopulation <- mean(x)
set.seed(1)
n <- 1000
count <- 0
for (i in 1:n) {
  meanOfSample <- mean(sample(x, 50))
  if (abs(meanOfSample - meanOfPopulation) > 1) {
    count = count + 1
  }
}
count/1000 #0.019

#Probability Distributions Exercises
library(gapminder)
data(gapminder)
head(gapminder)
x <- subset(gapminder[["lifeExp"]], gapminder[["year"]]==2007)
hist(x)

#1
#the empirical cumulative distribution function (or empirical cdf or ecdf) 
#tells the proportion of the values which are less than or equal to a value
#mean(x <= a) is a ecdf calculates the number of values in x which are <= a, 
#then divide it by the total number of values in x.
#the proportion of countries in 1952 that have a life expectancy <= 40
mean(x<=40) #0.2887324

#2
#the proportion of countries in 1952 that have a life expectancy between 60 and 40
mean(x<=60) - mean(x<=40) #0.4647887

#3
#the use of sapply
plot(ecdf(x))
prop = function(q) mean(x <= q)
prop(40) #test the custom function
qs = seq(from=min(x),to=max(x),length=20)
props = sapply(qs,prop)
props = sapply(qs,function(q) mean(x<=q))
props #print the content of props to console
plot(qs,props)

#Normal Distribution Excercises
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
aveOf5 <- vector("numeric",1000)
aveOf50 <- vector("numeric",1000)
for (i in 1:1000) {
  aveOf5[i] <- mean(sample(x,5))
  aveOf50[i] <- mean(sample(x,50))
}

#1
#draw histgrams of distribution of averages with sample size of 5 and 50
hist(aveOf5)
hist(aveOf50)

#2
#what proportion is between 23 and 25 in the distribution with sample size 50
mean(aveOf50<=25)-mean(aveOf50<=23) #0.985

#3
#compute the proportion of values below a value x with pnorm(x,mu,sigma)
#we can answer the Q2 with knowing mu=23.9 and sigma=0.43
pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43) #0.9765648

#Central Limit Theorem Exercises
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

#1
#what proportion of the mice (males on the control diet)
#are within one standard deviation away from the average weight
library(dplyr)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(y <= (mean(y) + sd(y))) - mean(y <= (mean(y) - sd(y))) # 0.6950673

#2
#What proportion of these numbers are within two standard deviations away from the list's average?
mean(y <= (mean(y) + sd(y)*2)) - mean(y <= (mean(y) - sd(y)*2)) # 0.9461883

#3
#What proportion of these numbers are within three standard deviations away from the list's average?
mean(y <= (mean(y) + sd(y)*3)) - mean(y <= (mean(y) - sd(y)*3)) # 0.9910314

#4
#ploting to see how well the mouse weights could be approximated
library(rafalib)
mypar(2,2) #layout the following 4 plottings on one page 
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / sd(y)
qqnorm(z);abline(0,1)

#5
#the random variable distribution of the average of many 25 sized samples
#is very well approximated by the normal distribution
set.seed(1)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
#use replicate() function to get the average of sample 10,000 times
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs) # the average of the distribution of the sample averages is 30.95581
sd(avgs) # the standard deviation of the distribution of sample averages: 0.8368611

#Sample size should be big enough
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
datUrl <- read.csv(url)
control <- filter(datUrl,Diet=="chow")
control <- control[,c("Bodyweight")]
treatment <- filter(datUrl,Diet=="hf")
treatment <- treatment[,c("Bodyweight")]
N <- length(treatment)
obs <- mean(treatment) - mean(control)
se <- sqrt(var(treatment)/N + var(control)/N) #the standard error
tstat <- obs / se #t statistic value
2*(1-pnorm(tstat))

#use t.test() function to perform the t-test
t.test(treatment,control)

population<-unlist(datUrl)
n<-10000
nulls<-vector("numeric",n)
N<-3 #comment this line in and out to see the different plot result
for(i in 1:n){
  control<-sample(population,N)
  treatment<-sample(population,N)
  se<-sqrt(var(treatment)/N+var(control)/N)
  nulls[i]<-(mean(treatment)-mean(control))/se
}
mypar()
qqnorm(nulls)
abline(0,1)

# prepare the data for exercises
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

#1
set.seed(1)
n<-100 #roll 100 dies for each time
p<-1/6 #the proportion of 6s
fun<-function(){
  x<-sample(1:6,n,replace=TRUE)
  z<-(mean(x==6)-p)/sqrt(p*(1-p)/n)
  return(z)
}
z<-replicate(10000,fun())
mean(abs(z)>2) # 0.0424 when p = 1/6
qqnorm(z)

#2
#find the mean and sd of sample X
#something wrong with the code bellow
#X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
#use these two lines of code bellow instead for getting the same result
X <- filter(dat, Diet=="chow")
X <- X[,c("Bodyweight")]
cat("the sample average of X is", mean(X), "\n")
cat("the standard deviation of X is", sd(X), "\n")

#3
#the probability that mean of sample X
#is off by 2 sd from mean of population x
#answer is as bellow, which I did not make it out
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) ) # 0.02189533
#reverse answer to find the route to solution
x_sd <- sd(X)/sqrt(12) # is not sd(X) is the sd of sample X already?
z <- 2/x_sd # convert 2 grams to num of sd
cd <- pnorm(z) # calculate the cumulative density to z
half <- 1 - cd # probability of onside off 2 grams from population mean
2 * half # 0.02189533

#4
#use standard error and absolute diff of mean calculate t statistic 
se <- sqrt(sd(X)^2/12+sd(Y)^2/12)
obs <- abs(mean(X) - mean(Y))
tstat <- obs/se
tstat

#5
#t-distribution gets close to normal as degree of freedom grows
#degree of freedom equals to the size of sample minus 1
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pt(3,df=9999)
1 - pnorm(3)
#p-value of probability observing a quantity as large as tstat
2 * (1 - pnorm(tstat))
t.test(X,Y)