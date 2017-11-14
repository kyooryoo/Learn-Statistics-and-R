  set.seed(1) #so that we get the same result when use reandom
  
  #install the package of downloader if not yet
  #install.packages("downloader")
  
  #download the raw data if not yet and read it into variable
  library(downloader) ##use install.packages to install
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
  
  #install the packages if not yet
  #install.packages("dplyr")
  
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
  set.seed(1)
  population <- unlist(population)
  meanOfAll <- mean(population)
  control <- sample(population,5)
  meanOfSample5 <- mean(control)
  abs(meanOfAll - meanOfSample5)
  set.seed(5)
  population <- unlist(population)
  meanOfAll <- mean(population)
  control <- sample(population,5)
  meanOfSample5 <- mean(control)
  abs(meanOfAll - meanOfSample5)
  control <- sample(population,12)
  mean(control)
  control <- sample(population,12)
  mean(control)
  control <- sample(population,12)
  mean(control)
  
  #calculate the p-value by following null hypothesis
  n <- 10000
  null <- vector("numeric",n)
  for (i in 1:n) {
    control <- sample(population,12)
    treatment <- sample(population,12)
    null[i] <- mean(treatment) - mean(control)
  }
  mean(null >= obsdiff)
  
  #instead of sampling, use cumulative distribution to describe data
  #install.packages("UsingR")
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
  #install.packages("rafalib")
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
  install.packages("gapminder")
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