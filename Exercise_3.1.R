set.seed(1786)
data.exercise.3.1<-exp(matrix(rnorm(2000), nrow=100))
index1.temp<-sample(1:100,10)
index2.temp<-sample(1:20,10)
for(i in 1:10){
  data.exercise.3.1[index1.temp[i],index2.temp[i]]<--1
}
my.data<-data.exercise.3.1

count<-0
rows<-nrow(my.data)
for(i in 1:rows) {
  row<-my.data[i,]
  negative<-length(which(row<0))
  if (!negative) {
    mean<-mean(row)
    cat("The mean of row number",i,"is",mean,"\n")
  } else {
    count<-count+1
    if(count>3){
      cat("Too many negative values\n")
      break
    } else {
      cat("<Row",i,"contains negative values>\n")
    }
  }
}