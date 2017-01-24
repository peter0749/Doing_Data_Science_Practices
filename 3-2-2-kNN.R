rm(list=ls(all=TRUE))
require(class)
num = 10000
data <- data.frame( round(rexp(num, r=0.05)), round(rnorm(num, mean=67, sd=13)))
data[,1] = sapply(data[,1], FUN = function(x){ if(x>128){return(128)}; if(x<0){return(0)}; return(x) })
data[,2] = sapply(data[,2], FUN = function(x){ return(ifelse(x<0, 0, x))  })
data$credit <- factor(ifelse((data[,1]<68&data[,1]>20&data[,2]>90&sample(1:1000, 1)<=997)|sample(1:1000, 1)<=1, 'high', 'low')) # 20<Age<68 && income > 90
names(data) = c('age', 'income.NT', 'credit')

n.sample.n = nrow(data)
n.sample.r = 0.8
training <- sample(1:n.sample.n, n.sample.r * n.sample.n, replace=FALSE)
n.test.n = n.sample.n - length(training)

train <- subset(data[training,], select=c('age','income.NT'))
testing <- setdiff(1:n.sample.n, training)
test <- subset(data[testing,], select=c('age','income.NT'))

lb <- data$credit[training]
lb.true <- data$credit[testing]

best = 0
p.cmp = Inf
for(k in 1:20) {
  print(k)
  predicted.labels <- knn(train,test,lb,k)
  n.inc.rate = sum(predicted.labels!=lb.true)
  mismatch.r = n.inc.rate / n.test.n
  if(mismatch.r < p.cmp) {
    p.cmp = mismatch.r
    best = k
  }
  print(mismatch.r)
  print(paste('Best',as.character(best),collapse = ' '))
}

res <- knn(train, c(57,90), lb, k=best)
print(res)
