rm(list=ls(all=T))
require(ggplot2)
data(diamonds)
#head(diamonds)

ggplot(diamonds) + geom_histogram(aes(x=price)) + 
  geom_vline(xintercept=12000)

diamonds$Expensive = ifelse(diamonds$price>=12000, 1, 0)
head(diamonds)
diamonds$price = NULL

require(glmnet)
x <- model.matrix(~., diamonds[, -ncol(diamonds)])
y <- as.matrix(diamonds$Expensive)
system.time(modGlmnet <- glmnet(x=x, y=y, family="binomial"))
plot(modGlmnet, label=T)

set.seed(48872)
rm(.Random.seed, envir=globalenv())
sample(1:10)

require(rpart)
modTree <- rpart(Expensive~.,data=diamonds)
plot(modTree)
text(modTree) ##Decision Tree

require(boot)
mean(diamonds$carat)
sd(diamonds$carat)

boot.mean <- function(x, i) {
  mean(x[i])
}

boot(data=diamonds$carat, statistic = boot.mean, R=120)
require(adabag)
modBag <- bagging(formula=Species~., iris, mfinal=10)
require(mboost)
system.time(modglmBoost <- glmboost(as.factor(Expensive)~., data=diamonds, 
                                    family=Binomial(link="logit")))
summary(modglmBoost)
?blackboost
require(randomForest)
system.time(modForest <- randomForest(Species~., data=iris, importance=TRUE, proximity=TRUE))
summary(modForest)
