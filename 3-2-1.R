rm(list=ls(all=TRUE))
x_1 <- rnorm(1000,-1,9)
x_2 <- rnorm(1000,1,97)
hist(x_1, col='grey')
true_error <- rnorm(1000,0,3)
true_beta_0 <- 1.1
true_beta_1 <- -8.2
true_beta_2 <- 0.3
y <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_error
hist(y)
limx=c(-100, 300)
limy=c(-300, 300)
plot(x_1, y, pch=20, col='red', xlim=limx, ylim=limy, xlab = 'x', ylab = 'f(x)')
par(new=TRUE)
plot(x_2, y, pch=20, col='grey', xlim=limx, ylim=limy, xlab = 'x', ylab = 'f(x)')

fit1 <- lm(y ~ x_1+x_2)
fit2 <- lm(y ~ x_1)
fit3 <- lm(y ~ x_2)
res1 <- summary(fit1)
res2 <- summary(fit2)
res3 <- summary(fit3)
abline(fit2$coefficients, col='blue')
abline(fit3$coefficients, col='orange')
abline(fit1$coefficients[c(1,2)], col='green')
abline(fit1$coefficients[c(1,3)], col='green')