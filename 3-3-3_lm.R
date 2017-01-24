rm(list=ls(all=TRUE))
require(gdata)
setwd('~/')
pureDigit <- function(X){ as.numeric(gsub('[^[:digit:]]','',X)) } #convert string to valid numeric type
data <- as.data.frame.list(read.xls('dds_datasets/rollingsales_manhattan.xls', skip=4, header=TRUE, nrow=1000, stringsAsFactors = FALSE))
names(data) = tolower(names(data))
data$gross.square.feet <- sapply(data$gross.square.feet, FUN = pureDigit)
data$land.square.feet <- sapply(data$land.square.feet, FUN = pureDigit)
data$sale.price <- sapply(data$sale.price, FUN= pureDigit)
data <- data[ which(data$gross.square.feet>0 & data$land.square.feet>0 & data$sale.price>0) ,] # Drop invalid/not-interested data
data <- cbind(data, log(data$gross.square.feet), log(data$land.square.feet), log(data$sale.price))
names(data) <- append(names(data)[1:(length(names(data))-3)], c('log_gross.sqrtf','log_land.sqrtf','log_sale.price'))
model1 <- lm(log_sale.price ~ log_gross.sqrtf, data=data) #sale.price v.s. gross
sum1 <- summary(model1)
plot(y=data$log_sale.price, x=data$log_gross.sqrtf, col='red')
abline(model1, col='green')
points(y=data$log_sale.price - resid(model1), x=data$log_gross.sqrtf, col='grey')
plot(resid(model1))

model2 <- lm(log_sale.price ~ log_gross.sqrtf + log_land.sqrtf + factor(neighborhood), data=data) #sale.price v.s. gross + land + neighborhood
sum2 <- summary(model2)
plot(resid(model2))

model2a <- lm(log_sale.price ~ 0 + log_gross.sqrtf + log_land.sqrtf + factor(neighborhood), data=data)
#sale.price v.s. gross + land + neighborhood (with out intercept) 
sum2a <- summary(model2a)

model3 <- lm(log_sale.price ~ log_gross.sqrtf + log_land.sqrtf + factor(neighborhood) + factor(building.class.category), data=data)
#sale.price v.s. gross + land + neighborhood + building category
sum3 <- summary(model3)
plot(resid(sum3))

model4 <- lm(log_sale.price ~ log_gross.sqrtf + log_land.sqrtf + factor(neighborhood) * factor(building.class.category), data=data)
#sale.price v.s. gross + land + neighborhood + building category + neighborhood:building category
sum4 <- summary(model4)
plot(resid(model4))
