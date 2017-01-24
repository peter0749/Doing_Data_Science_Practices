rm(list=ls(all=TRUE))
require(gdata)
setwd('~/')
pureDigit <- function(X){ as.numeric(gsub('[^[:digit:]]','', as.character(X))) } #convert string to valid numeric type
data <- as.data.frame.list(read.xls('dds_datasets/rollingsales_manhattan.xls', skip=4, header=TRUE, stringsAsFactors = FALSE, colClasses=rep('character',21)))
names(data) = tolower(names(data))
data$gross.square.feet <- sapply(data$gross.square.feet, FUN = pureDigit)
data$land.square.feet <- sapply(data$land.square.feet, FUN = pureDigit)
data$sale.price <- sapply(data$sale.price, FUN= pureDigit)
data <- data[ which(data$gross.square.feet>0 & data$land.square.feet>0 & data$sale.price>0) ,] # Drop invalid/not-interested data
data <- cbind(data, log(data$gross.square.feet), log(data$land.square.feet), log(data$sale.price))
names(data) <- append(names(data)[1:(length(names(data))-3)], c('log_gross.sqrtf','log_land.sqrtf','log_sale.price'))
data$sale.date <- as.Date(data$sale.date)
data$year.built <- sapply(data$year.built, FUN = pureDigit)
data$zip.code <- as.character(data$zip.code)
min_price = 8000
data <- data[which(data$sale.date>=min_price),]
n_obs = nrow(data)
data$address.noapt <- gsub('[,][[:print:]]*','', gsub('[ ]+',' ', trim(data$address)))
addr <- unique(data.frame(data$address.noapt, data$zip.code, stringsAsFactors = FALSE))
names(addr) <- c('address.noapt','zip.code')
addr <- addr[order(addr$address.noapt),]
dup <- duplicated(data$address.noapt)
##Todo: Get lat. lot.
