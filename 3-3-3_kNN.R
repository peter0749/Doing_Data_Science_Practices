rm(list=ls(all=TRUE))
require(gdata)
require(class)
require(jsonlite)
require(magrittr)
setwd('~/')
pureDigit <- function(X){ as.numeric(gsub('[^[:digit:]]','', as.character(X))) } #convert string to valid numeric type
addr2LT <- function(X) {
  testURL = (X %>% tolower %>% gsub(' east ',' e ', .) %>% gsub(' west ', ' w ', .) %>% gsub(' avenue',' ave', .) %>% gsub(' street',' st', .) %>% gsub('[ ]','+', .))
  testURL = paste('http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address=', testURL, ',+new%20york,+ny,+us', sep='',collapse='')
  res <- fromJSON(curl::curl(testURL))
  if(res$status != 'OK') {
    return(data.frame(lng=NA, lat=NA))
  } else {
    return(as.data.frame(res$results$geometry$location, col.names=c('lng','lat')))
  }
}
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
#data <- data[sample.int(nrow(data), size=ifelse(dim(data)[1]>1000, 1000, dim(data)[1])) ,]
n_obs = nrow(data)
data$address.noapt <- gsub('[,][[:print:]]*','', gsub('[ ]+',' ', trim(data$address)))
addr <- unique(data.frame(data$address.noapt, data$zip.code, stringsAsFactors = FALSE))
names(addr) <- c('address.noapt','zip.code')
addr <- as.data.frame(addr[order(addr$address.noapt),])
dup <- duplicated(addr$address.noapt)
##Todo: Get lat. lot.
dup_addr <- addr[dup,1]
if(length(dup_addr)>0) {
  addr <- addr[ which(addr$address.noapt!=dup_addr[1] & addr$zip.code!=dup_addr[2]) ,]
}
#delete duplicated record
addr_map <- addr
#k <- sapply(addr_sample$address.noapt, addr2LT)
addr_map <- as.list.data.frame(cbind(addr_map, as.data.frame(t(sapply(addr_map$address.noapt, addr2LT)))))
data$lng <- sapply(data$address.noapt, addr_map$lng, FUN=function(X,Y){ Y[[X]] })
data$lat <- sapply(data$address.noapt, addr_map$lat, FUN=function(X,Y){ Y[[X]] })
data <- data[ !is.na(data$lng) & !is.na(data$lat) ,]
data$lev <- factor(sapply(data$log_sale.price, FUN=floor))
data$address <- as.factor(data$address)
testrate = 0.8
n_objs = dim(data)[1]

train_index = sample.int(n_objs, floor(testrate*n_objs))
test_index = (1:n_objs)[-train_index]

k_max = 12
knn_pred = matrix(NA, ncol=k_max, nrow=length(test_index))
knn_err = rep(NA, times=k_max)
select_field = c(26,27)
lb_filed = 28

for(i in 1:k_max) {
  knn_pred[, i] = knn(data[train_index, select_field], data[test_index, select_field], 
                      cl=data[train_index, lb_filed], k=i)
  knn_err[i] = sum(knn_pred[,i]!=data[test_index,lb_filed])/length(test_index)
}
best = which(knn_err==min(knn_err))
plot(1:k_max, knn_err, type='l')

