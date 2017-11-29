#### require libraries
library(boot)
library(data.table)
library(ggplot2)

# download data
auto_spb <- read.csv('http://data.gov.spb.ru/opendata/43/versions/11/csv/',stringsAsFactors = F,encoding = "UTF-8")
auto_spb <- as.data.table(auto_spb)[,2:8]
head(auto_spb)
str(auto_spb)

# work with ...
auto_spb_pay <- auto_spb[type == "Платная",]
hist(auto_spb_pay$parking_space)

#### Bootstrap Confidence Intervals with Standard Errors ####

# real mean of data
parking_space.mean <- mean(auto_spb_pay$parking_space) # ?with()
parking_space.mean

# boot stats
B = 1000
n = nrow(auto_spb_pay)
boot.samples <- matrix(sample(x = auto_spb_pay$parking_space,
                             size = B*n,
                             replace = TRUE),
                      nrow = B,
                      ncol = n)

boot.statistics <- apply(boot.samples, 1, mean)

# vizualize
ggplot(data.frame(meanSpace = boot.statistics),aes(x=meanSpace)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")

# return confidence interval
space.se = sd(boot.statistics)
interval = round(parking_space.mean + c(-1,1)*2*space.se,1)
interval


#### Write function ####

boot.mean <-  function(x,B,binwidth=NULL){
  n = length(x)
  boot.samples = matrix(sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  require(ggplot2)
  if(is.null(binwidth)){binwidth = diff(range(boot.statistics))/30}
  p = ggplot(data.frame(x=boot.statistics),aes(x=x)) +
    geom_histogram(aes(y=..density..),binwidth=binwidth) + geom_density(color="red")
  plot(p)
  interval = mean(x) + c(-1,1)*2*se
  print(interval)
  return(list(boot.statistics = boot.statistics, interval=interval, se=se, plot=p))
}

out <- boot.mean(auto_spb_pay$parking_space, B = 10000)


#### Proportions ####
# Период N - 1, было 2618 новых пользователя, зарегестрировалось 62 |конверсия = 2,36%
# Период N, было 2417 новых пользователя, зарегестрировалось 67     |конверсия = 2,77%
# Кричим ура и требуем бонус?)
new_offer <- c(rep(1, 67), rep(0, 2417 - 67))
new_offer_result <- boot.mean(new_offer, 1000,binwidth = 0.0005)


#### boot package ####
library(boot)
my.mean <- function(x, indices) {
  return( mean( x[indices] ) )
}

space.boot <- boot(auto_spb_pay$parking_space, my.mean, 10000)
boot.ci(space.boot,type = "basic")
