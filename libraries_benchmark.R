#### require all libraries
library(data.table)
library(lubridate)
library(microbenchmark)
library(ggplot2)

#### create env for experiments
write.csv(x = auto_spb_pay, "auto_spb.csv",row.names = F,fileEncoding = "UTF-8")
date_data <- as.character( seq.Date(from = as.Date("2010-01-01"),to = Sys.Date(),by = 'day') )

#### compare fread() with read.table() ####
compare_1 <- microbenchmark(data.table = fread("auto_spb.csv",stringsAsFactors = F,encoding = "UTF-8"),
                                 basic = read.table("auto_spb.csv",stringsAsFactors = F,encoding = "UTF-8",sep = ",",header = T),
                                 times = 1000L)
summary(compare_1)
# Unit: microseconds
#       expr          min           lq        mean       median           uq            max      neval
# data.table      821.200     871.4275     961.931      942.178     1015.630       4541.031       1000
#      basic     2971.275    3059.1745    3227.951     3102.515     3172.186      86541.233       1000

# На этом примере data.table работает >= 3.3 раза лучше!
# vizualize      
autoplot(compare_1)
boxplot(compare_1)

#### compare ymd() with as.Date() ####
compare_2 <- microbenchmark(lubridate = ymd(date_data),
                            basic = as.Date(date_data),
                            times = 1000L)
summary(compare_2)
# Unit: milliseconds
#      expr           min            lq          mean        median            uq          max    neval
# lubridate      3.327731      3.503259      3.849812      3.643815      3.762093     10.01614     1000
#     basic     13.222347     13.371410     13.665615     13.429739     13.526954     22.73917     1000

# На этом примере lubridate работает >= 3.5 раза лучше!
# vizualize      
autoplot(compare_2)
boxplot(compare_2)

#### 'must have' libraries ####
# xgboost
# caret
# lubridate
# data.table OR dplyr (magrittr)
# microbenchmark
# RODBC
# ggplot
# googleAnalyticsR
# vkR
# jsonlite
# stringr

