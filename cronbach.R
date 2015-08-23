#!/usr/bin/Rscript

library(devtools)
load_all('apprentice',quiet=TRUE)
library('psy')

source('constants.r')

measures_f   <- paste(datadir,'measures.csv',sep='')
data         <- read.csv(measures_f,header=TRUE)
data         <- data[data$lastpage == 3,] # only completed surveys

do_cronbach <- function(s,data) {
  foo <- data[data$session == s,]
  #print(foo[order(foo$participant),])
  foo <- subset(foo, select=-c(participant,session))
  c <- cronbach(foo)
  c$alpha
}

sessions <- 1:35
dim(sessions) <- c(35,1)

## PANAS

# d
d_items <- sprintf(paste('panas','.SQ%03d.',sep=''),c(11,13))
d       <- data[data$'participant' %in% participants,c('participant','session',d_items)]
c       <- sapply(sessions,do_cronbach,data=d)
c
cat('d = ',median(c),range(c),"\n")

# PA
pa_items <- sprintf(paste('panas','.SQ%03d.',sep=''),c(2,5,6,7,9))
pa       <- data[data$'participant' %in% participants,c('participant','session',pa_items)]
c        <- sapply(sessions,do_cronbach,data=pa)
cat('PA = ',median(c),"\n")

# NA
na_items <- sprintf(paste('panas','.SQ%03d.',sep=''),c(1,3,4,8,10))
na       <- data[data$'participant' %in% participants,c('participant','session',na_items)]
c        <- sapply(sessions,do_cronbach,data=na)
cat('NA = ',median(c),range(c),"\n")

## GRS
reverse_grs <- function(x) { 7 - x + 1} # reverse item (7 point scale)
grs_f <- sprintf(paste('grs','.SQ%03d.',sep=''),1:4)
grs_r <- sprintf(paste('grs','.SQ%03d.',sep=''),5:7)
grs   <- data[data$'participant' %in% participants,c('participant','session',grs_f,grs_r)]
grs   <- cbind(grs[,c('participant','session',grs_f)],apply(grs[grs_r],2,reverse_grs))
c     <- sapply(sessions,do_cronbach,data=grs)
grs_c <- c(median(c),range(c))
print(grs_c)

