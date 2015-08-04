#!/usr/bin/Rscript

warnings()
library(methods)
library(lsr)
library(plyr)

options(width = 140)

# read pre and post outcomes
datadir <- '/media/paul/2E95-1293/study/participants/'
participants <- c(5:13,15,16,18,19)

read_data <- function(t) {
  data_f <- paste(datadir,t,'.csv',sep='')
  data   <- read.csv(data_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  # http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe
  data <- data[ data$participant %in% participants, ] # only real participant rows
  rrs_items  <- sprintf("rrspre.SQ%03d.",1:22)
  # FIXME: set pre/post columns when we have post data
  data$rrspre   <- rowSums(subset(data,select = rrs_items))  # RRS total
  pswq_items <- sprintf("pswqpre.SQ%03d.",1:16)
  # FIXME: set pre/post columns when we have post data
  data$pswqpre  <- rowSums(subset(data,select = pswq_items)) # PSWQ total
  data
}

## pre
pre <- read_data('pre')

# recipe 12.2
pre <- pre[order(pre$participant), ]

## post
pre_items  <- c('participant','rrspre','pswqpre','phq9pretotal','gad7pretotal')
pre  <- subset(pre,select = pre_items)
post <- pre
# FIXME: uncomment the next line when we have some post data!
# then replace pre data with real post data
#post <- read_data('post')
post <- rename(post, c("rrspre"="rrspost", 'pswqpre'='pswqpost', 'phq9pretotal'='phq9posttotal', 'gad7pretotal'='gad7posttotal'))

post_items <- c('participant',"rrspost",'pswqpost','phq9posttotal','gad7posttotal')
prepost <- merge(pre, post, by='participant')

## generate some random post data
prepost$rrspost  <- sample(22:88,13)
prepost$pswqpost <- sample(16:64,13)
prepost$phq9posttotal <- sample(0:27,13)
prepost$gad7posttotal <- sample(0:21,13)
prepost

## t-tests (recipe 9.15)
# Student's t-test: var.equal=TRUE
#http://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table
tests <- list()
tests[['RRS']] <- t.test(prepost$rrspre,prepost$rrspost, paired=TRUE, var.equal=TRUE)
tests[['PSWQ']] <- t.test(prepost$pswqpre,prepost$pswqpost, paired=TRUE, var.equal=TRUE)
tests[['PHQ-9']] <- t.test(prepost$phq9pretotal,prepost$phq9posttotal, paired=TRUE, var.equal=TRUE)
tests[['GAD-7']] <- t.test(prepost$gad7pretotal,prepost$gad7posttotal, paired=TRUE, var.equal=TRUE)

# need M(SD) for each

# extract your values using `sapply`
df <- sapply(tests, function(x) {
     c(x$estimate[1],
       x$estimate[2],
       ci.lower = x$conf.int[1],
       ci.upper = x$conf.int[2],
       x$parameter,
       x$statistic,
       p.value = x$p.value)
})
t(df)
cohensD(prepost$rrspre, prepost$rrspost, method="paired")
