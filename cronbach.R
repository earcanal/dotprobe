#!/usr/bin/Rscript

# ---- cronbach ----
library(xtable)
library(devtools)
load_all('apprentice',quiet=TRUE)
library('psy')

source('constants.r')

do_cronbach <- function(t) {
  data       <- readprepost(datadir,t,participants)
  rrs_items  <- sprintf(paste('rrs',t,'.SQ%03d.',sep=''),1:22)
  pswq_items <- sprintf(paste('pswq',t,'.SQ%03d.',sep=''),1:16)
  phq9_items <- sprintf(paste('phq9',t,'.SQ%03d.',sep=''),1:9)
  gad7_items <- sprintf(paste('gad7',t,'.SQ%03d.',sep=''),1:7)
  cat(t,"\n")
  cat("RRS: ",unlist(cronbach(subset(data,select = c(rrs_items)))),"\n")
  cat("PSWQ: ",unlist(cronbach(subset(data,select = c(pswq_items)))),"\n")
  cat("PHQ-9: ",unlist(cronbach(subset(data,select = c(phq9_items)))),"\n")
  cat("GAD-7: ",unlist(cronbach(subset(data,select = c(gad7_items)))),"\n")
}

## pre
do_cronbach('pre')

## post
do_cronbach('post')
