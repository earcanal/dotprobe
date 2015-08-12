#!/usr/bin/Rscript

# kintr chunk
# ---- prepost ----
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
  data       <- data[ data$participant %in% participants, ] # only real participant rows
  rrs_items  <- sprintf(paste('rrs',t,'.SQ%03d.',sep=''),1:22)
  data$rrs   <- rowSums(subset(data,select = rrs_items))  # RRS total
  pswq_items <- sprintf(paste('pswq',t,'.SQ%03d.',sep=''),1:16)
  data$pswq  <- rowSums(subset(data,select = pswq_items)) # PSWQ total
  data
}

## pre
pre        <- read_data('pre')
pre        <- rename(pre, c('rrs'='rrspre', 'pswq'='pswqpre'))
pre_items  <- c('participant','rrspre','pswqpre','phq9pretotal','gad7pretotal')
pre        <- subset(pre,select = pre_items)

## post
# generate some dummy post data
set.seed(1)
dummy <- data.frame(participant=participants, rrspost=1:13, pswqpost=1:13, phq9posttotal=1:13, gad7posttotal=1:13)
dummy$rrspost       <- sample(22:68,13,replace=TRUE) # 22:88
dummy$pswqpost      <- sample(16:44,13,replace=TRUE) # 16:64
dummy$phq9posttotal <- sample(0:10,13,replace=TRUE)  # 0:27
dummy$gad7posttotal <- sample(0:11,13,replace=TRUE)  # 0:21

# override dummy data with any real post data available
post       <- read_data('post')
post_items <- c('participant',"rrspost",'pswqpost','phq9posttotal','gad7posttotal')
post       <- rename(post, c('rrs'='rrspost', 'pswq'='pswqpost'))
post       <- subset(post,select = post_items)
override   <- match(post$participant,dummy$participant)
dummy      <- dummy[-override,]
post       <- rbind(post,dummy)

# merge pre and post data
prepost <- merge(pre, post, by='participant')
prepost <- prepost[order(prepost$participant), ] # recipe 12.2
prepost <- data.frame(prepost)

## t-tests (recipe 9.15)
# Student's t-test: var.equal=TRUE
#http://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table
tests <- list()
outcomes <- c('RRS','PSWQ','PHQ-9','GAD-7','rrspre','pswqpre','phq9pretotal','gad7pretotal','rrspost','pswqpost','phq9posttotal','gad7posttotal')
dim(outcomes) <- c(4,3)
formatted <- data.frame(1:4,1:4,1:4,1:4,1:4,row.names=outcomes[,1])
colnames(formatted) <- c('mean_pre','sd_pre','mean_post','sd_post','d')


tests <- apply(outcomes, 1, function(o) {
  outcome <- o[1]
  pre     <- o[2]
  post    <- o[3]
  formatted[outcome,'mean_pre']  <<- mean(prepost[[pre]])
  formatted[outcome,'sd_pre']    <<- sd(prepost[[pre]])
  formatted[outcome,'mean_post'] <<- mean(prepost[[post]])
  #cat(prepost[[post]],outcome,' ',post,' ',formatted[outcome,'mean_post'],"\\\\\n")
  formatted[outcome,'sd_post']   <<- sd(prepost[[post]])
  formatted[outcome,'d']         <<- cohensD(prepost[[pre]],prepost[[post]],method="paired")
  t.test(prepost[[pre]],prepost[[post]], paired=TRUE, var.equal=TRUE)
})
names(tests) <- outcomes[,1]

# extract values using `sapply`
results <- sapply(tests, function(x) {
     c(ci.lower = x$conf.int[1],
       ci.upper = x$conf.int[2],
       x$parameter,
       x$statistic,
       p.value = x$p.value)
})
results <- t(results)
#http://stackoverflow.com/questions/7739578/merge-data-frames-based-on-rownames-in-r
results <- merge(results,formatted,by="row.names")

library(stringr)
format <- function(x) {
  p <- x['p.value']
  if (round(p, digits=2) < 0.001) {
    p <- '< .001'
  } else {
    p <- sprintf("%0.3f",p)
    p <- str_replace(as.character(p), "^0\\.", ".")
  }
  pre  <- sprintf("%0.2f(%0.2f)",x['mean_pre'],x['sd_pre'])
  post <- sprintf("%0.2f(%0.2f)",x['mean_post'],x['sd_post'])
  df   <- sprintf("%d",x['df'])
  t    <- sprintf("%0.2f",x['t'])
  d    <- sprintf("%0.2f",x['d'])
  d    <- str_replace(as.character(d), "^0\\.", ".")
  ci   <- sprintf("%0.2f; %0.2f",x['ci.lower'],x['ci.upper'])
  x['ct_pre']  <- pre
  x['ct_post'] <- post
  x['df']      <- df
  x['t']       <- t
  x['p.value'] <- p
  x['d']       <- d
  x['ci']      <- ci
  x
}
row.names(results) <- results$Row.names
results <- apply(subset(results,select = -Row.names),1,format)
results <- t(results)
results <- subset(results,select = c('ct_pre','ct_post','t','df','p.value','ci','d'))

library(xtable)
strCaption <- paste0("Pre-post comparisons")
print(xtable(results, caption=strCaption, label="prepost", align=c('c','l','l','r','r','r','c','r')),
      size="footnotesize",
      include.rownames=TRUE,
      include.colnames=FALSE,
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  "& Pre & Post \\\\\n",
                                          "\\cline{2-3} \n",
                                          "Measure & ${M}$(${SD}$) & ${M}$(${SD}$) & ${t}$ & ${df}$ & ${p}$ & CI-diff$_{95\\%}$ & Cohen's d\\\\\n",
                                          "\\midrule \n"),
                                          "\\bottomrule \n")
				    )
		      )
