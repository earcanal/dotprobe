#!/usr/bin/Rscript

# kintr chunk
# ---- prepost ----
library(methods)
library(lsr)
library(plyr)
library(xtable)
library(devtools)
load_all('/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/apprentice',quiet=TRUE)
library('psy')
library(reshape2)

options(width = 140)

source('/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/constants.r')

# results table data structure
outcomes <- c('RRS','PSWQ','PHQ-9','GAD-7','rrspre','pswqpre','phq9pretotal','gad7pretotal','rrspost','pswqpost','phq9posttotal','gad7posttotal')
dim(outcomes) <- c(4,3)
formatted <- data.frame(1:4,1:4,1:4,1:4,1:4,1:4,1:4,row.names=outcomes[,1])
colnames(formatted) <- c('mean_pre','sd_pre','alpha_pre','mean_post','sd_post','alpha_post','d')

do_cronbach <- function(t) {
  data <- readprepost(datadir,t,participants)
  cell <- paste('alpha_',t,sep='')
  # RRS
  rrs_items  <- sprintf(paste('rrs',t,'.SQ%03d.',sep=''),1:22)
  rrs <- cronbach(subset(data,select = c(rrs_items)))
  formatted['RRS',cell] <<- rrs$alpha
  # PSWQ
  pswq_items <- sprintf(paste('pswq',t,'.SQ%03d.',sep=''),1:16)
  pswq <- cronbach(subset(data,select = c(pswq_items)))
  formatted['PSWQ',cell] <<- pswq$alpha
  # PHQ9
  phq9_items <- sprintf(paste('phq9',t,'.SQ%03d.',sep=''),1:9)
  phq9 <- cronbach(subset(data,select = c(phq9_items)))
  formatted['PHQ-9',cell] <<- phq9$alpha
  # GAD7
  gad7_items <- sprintf(paste('gad7',t,'.SQ%03d.',sep=''),1:7)
  gad7 <- cronbach(subset(data,select = c(gad7_items)))
  formatted['GAD-7',cell] <<- gad7$alpha
}

## pre
pre        <- readprepost(datadir,'pre',participants)
do_cronbach('pre')
pre        <- rename(pre, c('rrs'='rrspre', 'pswq'='pswqpre'))
pre_items  <- c('participant','rrspre','pswqpre','phq9pretotal','gad7pretotal')
pre        <- subset(pre,select = pre_items)

## post
# generate some dummy post data
set.seed(1)
n <- length(participants)
dummy <- data.frame(participant=participants, rrspost=1:n, pswqpost=1:n, phq9posttotal=1:n, gad7posttotal=1:n)
dummy$rrspost       <- sample(22:68,n,replace=TRUE) # 22:88
dummy$pswqpost      <- sample(16:44,n,replace=TRUE) # 16:64
dummy$phq9posttotal <- sample(0:10,n,replace=TRUE)  # 0:27
dummy$gad7posttotal <- sample(0:11,n,replace=TRUE)  # 0:21

# override dummy data with any real post data available
post       <- readprepost(datadir,'post',participants)
do_cronbach('post')
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

## check for outliers
# http://stackoverflow.com/questions/12866189/calculating-the-outliers-in-r
outliers <- function(d) {
  #reshape the data
  df <- melt(d,id="participant")
  #function to detect outliers
  outfun <- function(x) {
    abs(x-mean(x,na.rm=TRUE)) > 3*sd(x,na.rm=TRUE)
  }
  df$outlier <- outfun(df$value)
  print(df)
}
# SMELL: clunky way of calculating outliers for each DV
if (FALSE) {
outliers(prepost[,c('participant','gad7pretotal')])
outliers(prepost[,c('participant','phq9pretotal')])
outliers(prepost[,c('participant','rrspre')])
outliers(prepost[,c('participant','pswqpre')])
outliers(prepost[,c('participant','gad7posttotal')])
outliers(prepost[,c('participant','phq9posttotal')])
outliers(prepost[,c('participant','rrspost')])
outliers(prepost[,c('participant','pswqpost')])
}

## t-tests (recipe 9.15)
# Student's t-test: var.equal=TRUE
#http://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table
tests <- list()

tests <- apply(outcomes, 1, function(o) {
  outcome <- o[1]
  pre     <- o[2]
  post    <- o[3]
  formatted[outcome,'mean_pre']  <<- mean(prepost[[pre]])
  formatted[outcome,'sd_pre']    <<- sd(prepost[[pre]])
  formatted[outcome,'mean_post'] <<- mean(prepost[[post]])
  formatted[outcome,'sd_post']   <<- sd(prepost[[post]])
  formatted[outcome,'d']         <<- cohensD(prepost[[pre]],prepost[[post]],method='x.sd')
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
  pre   <- sprintf("%0.2f(%0.2f)",x['mean_pre'],x['sd_pre'])
  a_pre  <- sprintf("%0.2f",x['alpha_pre'])
  a_pre  <- str_replace(as.character(a_pre), "^0\\.", ".")
  post   <- sprintf("%0.2f(%0.2f)",x['mean_post'],x['sd_post'])
  a_post <- sprintf("%0.2f",x['alpha_post'])
  a_post <- str_replace(as.character(a_post), "^0\\.", ".")
  df    <- sprintf("%d",x['df'])
  t     <- sprintf("%0.2f",x['t'])
  d     <- sprintf("%0.2f",x['d'])
  d     <- str_replace(as.character(d), "^0\\.", ".")
  ci    <- sprintf("[%0.2f, %0.2f]",x['ci.lower'],x['ci.upper'])
  x['ct_pre']     <- pre
  x['alpha_pre']  <- a_pre
  x['ct_post']    <- post
  x['alpha_post'] <- a_post
  x['df']         <- df
  x['t']          <- t
  x['p.value']    <- p
  x['d']          <- d
  x['ci']         <- ci
  x
}
row.names(results) <- results$Row.names
results <- apply(subset(results,select = -Row.names),1,format)
results <- t(results)
results <- subset(results,select = c('ct_pre','alpha_pre','ct_post','alpha_post','t','df','p.value','ci','d'))
ci_head   <- "CI$_{95\\%}$\\tabfnm{a}"
strCaption <- paste0("Pre-post comparisons")
latex.tab <- xtable(results, caption=strCaption, label="prepost", align=c('c','l','r','l','r','r','r','r','c','r'))

# suppress print() output as we just want to capture it for now
# NOTE: This *must* be in {} otherwise knitr will still print() the output due to its internal use of sink()
{
  sink('/dev/null')
  table = print(latex.tab,
	size="footnotesize",
	include.rownames=TRUE,
	include.colnames=FALSE,
	caption.placement="top",
	hline.after=NULL,
	add.to.row = list(pos = list(-1, nrow(results)),
			  command = c(paste("\\toprule \n",
					    "& \\multicolumn{2}{l}{Pre} & \\multicolumn{2}{l}{Post} \\\\\n",
					    "\\cline{2-5} \n",
					    "Measure & ${M}$(${SD}$) & Cronbach's $\\alpha$ & ${M}$(${SD}$) & Cronbach's $\\alpha$ & ${t}$ & ${df}$ & ${p}$ &", ci_head, " & Cohen's ${d}$\\tabfnm{b}\\\\\n",
					    "\\midrule \n"),
					    "\\bottomrule \n")
				      )
			)
  sink()
}

# footnotes
table = sub("{table}","{threeparttable}",table,fixed=TRUE)
table = sub("\\end{table}","\\begin{tablenotes}[para,flushleft]\n{\\footnotesize \\tabfnt{a}Confidence interval represents difference in pre and post measures. \\tabfnt{b}Cohen's d calculated using standard deviation of pre measures.\n}\n\\end{tablenotes}\n\\end{threeparttable}",table,fixed=TRUE)
cat(table)
