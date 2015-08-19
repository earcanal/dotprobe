#!/usr/bin/Rscript

# kintr chunk
# ---- nwords ----
library(methods)
library(plyr)
library(xtable)
library(stringr)

options(width = 140)

# see #R transcript
  # note duration to get to this point (from cluelessness!)
  # most roads (questions) lead to Stack Overflow ;-)

# things I learnt
  # comments
  # shebang setup
  # no line terminators

words_d <- '/home/paul/Documents/psychology/msc/M210/apprenticeship/method/materials/words/n-words/'
words_f <- paste(words_d,'words.csv',sep='') # SAWL subset with only negative and neutral words

# output from filter_sawl with subsequent manual adjustment to match word frequencies
nwords <- read.csv(paste(words_d,'manually_paired_nwords.csv',sep=''), header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
colnames(nwords) <- c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")

## t-tests (recipe 9.15)
nwords <- rename(nwords,c('valence.category'='group','valence.mean'='valence','arousal.mean'='arousal','imageability.mean'='imageability','familiarity.mean'='familiarity'))
outcomes <- c('valence','arousal','imageability','familiarity','frequency','syllables','characters')
#http://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table
tests <- list()
tests <- lapply(outcomes, function(o) {
  negative  <- subset(nwords, group == 'negative')
  neutral   <- subset(nwords, group == 'neutral')
  # Yang et al. (2014) use equivalent 1-way ANOVA (F)
  c(t.test(get(o) ~ group, nwords, var.equal=TRUE),n_negative=nrow(negative),mean_negative=mean(negative[[o]]),sd_negative=sd(negative[[o]]),n_neutral=nrow(neutral),mean_neutral=mean(neutral[[o]]),sd_neutral=sd(neutral[[o]]))
})
names(tests) <- outcomes

# extract values using `sapply`
results <- sapply(tests, function(x) {
     c(n_negative = x$n_negative,
       mean_negative = x$mean_negative,
       sd_negative = x$sd_negative,
       n_neutral = x$n_neutral,
       mean_neutral = x$mean_neutral,
       sd_neutral = x$sd_neutral,
       ci.lower = x$conf.int[1],
       ci.upper = x$conf.int[2],
       x$parameter,
       x$statistic,
       p.value = x$p.value)
})

format <- function(x) {
  p <- x['p.value']
  if (round(p, digits=2) < 0.001) {
    p <- '< .001'
  } else {
    p <- sprintf("%0.3f",p)
    p <- str_replace(as.character(p), "^0\\.", ".")
  }
  n_negative    <- sprintf("%d",x['n_negative'])
  mean_negative <- sprintf("%0.2f",x['mean_negative'])
  sd_negative   <- sprintf("%0.2f",x['sd_negative'])
  n_neutral     <- sprintf("%d",x['n_neutral'])
  mean_neutral  <- sprintf("%0.2f",x['mean_neutral'])
  sd_neutral    <- sprintf("%0.2f",x['sd_neutral'])
  t    <- sprintf("%0.2f",x['t'])
  c(n_negative,mean_negative,sd_negative,n_neutral,mean_neutral,sd_neutral,t,p,sprintf("[%0.2f, %0.2f]",x['ci.lower'],x['ci.upper']))
}
results <- t(results)
results <- apply(results,1,format)
results <- data.frame(t(results))
names(results) <- c('n_negative','mean_negative','sd_negative','n_neutral','mean_neutral','sd_neutral','t','p','ci')

negative_title <- '\\multicolumn{2}{l}{Negative words}'
negative_n     <- paste('\\multicolumn{2}{c}{(${N}$ = ',results[1,'n_negative'],')}',sep='')
neutral_title  <- '\\multicolumn{2}{l}{Neutral words}'
neutral_n      <- paste('\\multicolumn{2}{c}{(${N}$ = ',results[1,'n_neutral'],')}',sep='')
results <- subset(results, select = c(-n_negative,-n_neutral))
strCaption <- paste0("Dimensional characteristics of Negative-Neutral word pairs")
print(xtable(results, caption=strCaption, label="prepost", align=c('l','r','r','r','r','r','r','r')),
      size="footnotesize",
      include.rownames=TRUE,
      include.colnames=FALSE,
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  '& ',negative_title,' & ',neutral_title,'\\\\\n',
					  '& ',negative_n,' & ',neutral_n,'\\\\\n',
                                          "\\cline{2-5} \n",
					  "Dimension & Mean & ${SD}$ & Mean & ${SD}$ & ${t}$ & ${p}$ & CI$_{95\\%}$\\\\\n",
                                          "\\midrule \n"),
                                          "\\bottomrule \n")
				    )
		      )


## filter_sawl was the initial pre-processing (only needed once) for generating N-words
filter_sawl <- function() {
  # words.csv is a subset of SAWL with only negative and neutral words
  sawl <- read.csv(words_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

  # valence
    # 3 mean
    # 4 SD
    # 5 category (negative/neutral)
  colnames(sawl) <- c("word","lexical class","valence.mean","valence.SD","valence.category","arousal.mean","arousal.SD","familiarity.mean","familiarity.SD","AoA.mean","AoA.SD","imageability.mean","imageability.SD","concreteness","characters","phonemes","syllables","frequency")

  # sort
  sawl <- sawl[with(sawl, order(valence.category,valence.mean)), ]
  foo <- sawl[,c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  write.table(foo,file="negative_neutral_by_valence.csv",sep=",")

  rows <- nrow(sawl)
  # 66 most negative/neutral, 60 for normal circumstances and 6 "spare" in case of i-word overlap
  #nwords <- sawl[c(1:66,(rows-65):rows),c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  nwords <- sawl[c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  write.table(nwords,file="nwords_raw.csv",sep=",")
}

#filter_sawl()
