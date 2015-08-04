#!/usr/bin/Rscript

options(width = 140)

# read pre and post outcomes
datadir <- '/media/paul/2E95-1293/study/participants/'
participants <- c(5:13,15,16,18,19)

read_data <- function(t) {
  data_f <- paste(datadir,t,'.csv',sep='')
  data <- read.csv(data_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  # http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe
  data[ data$participant %in% participants, ] # only real participant rows
}

## pre
pre <- read_data('pre')

# recipe 12.2
pre <- pre[order(pre$participant), ]
rrs_items <- sprintf("rrspre.SQ%03d.",1:22)
pre$rrs <- rowSums(subset(pre,select = rrs_items))   # RRS total
pswq_items <- sprintf("pswqpre.SQ%03d.",1:16)
pre$pswq <- rowSums(subset(pre,select = pswq_items)) # PSWQ total
pre[c('participant','phq9pretotal','rrs','pswq','gad7pretotal')]

## post
post <- read_data('post')

# fill missing data with pre score i.e. 'no change'

# Yang et al. (2014) use equivalent 1-way ANOVA (F)

# p < .05
# Student's t-test: var.equal=TRUE
#t.test(valence.mean ~ valence.category, nwords, var.equal=TRUE)

# Yang et al. (2014)
# p > .05
#t.test(arousal.mean ~ valence.category, nwords, var.equal=TRUE)
#t.test(imageability.mean ~ valence.category, nwords, var.equal=TRUE)
#t.test(familiarity.mean ~ valence.category, nwords, var.equal=TRUE)

# plus ...
# p > .05
#t.test(frequency ~ valence.category, nwords, var.equal=TRUE)
#t.test(syllables ~ valence.category, nwords, var.equal=TRUE)
#t.test(characters ~ valence.category, nwords, var.equal=TRUE)
