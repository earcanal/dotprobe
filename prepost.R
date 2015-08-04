#!/usr/bin/Rscript

options(width = 140)

# read pre and post outcomes
datadir <- '/media/paul/2E95-1293/study/participants/'
pre_f   <- paste(datadir,'pre.csv',sep='')
post_f  <- paste(datadir,'post.csv',sep='')


pre  <- read.csv(pre_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
post <- read.csv(post_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
pre  <- subset(pre, select = -submitdate)
post <- subset(post, select = -submitdate)

participants <- c(5:13,15,16,18,19)

# http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe
pre <- pre[ pre$participant %in% participants, ] # only rows real participants

# recipe 12.2
pre <- pre[order(pre$participant), ]
pre[c('participant','gad7pretotal','phq9pretotal')]

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
