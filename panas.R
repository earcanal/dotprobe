#!/usr/bin/Rscript

# the whole "meta" thing totally works!!!!
# it captures the "thinking and doing process"
# WATCH AND LEARN!
# there's a *benefit* to watching when someone doesn't know how to do it
# how do you *capture* that process and then present it speeded up?

# (you can) write in concise, imperative english
  # "self-documenting" code
  # learn the syntax as you go


datadir      <- '/media/paul/2E95-1293/study/participants'
participants <- c(5:13,15,16,18,19)
measures_f   <- paste(datadir,'/measures.csv',sep='')

# read daily measures for all participants
outcomes <- tryCatch(read.csv(measures_f, header=TRUE),  error = function(err) {
  cat(paste("Error opening",measures_f,':',err))
})

# remove redundant columns from data frame (recipe 137)
outcomes <- subset(outcomes , select = c(-id,-submitdate,-lastpage,-startdate,-datestamp))

# (Lime Survey subquestion)
# PA: Inspired(7), Alert(6), Excited(2), Enthusiastic(5), Determined(9) + Sad(11), Depressed(13)
# NA: Afraid(10), Upset(3), Nervous(8), Scared(4), Distressed(1) + Anxious(12), Worried(14)

#outcomes

# calculate item means
sapply(subset(outcomes, select = c('panas.SQ007.','panas.SQ006.')), mean)

# calculate PANAS PA/NA totals
# calculate PA/NA means

# R Graphics Cookbook recipe 13.1
"mcor <- cor(panas)
round(mcor, digits=2)
library(corrplot)"
# ...
