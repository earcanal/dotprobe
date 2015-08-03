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
pa <- c('007','006','002','005','009','011','013')
pa_names <- c('inspired','alert','excited','enthusiastic','determined','sad','depressed')
na <- c('010','003','008','004','001','012','014')
na_names <- c('afraid','upset','nervous','scared','distressed','anxious','worried')

rename_cols <- function(sq,name) {
  #rename(outcomes,paste('panas.SQ00',sq,sep='')=get(name))
  col <- paste('panas.SQ',sq,'.',sep='')
  # http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
  names(outcomes)[names(outcomes)==col] <<- name # note <<- for global variable scope!
  cat(paste('panas.SQ00',sq,'.',sep=''),"\n")
}

# rename PANAS columns (recipe 6.2)
foo <- mapply(rename_cols,pa,pa_names)
foo <- mapply(rename_cols,na,na_names)
outcomes
stop()

# calculate item means

sapply(subset(outcomes, select = c('panas.SQ007.','panas.SQ006.')), mean)

# calculate PANAS PA/NA totals
# calculate PA/NA means

# R Graphics Cookbook recipe 13.1
"mcor <- cor(panas)
round(mcor, digits=2)
library(corrplot)"
# ...
