#!/usr/bin/Rscript

# the whole "meta" thing totally works!!!!
# it captures the "thinking and doing process"
# WATCH AND LEARN!
# there's a *benefit* to watching when someone doesn't know how to do it
# how do you *capture* that process and then present it speeded up?

# (you can) write in concise, imperative english
  # "self-documenting" code
  # learn the syntax as you go

library(corrplot)

datadir      <- '/media/paul/2E95-1293/study/participants'
participants <- c(5:13,15,16,18,19)
measures_f   <- paste(datadir,'/measures.csv',sep='')

# read daily outcome measures for all participants
outcomes <- tryCatch(read.csv(measures_f, header=TRUE),  error = function(err) {
  cat(paste("Error opening",measures_f,':',err))
})

# remove redundant columns from data frame (recipe 137)
outcomes <- subset(outcomes , select = c(-id,-submitdate,-lastpage,-startdate,-datestamp))

pa <- c('007','006','002','005','009','011','013') # Lime Survey subquestions
na <- c('010','003','008','004','001','012','014') # Lime Survey subquestions
pa_items <- c('inspired','alert','excited','enthusiastic','determined')
pa_extra <- c('sad','depressed')
na_items <- c('afraid','upset','nervous','scared','distressed')
na_extra <- c('anxious','worried')

rename_cols <- function(sq,name) {
  col <- paste('panas.SQ',sq,'.',sep='')
  # http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
  names(outcomes)[names(outcomes)==col] <<- name # note <<- for global variable scope!
}

# rename PANAS columns (recipe 6.2)
foo <- mapply(rename_cols,c(pa,na),c(pa_items,pa_extra,na_items,na_extra))

options(width=120) # recipe (12.2)
X11(type="cairo")  # required for corrplot()

# PA
# R Graphics Cookbook recipe 13.1
pa_data <- subset(outcomes,select = c(pa_items,pa_extra))
# http://www.cookbook-r.com/Manipulating_data/Adding_and_removing_columns_from_a_data_frame/
pa_data$pa_total <- rowSums(subset(outcomes,select = pa_items)) # also correlate with combined PA items
pa_cor <- cor(pa_data)
pa_cor <- round(pa_cor, digits=2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(pa_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE",type='upper')
savePlot(filename='PAnas.jpg', type='jpeg')

# NA
na_data <- subset(outcomes,select = c(na_items,na_extra))
na_data$na_total <- rowSums(subset(outcomes,select = na_items)) # also correlate with combined NA items
na_cor <- cor(na_data)
na_cor <- round(na_cor, digits=2)
corrplot(na_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE",type='upper')
savePlot(filename='paNAs.jpg', type='jpeg')

