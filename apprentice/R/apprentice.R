readprepost <- function(datadir,pp,participants) {
  data_f <- paste(datadir,pp,'.csv',sep='')
  data   <- read.csv(data_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  # http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe
  data       <- data[ data$participant %in% participants, ] # only real participant rows
  rrs_items  <- sprintf(paste('rrs',pp,'.SQ%03d.',sep=''),1:22)
  data$rrs   <- rowSums(subset(data,select = rrs_items))  # RRS total
  pswq_items <- sprintf(paste('pswq',pp,'.SQ%03d.',sep=''),1:16)
  data$pswq  <- rowSums(subset(data,select = pswq_items)) # PSWQ total
  data
}
