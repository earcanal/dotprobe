#!/usr/bin/Rscript

options(error=traceback)
              # Single Case
library(plyr)
source('/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/constants.r')

read.tables <- function(file.names, ...) {
  ldply(file.names, function(fn) {
      df<-read.csv(fn,header=FALSE,sep=',',as.is=FALSE,skip=2)
      if ( nrow(df) > 194 ) { # phase B
	mean(df[c(192,288),5])
      } else {                # phase A
	df[96,5]
      }
    }
  )
}

schedules_f <- paste(datadir,'rumination study - schedules.csv',sep='')
schedules   <- read.csv(schedules_f,as.is=TRUE) # ignore non-numerics in Participants column
sessions    <- schedules[schedules$'Participant' %in% participants,c('Participant','Complete')]
sessions    <- rename(sessions,c('Participant'='participant','Complete'='sessions'))
sessions[,'participant'] <- sapply(sessions[,'participant'], as.numeric)
rownames(sessions)<-sessions$participant

acc <- mapply(function(p) {
  files <- mapply(function(s) {
    f<-sprintf("%s%d/dotprobe/p%ds%d.csv",datadir,p,p,s)
    if ( file.exists(f) ) { f } # else { cat(c("Skipping ",f," (file doesn't exist)\n")) }
  }, 1:35)
  files<-files[!sapply(files,is.null)] # remove NULLs
  data  <- read.tables(files)
  data  <- as.matrix(data)
  sprintf("%0.2f \\newline (%0.2f)",mean(data),sd(data))
},participants)
#acc<-as.matrix(acc)
#rownames(acc)<-participants
tt<-sprintf("[%% accuracy = [ %s ] %%]",paste(sprintf(" '%s', ",acc),collapse=''))
cat(tt,"\n")
