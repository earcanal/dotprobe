#!/usr/bin/Rscript

              # Single Case
library(SCRT) # Randomisation Tests
library(SCVA) # Visual Analysis
library(SCMA) # Meta Analysis

printf <- function(...) cat(sprintf(...))

statistic <- 'A-B'  # expect B to be more negative than A i.e. increased avoidance of N/I words
ES        <- 'PND-' # expected effect is more negative i.e. increased avoidance of N/I words

datadir <- '/media/paul/2E95-1293/study/participants/'
setwd(datadir)
participants <- c(5:13,15,16,18,19)
participants <- c(1,7)

# design properties
design <- 'AB'
mt     <- 35 # FIXME: varies per P
limit  <- 8

X11(type="cairo")

# generate plots for each outcome for each participant
for (participant in participants) {
  printf("Participant %s\n",participant);
  p_dir <- paste(datadir,participant,'/',sep='')
  # FIXME: split PA/NA
  # FIXME: dv specific ylab
  for (dv in c('i','n','grs','panas')) {
    printf("DV: %s\n",dv);
    dv_f <- paste(p_dir,'p',participant,'_',dv,'_scores',sep='')
    #graph.TREND(design,'LSR','mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (I-word pairs)")
    # FIXME: dv specific ylab
    graph.CL(design,'mean',data=read.table(dv_f),xlab="Measurement Times",ylab="Attentional Bias Score (ms)")
    savePlot(filename=paste(p_dir,'p',participant,'_',dv,'.jpg',sep=''), type='jpeg')
    p   <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(dv_f))
    pnd <- ES(design,ES,data = read.table(dv_f))
    printf("p = %0.3f\nPND = %0.3f\n",p,pnd)
  }
}

# MBD
# FIXME: try a MBD randomization analysis, which requires additional "possible start points" file
# what's the difference between this and SCMA?
# GRS
print("MBD GRS");
for (i in 0:6) {
  block <- paste('mbd_grs_',i,sep='')
  jpg <- paste(block,'.jpg',sep='')
  jpeg(jpg)
  mbd_grs <- paste(datadir,block,sep='')
  graph.CL('MBD','mean',data=read.table(mbd_grs),xlab="Measurement Times",ylab="GRS Score")
  dev.off()
}

make_schedule <- function() {
  #quantity(design="AB",MT=15,limit=6)
  #[1] 4
  #cat("design = ",design,", mt = ",mt,", limit = ",limit,"\n", sep='')
  #cat("transitions = ",quantity(design=design,MT=mt,limit=limit),"\n", sep='')
  schedule <- selectdesign(design=design,MT=mt,limit=limit)
  cat(schedule,"\n")
}
