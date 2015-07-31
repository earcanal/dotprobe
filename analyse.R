#!/usr/bin/Rscript

              # Single Case
library(SCRT) # Randomisation Tests
library(SCVA) # Visual Analysis
library(SCMA) # Meta Analysis

#quantity(design="AB",MT=15,limit=6)
#[1] 4
design <- "AB"
mt     <- 35
limit  <- 8

#cat("design = ",design,", mt = ",mt,", limit = ",limit,"\n", sep='')
#cat("transitions = ",quantity(design=design,MT=mt,limit=limit),"\n", sep='')
schedule <- selectdesign(design=design,MT=mt,limit=limit)
cat(schedule,"\n")

printf <- function(...) cat(sprintf(...))
save_jpg <- function(file) {
}

# output graph
#graph1(design,data=read.table(file.choose(new=FALSE)),xlab="Measurement Times",ylab="Scores")

# expected effect is more negative i.e. increased avoidance of N/I words
ES <- 'PND-'
# expect B to be more negative than A i.e. increased avoidance of N/I words
statistic <-'A-B'

datadir <- '/media/paul/2E95-1293/study/participants/'
p1_data <- paste(datadir,'1/dotprobe/',sep='')
# I words
printf("I-words\n");
jpeg('p1_iwords.jpg')
iwords <- paste(p1_data,"p1_iscores",sep='')
#graph.TREND(design,'LSR','mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (I-word pairs)")
graph.CL(design,'mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (ms)")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(iwords))
pnd <- ES(design,ES,data = read.table(iwords))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# N words
print("N-words");
jpeg('p1_nwords.jpg')
nwords <- paste(p1_data,"p1_nscores",sep='')
graph.CL(design,'mean',data=read.table(nwords),xlab="Measurement Times",ylab="Attentional Bias Score (ms)")
dev.off()
jpeg('p1_nwords_trend.jpg')
graph.TREND(design, TREND = "LSR", CL = "mean", data=read.table(nwords),xlab="Measurement Times",ylab="Attentional Bias Score (ms)")
dev.off()

p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(nwords))
pnd <- ES(design,ES,data = read.table(nwords))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# GRS
print("GRS");
jpeg('p1_grs.jpg')
grs <- paste(p1_data,"p1_grs_scores",sep='')
graph.CL(design,'mean',data=read.table(grs),xlab="Measurement Times",ylab="GRS Score")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(grs))
pnd <- ES(design,ES,data = read.table(grs))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# PANAS
print("PANAS");
jpeg('p1_panas.jpg')
panas <- paste(p1_data,"p1_panas_scores",sep='')
graph.CL(design,'mean',data=read.table(panas),xlab="Measurement Times",ylab="I-PANAS-SF + 'Sad' + 'Anxious' Score")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(panas))
pnd <- ES(design,ES,data = read.table(panas))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# MBD GRS
print("MBD GRS");
for (i in 0:6) {
  block <- paste('mbd_grs_',i,sep='')
  jpg <- paste(block,'.jpg',sep='')
  jpeg(jpg)
  mbd_grs <- paste(datadir,block,sep='')
  graph.CL('MBD','mean',data=read.table(mbd_grs),xlab="Measurement Times",ylab="GRS Score")
  dev.off()
}
