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

# output graph
#graph1(design,data=read.table(file.choose(new=FALSE)),xlab="Measurement Times",ylab="Scores")

# expected effect is more negative i.e. increased avoidance of N/I words
ES <- 'PND-'
# expect B to be more negative than A i.e. increased avoidance of N/I words
statistic <-'A-B'

# I words
printf("I-words\n");
jpeg('p1_iwords.jpg')
iwords <- "/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/data/p1_iscores"
#graph.TREND(design,'LSR','mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (I-word pairs)")
graph.CL(design,'mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (I-word pairs)")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(iwords))
pnd <- ES(design,ES,data = read.table(iwords))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# N words
print("N-words");
jpeg('p1_nwords.jpg')
nwords <- "/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/data/p1_nscores"
graph.CL(design,'mean',data=read.table(nwords),xlab="Measurement Times",ylab="Attentional Bias Score (N-word pairs)")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(nwords))
pnd <- ES(design,ES,data = read.table(nwords))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# GRS
print("GRS");
jpeg('p1_grs.jpg')
grs <- "/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/data/p1_grs_scores"
graph.CL(design,'mean',data=read.table(grs),xlab="Measurement Times",ylab="GRS Score")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(grs))
pnd <- ES(design,ES,data = read.table(grs))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)

# PANAS
print("PANAS");
jpeg('p1_panas.jpg')
panas <- "/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/data/p1_panas_scores"
graph.CL(design,'mean',data=read.table(panas),xlab="Measurement Times",ylab="I-PANAS-SF + 'Sad' + 'Anxious' Score")
dev.off()
p <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = read.table(panas))
pnd <- ES(design,ES,data = read.table(panas))
printf("p = %0.3f\nPND = %0.3f\n",p,pnd)
