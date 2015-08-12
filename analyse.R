#!/usr/bin/Rscript

# ---- grs ----

options(error=traceback)
              # Single Case
library(SCRT) # Randomisation Tests
library(SCVA) # Visual Analysis
library(SCMA) # Meta Analysis
library(plyr)
library(xtable)
library(stringr)

statistic <- 'A-B'  # expect B to be more negative than A i.e. increased avoidance of N/I words
ES        <- 'PND-' # expected effect is more negative i.e. increased avoidance of N/I words

datadir <- '/media/paul/2E95-1293/study/participants/'
setwd(datadir)
participants <- c(5:13,15,16,18,19)
participants <- c(5,7,8,9,10,11,12)

# design properties
design <- 'AB'
mt     <- 35 # FIXME: varies per P
limit  <- 8

## Functions

printf <- function(...) cat(sprintf(...))

# MBD
# GRS
mbd <- function() {
  #print("MBD GRS");
  # FIXME: try a MBD randomization analysis, which requires additional "possible start points" file
  #p <- pvalue.systematic('MBD',statistic,save = "no",limit = limit, data = read.table(mbd_grs), starts = starts_f)
  starts_f <- paste(datadir,'starts',sep='')
  mbd_grs <- paste(datadir,'mbd_grs_1',sep='')
  cat(mbd_grs,' ',starts_f,"\n")
  #p <- pvalue.systematic('MBD',statistic,save = "no",limit = limit, data = read.table(mbd_grs), starts = starts_f)
  p <- pvalue.random('MBD',statistic,save = "no",limit = limit, number=1000, data = read.table(mbd_grs), starts = starts_f)
  #printf("p = %0.3f\n",p)
  stop()
  # FIXME: what's the difference between pvalue.systematic(design="MBD" ...) and SCMA as they both produce 1 p value?
  for (i in 2:6) { # MBD divided into 6 blocks for testing graph output, possibly invalid approach!!!
    block <- paste('mbd_grs_',i,sep='')
    jpg <- paste(block,'.jpg',sep='')
    jpeg(jpg)
    mbd_grs <- paste(datadir,block,sep='')
    graph.CL('MBD','mean',data=read.table(mbd_grs),xlab="Measurement Times",ylab="GRS Score")
    dev.off()
  }
}

##

#mbd()

ylab <- list(i="I-Word Attentional Bias Score (ms)",n="N-Word Attentional Bias Score (ms)",grs="GRS Score",panas="I-PANAS-SF ++ Score")
#X11(type="cairo")

## analyse daily outcomes for each participant
p <- participants
rt <- data.frame(participant=p,sessions=p,i_a_mean=p,i_a_sd=p,i_b_mean=p,i_b_sd=p,i_p=p,i_pnd=p,n_a_mean=p,n_a_sd=p,n_b_mean=p,n_b_sd=p,n_p=p,n_pnd=p)
panas <- data.frame(participant=p,sessions=p,pa_a_mean=p,pa_a_sd=p,pa_b_mean=p,pa_b_sd=p,pa_p=p,pa_pnd=p,na_a_mean=p,na_a_sd=p,na_b_mean=p,na_b_sd=p,pa_b_p=p,pa_b_pnd=p)
grs <- data.frame(participant=p,mean_a=p,sd_a=p,mean_b=p,sd_b=p,p=p,pnd=p)
for (participant in participants) {
  #printf("Participant %s\n",participant);
  p_dir <- paste(datadir,participant,'/',sep='')
  # FIXME: split PA/NA
  for (dv in c('i','n','grs','panas')) {
    #printf("DV: %s\n",dv);
    dv_f <- paste(p_dir,'p',participant,'_',dv,'_scores',sep='')
    # generate plot for visual analysis
    #graph.TREND(design,'LSR','mean',data=read.table(iwords),xlab="Measurement Times",ylab="Attentional Bias Score (I-word pairs)")
    data <- read.table(dv_f)
    #graph.CL(design,'mean',data=data,xlab="Measurement Times",ylab=ylab[[dv]])
    #savePlot(filename=paste(p_dir,'p',participant,'_',dv,'.jpg',sep=''), type='jpeg')
    p   <- pvalue.systematic(design,statistic,save = "no",limit = limit, data = data)
    pnd <- ES(design,ES,data = data)
    # caclulate mean and sd for phases A and B
    a      <- data[data$V1 == 'A','V2']
    mean_a <- mean(a)
    sd_a   <- sd(a)
    b      <- data[data$V1 == 'B','V2']
    mean_b <- mean(b)
    sd_b   <- sd(b)
    if (dv == 'i' | dv == 'n') { # RTs table
    } else if (dv == 'panas') {  # PANAS table FIXME: should be 'pa' | 'na'
    } else {                     # GRS table
      grs[grs$participant == participant,'p']      <- p
      grs[grs$participant == participant,'pnd']    <- pnd
      grs[grs$participant == participant,'mean_a'] <- mean_a
      grs[grs$participant == participant,'sd_a']   <- sd_a
      grs[grs$participant == participant,'mean_b'] <- mean_b
      grs[grs$participant == participant,'sd_b']   <- sd_b
    }
  }
}

# merge sessions completed into tables
schedules_f <- paste(datadir,'rumination study - schedules.csv',sep='')
schedules   <- read.csv(schedules_f,as.is=TRUE) # ignore non-numerics in Participants column
sessions    <- schedules[schedules$'Participant' %in% participants,c('Participant','Complete')]
sessions    <- rename(sessions,c('Participant'='participant','Complete'='sessions'))
sessions[,'participant'] <- sapply(sessions[,'participant'], as.numeric)
#rt    <- merge(sessions,rt,by='participant')
#rt    <- rt[with(rt, order(participant,sessions)), ]
grs   <- merge(sessions,grs,by='participant')
grs   <- grs[with(grs, order(participant,sessions)), ]
#panas <- merge(sessions,panas,by='participant')
#panas <- panas[with(panas, order(participant,sessions)), ]

format <- function(x) {
  # FIXME: some tables have multiple values
  p <- as.numeric(x['p'])
  if (round(p, digits=2) < 0.001) {
    p <- '< .001'
  } else {
    p <- sprintf("%0.3f",p)
    p <- str_replace(as.character(p), "^0\\.", ".")
  }
  x['p']   <- p
  x['pnd'] <- sprintf("%0.2f",as.numeric(x['pnd']))
  x['a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['mean_a']),as.numeric(x['sd_a']))
  x['b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['mean_b']),as.numeric(x['sd_b']))
  x
}


results <- apply(grs,1,format)
results <- t(results)
results <- subset(results, select=c(participant,sessions,a,b,p,pnd))

strCaption <- paste0("\\textbf{Table n} GRS inferentials")
print(xtable(results, caption=strCaption, label="prepost", align=c('c','c','l','l','r','r','r')),
      size="footnotesize",
      include.rownames=FALSE,
      include.colnames=FALSE,
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  "& & A & B \\\\\n",
                                          "\\cline{3-4} \n",
                                          "Participant & Sessions &
					  ${M}$(${SD}$) & ${M}$(${SD}$) & rand ${p}$ & ${PND}$\\\\\n",
                                          "\\midrule \n"),
                                          "\\bottomrule \n")
				    )
		      )

make_schedule <- function() {
  #quantity(design="AB",MT=15,limit=6)
  #[1] 4
  #cat("design = ",design,", mt = ",mt,", limit = ",limit,"\n", sep='')
  #cat("transitions = ",quantity(design=design,MT=mt,limit=limit),"\n", sep='')
  schedule <- selectdesign(design=design,MT=mt,limit=limit)
  cat(schedule,"\n")
}
