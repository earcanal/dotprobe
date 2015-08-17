#!/usr/bin/Rscript

# ---- inferentials ----

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
participants <- c(5,6,7,8,9,10,11,12,15,16,18)

# design properties
design <- 'AB'
mt     <- 35 # FIXME: varies per P
limit  <- 8

## Functions
printf <- function(...) cat(sprintf(...))

pvalue <- function(p) {
  p <- as.numeric(p)
  if (round(p, digits=2) < 0.001) {
    p <- '< .001'
  } else {
    p <- sprintf("%0.3f",p)
    p <- str_replace(as.character(p), "^0\\.", ".")
  }
  p
}

ylab <- list(i="I-Word Attentional Bias Score (ms)",n="N-Word Attentional Bias Score (ms)",grs="GRS Score",panas="I-PANAS-SF ++ Score")
X11(type="cairo")

## analyse daily outcomes for each participant
p <- participants
rt <- data.frame(participant=p,i_mean_a=p,i_sd_a=p,i_mean_b=p,i_sd_b=p,i_p=p,i_pnd=p,n_mean_a=p,n_sd_a=p,n_mean_b=p,n_sd_b=p,n_p=p,n_pnd=p)
panas <- data.frame(participant=p,pa_mean_a=p,pa_sd_a=p,pa_mean_b=p,pa_sd_b=p,pa_p=p,pa_pnd=p,na_mean_a=p,na_sd_a=p,na_mean_b=p,na_sd_b=p,na_p=p,na_pnd=p,d_mean_a=p,d_sd_a=p,d_mean_b=p,d_sd_b=p,d_p=p,d_pnd=p)
grs <- data.frame(participant=p,mean_a=p,sd_a=p,mean_b=p,sd_b=p,p=p,pnd=p)
for (participant in participants) {
  #printf("Participant %s\n",participant);
  p_dir <- paste(datadir,participant,'/',sep='')
  # FIXME: split PA/NA
  for (dv in c('i','n','grs','pa','na','d')) {
    #printf("DV: %s\n",dv);
    dv_f <- paste(p_dir,'p',participant,'_',dv,'_scores',sep='')
    # generate plot for visual analysis
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
    if (dv == 'i' | dv == 'n') {                       # RTs table
      rt[rt$participant == participant,paste(dv,'_','mean_a',sep='')] <- mean_a
      rt[rt$participant == participant,paste(dv,'_','sd_a',sep='')]   <- sd_a
      rt[rt$participant == participant,paste(dv,'_','mean_b',sep='')] <- mean_b
      rt[rt$participant == participant,paste(dv,'_','sd_b',sep='')]   <- sd_b
      rt[rt$participant == participant,paste(dv,'_','p',sep='')]      <- p
      rt[rt$participant == participant,paste(dv,'_','pnd',sep='')]    <- pnd
    } else if (dv == 'pa' | dv == 'na' | dv == 'd') {  # PANAS table
      panas[panas$participant == participant,paste(dv,'_','mean_a',sep='')] <- mean_a
      panas[panas$participant == participant,paste(dv,'_','sd_a',sep='')]   <- sd_a
      panas[panas$participant == participant,paste(dv,'_','mean_b',sep='')] <- mean_b
      panas[panas$participant == participant,paste(dv,'_','sd_b',sep='')]   <- sd_b
      panas[panas$participant == participant,paste(dv,'_','p',sep='')]      <- p
      panas[panas$participant == participant,paste(dv,'_','pnd',sep='')]    <- pnd
    } else {                                           # GRS table
      grs[grs$participant == participant,'p']      <- p
      grs[grs$participant == participant,'pnd']    <- pnd
      grs[grs$participant == participant,'mean_a'] <- mean_a
      grs[grs$participant == participant,'sd_a']   <- sd_a
      grs[grs$participant == participant,'mean_b'] <- mean_b
      grs[grs$participant == participant,'sd_b']   <- sd_b
    }
  }
}

# ha ha ha! stack overflow et al. are use cases/patterns!!
# http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
ma <- c('i'=0,'n'=0,'pa'=0,'na'=0,'d'=0,'grs'=0)
ma <- sapply(seq_along(ma), function(p,outcome,i) {
  o <- outcome[[i]]
  if (o == 'i') {
    ps <- rt$i_p
  } else if (o == 'n') {
    ps <- rt$n_p
  } else if (o == 'pa') {
    ps <- panas$pa_p
  } else if (o == 'na') {
    ps <- panas$na_p
  } else if (o == 'd') {
    ps <- panas$d_p
  } else {
    ps <- grs$p
  }
  f <- '/tmp/p.tsv'
  write.table(ps, file=f, quote=FALSE, sep='\t', row.name=FALSE, col.names=FALSE)
  c(o,combine('+',pvalues = read.table(f))) # additive method (Edgington, 1972)
}, p=ma,outcome=names(ma))
meta_p <- ma[2,]
meta_p <- as.numeric(meta_p)
meta_p <- sapply(meta_p, pvalue)
names(meta_p) <- ma[1,]

# merge sessions completed into tables
schedules_f <- paste(datadir,'rumination study - schedules.csv',sep='')
schedules   <- read.csv(schedules_f,as.is=TRUE) # ignore non-numerics in Participants column
sessions    <- schedules[schedules$'Participant' %in% participants,c('Participant','Complete')]
sessions    <- rename(sessions,c('Participant'='participant','Complete'='sessions'))
sessions[,'participant'] <- sapply(sessions[,'participant'], as.numeric)
rt    <- merge(sessions,rt,by='participant')
rt    <- rt[with(rt, order(participant)), ]
grs   <- merge(sessions,grs,by='participant')
grs   <- grs[with(grs, order(participant)), ]
panas <- merge(sessions,panas,by='participant')
panas <- panas[with(panas, order(participant)), ]

format_grs <- function(x) {
  x['p']   <- pvalue(x['p'])
  x['pnd'] <- sprintf("%0.2f",as.numeric(x['pnd']))
  x['a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['mean_a']),as.numeric(x['sd_a']))
  x['b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['mean_b']),as.numeric(x['sd_b']))
  x
}

format_rt <- function(x) {
  x['i_p']   <- pvalue(x['i_p'])
  x['i_pnd'] <- sprintf("%0.2f",as.numeric(x['i_pnd']))
  x['i_a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['i_mean_a']),as.numeric(x['i_sd_a']))
  x['i_b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['i_mean_b']),as.numeric(x['i_sd_b']))
  x['n_p']   <- pvalue(x['n_p'])
  x['n_pnd'] <- sprintf("%0.2f",as.numeric(x['n_pnd']))
  x['n_a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['n_mean_a']),as.numeric(x['n_sd_a']))
  x['n_b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['n_mean_b']),as.numeric(x['n_sd_b']))
  x
}

format_panas <- function(x) {
  x['pa_p']   <- pvalue(x['pa_p'])
  x['pa_pnd'] <- sprintf("%0.2f",as.numeric(x['pa_pnd']))
  x['pa_a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['pa_mean_a']),as.numeric(x['pa_sd_a']))
  x['pa_b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['pa_mean_b']),as.numeric(x['pa_sd_b']))
  x['na_p']   <- pvalue(x['na_p'])
  x['na_pnd'] <- sprintf("%0.2f",as.numeric(x['na_pnd']))
  x['na_a']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['na_mean_a']),as.numeric(x['na_sd_a']))
  x['na_b']   <- sprintf("%0.2f(%0.2f)",as.numeric(x['na_mean_b']),as.numeric(x['na_sd_b']))
  x['d_p']    <- pvalue(x['d_p'])
  x['d_pnd']  <- sprintf("%0.2f",as.numeric(x['d_pnd']))
  x['d_a']    <- sprintf("%0.2f(%0.2f)",as.numeric(x['d_mean_a']),as.numeric(x['d_sd_a']))
  x['d_b']    <- sprintf("%0.2f(%0.2f)",as.numeric(x['d_mean_b']),as.numeric(x['d_sd_b']))
  x
}

# LaTeX table wording
meta_label <- "${p}$$_{meta}$\\footnote{\\textcite{onghena_customization_2005}}"

## PANAS table
p_head     <- "${p}$\\footnote{\\label{randp1}${p}$ value from randomisation test \\parencite{bulte_r_2008}}"
p_head_ref <- "${p}$\\textsuperscript{\\ref{randp1}}"
results <- apply(panas,1,format_panas)
results <- t(results)
results <- subset(results, select=c(participant,sessions,pa_a,pa_b,pa_p,pa_pnd,na_a,na_b,na_p,na_pnd,d_a,d_b,d_p,d_pnd))
strCaption <- paste0("I-PANAS-SF+sad+depressed inferentials")
print(xtable(results, caption=strCaption, label="panas", align=c('c','c','c','l','l','r','r@{\\hspace{2em}}','l','l','r','r@{\\hspace{2em}}','l','l','r','r')),
      size="scriptsize",
      include.rownames=FALSE,
      include.colnames=FALSE,
      floating.environment='sidewaystable',
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  "& & \\multicolumn{4}{l}{Positive Affect (PA)} & \\multicolumn{4}{l}{Negative Affect (NA)} & \\multicolumn{4}{l}{'Depression' (items 'sad' and 'depressed')}\\\\\n",
                                          "\\cline{3-14} \n",
                                          "Participant & Sessions & Phase A ${M}$(${SD}$) & Phase B ${M}$(${SD}$)
					  & ", p_head, "& ${PND}$ & Phase A ${M}$(${SD}$) & Phase B
					  ${M}$(${SD}$) & ", p_head_ref, "& ${PND}$ & Phase A ${M}$(${SD}$) &
					  Phase B ${M}$(${SD}$) & ", p_head_ref, " & ${PND}$\\\\\n",
                                          "\\midrule \n"),
				          paste("\\cline{5-5} \\cline{9-9} \\cline{13-13}\n","& & &", meta_label, " & ", meta_p['pa'], "& & & & ", meta_p['na'],"& & & & ", meta_p['d'],"\\\\\n",
						  "\\bottomrule \n"))
				    )
		      )

## RT table
p_head     <- "${p}$\\footnote{\\label{randp2}${p}$ value from randomisation test \\parencite{bulte_r_2008}}"
p_head_ref <- "${p}$\\textsuperscript{\\ref{randp2}}"
results <- apply(rt,1,format_rt)
results <- t(results)
results <- subset(results, select=c(participant,sessions,i_a,i_b,i_p,i_pnd,n_a,n_b,n_p,n_pnd))
strCaption <- paste0("RT inferentials")
print(xtable(results, caption=strCaption, label="rt", align=c('c','c','c','l','l','r','r','l','l','r','r')),
      size="footnotesize",
      include.rownames=FALSE,
      include.colnames=FALSE,
      floating.environment='sidewaystable',
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  "& & I words & & & & N words\\\\\n",
                                          "\\cline{3-8} \n",
					  "& & A & B & & & A & B\\\\\n",
                                          "\\cline{3-10} \n",
                                          "Participant & Sessions &
					  ${M}$(${SD}$) & ${M}$(${SD}$) & ",
					  p_head, " & ${PND}$ & ${M}$(${SD}$) &
					  ${M}$(${SD}$) & ~", p_head_ref, " & ${PND}$\\\\\n",
                                          "\\midrule \n"),
				          paste("\\cline{5-5} \\cline{9-9}
						\n","& & &", meta_label, " & ",
						meta_p['i'], "& & & & ",
						meta_p['n'], "\\\\\n",
                                          "\\bottomrule \n"))
				    )
		      )

## GRS table
p_head     <- "${p}$\\footnote{\\label{randp3}${p}$ value from randomisation test \\parencite{bulte_r_2008}}"
p_head_ref <- "${p}$\\textsuperscript{\\ref{randp3}}"
results <- apply(grs,1,format_grs)
results <- t(results)
results <- subset(results, select=c(participant,sessions,a,b,p,pnd))
strCaption <- paste0("GRS inferentials")
print(xtable(results, caption=strCaption, label="grs", align=c('c','c','c','l','l','r','r')),
      size="footnotesize",
      include.rownames=FALSE,
      include.colnames=FALSE,
      floating.environment='sidewaystable',
      caption.placement="top",
      hline.after=NULL,
      add.to.row = list(pos = list(-1, nrow(results)),
                        command = c(paste("\\toprule \n",
					  "& & A & B \\\\\n",
                                          "\\cline{3-4} \n",
                                          "Participant & Sessions & ${M}$(${SD}$) & ${M}$(${SD}$) & ", p_head, "& ${PND}$\\\\\n",
                                          "\\midrule \n"),
				          paste("\\cline{5-5} \n","& & &", meta_label, " & ", meta_p['grs'], "\\\\\n",
                                          "\\bottomrule \n"))
				    )
		      )
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

#mbd()

make_schedule <- function() {
  #quantity(design="AB",MT=15,limit=6)
  #[1] 4
  #cat("design = ",design,", mt = ",mt,", limit = ",limit,"\n", sep='')
  #cat("transitions = ",quantity(design=design,MT=mt,limit=limit),"\n", sep='')
  schedule <- selectdesign(design=design,MT=mt,limit=limit)
  cat(schedule,"\n")
}
