#!/usr/bin/Rscript

# ---- iwords ----

options(error=traceback)
library(xtable)

source('/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/constants.r')
l <- length(participants)

m_sd <- function(df,row,s) {
  x <- df[row,s]
  x <- as.numeric(x)
  sprintf("%0.2f(%0.2f)",mean(x),sd(x))
}
pairs <- data.frame(duration=1:l,p1=1:l,p2=1:l,p3=1:l,p4=1:l,p5=1:l,p6=1:l,row.names=participants)
stats <- data.frame(i_freq=1:l,i_imag=1:l,i_fam=1:l,i_valence=1:l,p_freq=1:l,p_imag=1:l,p_fam=1:l,p_valence=1:l,row.names=participants)
v <- data.frame(w1=1:l,w2=1:l,w3=1:l,w4=1:l,w5=1:l,w6=1:l,row.names=participants)
v['5',] <- c(1,3,-1,-1,-3,-2)
v['6',] <- c(1,-2,-3,-3,0,0)
v['7',] <- c(-2,-2,-1,-1,0,0)
v['8',] <- c(-3,-2,-2,-1,-2,-1)
v['9',] <- c(1,1,-3,-3,-3,-3)
v['10',] <- c(-3,-3,-1,-3,-3,-3)
v['11',] <- c(3,-2,0,-2,-3,-1)
v['12',] <- c(3,-3,-2,-1,-2,3)
v['15',] <- c(-3,-3,-3,-3,-3,-3)
v['16',] <- c(-3,-3,-3,-3,-2,-3)
v['18',] <- c(-2,-1,1,-2,-3,-3)
v['19',] <- c(-3,2,-1,3,-2,-3)

for (participant in participants) {
  p_f   <- paste(datadir,participant,'/','p',participant,'.csv',sep='')
  df    <- read.csv(p_f,header=TRUE,as.is=TRUE)
  df    <- head(df,n = 10L)
  p     <- toString(participant)

  ## word pairs
  words <- foo <- df[1,2:13]
  odd   <- foo[ c(TRUE,FALSE) ]
  even  <- foo[ c(FALSE,TRUE) ]
  pairs[p,2:7] <- mapply(function(o,e) { sprintf("\\textsf{%s}---\\textsf{%s}",o,e) }, odd, even)

  ### descriptives
  ## pairs
  s <- seq(3,13,by=2)
  stats[p,'p_valence'] <- m_sd(df,2,s)  # valence
  stats[p,'p_freq']    <- m_sd(df,4,s)  # frequency
  stats[p,'p_imag']    <- m_sd(df,9,s)  # imageability
  stats[p,'p_fam']     <- m_sd(df,10,s) # familiarity
  ## words
  s <- seq(2,12,by=2)
  stats[p,'i_freq']    <- m_sd(df,4,s)  # frequency
  stats[p,'i_imag']    <- m_sd(df,9,s)  # imageability
  stats[p,'i_fam']     <- m_sd(df,10,s) # familiarity
  stats[p,'i_valence'] <- m_sd(v,p,1:6) # valence
}

missing <- function(pp,p) {
  pp <- pp + 1
  s <- pairs[toString(p),pp]
  pairs[toString(p),pp] <<- sub('---','\\tabfnm{a}{---}',s,fixed=TRUE)
}

fnm <- function(p,d,fn) {
  s <- stats[toString(p),d]
  stats[toString(p),d] <<- paste(s,'\\tabfnm{',fn,'}',sep='')
}

# 5
pairs['5','duration'] <- '1 month 3 days'
foo <- lapply(3,missing,p=5)
fnm(5,'i_freq','a')
fnm(5,'i_imag','b')
fnm(5,'i_fam','b')
fnm(5,'p_imag','c')
fnm(5,'p_fam','c')
# 6
pairs['6','duration'] <- '2 years'
fnm(6,'i_imag','c')
fnm(6,'i_fam','c')
# 7
pairs['7','duration'] <- '1 year'
fnm(7,'i_imag','c')
fnm(7,'i_fam','c')
# 8
pairs['8','duration'] <- '8 years'
fnm(8,'i_imag','c')
fnm(8,'i_fam','c')
# 9
pairs['9','duration'] <- '6 months'
fnm(9,'i_imag','c')
fnm(9,'i_fam','c')
# 10
pairs['10','duration'] <- '3 months'
foo <- lapply(3,missing,p=10)
foo <- lapply(5,missing,p=10)
fnm(10,'i_freq','a')
fnm(10,'i_imag','b')
fnm(10,'i_fam','b')
fnm(10,'p_imag','c')
fnm(10,'p_fam','c')
# 11
pairs['11','duration'] <- '2 months'
foo <- lapply(4,missing,p=11)
fnm(11,'i_freq','a')
fnm(11,'i_imag','b')
fnm(11,'i_fam','b')
fnm(11,'p_imag','c')
# 12
foo <- lapply(4,missing,p=12)
pairs['12','duration'] <- '1 year 1 month'
fnm(12,'i_freq','a')
fnm(12,'i_imag','b')
fnm(12,'i_fam','b')
# 15
pairs['15','duration'] <- '1 year 6 months'
fnm(15,'i_imag','c')
fnm(15,'i_fam','c')
fnm(15,'p_imag','c')
fnm(15,'p_fam','c')
# 16
pairs['16','duration'] <- '21 years'
fnm(16,'i_imag','c')
fnm(16,'i_fam','c')
fnm(16,'p_imag','c')
fnm(16,'p_fam','c')
# 18
pairs['18','duration'] <- '8 months 19 days'
fnm(18,'i_imag','c')
fnm(18,'i_fam','c')
fnm(18,'p_imag','c')
fnm(18,'p_fam','c')
# 19
pairs['19','duration'] <- '2 months 25 days'
foo <- lapply(2,missing,p=19)
fnm(19,'i_freq','a')
fnm(19,'i_imag','b')
fnm(19,'i_imag','c')
fnm(19,'i_fam','b')
fnm(19,'i_fam','c')

options(xtable.sanitize.text.function=identity)
strCaption <- paste0("Words chosen by each participant as likely to initiate rumination over their current concernt (I-words) and matched pairs selected by the experimenter (\\textsf{WORD}---\\textsf{PAIR}).")
latex.tab <- xtable(pairs, caption=strCaption, label="p_iwords", align=c('c','l','c','c','c','c','c','c'))
{
  sink('/dev/null')
  table = print(latex.tab,
	size="tiny",
	include.rownames=TRUE,
	include.colnames=FALSE,
	caption.placement="top",
	hline.after=NULL,
	add.to.row = list(pos = list(-1, nrow(pairs)),
			  command = c(paste("\\toprule \n",
					    "Participant & Concern duration\\tabfnm{1} & Pair 1 & Pair 2 & Pair 3 & Pair 4 & Pair 5 & Pair 6\\\\\n\\midrule \n"),
					    "\\bottomrule \n")
				      )
			)
  sink()
}

# footnotes
table <- sub("\\begin{table}","\\begin{sidewaystable}[!ph]\n\\begin{threeparttable}\n",table,fixed=TRUE)
fn    <- paste("\\begin{tablenotes}[para,flushleft]\n{\\footnotesize \\tabfnt{1}Answer to the question \\enquote{How long has this been a difficulty for you?}at the start of the study. \\tabfnt{2}Female and male names redacted with equivalent number of \\textsf{F} or \\textsf{M} characters respectively. \\tabfnt{a}I-word not in database.}\n\\end{tablenotes}\n\\end{threeparttable}\n\\end{sidewaystable}",sep='')
table <- sub("\\end{table}",fn,table,fixed=TRUE)
cat(table)

strCaption <- paste0("Characteristics of each participant's I-words and matched pairs.")
latex.tab <- xtable(stats, caption=strCaption, label="p_iwords", align=c('c','l','l','l','l','l','l','l','l'))
{
  sink('/dev/null')
  table = print(latex.tab,
	size="footnotesize",
	include.rownames=TRUE,
	include.colnames=FALSE,
	caption.placement="top",
	hline.after=NULL,
	add.to.row = list(pos = list(-1, nrow(stats)),
			  command = c(paste("\\toprule \n",
					    "& \\multicolumn{4}{l}{I-words ${M}$(${SD}$)} & \\multicolumn{4}{l}{Pairs ${M}$(${SD}$)}\\\\\n",
					    "\\cline{2-9} \n", "Participant & Frequency\\tabfnm{1} & Imageability\\tabfnm{2} & Familiarity\\tabfnm{2} & Valence\\tabfnm{3} & Frequency\\tabfnm{1} & Imageability\\tabfnm{2} & Familiarity\\tabfnm{2} & Valence\\tabfnm{4} \\\\\n\\midrule \n"),
					    "\\bottomrule \n")
				      )
			)
  sink()
}

# footnotes
table <- sub("\\begin{table}","\\begin{sidewaystable}[!ph]\n\\begin{threeparttable}\n",table,fixed=TRUE)
fn <- paste("\\begin{tablenotes}[para,flushleft]\n{\\footnotesize \\tabfnt{1}Frequency norms from \\textcite{kucera_computational_1967}. \\tabfnt{2}Merged from three sets of norms \\parencite{wilson_mrc_1988}. \\tabfnt{3}Participant's subjective valence rating using procedure from \\textcite{citron_how_2014}. \\tabfnt{4} Valence norms from \\textcite{bradley_affective_1999} \\tabfnt{a}Frequency of 1 assumed for I-words not in database.\\tabfnt{b}Missing values were set to 0 for I-words not in database.\n\\tabfnt{c}Includes one or more 0 values likely to be missing data rather than normative values.}\n\\end{tablenotes}\n\\end{threeparttable}\\end{sidewaystable}\n",sep='')
table <- sub("\\end{table}",fn,table,fixed=TRUE)
cat(table)
