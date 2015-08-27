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
for (participant in participants) {
  p_f   <- paste(datadir,participant,'/','p',participant,'.csv',sep='')
  df    <- read.csv(p_f,header=TRUE,as.is=TRUE)
  df    <- head(df,n = 10L)
  p     <- toString(participant)

  ## word pairs
  words <- foo <- df[1,2:13]
  odd   <- foo[ c(TRUE,FALSE) ]
  even  <- foo[ c(FALSE,TRUE) ]
  pairs[p,2:7] <- mapply(function(o,e) { sprintf("%s-%s",o,e) }, odd, even)

  ### descriptives
  ## pairs
  s <- seq(3,13,by=2)
  stats[p,'p_valence'] <- m_sd(df,2,s)  # valence
  stats[p,'p_freq']    <- m_sd(df,4,s)  # frequency
  stats[p,'p_imag']    <- m_sd(df,9,s)  # imageability
  stats[p,'p_fam']     <- m_sd(df,10,s) # familiarity
}

strCaption <- paste0("I-word pairs")
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
					    "Participant & Duration & 1 & 2 & 3 & 4 & 5 & 6\\\\\n\\midrule \n"),
					    "\\bottomrule \n")
				      )
			)
  sink()
}

# footnotes
table <- sub("\\begin{table}","\\begin{sidewaystable}[!ph]\n\\begin{threeparttable}\n",table,fixed=TRUE)
fn    <- paste("\\begin{tablenotes}[para,flushleft]\n{\\footnotesize \\tabfnt{a}Footnote a. \\tabfnt{b}Footnote b.}\n\\end{tablenotes}\n\\end{threeparttable}\n\\end{sidewaystable}",sep='')
table <- sub("\\end{table}",fn,table,fixed=TRUE)
cat(table)

strCaption <- paste0("I-word characteristics")
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
					    "\\cline{2-9} \n",
					    "Participant & Frequency & Imageability & Familiarity & Valence & Frequency & Imageability & Familiarity & Valence \\\\\n\\midrule \n"),
					    "\\bottomrule \n")
				      )
			)
  sink()
}

# footnotes
table <- sub("\\begin{table}","\\begin{threeparttable}\n",table,fixed=TRUE)
fn    <- paste("\\begin{tablenotes}[para,flushleft]\n{\\footnotesize \\tabfnt{a}Footnote a. \\tabfnt{b}Footnote b.}\n\\end{tablenotes}\n\\end{threeparttable}\n",sep='')
table <- sub("\\end{table}",fn,table,fixed=TRUE)
cat(table)
