#!/usr/bin/Rscript

# ---- iwords ----

options(error=traceback)
library(xtable)

source('/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe/constants.r')
l <- length(participants)

data <-
  data.frame(duration=1:l,p1=1:l,p2=1:l,p3=1:l,p4=1:l,p5=1:l,p6=1:l,i_freq=1:l,i_imag=1:l,i_fam=1:l,i_valence=1:l,p_freq=1:l,p_imag=1:l,p_fam=1:l,p_valence=1:l,row.names=participants)
for (participant in participants) {
  p_f   <- paste(datadir,participant,'/','p',participant,'.csv',sep='')
  df    <- read.csv(p_f,header=TRUE,as.is=TRUE)
  df    <- head(df,n = 10L)
  words <- foo <- df[1,2:13]
  odd   <- foo[ c(TRUE,FALSE) ]
  even  <- foo[ c(FALSE,TRUE) ]
  pairs <- mapply(function(o,e) { sprintf("%s-%s",o,e) }, odd, even)
  data[toString(participant),2:7] <- pairs
}

strCaption <- paste0("I-word characteristics")
latex.tab <- xtable(data, caption=strCaption, label="p_iwords", align=c('c','l','c','c','c','c','c','c','l','l','l','l','l','l','l','l'))
{
  sink('/dev/null')
  table = print(latex.tab,
	size="tiny",
	include.rownames=TRUE,
	include.colnames=FALSE,
	caption.placement="top",
	hline.after=NULL,
	add.to.row = list(pos = list(-1, nrow(data)),
			  command = c(paste("\\toprule \n",
					    "& & & & & & & & \\multicolumn{4}{l}{I-words} ${M}$(${SD}$) & \\multicolumn{4}{l}{Pairs} ${M}$(${SD}$)\\\\\n",
					    "\\cline{9-12} \n",
					    "Duration & 1 & 2 & 3 & 4 & 5 & 6 & Frequency & Imageability & Familiarity & Valence & Frequency & Imageability & Familiarity & Valence \\\\\n\\midrule \n"),
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
