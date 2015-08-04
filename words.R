#!/usr/bin/Rscript

options(width = 140)

# see #R transcript
  # note duration to get to this point (from cluelessness!)
  # most roads (questions) lead to Stack Overflow ;-)

# things I learnt
  # comments
  # shebang setup
  # no line terminators

words_d <- '../../method/materials/words/n-words/'
words_f <- paste(words_d,'words.csv',sep='') # SAWL subset with only negative and neutral words
## filter_sawl was the initial pre-processing (only needed once) for generating N-words
filter_sawl <- function() {
  # words.csv is a subset of SAWL with only negative and neutral words
  sawl <- read.csv(words_f, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

  # can now get at fields
  # valence
    # 3 mean
    # 4 SD
    # 5 category (negative/neutral)
  colnames(sawl) <- c("word","lexical class","valence.mean","valence.SD","valence.category","arousal.mean","arousal.SD","familiarity.mean","familiarity.SD","AoA.mean","AoA.SD","imageability.mean","imageability.SD","concreteness","characters","phonemes","syllables","frequency")

  # sort
  sawl <- sawl[with(sawl, order(valence.category,valence.mean)), ]
  foo <- sawl[,c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  write.table(foo,file="negative_neutral_by_valence.csv",sep=",")

  rows <- nrow(sawl)
  # 66 most negative/neutral, 60 for normal circumstances and 6 "spare" in case of i-word overlap
  #nwords <- sawl[c(1:66,(rows-65):rows),c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  nwords <- sawl[c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")]
  write.table(nwords,file="nwords_raw.csv",sep=",")
}

#filter_sawl()

# output from filter_sawl with subsequent manual adjustment to match word frequencies
#nwords <- read.csv('nwords.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
nwords <- read.csv(paste(words_d,'manually_paired_nwords.csv',sep=''), header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
colnames(nwords) <- c("word","valence.category","valence.mean","arousal.mean","imageability.mean","familiarity.mean","frequency","syllables","characters")
nwords

# Yang et al. (2014) use equivalent 1-way ANOVA (F)

# p < .05
# Student's t-test: var.equal=TRUE
t.test(valence.mean ~ valence.category, nwords, var.equal=TRUE)

# Yang et al. (2014)
# p > .05
t.test(arousal.mean ~ valence.category, nwords, var.equal=TRUE)
t.test(imageability.mean ~ valence.category, nwords, var.equal=TRUE)
t.test(familiarity.mean ~ valence.category, nwords, var.equal=TRUE)

# plus ...
# p > .05
t.test(frequency ~ valence.category, nwords, var.equal=TRUE)
t.test(syllables ~ valence.category, nwords, var.equal=TRUE)
t.test(characters ~ valence.category, nwords, var.equal=TRUE)
