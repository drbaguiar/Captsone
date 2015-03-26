fsample <-
  function(fname, n, seed, header=FALSE, reader = readLines)
  {
    set.seed(seed)
    con <- file(fname, open="r")
    hdr <- if (header) {
      readLines(con, 1L)
    } else character()
    
    buf <- readLines(con, n)
    n_tot <- length(buf)
    
    repeat {
      txt <- readLines(con, n)
      if ((n_txt <- length(txt)) == 0L)
        break
      
      n_tot <- n_tot + n_txt
      n_keep <- rbinom(1, n_txt, n_txt / n_tot)
      if (n_keep == 0L)
        next
      
      keep <- sample(n_txt, n_keep)
      drop <- sample(n, n_keep)
      buf[drop] <- txt[keep]
    }
    
    reader(textConnection(c(hdr, buf)))
  }

# extracts a subsample of records from a file

# assumes the goal is to avoid reading the entire file into 
# memory at once; here, only 1 line will be in memory 
# at any time

# also assumes the total number of records in the file is 
# not known (on # Linux/Mac, obtain as output of wc -l infile, 
# but may take a long time)

# every k-th record will extracted

# arguments:
#
#   infile:  name of input file, quoted
#   outfile:  name of output file, quoted
#   k:  every k-th record of infile will be extracted
#   header:  TRUE means infile has a header record

# value:  number of records extracted 

subsamfile <- function(infile,outfile,k,header=T) {
  ci <- file(infile,"r")
  co <- file(outfile,"w")
  if (header) {
    hdr <- readLines(ci,n=1)
    writeLines(hdr,co)
  }
  recnum = 0
  numout = 0
  while (TRUE) {
    inrec <- readLines(ci,n=1)
    if (length(inrec) == 0) {  # end of file?
      close(co) 
      return(numout)
    }
    recnum <- recnum + 1
    if (recnum %% k == 0) {
      numout <- numout + 1
      writeLines(inrec,co)
    }
  }
}

setwd("G:/DataCapstone")

#Subset the data
#subsamfile("en_US.news.txt","newsout.txt",20)
#subsamfile("en_US.blogs.txt","blogout.txt",20)
#subsamfile("en_US.twitter.txt","twitterout.txt",20)

#data <- fsample("en_US.news.txt",2000,1234)
#rm(list=ls())

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(openNLP)
library(RWeka)
library(qdap)

##Set variables used 
min_freq=25
max_freq=1000000
max_words=100
sparse_terms_nbr = .95  # Enter number as a decimal Higer removes less
assoc_word = "data"
correlation = .45

#Get the data
datanews <- readLines("newsout.txt")
datablogs <- readLines("blogout.txt")
datatwitter <- readLines("twitterout.txt")

##Convert to corpus
txtnews<-VectorSource(datanews)
txtblogs <-VectorSource(datablogs)
txttwitter<-VectorSource(datatwitter)

txt.corpus.news <-Corpus(txtnews)
txt.corpus.blogs <-Corpus(txtblogs)
txt.corpus.twitter <-Corpus(txttwitter)

rm(datanews)
rm(datablogs)
rm(datatwitter)

rm(txtnews)
rm(txtblogs)
rm(txttwitter)

##Clean up the text
##Conversion to lower case by:
txt.corpus.news<- tm_map(txt.corpus.news, content_transformer(tolower))
txt.corpus.blogs<- tm_map(txt.corpus.blogs, content_transformer(tolower))
txt.corpus.twitter<- tm_map(txt.corpus.twitter, content_transformer(tolower))

##Remove Numbers
txt.corpus.news<- tm_map(txt.corpus.news, removeNumbers)
txt.corpus.blogs<- tm_map(txt.corpus.blogs, removeNumbers)
txt.corpus.twitter<- tm_map(txt.corpus.twitter, removeNumbers)

##Remove Punctuation
txt.corpus.news<- tm_map(txt.corpus.news, removePunctuation)
txt.corpus.blogs<- tm_map(txt.corpus.blogs, removePunctuation)
txt.corpus.twitter<- tm_map(txt.corpus.twitter, removePunctuation)

##Removal of stopwords by:
txt.corpus.news<- tm_map(txt.corpus.news, removeWords, stopwords("english"))
txt.corpus.blogs<- tm_map(txt.corpus.blogs, removeWords, stopwords("english"))
txt.corpus.twitter<- tm_map(txt.corpus.twitter, removeWords, stopwords("english"))

##Stem the Document
#txt.corpus<- tm_map(txt.corpus, stemDocument)

##Strip white spaces from the Document
txt.corpus.news<- tm_map(txt.corpus.news, stripWhitespace)
txt.corpus.blogs<- tm_map(txt.corpus.blogs, stripWhitespace)
txt.corpus.twitter<- tm_map(txt.corpus.twitter, stripWhitespace)

##Inspect the corpus
#inspect(txt.corpus.news)
#inspect(txt.corpus.blogs)
#inspect(txt.corpus.twitter)

##Ready to Analyze the data
tdm.news <-TermDocumentMatrix(txt.corpus.news)
tdm.blogs <-TermDocumentMatrix(txt.corpus.blogs)
tdm.twitter <-TermDocumentMatrix(txt.corpus.twitter)

#inspect(tdm.news)
#inspect(tdm.blogs)
#inspect(tdm.twitter)

tdm.news <- removeSparseTerms(tdm.news, sparse_terms_nbr)
tdm.blogs <- removeSparseTerms(tdm.blogs, sparse_terms_nbr)
tdm.twitter <- removeSparseTerms(tdm.twitter, sparse_terms_nbr)

rm(txt.corpus.news)
rm(txt.corpus.blogs)
rm(txt.corpus.twitter)

# Establish Frequence  can set Low freq and high freq
findFreqTerms(x=tdm.news, lowfreq=min_freq, highfreq=max_freq)
findFreqTerms(x=tdm.blogs, lowfreq=min_freq, highfreq=max_freq)
findFreqTerms(x=tdm.twitter, lowfreq=min_freq, highfreq=max_freq)

# make a bagofwords and remove duplicates
bagofwords <- c(findFreqTerms(x=tdm.news, lowfreq=min_freq, highfreq=max_freq),findFreqTerms(x=tdm.blogs, lowfreq=min_freq, highfreq=max_freq),findFreqTerms(x=tdm.twitter, lowfreq=min_freq, highfreq=max_freq))
duplicatedwords <- unique(bagofwords[duplicated(bagofwords)]) #make a list of duplicated words
bagofwordsunique <- unique(bagofwords) #remove duplicated words

#words.twitter <- as.data.frame(findFreqTerms(x=tdm.twitter, lowfreq=min_freq, highfreq=max_freq))
write.csv(bagofwords, "bagofwords.csv", row.names=FALSE)
write.table(bagofwords, "bagofwords.txt", row.names=FALSE, sep="\t")
write.csv(bagofwordsunique, "bagofwordsunique.csv", row.names=FALSE)
write.table(bagofwordsunique, "bagofwordsunique.txt", row.names=FALSE, sep="\t")
write.csv(duplicatedwords, "bagofwordsduplicate.csv", row.names=FALSE)
write.table(duplicatedwords, "bagofwordsduplicate.txt", row.names=FALSE, sep="\t")

sum(duplicated(bagofwords))

# Find assocaitions with a selected word
#findAssocs(tdm, assoc_word, correlation)

# Cluster 
#news.df.scale <- scale(news.df)
#d <- dist(news.df.scale, method = "euclidean") # distance matrix
#fit <- hclust(d, method="ward.D")
#plot(fit) # display dendogram?
#groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
#rect.hclust(fit, k=5, border="red")