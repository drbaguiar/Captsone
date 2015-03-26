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

#Subset the data
#subsamfile("en_US.news.txt","newsout.txt",20)
#subsamfile("en_US.blogs.txt","blogout.txt",20)
#subsamfile("en_US.twitter.txt","twitterout.txt",20)

setwd("G:/DataCapstone")
library(tm)

# Load the text
txt <- paste(readLines("newsout.txt"),readLines("twitterout.txt"),readLines("blogout.txt"))

# Create a corpus
corpus <- Corpus(VectorSource(txt))

# Clean the coorpus
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
corpus <- tm_map(corpus, content_transformer(stemDocument))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))

# create a tdm
tdm <- TermDocumentMatrix(corpus)

# find words occurring 10,000, 20,000, 30,000, and 40,000 times
findFreqTerms(tdm, 10000)
findFreqTerms(tdm, 20000)
findFreqTerms(tdm, 30000)
findFreqTerms(tdm, 40000)

# remove sparse terms
sparse <- removeSparseTerms(tdm, sparse=0.97)
sparse <- as.data.frame(inspect(sparse))
sparse.scale <- scale(sparse)

# Create N grams
library(RWeka)
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
UnigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

trigram <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
bigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
d <- dist(sparse.scale, method = "euclidean")