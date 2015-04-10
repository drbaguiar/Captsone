library(tm)
library(reshape)
setwd("G:/DataCapstone")
# Function to convert 1 to data frame
dfit <- function (file){
  m <- as.matrix(file)
  v <- sort(rowSums(m),decreasing=TRUE)
  df <- data.frame(freq=v,w1= names(v))
  dfhead(df, 10)
  return (df)
}

load("sparsetdm.Rdata")
load("sparsebigram.Rdata")
load("sparsetrigram.Rdata")
load("sparsefourgram.Rdata")

df<-dfit(sparsetdm)
N1T <- data.frame(freq=df$freq,w1=df$w1)

df<-dfit(sparsebigram)
N2T <- data.frame(cbind(freq=df$freq,colsplit(df$w1,split=" ",names=c("w1","w2"))))

df<-dfit(sparsetrigram)
N3T <- data.frame(cbind(freq=df$freq,colsplit(df$w1,split=" ",names=c("w1","w2","w3"))))

df<-dfit(sparsefourgram)
N4T <- data.frame(cbind(freq=df$freq,colsplit(df$w1,split=" ",names=c("w1","w2","w3","w4"))))

rm(df)
rm(sparsetdm)
rm(sparsebigram)
rm(sparsetrigram)
rm(sparsefourgram)
save(N4T, N3T, N2T, N1T, file="ngramsdata.RData")
rm(N4T)
rm(N3T)
rm(N2T)
rm(N1T)