library(RWeka)
library(tm)
setwd("G:/DataCapstone/")

dfit <- function (file){
txt<-scan(file,"char",sep="\n")
# remove any character that is not an alpha-numeric or space
content <- gsub("[^a-zA-Z0-9 ]", "", txt)
rm(txt)
# convert to all lowercase
content = tolower(content)
# remove any extra whitespace and trailing or ending whitespace.
content <- gsub(" +( )", "\\1", content)
content <- gsub("^ *| *$", "", content)
return(content)
}

twitter_content <- dfit("en_US.twitter.txt")
blogs_content <- dfit("en_US.blogs.txt")
news_content <-dfit("en_US.news.txt")
rm(content)

content = c(twitter_content, blogs_content, news_content)

rm(twitter_content)
rm(blogs_content)
rm(news_content)

save(content, file="comb.Rdata")

options(mc.cores=1)
