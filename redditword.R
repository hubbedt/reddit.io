

redditcombo<- bind_rows(reddit1, reddit2)
write.csv(redditcombo, "~/Reddit Gaming/redditcombo.csv")

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidyverse)

getOption("max.print")
options(max.print=3000)

wordcount<- readLines(file.choose("~/Reddit Gaming/reddittext.txt"))

docs <- Corpus(VectorSource(wordcount))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("game", "games","hey","can","may","iâ€™m","also","try","want","still","now")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

tm::inspect(dtm)







set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10, "Paired"))


############## for "like" wordcloud

wordcount2<- readLines(file.choose("~/Reddit Gaming/liketext.txt"))

doc2 <- Corpus(VectorSource(wordcount2))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
doc2 <- tm_map(doc2, toSpace, "/")
doc2 <- tm_map(doc2, toSpace, "@")
doc2 <- tm_map(doc2, toSpace, "\\|")

# Convert the text to lower case
doc2 <- tm_map(doc2, content_transformer(tolower))
# Remove numbers
doc2 <- tm_map(doc2, removeNumbers)
# Remove english common stopwords
doc2 <- tm_map(doc2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
doc2 <- tm_map(doc2, removeWords, c("like","game","games")) 
# Remove punctuations
doc2 <- tm_map(doc2, removePunctuation)
# Eliminate extra white spaces
doc2 <- tm_map(doc2, stripWhitespace)


doc2 <- doc2 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
doc2 <- tm_map(doc2, content_transformer(tolower))
doc2 <- tm_map(doc2, removeWords, stopwords("english"))

dtm2 <- TermDocumentMatrix(doc2)
m <- as.matrix(dtm2)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 999)

tm::inspect(dtm2)



set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, scale=c(4,.75),
          colors=brewer.pal(8, "Dark2"))




