#Installation
install.packages("RISmed")

#Loading the library
library(RISmed)
library(tm)
library(wordcloud)

#Searching the word putting specific constraints
search_topic <- 'deep learning'
search_query <- EUtilsSummary(search_topic, retmax = 1000, mindate = 2017, maxdate = 2018)
summary(search_query)
QueryId(search_query)
records <- EUtilsGet(search_query)
class(records)
str(records)
pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))

#Clean Text Data
deeplearning_corpus <- Corpus(VectorSource(pubmed_data))

#Document Term Matrix
doc.matrix <- TermDocumentMatrix(deeplearning_corpus,
                                 control = list(removePunctuation = TRUE,
                                                stopwords = stopwords('english'),
                                                removeNumbers = TRUE,
                                                tolower = TRUE))

#Convert object to matrix
term.doc.matrix <- as.matrix(doc.matrix)

#Get Word Count
word.freq <- sort(rowSums(term.doc.matrix), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq)

#Create wordcloud
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, 'Dark2'))