library (wordcloud)
library (tm)
library (RISmed)
library (cluster) 

query <- 'cancer'
query_level2 <- EUtilsSummary(query, retmax=100, mindate=2016, maxdate=2018)
query_level3 <- EUtilsGet(query_level2)
class(query_level3)

#Retrieval of abstracts from PubMed
pubmed_data <- data.frame('Abstract'= AbstractText(query_level3))

#Storage of retrieved abstract in the form of individual text document in a directory called Corpus
for (Abs in 1:1000) 
{
doc1 <- data.frame(pubmed_data[Abs, ])
doc2 <- file.path("/Users/vimal/Machine-learning/corpus/", paste0("Abs", Abs, ".txt"))
write.table(doc1, file = doc2, sep = "", row.names = FALSE, col.names = FALSE, quote = FALSE,      
            append = FALSE)
}

#Setting of Directory for Text Mining 
source <-DirSource("/Users/vimal/Machine-learning/corpus") 
testdoc <- Corpus(source)

#Removal of Stop words
testdoc1 <- tm_map(testdoc, removeWords, c("may","are","use","can","the", "then", "this", "is", 
                                 "a", "well", stopwords("english")))

#Removal of whitespace, stemming of words to its root word, removal of numbers
testdoc2 <- TermDocumentMatrix (testdoc1, control = list(tokenize=scan_tokenizer,  stopwords =  
                                 TRUE,  removePunctuation = TRUE,  stripWhitespace = TRUE,  
                                 stemming = TRUE,  removeNumbers= TRUE))

testdoc3 <- as.matrix(testdoc2)
testdoc4 <- sort(rowSums(testdoc3),decreasing=TRUE)
testdoc5 <- data.frame(word= names(testdoc4),freq=testdoc4)
head(testdoc5, 10)

#Association found for the term infect with other term in document corpus 
findAssocs(x=testdoc2, term="cancer", corlimit=0.6)

# Construction of the word cloud
set.seed(1234)
wordcloud(words = testdoc5$word, freq = testdoc5$freq, min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))

#Creation of cluster of words using Hierarchical clustering technique
# removal of Sparse Terms
testdoc5 <-removeSparseTerms(testdoc2, 0.70) 

#Conversion of Term Document Matrix to normal matrix
c1 <- as.matrix(testdoc5)

#Calculation of distances 
c2 <- dist(c1)
c3 <- hclust(c2, method="ward.D")  

#Dendogram 
plot(c3, hang=-1) 