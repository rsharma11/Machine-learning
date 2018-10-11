install.packages("RISmed")
library(RISmed)
search_topic <- 'cancer'
search_query <- EUtilsSummary(search_topic, retmax = 100, mindate = 2012, maxdate = 2012)
summary(search_query)