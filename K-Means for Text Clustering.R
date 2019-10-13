library(dplyr)
library(tidyr)
library(caret)
library(RColorBrewer)
library(ggplot2)
library(factoextra)
library(NbClust)
library(wordcloud)
library(wordcloud2)
library(tm)
library(cluster)

TFIDF<-read.csv("C:/Users/muhau/Downloads/TFIDF.csv")
df_clust<-TFIDF%>%select(-c(kalimat_tweet, X))

##K-Means Evaluation
Kmeans <- function(data)
{
  sil_coef <- matrix()
  for (k in 2:10)
  {
    set.seed(12)
    KMeans <- kmeans(data, centers = k, nstart = 100)
    silcoef <- silhouette(KMeans$cluster, dist(data))
    sil_coef[k] <- summary(silcoef)$avg.width
  }
  win.graph()
  plot(sil_coef, xlab = "k", type = "b")
  win.graph()
  list(Sil_Coef = sil_coef)
}
Kmeans(df_clust)

set.seed(12)
kmeans<-kmeans(df_clust, 10, nstart = 20)
kmeans$size
silcoef <- silhouette(kmeans$cluster, dist(df_clust))
summary(silcoef)$avg.width

TFIDF$clust<-kmeans$cluster
clusters<-TFIDF%>%select(c(kalimat_tweet, clust))
for (i in 1:10){
  docs<-Corpus(VectorSource(clusters$kalimat_tweet[clusters$clust==i]))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  win.graph()[i]
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            scale=c(2,0.5), colors=brewer.pal(8, "Dark2"))
} 


