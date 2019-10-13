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

TFIDF<-read.csv("C:/Users/muhau/Downloads/TFIDF_stream (1).csv")
df_clust<-TFIDF%>%select(-c(kalimat_tweet, X))

DBSCAN <-dbscan::dbscan(df_clust, eps=0.1, minPts = 600)
x<-data.frame(df_clust, DBSCAN$cluster)
y<-x[which(x$DBSCAN.cluster!=0),]
silcoef<-silhouette(y$DBSCAN.cluster,dist(y[,-516]))
sil_coef_0.1<-summary(silcoef)$avg.width
sil_coef_0.1

z <-data.frame(df_clust,TFIDF$kalimat_tweet, DBSCAN$cluster)
y<-z[which(z$DBSCAN.cluster!=0),]


clusters<-y%>%select(c(TFIDF.kalimat_tweet, DBSCAN.cluster))
for (i in 1:2){
  docs<-Corpus(VectorSource(clusters$TFIDF.kalimat_tweet[clusters$DBSCAN.cluster==i]))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  win.graph()[i]
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            scale=c(2,0.5), colors=brewer.pal(8, "Dark2"))
} 

