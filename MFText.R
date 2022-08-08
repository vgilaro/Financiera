

textMF<-read.csv("textoMF.csv",stringsAsFactors = FALSE)


str(textMF)

# Install new packages


library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(tidyverse)

# Create corpus
corpus = VCorpus(VectorSource(textMF))


# Look at corpus
corpus
corpus[[1]]$content


# Convert to lower-case

corpus = tm_map(corpus, content_transformer(tolower))

corpus[[1]]$content

# Remove punctuation and numbers

corpus <- corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

corpus[[1]]$content
gsub("renta","rentas",corpus)
gsub("vpna","vpn",corpus)
gsub("vpnb","vpn",corpus)
gsub("vpnc","vpn",corpus)
gsub("saldo","saldos",corpus)
gsub("descuenta","descuento",corpus)
# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("cada","¿cual", stopwords("spanish")))

corpus[[1]]$content


# Create matrix


frequencies = DocumentTermMatrix(corpus)
matrix <- as.matrix(frequencies) 
words <- sort(colSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
  
wordcloud(words = df$word, freq = df$freq, min.freq = 7,max.words=200, random.order=FALSE, 
          rot.per=0.35,colors =c(brewer.pal(7, "OrRd"), "darkgoldenrod1","tomato","#AD1DA5"),
          family="serif", scale=c(8,.3))

colors = "#AD1DA5"
colors=brewer.pal(8, "Paired")
colors=c("grey80", "darkgoldenrod1","tomato","#AD1DA5")
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
wordcloud(words = df$word, freq = df$freq,scale=c(8,.3),min.freq=6,max.words=200, random.order=F, rot.per=.15, colors=pal, family = "helvetica", font = 3)


wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud2(data=df, size = 0.7, shape = 'pentagon')
wordcloud2(df, size=1.6, color=rep_len( c("green","blue"), nrow(df) ) )
wordcloud2(df, size=1.6, color='random-light', backgroundColor="black", shape = 'cardioid')
