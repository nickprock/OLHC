load("OLHC/data/tweets.rda")
#Tweets201610 <- flu_201610[,c(1,5,8,11)]
#names(flu_related_tweets_location) <- c("Timestamp","TweetId","Text","UserName","ScreenName", "Location")
#Tweets <- flu_related_tweets_location
# estrazione hashtag e mentions
library(tm)
library(stringr)
# estrazione hashtag
hashtag.regex <- "(?<=^|\\s)#\\S+"
# estrazione mention
mention.regex <- "(@[[:alnum:]_]*)"
for (i in 1:length(tweets$text)){
  print(i)
  # estrazione hashtag
  hashtag <- str_extract_all(tweets$text[i], hashtag.regex)
  
  # 1-gram per hashtag
  tweets$hU[i] <- hashtag
  # estrazione mention
  mention <- str_extract_all(tweets$text[i], mention.regex)
  # 1-gram per mention
  tweets$mU[i] <- mention
  # inizio procedura bigram
  hashtag[hashtag=="character(0)"] <-"NA"
  mention[mention=="character(0)"] <-"NA"
  # Remove the first element and add a period at the end
  uni2H <- c(hashtag[[1]][-1], ".")
  uni2M <- c(mention[[1]][-1], ".")
  # Find all bigrams using paste
  bigH<-list(NA)
  bigM<-list(NA)
  # hashtag
  for (j in 1:length(hashtag[[1]])){
    bigH[[1]][j]<-paste(hashtag[[1]][j], uni2H[[j]])
  }
  # mentions
  for (j in 1:length(mention[[1]])){
    bigM[[1]][j]<-paste(mention[[1]][j], uni2M[[j]])
  }
  # insert in data.frame hashtag
  tweets$hB[i] <- bigH
  tweets$hB[[i]] <- tweets$hB[[i]][-length(tweets$hB[[i]])]
  # insert in data.frame mentions
  tweets$mB[i] <- bigM
  tweets$mB[[i]] <- tweets$mB[[i]][-length(tweets$mB[[i]])]
}
########################################################################################
########################################################################################
# remove URLs
tweets$TextClean <- gsub('http\\S+\\s*', '', tweets$text)
# remove emoji
tweets$TextClean <- gsub("(\\\\[[:alnum:]_]*)", '', tweets$TextClean)
tweets$TextClean <- gsub("[^[:graph:]]", " ", tweets$TextClean)
# rimozione hashtag dal testo
tweets$TextClean <- gsub("#\\S+", '', tweets$TextClean)
# rimozione mention
tweets$TextClean <- gsub("@\\S+", '', tweets$TextClean)
# rimozione punteggiatura
tweets$TextClean <- removePunctuation(tweets$TextClean)
# tutto minuscolo
tweets$TextClean <- tolower(tweets$TextClean)
#remove stopword
tweets$TextClean <- removeWords(tweets$TextClean, c(stopwords("en"), stopwords("es"), stopwords("fr"), stopwords("de"),stopwords("it"), "rt", "RT", "the"))
# rimozione spazi superflui inizio e fine stringa
tweets$TextClean <- trimws(tweets$TextClean, which = "both")
# rimozione spazi doppi
tweets$TextClean <- stripWhitespace(tweets$TextClean)
# NOT AVAIBLE
tweets$TextClean[tweets$TextClean==""] <- NA
# Stemming
library(SnowballC)
tweets$TextClean <- wordStem(tweets$TextClean, language = c("en", "es", "fr", "de", "it"))
########################################################################################
########################################################################################
# per tirare fuori i Ngram dal testo pulito
for(i in 1:length(tweets$TextClean)){ 
  print(i)
  # 1-gram 
  uniT<-strsplit(tweets$TextClean[i], " ")
  tweets$wU[i] <- uniT
  # Remove the first element and add a period at the end
  uni2T <- c(uniT[[1]][-1], ".")
  # Find all bigrams using paste
  bigT<-list(NA)
  for (j in 1:length(uniT[[1]])){
    bigT[[1]][j]<-paste(uniT[[1]][j], uni2T[[j]])
  }
  # insert in data.frame
  tweets$wB[i] <- bigT
  tweets$wB[[i]] <- tweets$wB[[i]][-length(tweets$wB[[i]])]
}
for (i in 1:length(tweets$hU)){
  tweets$hU[[i]] <- removePunctuation(tweets$hU[[i]])
  tweets$hU[[i]] <- gsub("(\\\\[[:alnum:]_]*)", '', tweets$hU[[i]])
  tweets$hU[[i]] <- gsub("[^[:graph:]]", "", tweets$hU[[i]])
  tweets$hU[[i]] <- tolower(tweets$hU[[i]])
  tweets$hB[[i]] <- removePunctuation(tweets$hB[[i]])
  tweets$hB[[i]] <- gsub("(\\\\[[:alnum:]_]*)", ' ', tweets$hB[[i]])
  tweets$hB[[i]] <- gsub("[^[:graph:]]", " ", tweets$hB[[i]])
  tweets$hB[[i]] <- tolower(tweets$hB[[i]])
  tweets$mU[[i]] <- removePunctuation(tweets$mU[[i]])
  tweets$mU[[i]] <- gsub("(\\\\[[:alnum:]_]*)", '', tweets$mU[[i]])
  tweets$mU[[i]] <- gsub("[^[:graph:]]", "", tweets$mU[[i]])
  tweets$mU[[i]] <- tolower(tweets$mU[[i]])
  tweets$mB[[i]] <- removePunctuation(tweets$mB[[i]])
  tweets$mB[[i]] <- gsub("(\\\\[[:alnum:]_]*)", ' ', tweets$mB[[i]])
  tweets$mB[[i]] <- gsub("[^[:graph:]]", " ", tweets$mB[[i]])
  tweets$mB[[i]] <- tolower(tweets$mB[[i]])
  tweets$wU[[i]] <- removePunctuation(tweets$wU[[i]])
  tweets$wU[[i]] <- gsub("(\\\\[[:alnum:]_]*)", '', tweets$wU[[i]])
  tweets$wU[[i]] <- gsub("[^[:graph:]]", "", tweets$wU[[i]])
  tweets$wU[[i]] <- tolower(tweets$wU[[i]])
  tweets$wB[[i]] <- removePunctuation(tweets$wB[[i]])
  tweets$wB[[i]] <- gsub("(\\\\[[:alnum:]_]*)", ' ', tweets$wB[[i]])
  tweets$wB[[i]] <- gsub("[^[:graph:]]", " ", tweets$wB[[i]])
  tweets$wB[[i]] <- tolower(tweets$wB[[i]])
}
