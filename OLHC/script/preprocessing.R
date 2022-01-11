preprocessing <- function(myTweetsTXT){
  # carico il file già pulito
  #Tweets <- read.delim("~/Documents/R/Bigram/Tweets.txt", encoding="UTF8", header=FALSE, stringsAsFactors=FALSE)
  Tweets <- read.delim(myTweetsTXT, encoding="UTF8", header=FALSE, stringsAsFactors=FALSE)
  # nome alle colonne, manca la 13 nell'e-mail la nomino y.a.Count (yet another Count)
  names(Tweets) <- c("TweetId","UserId","Timestamp","Lat.Lon","Address","BoxCoordinates",
                     "Hashtag","Mention", "InReplyToStatusId","InReplyToUserId","isRetweet",
                     "RetweetCount","y.a.Count","Source","Text")
  # la mia parte di preprocessing
  library(tm)
  library(stringr) 
  # tolower hashtag e mentions
  Tweets$Hashtag <- tolower(Tweets$Hashtag)
  Tweets$Mention <- tolower(Tweets$Mention)
  ########################################################################################
  ########################################################################################
  # per tirare fuori i Ngram da hashtag e mentions
  for(i in 1:length(Tweets$Hashtag)){ #questo indice è uguale per entrambi
    # 1-gram per hashtag
    uniH<-str_extract_all(Tweets$Hashtag[i], "text(:[[:alnum:]_]*)")
    Tweets$hU[i] <- uniH
    Tweets$hU[[i]] <- sub("^text:","",Tweets$hU[[i]]) # elimina caratteri superflui
    uniH[uniH=="character(0)"]<-"NA"
    # 1-gram per mentions
    uniM<-str_extract_all(Tweets$Mention[i], "screen_name(:[[:alnum:]_]*)")
    Tweets$mU[i] <- uniM
    Tweets$mU[[i]] <- sub("^screen_name:","",Tweets$mU[[i]]) # elimina caratteri superflui
    uniM[uniM=="character(0)"]<-"NA"
    # Remove the first element and add a period at the end
    uni2H <- c(uniH[[1]][-1], ".")
    uni2M <- c(uniM[[1]][-1], ".")
    # Find all bigrams using paste
    bigH<-list(NA)
    bigM<-list(NA)
    # hashtag
    for (j in 1:length(uniH[[1]])){
      bigH[[1]][j]<-paste(uniH[[1]][j], uni2H[[j]])
    }
    # mentions
    for (j in 1:length(uniM[[1]])){
      bigM[[1]][j]<-paste(uniM[[1]][j], uni2M[[j]])
    }
    # insert in data.frame hashtag
    Tweets$hB[i] <- bigH
    Tweets$hB[[i]] <- Tweets$hB[[i]][-length(Tweets$hB[[i]])]
    Tweets$hB[[i]] <- sub("^text:","",Tweets$hB[[i]]) # elimina caratteri superflui
    Tweets$hB[[i]] <- sub("text:","",Tweets$hB[[i]]) # elimina caratteri superflui
    # insert in data.frame mentions
    Tweets$mB[i] <- bigM
    Tweets$mB[[i]] <- Tweets$mB[[i]][-length(Tweets$mB[[i]])]
    Tweets$mB[[i]] <- sub("^screen_name:","",Tweets$mB[[i]]) # elimina caratteri superflui
    Tweets$mB[[i]] <- sub("screen_name:","",Tweets$mB[[i]]) # elimina caratteri superflui
    print(paste0("hashtag/mention done: ", i))
  }
  ########################################################################################
  ########################################################################################
  # remove URLs
  Tweets$TextClean <- gsub('http\\S+\\s*', '', Tweets$Text)
  # remove emoji
  Tweets$TextClean <- gsub("(\\\\[[:alnum:]_]*)", '', Tweets$TextClean)
  # rimozione hashtag dal testo
  Tweets$TextClean <- gsub("#\\S+", '', Tweets$TextClean)
  # rimozione mention
  Tweets$TextClean <- gsub("@\\S+", '', Tweets$TextClean)
  # rimozione punteggiatura
  Tweets$TextClean <- removePunctuation(Tweets$TextClean)
  # tutto minuscolo
  Tweets$TextClean <- tolower(Tweets$TextClean)
  #remove stopword
  Tweets$TextClean <- removeWords(Tweets$TextClean, c(stopwords("en"), stopwords("es"), stopwords("fr"), stopwords("de"), "rt ", "the"))
  # rimozione spazi superflui inizio e fine stringa
  Tweets$TextClean <- trimws(Tweets$TextClean, which = "both")
  # rimozione spazi doppi
  Tweets$TextClean <- stripWhitespace(Tweets$TextClean)
  # tutto minuscolo
  Tweets$TextClean <- tolower(Tweets$TextClean)
  # NOT AVAIBLE
  Tweets$TextClean[Tweets$TextClean==""] <- NA
  # Stemming
  library(SnowballC)
  Tweets$TextClean <- wordStem(Tweets$TextClean, language = "en")
  ########################################################################################
  ########################################################################################
  # per tirare fuori i Ngram dal testo pulito
  for(i in 1:length(Tweets$TextClean)){ 
    # 1-gram 
    uniT<-strsplit(Tweets$TextClean[i], " ")
    Tweets$wU[i] <- uniT
    # Remove the first element and add a period at the end
    uni2T <- c(uniT[[1]][-1], ".")
    # Find all bigrams using paste
    bigT<-list(NA)
    for (j in 1:length(uniT[[1]])){
      bigT[[1]][j]<-paste(uniT[[1]][j], uni2T[[j]])
    }
    # insert in data.frame
    Tweets$wB[i] <- bigT
    Tweets$wB[[i]] <- Tweets$wB[[i]][-length(Tweets$wB[[i]])]
    print(paste0("word done: ", i))
  }
  return(Tweets)
}
