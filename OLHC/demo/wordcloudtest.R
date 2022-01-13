wordcloudtest <- function(idx, Tweets, minfreq, type){
  library(wordcloud)
  if (type=="hashtag"){
    wordcloud(words = unlist(Tweets$CentroidHistory$hU[[idx]]), 
              freq = unlist(Tweets$CentroidHistory$hUFreq[[idx]]), min.freq = minfreq,
              scale = c(3, .8))
  } else {
    if (type=="word"){
      wordcloud(words = unlist(Tweets$CentroidHistory$wU[[idx]]), 
                freq = unlist(Tweets$CentroidHistory$wUFreq[[idx]]), min.freq = minfreq,
                scale = c(3, .8))
    } else {
      if (type=="mention"){
        wordcloud(words = unlist(Tweets$CentroidHistory$mU[[idx]]), 
                  freq = unlist(Tweets$CentroidHistory$mUFreq[[idx]]), min.freq = minfreq,
                  scale = c(5, .7))
      } else {
        print("ERRORE: type non valido")
      }
    }
  } 
}