


# caricare il file
load("OLHC/data/tweets.rda")
# caricare la funzione per analizzare i dati di Carmela
source("script/onlineClustering.R")
# # Cambiare la data
Tweets20141223_test2 <- OLHC(Tweets = Tweets[as.Date(Tweets$Timestamp)=="2016-01-22",], 
                                        e = 0.1, delta = 1, h = 0.25, period = 5)
# per utilizzare i dati provenienti direttamente da Twitter o quelli di FACup, ...
# bisogna usare la funzione di seguito
source("script/onlineClustering4string.R")
# funziona come la precedente ma gli ID di Twitter sono stringhe e non interi

# per fare le wordcloud
source("script/wordcloudtest.R")
# idx: id del cluster
# Tweets: il file in cui cercare
# minfreq: la frequenza minima che deve avere un termine
# type: "hashtag", "mention" o "word"
wordcloudtest(idx = 160, Tweets = Tweets20141223_test2, minfreq = 5, type = "hashtag")