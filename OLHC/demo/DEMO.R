# caricare il file
load("OLHC/data/demo.rda")
# caricare la funzione per analizzare i dati di Carmela
source("OLHC/script/OLHC.R")
# # 
output<- OLHC(Tweets = Tweets_2, e = 0.1, delta = 1, h = 0.25, period = 5)

# per fare le wordcloud
source("OLHC/demo//wordcloudtest.R")
# idx: id del cluster
# Tweets: il file in cui cercare
# minfreq: la frequenza minima che deve avere un termine
# type: "hashtag", "mention" o "word"
wordcloudtest(idx = 1, Tweets = output, minfreq = 5, type = "mention")
