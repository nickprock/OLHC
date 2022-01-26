# load data
load("OLHC/data/demo.rda")
# load function
source("OLHC/script/OLHC.R")
# # 
# Tweets: dataset (as demo)
# e: similarity bound
# delta: the minimum time period a cluster is maintained even if no new tweet is added
# h: the temporal horizon a cluster is considered active
# period: how many period check the life span
output<- OLHC(Tweets = Tweets500, e = 0.1, delta = 0.5, h = 1, period = 10)

# wordcloud
source("OLHC/demo//wordcloudtest.R")
# idx: cluster id
# Tweets: the output file returned by OLHC
# minfreq: minimun term frequency
# type: "hashtag", "mention" or "word"
wordcloudtest(idx = 1, Tweets = output, minfreq = 5, type = "mention")
