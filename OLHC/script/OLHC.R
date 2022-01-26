# load functions
source("OLHC//script/similarity.R")
source("OLHC//script/centroid.R")
source("OLHC//script/detectBurstee.R")
OLHC <- function(Tweets, e, delta, h, period){
  ########################################################
  # creation centroid
  ########################################################
  Tweets$Cluster <- 0
  Centroid <- data.frame(UserId = character(1), Timestamp = character(1), Lat.Lon = character(1), 
                         stringsAsFactors = FALSE)
  CentroidHistory <- data.frame(stringsAsFactors = FALSE)
  pb = txtProgressBar(min = 0, max = length(Tweets$TweetId), initial = 0)
  ########################################################
  # START
  ########################################################
  for (i in 1:length(Tweets$TweetId)){
    setTxtProgressBar(pb,i)
    sgn <- Tweets[i,]
    if (Centroid$UserId[1] == "" || is.na(Centroid$UserId[1])){
      Centroid$UserId[1] <- Tweets$UserId[i]
      Centroid$Timestamp[1] <- Tweets$Timestamp[i]
      Centroid$Timestamp0[1] <- Tweets$Timestamp[i]
      Centroid$Lat.Lon[1] <- Tweets$Lat.Lon[i]
      Centroid$hU[1] <- Tweets$hU[i]
      Centroid$hUFreq[[1]] <- rep(1, length(Centroid$hU[[1]]))
      Centroid$hB[1] <- Tweets$hB[i]
      Centroid$hBFreq[[1]] <- rep(1, length(Centroid$hB[[1]]))
      Centroid$mU[1] <- Tweets$mU[i]
      Centroid$mUFreq[[1]] <- rep(1, length(Centroid$mU[[1]]))
      Centroid$mB[1] <- Tweets$mB[i]
      Centroid$mBFreq[[1]] <- rep(1, length(Centroid$mB[[1]]))
      Centroid$wU[[1]] <- unique(Tweets$wU[[i]])
      Centroid$wUFreq[[1]] <- rep(1, length(Centroid$wU[[1]]))
      Centroid$wB[[1]] <- unique(Tweets$wB[[i]])
      Centroid$wBFreq[[1]] <- rep(1, length(Centroid$wB[[1]]))
      Centroid$Cluster[1] <- 1
      #############################
      # start detect burstee
      #############################
      # User
      Centroid$User[1] <- list(NULL)
      Centroid$User[[1]] <- Tweets$UserId[i]
      Centroid$UserFreq[1] <- list(NULL) 
      Centroid$UserFreq[[1]] <- 1
      Centroid$MediaUser <- 0
      Centroid$VarianzaUser <- 0
      # Location
      Centroid$Location[1] <- list(NULL)
      Centroid$Location[[1]] <- Tweets$Lat.Lon[i]
      Centroid$LocationFreq[1] <- list(NULL) 
      Centroid$LocationFreq[[1]] <- 1
      Centroid$MediaLocation <- 0
      Centroid$VarianzaLocation <- 0
      # Tweets
      Centroid$MediaTweets <- 0
      Centroid$VarianzaTweets <- 0
      # Entropia
      Centroid$MediaEntropia <- 0
      Centroid$VarianzaEntropia <- 0
      #############################
      # stop detect burstee
      #############################
      # nn: every centroid has a temporal line
      Centroid$nn[1] <- 1
      #############################      
      Tweets$Cluster[1] <- 1
    }
    else {
      Centroid <- cleanCentroid(Centroid, sgn, delta, h)
      sim <- computeSimilarity(Centroid, sgn)
      simmax <- max(sim)
      if (simmax >= e){
        Tweets$Cluster[i] <- min(Centroid$Cluster[which(sim == simmax)])
        
        if (difftime(time1 = strptime(sgn$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                     time2 = strptime(Centroid$Timestamp0[which(Centroid$Cluster==Tweets$Cluster[i])], "%Y-%m-%d %H:%M:%S"), 
                     units = "hour")<=(Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])]*((delta+h)/period))){
          Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i], h, delta, period)
          
        } else {
          Centroid <- detectBurstee(Centroid, sgn, Tweets$Cluster[i], h, delta, period, Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])])
          Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])] <- Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])] + 1
          Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i], h, delta, period)
      }
      } else {
        new_Centroid <- data.frame(UserId = Tweets$UserId[i], Timestamp = Tweets$Timestamp[i],
                                   Timestamp0 = Tweets$Timestamp[i],
                                   Lat.Lon = Tweets$Lat.Lon[i], stringsAsFactors = FALSE)
        new_Centroid$hU <- Tweets$hU[i]
        new_Centroid$hUFreq[[1]] <- rep(1, length(new_Centroid$hU[[1]]))
        new_Centroid$hB <- Tweets$hB[i]
        new_Centroid$hBFreq[[1]] <- rep(1, length(new_Centroid$hB[[1]]))
        new_Centroid$mU <- Tweets$mU[i]
        new_Centroid$mUFreq[[1]] <- rep(1, length(new_Centroid$mU[[1]]))
        new_Centroid$mB <- Tweets$mB[i]
        new_Centroid$mBFreq[[1]] <- rep(1, length(new_Centroid$mB[[1]]))
        new_Centroid$wU[[1]] <- unique(Tweets$wU[[i]])
        new_Centroid$wUFreq[[1]] <- rep(1, length(new_Centroid$wU[[1]]))
        new_Centroid$wB[[1]] <- unique(Tweets$wB[[i]])
        new_Centroid$wBFreq[[1]] <- rep(1, length(new_Centroid$wB[[1]]))
        new_Centroid$Cluster[1] <- max(Tweets$Cluster) + 1
        #############################
        # start detect burstee
        #############################
        # User
        new_Centroid$User[1] <- list(NULL)
        new_Centroid$User[[1]] <- Tweets$UserId[i]
        new_Centroid$UserFreq[1] <- list(NULL) 
        new_Centroid$UserFreq[[1]] <- 1
        new_Centroid$MediaUser <- 0
        new_Centroid$VarianzaUser <- 0
        # Location
        new_Centroid$Location[1] <- list(NULL)
        new_Centroid$Location[[1]] <- Tweets$Lat.Lon[i]
        new_Centroid$LocationFreq[1] <- list(NULL) 
        new_Centroid$LocationFreq[[1]] <- 1
        new_Centroid$MediaLocation <- 0
        new_Centroid$VarianzaLocation <- 0
        # Tweets
        new_Centroid$MediaTweets <- 0
        new_Centroid$VarianzaTweets <- 0
        # entropy
        new_Centroid$MediaEntropia <- 0
        new_Centroid$VarianzaEntropia <- 0
        #############################
        # stop detect burstee
        #############################
        new_Centroid$nn <- 1
        Centroid <- rbind(Centroid, new_Centroid)
        # label
        Tweets$Cluster[i] <- max(Tweets$Cluster) + 1
      }
      
    }
    CentroidHistory <- rbind(na.omit(CentroidHistory), Centroid)
    CentroidHistory <-CentroidHistory[!duplicated(CentroidHistory[,c(1,3,4,17)], fromLast = TRUE),]
  }
  output <- list(Tweets = Tweets, Centroid = Centroid, CentroidHistory = CentroidHistory)
  return(output)
}