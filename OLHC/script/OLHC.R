# carico gli script per calcolare la similarità e aggiornare il centroide
source("script/similarity.R")
source("script/centroid.R")
source("script/detectBurstee.R")
OLHC <- function(Tweets, e, delta, h, period, burstyFileOutput, fileOutputTemp){
  ########################################################
  # creazione colonna cluster e data.frame dei centroidi
  ########################################################
  Tweets$Cluster <- 0
  #nn <- 1
  Centroid <- data.frame(UserId = character(1), Timestamp = character(1), Lat.Lon = character(1), 
                         stringsAsFactors = FALSE)
  CentroidHistory <- data.frame(stringsAsFactors = FALSE)
  ########################################################
  # inizia la prcedura
  ########################################################
  for (i in 1:length(Tweets$TweetId)){
    sgn <- Tweets[i,]
    # Centroid <- cleanCentroid(Centroid, sgn, delta, h)
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
      # codice burstee
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
      # fine codice burstee
      #############################
      # nn dovrebbe far scorrere il tempo
      # ogni centroide ha una sua linea temporale
      Centroid$nn[1] <- 1
      #############################      
      Tweets$Cluster[1] <- 1
    } # inizializzo il primo centroide
    else {
      Centroid <- cleanCentroid(Centroid, sgn, delta, h)
      sim <- computeSimilarity(Centroid, sgn)
      simmax <- max(sim)
      #e <- 0.01 # valore da inserire
      if (simmax >= e){
        Tweets$Cluster[i] <- min(Centroid$Cluster[which(sim == simmax)])
        #Tweets$Cluster[i] <- which(sim == simmax) # assegna il tweet ad un cluser
        #Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i]) # aggiorna centroide
        #Centroid <- detectBurstee(centroid = Centroid, sgn = sgn, clus = Tweets$Cluster[i], h = h, delta = delta, period = period)
        
        if (difftime(time1 = strptime(sgn$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                     time2 = strptime(Centroid$Timestamp0[which(Centroid$Cluster==Tweets$Cluster[i])], "%Y-%m-%d %H:%M:%S"), 
                     units = "hour")<=(Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])]*((delta+h)/period))){
          Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i], h, delta, period, fileOutputTemp) # aggiorna centroide
          
        } else {
          Centroid <- detectBurstee(Centroid, sgn, Tweets$Cluster[i], h, delta, period, Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])],burstyFileOutput)
        Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])] <- Centroid$nn[which(Centroid$Cluster==Tweets$Cluster[i])] + 1
        #nn <- nn+1
        Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i], h, delta, period, fileOutputTemp) # aggiorna centroide
      }
        #Centroid <- updateCentroid(Centroid, sgn, Tweets$Cluster[i], h, delta, period) # aggiorna centroide
      } else {
        # genera un nuovo centroide
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
        #new_Centroid$Cluster[1] <- max(Centroid$Cluster) + 1
        #############################
        # codice burstee
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
        # entropia
        new_Centroid$MediaEntropia <- 0
        new_Centroid$VarianzaEntropia <- 0
        #############################
        # fine codice burstee
        #############################
        new_Centroid$nn <- 1
        # inserisci il nuovo centroide nel data.frame
        Centroid <- rbind(Centroid, new_Centroid)
        # etichetta il tweet
        Tweets$Cluster[i] <- max(Tweets$Cluster) + 1
      }
      
    }
    CentroidHistory <- rbind(na.omit(CentroidHistory), Centroid)
    CentroidHistory <-CentroidHistory[!duplicated(CentroidHistory[,c(1,3,4,17)], fromLast = TRUE),]
#    print(i)
#     if (as.numeric(format(Sys.time(), format = "%M")) %% 5==0 && format(Sys.time(), format = "%S")=="01"){
#       print("=======================================")
#       print(paste0("numero di cluster totali: ", length(CentroidHistory$Cluster)))
#       print(paste0("numero di cluster attuali: ", length(Centroid$Cluster)))
#       print(paste0("numero di singleton attuali: ", length(which(table(Tweets$Cluster[Tweets$Cluster %in% intersect(Tweets$Cluster, Centroid$Cluster)])==1))))
#       print(paste0("numero di tweet processati: ", i))
#       print("=======================================")
#     }
  }
  output <- list(Tweets = Tweets, Centroid = Centroid, CentroidHistory = CentroidHistory)
  return(output)
}
