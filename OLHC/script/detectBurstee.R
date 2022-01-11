source("script/zscore.R")
detectBurstee <- function(centroid, sgn, clus, h, delta, period, nn, fileOutput){
    time <- as.numeric(difftime(time1 = strptime(sgn$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                                time2 = strptime(centroid$Timestamp0[which(centroid$Cluster==clus)], "%Y-%m-%d %H:%M:%S"), 
                                units = "hour"))
    n <- (time%/%((delta+h)/period))
    dat <- data.frame(clus = numeric(1), times = character(1), period = numeric(1), user = numeric(1), location = numeric(1), tweets = numeric(1), entropia = numeric(1))
    # utente
    mediaOLDU <- centroid$MediaUser[which(centroid$Cluster==clus)]
    #print(paste("mediaOLD_U", clus, "al tempo", sgn$Timestamp, "è pari:", mediaOLDU, sep = " "))
    varianzaOLDU <- centroid$VarianzaUser[which(centroid$Cluster==clus)]
    #print(paste("S_OLD_U", clus, "al tempo", sgn$Timestamp, "è pari:", varianzaOLDU, sep = " "))
    xiU <- length(centroid$UserFreq[[which(centroid$Cluster==clus)]])
    #print(paste("xiU", clus, "al tempo", sgn$Timestamp, "è pari:", xiU, sep = " "))
    centroid$MediaUser[which(centroid$Cluster==clus)] <- mediaOLDU+((xiU-mediaOLDU)/n)
    if (n-nn>=1){
      xiU <- 0
      varianzaOLDU <- 1
    } else {
      xiU <- xiU
      varianzaOLDU <- varianzaOLDU
    }
    SU <- varianzaOLDU + (xiU - mediaOLDU)*(xiU - centroid$MediaUser[which(centroid$Cluster==clus)])
    centroid$VarianzaUser[which(centroid$Cluster==clus)] <- SU
    dev.st_U <- sqrt(SU/n)
#     if (n-nn>=1){
#       zUser <- z.score(x = 0, mu = centroid$MediaUser[which(centroid$Cluster==clus)], 
#                        sigma = dev.st_U)
#     } else {
#       zUser <- z.score(x = xiU, mu = centroid$MediaUser[which(centroid$Cluster==clus)], 
#                        sigma = dev.st_U)
#     }
    zUser <- z.score(x = xiU, mu = centroid$MediaUser[which(centroid$Cluster==clus)], 
                      sigma = dev.st_U)
    if (!is.na(zUser) && zUser>2){
      print(n-nn)
      print(paste("il cluster", clus, "al tempo", sgn$Timestamp, "è burstee per gli user con z-score:", zUser, sep = " "))
      print("======================================================")
      dat$clus <- clus
      dat$times <- sgn$Timestamp
      dat$period <- n
      dat$user <- zUser
    }
    # location
    mediaOLDL <- centroid$MediaLocation[which(centroid$Cluster==clus)]
    varianzaOLDL <- centroid$VarianzaLocation[which(centroid$Cluster==clus)]
    xiL <- length(centroid$LocationFreq[[which(centroid$Cluster==clus)]])
    centroid$MediaLocation[which(centroid$Cluster==clus)] <- mediaOLDL+((xiL - mediaOLDL)/n)
#    diffL <- xiL - centroid$MediaLocation[which(centroid$Cluster==clus)]
    if (n-nn>=1){
      xiL <- 0
      varianzaOLDL <- 1
    } else {
      xiL <- xiL
      varianzaOLDL <- varianzaOLDL
    }
    SL <- varianzaOLDL + (xiL - mediaOLDL)*(xiL - centroid$MediaLocation[which(centroid$Cluster==clus)])
    centroid$VarianzaLocation[which(centroid$Cluster==clus)] <- SL
    dev.st_L <- sqrt(SL/n)
#     if (n-nn>=1){
#       zLocation <- z.score(x = 0, mu = centroid$MediaLocation[which(centroid$Cluster==clus)], 
#                            sigma = dev.st_L)
#     } else {
#       zLocation <- z.score(x = xiL, mu = centroid$MediaLocation[which(centroid$Cluster==clus)], 
#                            sigma = dev.st_L)
#     }
     zLocation <- z.score(x = xiL, mu = centroid$MediaLocation[which(centroid$Cluster==clus)], 
                      sigma = dev.st_L)
    if (!is.na(zLocation) && zLocation>2){
      print(n-nn)
      print(paste("il cluster", clus, "al tempo", sgn$Timestamp, "è burstee per la location con z-score:", zLocation, sep = " "))
      print("======================================================")
      dat$clus <- clus
      dat$times <- sgn$Timestamp
      dat$period <- n
      dat$location <- zLocation
    }
    # tweets
    mediaOLDT <- centroid$MediaTweets[which(centroid$Cluster==clus)]
    varianzaOLDT <- centroid$VarianzaTweets[which(centroid$Cluster==clus)]
    xiT <- sum(centroid$UserFreq[[which(centroid$Cluster==clus)]])
    centroid$MediaTweets[which(centroid$Cluster==clus)] <- mediaOLDT+((xiT - mediaOLDT)/n)
    if (n-nn>=1){
      xiT <- 0
      varianzaOLDT <- 1
    } else {
      xiT <- xiT
      varianzaOLDT <- varianzaOLDT
    }
    ST <- varianzaOLDT + (xiT - mediaOLDT)*(xiT - centroid$MediaTweets[which(centroid$Cluster==clus)])
    centroid$VarianzaTweets[which(centroid$Cluster==clus)] <- ST
    dev.st_T <- sqrt(ST/n)
#     if (n-nn>=1){
#       zTweets <- z.score(x = 0, mu = centroid$MediaTweets[which(centroid$Cluster==clus)], 
#                          sigma = dev.st_T)
#     } else {
#       zTweets <- z.score(x = xiT, mu = centroid$MediaTweets[which(centroid$Cluster==clus)], 
#                          sigma = dev.st_T)
#     }
    zTweets <- z.score(x = xiT, mu = centroid$MediaTweets[which(centroid$Cluster==clus)], 
                         sigma = dev.st_T)
    if (!is.na(zTweets) && zTweets>2){
      print(n-nn)
      print(paste("il cluster", clus, "al tempo", sgn$Timestamp, "è burstee per i tweets con z-score:", zTweets, sep = " "))
      print("======================================================")
      dat$clus <- clus
      dat$times <- sgn$Timestamp
      dat$period <- n
      dat$tweets <- zTweets
    }
    # entropia
    mediaOLDE <- centroid$MediaEntropia[which(centroid$Cluster==clus)]
    varianzaOLDE <- centroid$VarianzaEntropia[which(centroid$Cluster==clus)]
    # calcolo entropia
    pilogpi <- c()
    for (i in 1:length(centroid$UserFreq[[which(centroid$Cluster==clus)]])){
      pi <- centroid$UserFreq[[which(centroid$Cluster==clus)]][i]/xiT
      logpi <- log(pi)
      pilogpi[i] <- pi*logpi
    }
    entropia <- -1 * sum(pilogpi)
    centroid$MediaEntropia[which(centroid$Cluster==clus)] <- mediaOLDE+((entropia - mediaOLDE)/n)
    if (n-nn>=1){
      entropia <- 0
      varianzaOLDE <- 1
    } else {
      entropia <- entropia
      varianzaOLDE <- varianzaOLDE
    }
    SE <- varianzaOLDE + (entropia - mediaOLDE)*(entropia - centroid$MediaEntropia[which(centroid$Cluster==clus)])
    centroid$VarianzaEntropia[which(centroid$Cluster==clus)] <- SE
    dev.st_E <- sqrt(SE/n)
#     if (n-nn>=1){
#       zEntropia <- z.score(x = 0, mu = centroid$MediaEntropia[which(centroid$Cluster==clus)], 
#                            sigma = dev.st_E)
#     } else {
#       zEntropia <- z.score(x = entropia, mu = centroid$MediaEntropia[which(centroid$Cluster==clus)], 
#                            sigma = dev.st_E)
#     }
    zEntropia <- z.score(x = entropia, mu = centroid$MediaEntropia[which(centroid$Cluster==clus)], 
                       sigma = dev.st_E)
    if (!is.na(zEntropia) && zEntropia>2){
      print(n-nn)
      print(paste("il cluster", clus, "al tempo", sgn$Timestamp, "è burstee per l'entropia con z-score:", zEntropia, sep = " "))
      print("======================================================")
      dat$clus <- clus
      dat$times <- sgn$Timestamp
      dat$period <- n
      dat$entropia <- zEntropia
    }
    #dat <- data.frame(cluster = clus, period = n, num.utenti = xiU, num.tweets = xiT, entropia = entropia)
    if ((!is.na(zEntropia) && zEntropia>2) || (!is.na(zTweets) && zTweets>2) || (!is.na(zLocation) && zLocation>2) || (!is.na(zUser) && zUser>2)){
      write.table(x = dat, file = fileOutput, append = T, sep = ";")
    }
    centroid$User[which(centroid$Cluster==clus)] <- list(NULL)
    centroid$UserFreq[which(centroid$Cluster==clus)] <- list(NULL)
    centroid$Location[which(centroid$Cluster==clus)] <- list(NULL)
    centroid$LocationFreq[which(centroid$Cluster==clus)] <- list(NULL)
  
  return(centroid)
}