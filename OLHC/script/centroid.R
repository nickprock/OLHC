require(sets)
updateCentroid <- function(centroid, sgn, clus, h, delta, period){
  # update Timestamp
  centroid$Timestamp[which(centroid$Cluster==clus)] <- sgn$Timestamp
  # new terms count
  diffhU <- setdiff(sgn$hU[[1]],centroid$hU[[which(centroid$Cluster==clus)]])
  inthU <- intersect(centroid$hU[[which(centroid$Cluster==clus)]], sgn$hU[[1]])
  if (length(inthU)>0) {
    for (i in 1:length(inthU)){
      centroid$hUFreq[[which(centroid$Cluster==clus)]][which(centroid$hU[[which(centroid$Cluster==clus)]]==inthU[[i]])] <- centroid$hUFreq[[which(centroid$Cluster==clus)]][which(centroid$hU[[which(centroid$Cluster==clus)]]==inthU[[i]])]+1
    }
  }
  # update list of terms
  if (length(diffhU)>0){
    centroid$hU[[which(centroid$Cluster==clus)]]<-union(centroid$hU[[which(centroid$Cluster==clus)]], sgn$hU[[1]])
    # update frequencies
    centroid$hUFreq[[which(centroid$Cluster==clus)]] <- c(centroid$hUFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffhU)))
  }
  diffhB <- setdiff(sgn$hB[[1]],centroid$hB[[which(centroid$Cluster==clus)]])
  inthB <- intersect(centroid$hB[[which(centroid$Cluster==clus)]], sgn$hB[[1]])
  if (length(inthB)>0) {
    for (i in 1:length(inthB)){
      centroid$hBFreq[[which(centroid$Cluster==clus)]][which(centroid$hB[[which(centroid$Cluster==clus)]]==inthB[[i]])] <- centroid$hBFreq[[which(centroid$Cluster==clus)]][which(centroid$hB[[which(centroid$Cluster==clus)]]==inthB[[i]])]+1
    }
  }
  if (length(diffhB)>0){
    centroid$hB[[which(centroid$Cluster==clus)]]<-union(centroid$hB[[which(centroid$Cluster==clus)]], sgn$hB[[1]]) 
    centroid$hBFreq[[which(centroid$Cluster==clus)]] <- c(centroid$hBFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffhB)))
  }
  diffmU <- setdiff(sgn$mU[[1]],centroid$mU[[which(centroid$Cluster==clus)]])
  intmU <- intersect(centroid$mU[[which(centroid$Cluster==clus)]], sgn$mU[[1]])
  if (length(intmU)>0) {
    for (i in 1:length(intmU)){
      centroid$mUFreq[[which(centroid$Cluster==clus)]][which(centroid$mU[[which(centroid$Cluster==clus)]]==intmU[[i]])] <- centroid$mUFreq[[which(centroid$Cluster==clus)]][which(centroid$mU[[which(centroid$Cluster==clus)]]==intmU[[i]])]+1
    }
  }
  if (length(diffmU)>0){
    centroid$mU[[which(centroid$Cluster==clus)]]<-union(centroid$mU[[which(centroid$Cluster==clus)]], sgn$mU[[1]]) 
    centroid$mUFreq[[which(centroid$Cluster==clus)]] <- c(centroid$mUFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffmU)))
  }
  diffmB <- setdiff(sgn$mB[[1]],centroid$mB[[which(centroid$Cluster==clus)]])
  intmB <- intersect(centroid$mB[[which(centroid$Cluster==clus)]], sgn$mB[[1]])
  if (length(intmB)>0) {
    for (i in 1:length(intmB)){
      centroid$mBFreq[[which(centroid$Cluster==clus)]][which(centroid$mB[[which(centroid$Cluster==clus)]]==intmB[[i]])] <- centroid$mBFreq[[which(centroid$Cluster==clus)]][which(centroid$mB[[which(centroid$Cluster==clus)]]==intmB[[i]])]+1
    }
  }
  if (length(diffmB)>0){
    centroid$mB[[which(centroid$Cluster==clus)]]<-union(centroid$mB[[which(centroid$Cluster==clus)]], sgn$mB[[1]])
    centroid$mBFreq[[which(centroid$Cluster==clus)]] <- c(centroid$mBFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffmB)))
  }
  diffwU <- setdiff(sgn$wU[[1]],centroid$wU[[which(centroid$Cluster==clus)]])
  intwU <- intersect(centroid$wU[[which(centroid$Cluster==clus)]], sgn$wU[[1]])
  if (length(intwU)>0) {
    for (i in 1:length(intwU)){
      centroid$wUFreq[[which(centroid$Cluster==clus)]][which(centroid$wU[[which(centroid$Cluster==clus)]]==intwU[[i]])] <- centroid$wUFreq[[which(centroid$Cluster==clus)]][which(centroid$wU[[which(centroid$Cluster==clus)]]==intwU[[i]])]+1
    }
  }
  if (length(diffwU)>0){
    centroid$wU[[which(centroid$Cluster==clus)]]<-union(centroid$wU[[which(centroid$Cluster==clus)]], sgn$wU[[1]])
    centroid$wUFreq[[which(centroid$Cluster==clus)]] <- c(centroid$wUFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffwU)))
  }
  diffwB <- setdiff(sgn$wB[[1]],centroid$wB[[which(centroid$Cluster==clus)]])
  intwB <- intersect(centroid$wB[[which(centroid$Cluster==clus)]], sgn$wB[[1]])
  if (length(intwB)>0) {
    for (i in 1:length(intwB)){
      centroid$wBFreq[[which(centroid$Cluster==clus)]][which(centroid$wB[[which(centroid$Cluster==clus)]]==intwB[[i]])] <- centroid$wBFreq[[which(centroid$Cluster==clus)]][which(centroid$wB[[which(centroid$Cluster==clus)]]==intwB[[i]])]+1
    }
  }
  if (length(diffwB)>0){
    centroid$wB[[which(centroid$Cluster==clus)]]<-union(centroid$wB[[which(centroid$Cluster==clus)]], sgn$wB[[1]]) 
    centroid$wBFreq[[which(centroid$Cluster==clus)]] <- c(centroid$wBFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffwB)))
  }
  #######################################
  # start detect burstee
  #######################################  
  diffUser <- setdiff(sgn$UserId[1],centroid$User[[which(centroid$Cluster==clus)]])
  intUser <- intersect(centroid$User[[which(centroid$Cluster==clus)]], sgn$UserId[1])
  if (length(intUser)>0) {
    for (i in 1:length(intUser)){
      centroid$UserFreq[[which(centroid$Cluster==clus)]][which(centroid$User[[which(centroid$Cluster==clus)]]==intUser[[i]])] <- centroid$UserFreq[[which(centroid$Cluster==clus)]][which(centroid$User[[which(centroid$Cluster==clus)]]==intUser[[i]])]+1
    }
  }
  if (length(diffUser)>0){
    centroid$User[[which(centroid$Cluster==clus)]]<-union(centroid$User[[which(centroid$Cluster==clus)]], sgn$UserId[1])
    centroid$UserFreq[[which(centroid$Cluster==clus)]] <- c(centroid$UserFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffUser)))
  }
  
  diffLocation <- setdiff(sgn$Lat.Lon[1],centroid$Location[[which(centroid$Cluster==clus)]])
  intLocation <- intersect(centroid$Location[[which(centroid$Cluster==clus)]], sgn$Lat.Lon[1])
  if (length(intLocation)>0) {
    for (i in 1:length(intLocation)){
      centroid$LocationFreq[[which(centroid$Cluster==clus)]][which(centroid$Location[[which(centroid$Cluster==clus)]]==intLocation[[i]])] <- centroid$LocationFreq[[which(centroid$Cluster==clus)]][which(centroid$Location[[which(centroid$Cluster==clus)]]==intLocation[[i]])]+1
    }
  }
  if (length(diffLocation)>0){
    centroid$Location[[which(centroid$Cluster==clus)]]<-union(centroid$Location[[which(centroid$Cluster==clus)]], sgn$Lat.Lon[1])
    centroid$LocationFreq[[which(centroid$Cluster==clus)]] <- c(centroid$LocationFreq[[which(centroid$Cluster==clus)]], rep(1, length(diffLocation)))
  }
  return(centroid)
}


cleanCentroid <- function (Centroid, sgn, delta, h){
  tct0 <- difftime(time1 = strptime(Centroid$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                   time2 = strptime(Centroid$Timestamp0, "%Y-%m-%d %H:%M:%S"), 
                   units = "hour")
  lf <- delta+((2^h)*tct0)
  lambda <- 1/as.numeric(lf)
  tso <- difftime(time1 = strptime(sgn$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                  time2 = strptime(Centroid$Timestamp, "%Y-%m-%d %H:%M:%S"), 
                  units = "hour")
  ftc <- 2^(-lambda*as.numeric(tso))
  Centroid <- Centroid[ftc >= 0.5,]
  return(Centroid)
}