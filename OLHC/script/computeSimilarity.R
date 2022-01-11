source("script/similarity.R")
computeSimilarity <- function(centroid, sgn){
  l <- length(centroid[,1])
  sim <- vector(mode = "list", length = l)
  similarity <- c()
  for (i in 1:l){
    if (length(unlist(sgn$hU))!=0){
      sim[[i]][1] <- newSim(sgn$hU[[1]], centroid$hU[[i]], centroid$hUFreq[[i]])
    } else {
      sim[[i]][1] <- NA
    }
    if (length(unlist(sgn$hB))!=0){
      sim[[i]][2] <- newSim(sgn$hB[[1]], centroid$hB[[i]], centroid$hBFreq[[i]])
    } else {
      sim[[i]][2] <- NA
    }
    if (length(unlist(sgn$mU))!=0){
      sim[[i]][3] <- newSim(sgn$mU[[1]], centroid$mU[[i]], centroid$mUFreq[[i]])
    } else {
      sim[[i]][3] <- NA
    }
    if (length(unlist(sgn$mB))!=0){
      sim[[i]][4] <- newSim(sgn$mB[[1]], centroid$mB[[i]], centroid$mBFreq[[i]])
    } else {
      sim[[i]][4] <- NA
    }
    if (length(unlist(sgn$wU))!=0){
      sim[[i]][5] <- newSim(sgn$wU[[1]], centroid$wU[[i]], centroid$wUFreq[[i]])
    } else {
      sim[[i]][5] <- NA
    }
    if (length(unlist(sgn$wB))!=0){
      sim[[i]][6] <- newSim(sgn$wB[[1]], centroid$wB[[i]], centroid$wBFreq[[i]])
    } else {
      sim[[i]][6] <- NA
    }
    # f <- e^(-(sgn$Timestamp - centroid$Timestamp)^2)
    similarity[i] <- mean(sim[[i]], na.rm = TRUE)
  }
  return (similarity)
}