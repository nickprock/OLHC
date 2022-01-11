similarity <- function(sgn, centroid, cF){
  inter <- intersect(sgn, centroid)
  sommaI <- c()
  for (j in 1:length(inter)){
    sommaI[j] <- cF[which(centroid==inter[j])]
  }
  somma <-sum(sommaI)
  u <- sum(cF) + length(setdiff(sgn,centroid))
  out <- somma/u
  return(out)
}
