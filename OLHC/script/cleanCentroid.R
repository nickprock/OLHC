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
#   print(paste0("tct0: ",tct0))
#   print(paste0("lf: ",lf))
#   print(paste0("lamba: ",lambda))
#   print(paste0("tso: ",tso))
#  print(paste0("ftc: ",ftc))
  return(Centroid)
}