z.score <- function(x, mu, sigma){
  z <- (x - mu)/sigma
  return(z)
}