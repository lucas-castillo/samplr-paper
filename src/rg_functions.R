get_shape <- function(v){
  v <- v[!is.na(v)]
  lb <- min(v)
  ub <- max(v)
  m <- mean(v)
  s <- sd(v)
  
  sum(dnorm(v, m, s, log = T) - dunif(v, lb, ub, log = T)) / length(v)
}
get_measures <- function(v){
  v <- round(v)
  v <- v[!is.na(v)]
  v2 <- v[c(T, diff(v) != 0)]
  R <- mean(diff(v) == 0)
  A <- mean(diff(v2) == 1)
  one <-   v2[1:(length(v2) - 2)]
  two <-   v2[2:(length(v2) - 1)]
  three <- v2[3:(length(v2) - 0)]
  TP <- mean((one > two & two < three) | (one < two & two > three) )
  D <- mean(abs(diff(v2)))
  S <- get_shape(v)
  return(tibble(R,A,TP,D,S))
}