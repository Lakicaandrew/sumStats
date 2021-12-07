#' my_sd Function
#'
#' @param n is a vector of elements
#'
#' @return Standard deviation after imputing for Outliers and NAs with mean of elements without NAs
#' @export
#'
#' @examples
#' x<- c(rep(c(45:60, 70:80, NA), 10), 400, 200, 9, rep(c(NA), 20))
#' my_sd(x)
my_sd<- function(n){
  iqr<-1.5*IQR(n, na.rm = T)
  Lb<- quantile(n, .25, na.rm = T)
  Ub<- quantile(n, .75, na.rm = T)

  Ub <- Ub + iqr
  Lb <- Lb - iqr

  ## get the outliers
  # n[n>Ub|n<Lb]
  p<- n[n<=Ub&n>=Lb]
  p<- my_mean(p, method_na = 'impute')
  n[(n>Ub|n<Lb) & !is.na(n)] <- p
  n[is.na(n)] <- p
  mean_na_outliers<- my_mean(n, method_na = 'impute')
  for (i in 1:length(n)) {
    sqrdf=sum((n-my_mean(n, method_na = 'impute'))*(n-my_mean(n, method_na = 'impute')))#summing over n interval
  }
  pp=sqrdf/(length(n)-1)

  sd=(sqrt(pp))
  return(sd)
}
