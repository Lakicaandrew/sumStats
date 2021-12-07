#' Fun_miss
#' A function that takes a vector as an argument and returns the number of Missing variables it has
#' @param z is a vector with a number of elements
#'
#' @return Returns a count of the Missing Variables
#' @export
#'
#' @examples
#' y <- c(rep(c(45:60, 70:80, NA), 10), 400, 200, 9, rep(c(NA), 20))
#' fun_miss(z)
fun_miss<- function(z){
  y<- sum(as.numeric(is.na(z)))
  return(y)
}

