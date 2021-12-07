#' my_mean function
#'
#' @param y is a vector with a number of elements
#' @param method_na uses a method to impute Missing Values with the mean of non NA vector values
#'
#' @return Returns a mean of a vector with Missing Values imputed with the mean of non NA vector values
#' @export
#'
#' @examples
#' y <- c(rep(c(45:60, 70:80, NA), 10), 400, 200, 9, rep(c(NA), 20))
#' my_mean(y, method_na = "impute")
#'
my_mean<- function(y, method_na){

  if(method_na == 'rm'){
    z<- sum(y, na.rm = TRUE)/length(y[!is.na(y)])
  } else if (method_na == 'impute'){
    mean_with_no_na<- sum(y, na.rm = TRUE)/length(y[!is.na(y)])
    y[is.na(y)] <- mean_with_no_na
    z<- sum(y)/length(y)

  }else{
    z<- sum(y)/length(y)
  }
  return(z)
}
