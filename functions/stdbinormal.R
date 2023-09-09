
std_binormal <- function(x1, x2, rho){
  front <- 2*pi*((1-rho^2)^0.5)
  front <- 1/front

  first_inside <- -1/(2*(1-rho^2))
  second_inside <- x1^2-2*rho*x1*x2 + x2^2

  result <- front *exp(first_inside * second_inside)
  return(result)
}
