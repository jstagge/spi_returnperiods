dgno <- function(x, loc, scale, shape){
  dist_list <- list(type = "gno",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  dens <- pdfgno(x, dist_list)
  return(dens)

}



pgno <- function(q, loc, scale, shape){
  dist_list <- list(type = "gno",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  p <- cdfgno(q, dist_list)
  return(p)

}




qgno <- function(p, loc, scale, shape){
  dist_list <- list(type = "gno",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  q <- quagno(p, dist_list)
  return(q)

}
