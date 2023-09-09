dgev2 <- function(x, loc, scale, shape){
  dist_list <- list(type = "gev",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  if(scale <= 0){
      dens <- 0
  } else {
    dens <- pdfgev(x, dist_list)
  }
  return(dens)

}


pgev2 <- function(q, loc, scale, shape){
  dist_list <- list(type = "gev",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  p <- cdfgev(q, dist_list)
  return(p)

}

qgev2 <- function(p, loc, scale, shape){
  dist_list <- list(type = "gev",
    para = c(xi = loc, alpha = scale, kappa = shape),
    source = "manual")

  q <- quagev(p, dist_list)
  return(q)

}
