dpe3 <- function(x, mu, sigma, gamma){
  dist_list <- list(type = "pe3",
    para = c(mu = mu, sigma = sigma, gamma = gamma),
    source = "manual")

  dens <- pdfpe3(x, dist_list)
  return(dens)

}


ppe3 <- function(q, mu, sigma, gamma){
  dist_list <- list(type = "pe3",
    para = c(mu = mu, sigma = sigma, gamma = gamma),
    source = "manual")

  p <- cdfpe3(q, dist_list)
  return(p)

}

qpe3 <- function(p, mu, sigma, gamma){
  dist_list <- list(type = "pe3",
    para = c(mu = mu, sigma = sigma, gamma = gamma),
    source = "manual")

  q <- quape3(p, dist_list)
  return(q)

}
