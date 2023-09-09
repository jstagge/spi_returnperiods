
lines_lmom <- function(){

lmr <-  lmomco::lmrdia()

lines_df <- as.data.frame(lmr$aep4) %>% mutate(label = "aep4") %>%
  bind_rows(as.data.frame(lmr$gev) %>% mutate(label = "gev")) %>%
  bind_rows(as.data.frame(lmr$glo) %>% mutate(label = "glo")) %>%
  bind_rows(as.data.frame(lmr$gpa) %>% mutate(label = "gpa")) %>%
  bind_rows(as.data.frame(lmr$gno) %>% mutate(label = "gno")) %>%
  bind_rows(as.data.frame(lmr$gov) %>% mutate(label = "gov")) %>%
  bind_rows(as.data.frame(lmr$pe3) %>% mutate(label = "pe3")) %>%
  bind_rows(as.data.frame(lmr$gov) %>% mutate(label = "gov")) %>%
  bind_rows(as.data.frame(lmr$rgov) %>% mutate(label = "rgov")) %>%
  bind_rows(as.data.frame(lmr$rgpa) %>% mutate(label = "rgpa")) %>%
  bind_rows(as.data.frame(lmr$wei) %>% mutate(label = "wei"))

return(lines_df)
}


limit_lmom <- function(){

lmr <-  lmomco::lmrdia()

limit_df <- as.data.frame(lmr$limits) %>% mutate(label = "limits")

return(limit_df)
}



points_lmom <- function(){

lmr <-  lmomco::lmrdia()

points_df <- as.data.frame(lmr$cau) %>% mutate(label = "cau") %>%
    bind_rows(as.data.frame(lmr$exp) %>% mutate(label = "exp")) %>%
    bind_rows(as.data.frame(lmr$gum) %>% mutate(label = "gum")) %>%
    bind_rows(as.data.frame(lmr$nor) %>% mutate(label = "nor")) %>%
    bind_rows(as.data.frame(lmr$ray) %>% mutate(label = "ray")) %>%
    bind_rows(as.data.frame(lmr$uniform) %>% mutate(label = "uniform"))  %>%
    bind_rows(as.data.frame(lmr$slash) %>% mutate(label = "slash"))

return(points_df)
}
