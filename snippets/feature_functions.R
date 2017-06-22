library(dplyr)
library(lazyeval)

# fit beta distribution with mle
dbetabinom <- function(k, n, a, b) { 
  n2 <- ifelse(n > 100, 100, n)
  k2 <- round(k * n2 / n)
  beta(k2 + a, n2 - k2 + b) / beta(a, b)
} 

betabinom_ll <- function(k, n, par) { 
  sum(-log(dbetabinom(k, n, par[1], par[2]))) 
} 

beta_mle <- function(...){
  par <- optim(par = c(1,1), fn=betabinom_ll, method="L-BFGS-B", lower=c(0.5,0.5), upper=c(500,500), ...)$par
  return(data.frame(a = par[1], b = par[2]))
}

# function for probabilizing high cardinality categorical variable
probabilize_high_card_cat <- function(df, y, x, seg = 1, loo = 1){
  
  # set x, y, and seg
  df$y <- f_eval(f_capture(y), df)
  df$x <- f_eval(f_capture(x), df)
  df$seg <- f_eval(f_capture(seg), df) 

  # determine prior for each segment
  dist <- df %>% 
    filter(!is.na(y)) %>% # df includes both test and train
    group_by(seg, x) %>% summarise(k = sum(y), n = n()) %>% ungroup() %>%
    group_by(seg) %>% do(beta_mle(k = .$k, n = .$n)) %>% ungroup()
  
  # calculate posterior probabilities
  df <- df %>% 
    left_join(dist, by = c("seg")) %>%
    group_by(x) %>% mutate(
      k = sum(y, na.rm = TRUE) - loo * ifelse(!is.na(y), y, 0), 
      n = sum(!is.na(y), na.rm = TRUE) - loo * as.integer(!is.na(y))
    ) %>% ungroup() %>%
    mutate(y2 = (a + k) / (a + b + n))
  
  return(df$y2)

}

# add some noise
add_noise <- function(df, rows, fields, std = 0.05){
  df[rows,fields] <- df[rows,fields] * 
    matrix(rnorm(length(fields) * length(rows), 1, std), ncol = length(fields), nrow = length(rows))
  return(df)
}

# make binary
make_binary <- function(df, fields, cutoff){
  df[,fields] <- as.integer(df[,fields] > cutoff)
  return(df)
}
 
# impute function
impute <- function(df, fields, fun){
  for (f in fields){
    df[[f]][is.na(df[[f]])] <- fun(df[[f]], na.rm = TRUE)
  }
  return(df)
}