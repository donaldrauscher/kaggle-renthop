library(dplyr)
library(lazyeval)

# fit beta distribution with mle
beta_ll <- function(par, x) { 
  x2 <- x
  x2[x2 == 0] <- 0.01
  x2[x2 == 1] <- 0.99
  return(sum(-dbeta(x2, par[1], par[2], log=TRUE))) 
} 

beta_mle <- function(x){
  return(optim(par = c(2,2), fn=beta_ll, x=x, method="L-BFGS-B", lower=c(0.5,0.5))$par)
}

# function for probabilizing high cardinality categorical variable
probabilize_high_card_cat <- function(df, y, x, n_fit  = 30){
  df$y <- f_eval(f_capture(y), df); df$x <- f_eval(f_capture(x), df)
  dist <- df %>% group_by(x) %>% summarise(y = mean(y), n = n()) %>% ungroup()
  par <- beta_mle(dist$y[dist$n >= n_fit])
  a <- par[1]; b <- par[2]
  dist <- mutate(dist, a = a, b = b, y2 = (a + y*n) / (a + y*n + b + (1-y)*n))
  return(df %>% left_join(dist, by = c("x")) %>% .$y2)
}

# add some noise
add_noise <- function(df, fields, std = 0.05){
  df[,fields] <- df[,fields] * 
    matrix(rnorm(length(fields) * nrow(df), 1, std), ncol = length(fields), nrow = nrow(df))
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