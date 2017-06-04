library(dplyr)
library(lazyeval)

# fit beta distribution with mle
dbetabinom <- function(k, n, a, b) { 
  choose(n,k) * beta(k + a, n - k + b) / beta(a, b)
} 

dbetabinom_ll <- function(k, n, par) { 
  sum(-log(dbetabinom(k, n, par[1], par[2]))) 
} 

beta_ll <- function(par, x) { 
  x2 <- x
  x2[x2 == 0] <- 0.01
  x2[x2 == 1] <- 0.99
  return(sum(-dbeta(x2, par[1], par[2], log=TRUE))) 
} 

beta_mle <- function(ll_fn, ...){
  return(optim(par = c(1,1), fn=ll_fn, method="L-BFGS-B", lower=c(0.5,0.5), upper=c(500,500), ...)$par)
}

# function for probabilizing high cardinality categorical variable
probabilize_high_card_cat <- function(df, y, x, seg){
  df$y <- f_eval(f_capture(y), df); df$x <- f_eval(f_capture(x), df)
  df <- df %>% group_by(x) %>% mutate(n_bkt = cut(n(), breaks = seg), a = NA, b = NA) %>% ungroup()
  dist <- df %>% 
    filter(!is.na(y)) %>% # df includes both test and train
    group_by(x, n_bkt, a, b) %>% summarise(k = sum(y), n = n()) %>% ungroup()
  lvls <- levels(dist$n_bkt)
  max_lvl <- tail(lvls, 1)
  for (l in lvls){
    is_lvl <- dist$n_bkt == l
    if (l == max_lvl){
      par <- beta_mle(ll_fn = beta_ll, x = (dist$k/dist$n)[dist$n_bkt == l])
    } else {
      par <- beta_mle(ll_fn = dbetabinom_ll, k = dist$k[dist$n_bkt == l], n = dist$n[dist$n_bkt == l])
    }
    dist$a[dist$n_bkt == l] <- par[1]; dist$b[dist$n_bkt == l] <- par[2]
    df$a[df$n_bkt == l] <- par[1]; df$b[df$n_bkt == l] <- par[2]
  }
  dist <- mutate(dist, y2 = (a + k) / (a + b + n))
  df <- df %>% 
    left_join(dist %>% select(x, y2), by = c("x")) %>%
    mutate(y2 = ifelse(is.na(y2), a / (a + b), y2))
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