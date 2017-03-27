library(glmnet)
library(Matrix)
library(doParallel)
library(dplyr)

set.seed(1)
registerDoParallel(8)

# load data
load("./data/extract_train.Rdata")
source("extra.R")

# set up x and y
ydata <- as.numeric(data_model_train$interest_level)-1
xdata <- Matrix(as.matrix(data_model_train[,setdiff(names(data_model_train), c("interest_level"))]), sparse = TRUE)

# set up cross-validation
cv <- sample(1:3, length(ydata), replace=TRUE)

# base glm model to establish lambda path
glm_base <- glmnet(x = xdata, y = ydata, family = "multinomial", alpha = 0.5, nlambda = 250)
lambda_path <- glm_base$lambda

# create models with each tuning parameter
params <- data.frame()
for (a in seq(0, 1, 0.5)){
  cat(sprintf("Testing alpha = %f...\n", a))
  params_temp <- foreach(j = 1:3, .combine = rbind) %dopar% {
    # seperate into test/train
    ydata_train <- ydata[-which(cv == j)]
    ydata_test <- ydata[which(cv == j)]
    xdata_train <- xdata[-which(cv == j),]
    xdata_test <- xdata[which(cv == j),]
    
    # run model
    glm <- glmnet(x = xdata_train, y = ydata_train, family = "multinomial", alpha = a, lambda = lambda_path)
    
    # generate predictions
    lambda_mlogloss <- sapply(glm$lambda, function(l){
      predictions <- matrix(predict(glm, xdata_test,  s = l, type = "response"), ncol = 3, byrow = FALSE)
      has_na <- apply(predictions, 1, function(x) sum(is.na(x))>0)
      return(multiloss(predictions[!has_na,], ydata_test[!has_na]))
    })
    
    # determine optimal lambda
    optimal_lambda <- glm$lambda[which.min(lambda_mlogloss)]
    predictions <- matrix(predict(glm, xdata_test,  s = optimal_lambda, type = "response"), ncol = 3, byrow = FALSE)
    has_na <- apply(predictions, 1, function(x) sum(is.na(x))>0)
    mlogloss <- multiloss(predictions[!has_na,], ydata_test[!has_na])
    
    # return
    data.frame(lambda = optimal_lambda, mlogloss = mlogloss)
  }
  params <- rbind(params, mutate(params_temp, alpha = a))
}

# determine optimal alpha / lambda
params2 <- params %>% group_by(alpha) %>% summarise_each(funs(mean)) %>% arrange(mlogloss) %>% head(.,1)
alpha <- params2$alpha
lambda <- lambda_path[lambda_path >= params2$lambda]
mlogloss <- params2$mlogloss
glm_params <- list(alpha = alpha, lambda = lambda, mlogloss = mlogloss)

# save
save(file="./data/glm_tune.Rdata", list = c("glm_params"))

