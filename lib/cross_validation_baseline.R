########################
### Cross Validation ###
########################

### Author: Wenting Yu
### Project 3

cv.function <- function(X.train, y.train, K, depth, numTrees, minNode, shrinkage){
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    par <- list(depth=depth, numTrees = numTrees, minNode = minNode, shrinkage = shrinkage )
    
    tm_train <- system.time(fit <- train(train.data, train.label, par))
    
    tm_fit <-   system.time(pred <- test(fit, test.data) )
    print(tm_train)
    print(tm_fit)
    cv.error[i] <- mean((pred - test.label)^2)  
    print(paste('current is in fold ', i))
  }			
  return(c(mean(cv.error),sd(cv.error)))
}
