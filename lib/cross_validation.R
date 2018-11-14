########################
### Cross Validation ###
########################

### Project 3

cv.function <- function(X.train, y.train,nrounds, depth, K=3){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  #N<-K*length(eta)
  cv.error <- rep(NA, K)
  
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]

    par <- list(depth=depth, nrounds=nrounds)
    fit <- train(train.data, train.label, par)
    pred <- test(fit, test.data)
    
    
    #cv.error[i] <- mean((pred - test.label)^2)  
    cv.error[i] <- mean((pred - test.label)^2)
    print(paste('current is in fold ', i))
  }
  
  return(c(mean(cv.error),sd(cv.error)))
}

