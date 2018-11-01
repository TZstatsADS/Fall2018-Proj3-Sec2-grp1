gbm_train <- function(data, label, n.trees=250, n.shrinkage=0.1, run.cv=F){
  library('gbm')
  
  train_df <- data
  train_df$label <- label
  if(run.cv){
    best.n.trees = cv.gbm(trees = 500, K = 10, train_df = train_df)
    best.shrinkage <- n.shrinkage
    
  } 
  else{
    best.n.trees <- n.trees
    best.shrinkage <- n.shrinkage
    
  }
  cat('best number of tress is: ',best.n.trees)
  
  best_gbm_fit <- gbm(label~ ., data = train_df, interaction.depth = 1, 
                      distribution="multinomial", n.trees = best.n.trees, 
                      shrinkage = best.shrinkage)
  return(best_gbm_fit)
  
}

cv.gbm <- function(trees, K, train_df){
  
  gbmWithCrossValidation = gbm(label~ ., data = train_df, distribution = "multinomial", 
                               n.trees = trees, shrinkage = .1, cv.folds = 10, n.cores = 1)
  
  return(gbm.perf(gbmWithCrossValidation))
}

gbm_test <- function(model_fit,data){
  
  pred <- predict(model_fit, newdata=data, n.trees=model_fit$n.trees, type="response")
  
  pred <- data.frame(pred[,,1])
  
  colnames(pred) <- c('0','1','2')
  
  pred_label <- apply(pred,1,function(x){return(which.max(x)-1)})
  
  return(pred_label)
}