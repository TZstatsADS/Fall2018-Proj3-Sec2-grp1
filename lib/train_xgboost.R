#########################################################
### Train a classification model with training features ###
#########################################################

### Project 3

train <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  ### load libraries
  library("gbm")
  #install.packages("caret") 
  #install.packages("e1071") 
  library(caret)
  library(e1071)
  library(xgboost)
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    nrounds<-5
    depth<-3
  } else {
    #eta <- par$eta
    nrounds<-par$nrounds
    depth<-par$depth
    
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    
    dtrain<-xgb.DMatrix(data = featMat, label=labMat)
   
    
    ###########################xgboost second method: 
    #print(min_logloss_index)
    # train model on all the train dataset based on the optimal nrounds
    paras<-list(objective="reg:linear",
                eta=0.5, 
                nthread = 2,
                max.depth=depth)
    fit.xgb <- xgboost(data = featMat, label = labMat,    
                        nround = nrounds, 
                        params=paras)
    modelList[[i]]<-list(fit=fit.xgb)
    
    
  }
  
  
  return(modelList)
}
