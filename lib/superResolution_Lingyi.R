########################
### Super-resolution ###
########################

### Project 3

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### read LR/HR image pairs
 
    for(i in 1:n_files){
      imgLR1 <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
      imgLR<-imgLR1
      pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
      featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
      
      n_row<-nrow(imgLR)
      n_col<-ncol(imgLR)
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
      imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
      imgLR <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
      
      for (j in 2:(dim(imgLR)[1]-1)){
        for (k in 2:(dim(imgLR)[2]-1)){
          for (c in 1:3){
            neighbour8 <- c(imgLR[j-1, (k-1):(k+1), c],
                            imgLR[j, k-1, c], imgLR[j, k+1, c], 
                            imgLR[j+1, (k-1):(k+1), c])
            featMat[(j-1)*(k-1), 1:8, c] =  neighbour8 - c(imgLR[j, k, c])
          }
        }
      }
      imgLR <- imgLR[-c(1,dim(imgLR)[1]), -c(1,dim(imgLR)[2]), 1:3]
    
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    
    
  }
  
}