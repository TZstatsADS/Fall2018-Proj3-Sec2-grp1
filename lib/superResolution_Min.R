 ########################
### Super-resolution ###
########################

### Author: Chengliang Tang
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
#    LR_dir = train_LR_dir
#    imgLR <- readImage(paste0(LR_dir,"img_0003.jpg"))    
#    pathHR <- paste0(HR_dir,"img_0003.jpg")   
    
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
    
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
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
    
    imgHR_rec = array(NA, c((dim(imgLR)[1])*2, (dim(imgLR)[2])*2, 3)) 
    for (c in 1:3){
    ### step 2. apply the modelList over featMat
    # modelList = fit_train
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    predMat_tran <- array(predMat, c(dim(imgLR)[1]*dim(imgLR)[2],4,3))
    imgLR_arr_1 <- as.vector(t(imgLR[,,c]))
#    imgLR_arr_2 <- as.vector(t(imgLR[,,2]))
#    imgLR_arr_3 <- as.vector(t(imgLR[,,3]))
    imgLR_arr_1_4 <- cbind(imgLR_arr_1,imgLR_arr_1,imgLR_arr_1,imgLR_arr_1)
#    imgLR_arr_2_4 <- cbind(imgLR_arr_1,imgLR_arr_2,imgLR_arr_2,imgLR_arr_2)
#    imgLR_arr_3_4 <- cbind(imgLR_arr_1,imgLR_arr_3,imgLR_arr_3,imgLR_arr_3)
    predMat_tran_1 <- predMat_tran[,,c] + imgLR_arr_1_4
#    predMat_tran_2 <- predMat_tran[,,2] + imgLR_arr_2_4
#    predMat_tran_3 <- predMat_tran[,,3] + imgLR_arr_3_4
#    predMat_all <- array(c(predMat_tran_1,predMat_tran_2,predMat_tran_3), 
#                          c(dim(imgLR)[1]*dim(imgLR)[2],4,3))
    col12 <- c(rbind(predMat_tran_1[,1], predMat_tran_1[,2]))
    col34 <- c(rbind(predMat_tran_1[,3], predMat_tran_1[,4]))
#    col12 <- as.vector(matrix(c(col1,col2), nrow = 2, byrow = TRUE))
#    col34 <- as.vector(matrix(c(col3,col4), nrow = 2, byrow = TRUE))
    col12_tran <- matrix(col12, nrow = dim(imgLR)[1],byrow = TRUE)
    col34_tran <- matrix(col34, nrow = dim(imgLR)[1],byrow = TRUE)
    # rownames(col12_tran) <- seq(1,dim(imgLR)[1]*2,2)
    # rownames(col34_tran) <- seq(2,dim(imgLR)[1]*2,2)
    # imgHR_rec2 <- merge(col12_tran, col34_tran, by = 'row.names', all = TRUE)
    imgHR_rec[seq(1,dim(imgLR)[1]*2,2),,c] <- col12_tran
    imgHR_rec[seq(2,dim(imgLR)[1]*2,2),,c] <- col34_tran
    }
   return (display(imgHR_rec))
  }

}


