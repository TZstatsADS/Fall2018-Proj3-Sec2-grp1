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
  MSE <- array(NA, n_files)
  PSNR <- array(NA, n_files)
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    LR_rows = dim(imgLR)[1];  LR_cols = dim(imgLR)[2]
    
    # Zero Padding LR images for feature extraction
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR_padded <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
    
    #print(dim(imgLR_padded))
    #print(dim(imgLR))
    
    for (j in 2:(dim(imgLR_padded)[1]-1)){
      for (k in 2:(dim(imgLR_padded)[2]-1)){
        for (c in 1:3){
          #print(j);print(k);print(c)
          
          nb_points = c(imgLR_padded[(j-1),(k-1):(k+1),c], 
                        imgLR_padded[j,c(k-1,k+1),c],
                        imgLR_padded[(j+1),(k-1):(k+1),c])
          featMat[(j-1)*(k-1), , c] =  nb_points - c(imgLR_padded[j, k, c])
        }
      }
    }

    
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    # print(dim(featMat))    # 61060     8     3
    # print(dim(predMat))    # 61060     4     3
    
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imgHR_recover = array(NA, c(LR_rows*2, LR_cols*2, 3))
    
    for (c in 1:3) {
      recoverMat = predMat[,,c] + as.vector(t(imgLR_padded[2:(LR_rows+1), 2:(LR_cols+1), c]))
      imgHR_recover[seq(1,LR_rows*2,2),,c] <- matrix(c(t(recoverMat[,1:2])), nrow = LR_rows,byrow = TRUE)
      imgHR_recover[seq(2,LR_rows*2,2),,c] <- matrix(c(t(recoverMat[,3:4])), nrow = LR_rows,byrow = TRUE)
    }

    imgHR_recover_bw <- imageData(imgHR_recover)
    imgHR_recover_col <- Image(imgHR_recover_bw, colormode=Color)

    writeImage(imgHR_recover_col, paste0(HR_dir,  "recover_img", "_", sprintf("%04d", i), ".jpg"))
    print(i)
    
    #imgHR <- readImage(paste0("../data/test_set1/HR/",  "img", "_", sprintf("%04d", i), ".jpg"))
    #MSE[i] <- (1/(3*dim(imgLR)[1]*2*dim(imgLR)[2]*2)) * (imgHR - imgHR_recover_col)^2
    #PSNR[i] <- 10 * log10(max(imgHR)) - 10 * log10(MSE[i])
  }


}