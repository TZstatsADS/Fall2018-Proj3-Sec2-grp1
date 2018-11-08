


superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  get_feature <- function(pad_col_idx, pad_row_idx, padd_LRimage = imgLR_padded){
    feature_array <- array(0,c(8, 3))
    nb_8points <- padd_LRimage[c(pad_row_idx-1,pad_row_idx,pad_row_idx+1), 
                               c(pad_col_idx-1, pad_col_idx, pad_col_idx +1), ]
    LC_t <- nb_8points - rep(nb_8points[2,2, ], each=9)  
    feature_array[1:3, ]<- LC_t[1, , ]
    feature_array[4:5, ]<- LC_t[2, c(1, 3), ]
    feature_array[6:8, ]<- LC_t[3, , ]
    return(feature_array)
  }
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))

  # get_feature <- function(pad_col_idx, pad_row_idx){
  #   neigh_pixels <- array(0,c(8, 3))
  #   nb_8points <- imgLR_padded[c(pad_row_idx-1,pad_row_idx,pad_row_idx+1), 
  #                        c(pad_col_idx-1, pad_col_idx, pad_col_idx +1), ]
  #   LC_t <- nb_8points - rep(nb_8points[2,2, ], each=9)  
  #   neigh_pixels[1:3, ]<- LC_t[1, , ]
  #   neigh_pixels[4:5, ]<- LC_t[2, c(1, 3), ]
  #   neigh_pixels[6:8, ]<- LC_t[3, , ]
  #   return(neigh_pixels)
  # }
  # 
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgLR_original <- imgLR
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    LR_rows = dim(imgLR)[1];  LR_cols = dim(imgLR)[2]
    col_idx<- rep(1:LR_cols, each=LR_rows)
    row_idx<- rep(1:LR_rows, LR_cols)
    HR_cidx<- 2*(1:LR_cols)
    HR_ridx<- 2*(1:LR_rows)
    

    # Zero Padding LR images for feature extraction
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR_padded <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
     
    
    MID<- mapply(get_feature, col_idx+1, row_idx+1)
    featMat[ , , 1]<- t(MID[1:8, ])
    featMat[ , , 2]<- t(MID[9:16, ])
    featMat[ , , 3]<- t(MID[17:24, ])
    
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    predMat <- array(predMat, dim = c(LR_rows * LR_cols, 4, 3)) # transforming predMat into image size
    
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imgHR_recover<- array(NA, c(LR_rows*2, LR_cols*2, 3))  # empty recovered HR image
    
    LRMat<- array(c(rep(as.numeric(imgLR_original[,,1]), 4), rep(as.numeric(imgLR_original[,,2]), 4), 
                    rep(as.numeric(imgLR_original[,,3]), 4)), dim = c(LR_rows * LR_cols, 4, 3))
    HRMat<- predMat + LRMat
    
    imgHR_recover[HR_ridx-1, HR_cidx-1, ]<- HRMat[, 1, ]
    imgHR_recover[HR_ridx-1, HR_cidx, ]<- HRMat[, 2, ]
    imgHR_recover[HR_ridx, HR_cidx-1, ]<- HRMat[, 3, ]
    imgHR_recover[HR_ridx, HR_cidx, ]<- HRMat[, 4, ]
    
    
    imgHR_recover_bw <- imageData(imgHR_recover)
    imgHR_recover_col <- Image(imgHR_recover_bw, colormode=Color)
    
    writeImage(imgHR_recover_col, paste0(HR_dir,  "recover_img", "_", sprintf("%04d", i), ".jpg"))
    print(i)

  }
  
  
}
