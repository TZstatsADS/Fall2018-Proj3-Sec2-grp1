#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  
#  LR_dir = train_LR_dir
#  HR_dir = train_HR_dir
#  n_points = 1000
  
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    
#    imgLR <- readImage(paste0(LR_dir,"img_0001.jpg"))
#    imgHR <- readImage(paste0(HR_dir,"img_0001.jpg"))
    
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
    
    imgHR <- abind(array(0, replace(dim(imgHR), 2, 2)),imgHR, array(0, replace(dim(imgHR), 2, 2)), along = 2)
    imgHR <- abind(array(0, replace(dim(imgHR), 1, 2)),imgHR, array(0, replace(dim(imgHR), 1, 2)), along = 1)
    
    ### step 1. sample n_points from imgLR
    ind_row <- sample(2:(dim(imgLR)[1]-2), size=n_points, replace = TRUE)
    ind_col <- sample(2:(dim(imgLR)[2]-2), size=n_points, replace = TRUE)
    ### step 2. for each sampled point in imgLR,
    for (j in 1:n_points){
      for (c in 1:3){
      ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
      ###           tips: padding zeros for boundary points
      
      neighbour8 <- c(imgLR[ind_row[j]-1, (ind_col[j]-1):(ind_col[j]+1), c], 
                      imgLR[ind_row[j], ind_col[j]-1, c], imgLR[ind_row[j], ind_col[j]+1, c], 
                      imgLR[ind_row[j]+1, (ind_col[j]-1):(ind_col[j]+1), c])  
      featMat[(i-1)*1000+j, 1:8, c] =  neighbour8 - imgLR[ind_row[j], ind_col[j], c]
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat      
      neighbour4 <- imgHR[(ind_row[j]*2-1):(ind_row[j]*2), (ind_col[j]*2-1):(ind_col[j]*2), c]
      labMat[(i-1)*1000+j, 1:4, c] = neighbour4 - imgLR[ind_row[j], ind_col[j], c]
         ### step 3. repeat above for three channels
      }
    }
    
      
  }
  return(list(feature = featMat, label = labMat))
}
