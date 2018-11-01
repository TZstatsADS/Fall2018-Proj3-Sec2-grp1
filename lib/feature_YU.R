###

feature <- function(LR_dir, HR_dir, n_points=1000){

  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))  # n_files * n_points rows by 8 cols
  labMat <- array(NA, c(n_files * n_points, 4, 3))   # n_files * n_points rows by 4 cols
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    LR_rows = dim(imgLR)[1];LR_cols = dim(imgLR)[2]
    HR_rows = dim(imgHR)[1];HR_cols = dim(imgHR)[2]
    
    # Zero Padding LR images for feature extraction
    imgLR_padded = array(data = 0, c(LR_rows+2, LR_cols+2, 3)) 
    imgLR_padded[2:(LR_rows+1), 2:(LR_cols+1), 1] = imageData(imgLR[,,1]) 
    imgLR_padded[2:(LR_rows+1), 2:(LR_cols+1), 2] = imageData(imgLR[,,2]) 
    imgLR_padded[2:(LR_rows+1), 2:(LR_cols+1), 3] = imageData(imgLR[,,3]) 
    
    imgHR_padded = array(data = 0, c(HR_rows+2, HR_cols+2, 3)) 
    imgHR_padded[2:(HR_rows+1), 2:(HR_cols+1), 1] = imageData(imgHR[,,1]) 
    imgHR_padded[2:(HR_rows+1), 2:(HR_cols+1), 2] = imageData(imgHR[,,2]) 
    imgHR_padded[2:(HR_rows+1), 2:(HR_cols+1), 3] = imageData(imgHR[,,3]) 
    
    ### step 1. sample n_points from imgLR
    row_idx = sample(2:(dim(imgLR_padded)[1] - 1), n_points, replace = TRUE)
    col_idx = sample(2:(dim(imgLR_padded)[2] - 1), n_points, replace = TRUE)
    
    
    for (p in 1:n_points){
 
      
      for (ch in 1:3){
        
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        ###           tips: padding zeros for boundary points
        
        nb_points = c(imgLR_padded[row_idx[p]-1,(col_idx[p]-1):(col_idx[p]+1),ch], 
                      imgLR_padded[row_idx[p],c(col_idx[p]-1,col_idx[p]+1),ch],
                      imgLR_padded[row_idx[p]+1,(col_idx[p]-1):(col_idx[p]+1),ch])
        
        featMat[p + (i-1)*n_points,,ch] = nb_points - imgLR_padded[row_idx[p], col_idx[p], ch]
        
        ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
        sub_points = imgHR_padded[(2*row_idx[p]-1):(2*row_idx[p]), (2*col_idx[p]-1):(2*col_idx[p]) ,ch]
        labMat[p + (i-1)*n_points,,ch] = sub_points - imgLR_padded[row_idx[p], col_idx[p], ch] 
                                        
      }
      
    }

    
  }
  return(list(feature = featMat, label = labMat))
}