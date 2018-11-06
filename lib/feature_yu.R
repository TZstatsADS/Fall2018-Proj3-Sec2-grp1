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


    # # Zero Padding LR images for feature extraction
     imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
     imgLR <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
     
     imgHR <- abind(array(0, replace(dim(imgHR), 2, 2)),imgHR, array(0, replace(dim(imgHR), 2, 2)), along = 2)
     imgHR <- abind(array(0, replace(dim(imgHR), 1, 2)),imgHR, array(0, replace(dim(imgHR), 1, 2)), along = 1)
     
    # ### step 1. sample n_points from imgLR
     row_idx <- sample(2:(dim(imgLR)[1] - 1), n_points, replace = TRUE)
     col_idx <- sample(2:(dim(imgLR)[2] - 1), n_points, replace = TRUE)

     
     for (p in 1:n_points){
       
       for (ch in 1:3){
       
         ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
         ###           tips: padding zeros for boundary points
         
         nb_points = c(imgLR[row_idx[p]-1,(col_idx[p]-1):(col_idx[p]+1),ch], 
                       imgLR[row_idx[p],col_idx[p]-1,ch],
                       imgLR[row_idx[p],col_idx[p]+1,ch],
                       imgLR[row_idx[p]+1,(col_idx[p]-1):(col_idx[p]+1),ch])
         
         featMat[(p + (i-1)*n_points),,ch] = nb_points - imgLR[row_idx[p], col_idx[p], ch]
         
         ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
         sub_points = imgHR[(2*row_idx[p]-1):(2*row_idx[p]), (2*col_idx[p]-1):(2*col_idx[p]) ,ch]
         labMat[p + (i-1)*n_points,,ch] = sub_points - imgLR[row_idx[p], col_idx[p], ch] 
         
       }
       
     }

  }
  return(list(feature = featMat, label = labMat))
}
