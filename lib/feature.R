


feature <- function(LR_dir, HR_dir, n_points=1000){
  
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
  
  
  get_label <- function(HR_col_idx, HR_row_idx, HR_image = as.array(imgHR), padd_LRimage = imgLR_padded){
    lable_array <- array(0, c(4,3))
    lable_4points <- HR_image[c(HR_row_idx-1, HR_row_idx), c(HR_col_idx-1, HR_col_idx), ]
    HR_t <- lable_4points - rep(padd_LRimage[HR_row_idx/2+1, HR_col_idx/2+1, ], each=4)
    lable_array[1:2, ]<- HR_t[1, , ]
    lable_array[3:4, ]<- HR_t[2, , ]
    return(lable_array)
  }
  
  library("EBImage")
  n_files <- length(list.files(LR_dir))

  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))  # n_files * n_points rows by 8 cols
  labMat <- array(NA, c(n_files * n_points, 4, 3))   # n_files * n_points rows by 4 cols
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- as.array(readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg")))
    imgHR <- as.array(readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg")))
    
    
    ### step 1. sample n_points from imgLR
    row_idx <- sample(dim(imgLR)[1], n_points, replace = TRUE)
    col_idx <- sample(dim(imgLR)[2], n_points, replace = TRUE)
    HR_ridx <- 2*row_idx
    HR_cidx <- 2*col_idx
    
    # imgLR_padded <- array(0, c(nrow_LR+2, ncol_LR+2, 3))
    # imgLR_padded[ , , 1]<- rbind(rep(0, ncol_LR+2), cbind(rep(0, nrow_LR), as.matrix(imgLR[ , , 1]), rep(0, nrow_LR)), rep(0, ncol_LR+2))
    # imgLR_padded[ , , 2]<- rbind(rep(0, ncol_LR+2), cbind(rep(0, nrow_LR), as.matrix(imgLR[ , , 2]), rep(0, nrow_LR)), rep(0, ncol_LR+2))
    # imgLR_padded[ , , 3]<- rbind(rep(0, ncol_LR+2), cbind(rep(0, nrow_LR), as.matrix(imgLR[ , , 3]), rep(0, nrow_LR)), rep(0, ncol_LR+2))
    
    
    ### Zero Padding LR images for feature extraction
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR_padded <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)
    
    img_id <- (n_points*(i-1)+1):(n_points*i)
    
    MID_feature <- mapply(get_feature, col_idx+1, row_idx+1)
    featMat[img_id, , 1]<- t(MID_feature[1:8, ])
    featMat[img_id, , 2]<- t(MID_feature[9:16, ])
    featMat[img_id, , 3]<- t(MID_feature[17:24, ])
    
    MID_label <- mapply(get_label, HR_cidx, HR_ridx )
    labMat[img_id, , 1]<- t(MID_label[1:4, ])
    labMat[img_id, , 2]<- t(MID_label[5:8, ])
    labMat[img_id, , 3]<- t(MID_label[9:12, ])

  }
  return(list(feature = featMat, label = labMat))
}