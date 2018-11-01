#############################################################
### Construct features and responses for training images###
#############################################################

### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  #n_files<-5
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for (j in 1:3){
    g<-1
    print(j)
  for(k in 1:n_files){
    imgLR1 <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", k), ".jpg"))
    imgLR<-imgLR1
    imgHR1 <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", k), ".jpg"))
    imgHR<-imgHR1
    ### step 1. sample n_points from imgLR
    n_row<-nrow(imgLR)
    n_col<-ncol(imgLR)
    total<-n_row*n_col
    sample_LR<-sort(sample(1:total, n_points), decreasing = FALSE)#uniform distribution sampling and this is index
    
    ### step 2. for each sampled point in imgLR,
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    left_boundry<-seq(1,(n_row*n_col-1), by=n_col)[-1]
    top_boundry<-c(2:(n_col-1))
    right_boundry<-seq(n_col,(n_row*n_col-1), by=n_col )[-1]
    bottom_bountry<-c(((n_row*n_col)-n_col+2):(n_row*n_col-1))
    four_corner<-c(1,n_col, n_row*n_col-n_col+1, n_row*n_col)
    boundry_index<-unique(c(left_boundry,top_boundry,right_boundry,bottom_bountry))
    #mean_matrix<-matrix(rep(1/9,9),nrow = 3)
    
    for(i in sample_LR){
      #print(i)
      #print(g)
      #print(k)
      if(all(i !=right_boundry)){
        i_row<-(i%/%n_col)+1
        i_col<-(i%%n_col)
      }
      else{
        i_row<-(i%/%n_col)
        i_col<-n_col
      }
      
      
      if(all(i !=boundry_index)){

        #stored in featMat:
        featMat[g,1,j]<-as.numeric(imgLR[i_row-1,i_col-1,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,2,j]<-as.numeric(imgLR[i_row-1,i_col,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,3,j]<-as.numeric(imgLR[i_row-1, i_col+1,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,4,j]<-as.numeric(imgLR[i_row,i_col-1,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,5,j]<-as.numeric(imgLR[i_row, i_col+1,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,6,j]<-as.numeric(imgLR[i_row+1,i_col-1,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,7,j]<-as.numeric(imgLR[i_row+1,i_col,j])-as.numeric(imgLR[i_row, i_col,j])
        featMat[g,8,j]<-as.numeric(imgLR[i_row+1, i_col+1,j])-as.numeric(imgLR[i_row, i_col,j])
      }
      else{
        if(i == four_corner[1]){
          featMat[g,1,j]<-0
          featMat[g,2,j]<-0
          featMat[g,3,j]<-0
          featMat[g,4,j]<-0
          featMat[g,5,j]<-as.numeric(imgLR[i_row,i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,6,j]<-0
          featMat[g,7,j]<-as.numeric(imgLR[i_row+1,i_col,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,8,j]<-as.numeric(imgLR[i_row+1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
        }
        if(i == four_corner[2]){
          featMat[g,1,j]<-0
          featMat[g,2,j]<-0
          featMat[g,3,j]<-0
          featMat[g,4,j]<-as.numeric(imgLR[i_row, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,5,j]<-0
          featMat[g,6,j]<-as.numeric(imgLR[i_row+1,i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,7,j]<-as.numeric(imgLR[i_row+1,i_col,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,8,j]<-0
        }
        if(i == four_corner[3]){
          featMat[g,1,j]<-0
          featMat[g,2,j]<-as.numeric(imgLR[i_row-1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,3,j]<-as.numeric(imgLR[i_row-1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,4,j]<-0
          featMat[g,5,j]<-as.numeric(imgLR[i_row, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,6,j]<-0
          featMat[g,7,j]<-0
          featMat[g,8,j]<-0
        }
        if(i == four_corner[4]){
          featMat[g,1,j]<-as.numeric(imgLR[i_row-1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,2,j]<-as.numeric(imgLR[i_row-1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,3,j]<-0
          featMat[g,4,j]<-as.numeric(imgLR[i_row, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
          featMat[g,5,j]<-0
          featMat[g,6,j]<-0
          featMat[g,7,j]<-0
          featMat[g,8,j]<-0
        }
        else{
          if(any(i == top_boundry)){
            featMat[g,1,j]<-0
            featMat[g,2,j]<-0
            featMat[g,3,j]<-0
            featMat[g,4,j]<-as.numeric(imgLR[i_row, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,5,j]<-as.numeric(imgLR[i_row, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,6,j]<-as.numeric(imgLR[i_row+1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,7,j]<-as.numeric(imgLR[i_row+1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,8,j]<-as.numeric(imgLR[i_row+1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
          }
          if(any(i == bottom_bountry)){
            featMat[g,1,j]<-as.numeric(imgLR[i_row-1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,2,j]<-as.numeric(imgLR[i_row-1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,3,j]<-as.numeric(imgLR[i_row-1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,4,j]<-as.numeric(imgLR[i_row, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,5,j]<-as.numeric(imgLR[i_row, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,6,j]<-0
            featMat[g,7,j]<-0
            featMat[g,8,j]<-0
          }
          
          if(any(i == left_boundry)){
            featMat[g,1,j]<-0
            featMat[g,2,j]<-as.numeric(imgLR[i_row-1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,3,j]<-as.numeric(imgLR[i_row-1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,4,j]<-0
            featMat[g,5,j]<-as.numeric(imgLR[i_row, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,6,j]<-0
            featMat[g,7,j]<-as.numeric(imgLR[i_row+1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,8,j]<-as.numeric(imgLR[i_row+1, i_col+1,j])-as.numeric(imgLR[i_row,i_col,j])
          }
          
          if(any(i == right_boundry)){
            featMat[g,1,j]<-as.numeric(imgLR[i_row-1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,2,j]<-as.numeric(imgLR[i_row-1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,3,j]<-0
            featMat[g,4,j]<-as.numeric(imgLR[i_row, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,5,j]<-0
            featMat[g,6,j]<-as.numeric(imgLR[i_row+1, i_col-1,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,7,j]<-as.numeric(imgLR[i_row+1, i_col,j])-as.numeric(imgLR[i_row,i_col,j])
            featMat[g,8,j]<-0
          }
        }
      }

    ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      
      
    
      labMat[g,1,j]<-imgHR[2*i_row-1, 2*i_col-1,j]
      labMat[g,2,j]<-imgHR[2*i_row-1, 2*i_col,j]
      labMat[g,3,j]<-imgHR[2*i_row, 2*i_col-1,j]
      labMat[g,4,j]<-imgHR[2*i_row, 2*i_col,j]
      g<-g+1 
    }
  }
    ### step 3. repeat above for three channels
   #add for-loop above
    
  }
  return(list(feature = featMat, label = labMat))
}