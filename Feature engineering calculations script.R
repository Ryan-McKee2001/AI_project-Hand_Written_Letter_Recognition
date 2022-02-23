{
  library(readr)
  library(utile.tables)
  library(raster)
  
  # creating a list of all the files in the data_set
  folder_path <- "dataset/csv_images_dataset"
  data_folder <- list.files(path = folder_path)
  
  # updating the file path string so it can be used later
  folder_path <- paste(folder_path, "/", sep="")
  
  # program functions
  # These are the functions for calculating each of the features the dataset files
  
  # function returns the label for the current_file passed in parameters
  getFileLabel <- function(current_file_name){
    file_name <- current_file_name
    file_name_split <- strsplit(file_name, "_")
    
    file_names_split_matrix <- matrix(unlist(file_name_split), nrow = 1, byrow = TRUE)
    
    label <- file_names_split_matrix[1,2]
    
    return(label)
  }
  
  # gets the current files index from the file name
  getIndex <- function(current_file_name){
    file_name <- current_file_name
    file_name_split <- strsplit(file_name, "_")
    
    file_name_split_matrix <- matrix(unlist(file_name_split), nrow = 1, byrow = TRUE)
    
    index_with_extension <- file_name_split_matrix[1,3]
    
    index <- substr(index_with_extension, 1, 2)
    return(index)
  }
  
  # return the number of black pixels in image
  nr_pix <- function(current_file_image_matrix){
    return(sum(current_file_image_matrix))
  }
   
  # counts the number of rows with 1 
  rows_with_1 <- function(current_file_image_matrix){
    rows_greater_than_1_sum <- 0
    row_sums<- c(rowSums(current_file_image_matrix))
    
    for(current_index in 1:length(row_sums)){
      if( row_sums[current_index] > 0 ){
        rows_greater_than_1_sum <- rows_greater_than_1_sum + 1
      }
    }
    
    return(rows_greater_than_1_sum)
  }

  cols_with_1 <- function(current_file){
    cols_greater_than_1_sum <- 0
    col_sums<- c(colSums(current_file_image_matrix))
    
    for(current_index in 1:length(col_sums)){
      if( col_sums[current_index] > 0 ){
        cols_greater_than_1_sum <- cols_greater_than_1_sum + 1
      }
    }
    
    return(cols_greater_than_1_sum)
  }

  # returns number of rows with 3 or more pixels from image
  rows_with_3p <- function(current_file_image_matrix){
    rows_greater_than_3_sum <- 0
    row_sums<- c(rowSums(current_file_image_matrix))
    
    for(current_index in 1:length(row_sums)){
      if( row_sums[current_index] >= 3){
        rows_greater_than_3_sum <- rows_greater_than_3_sum + 1
      }
    }
    
    return(rows_greater_than_3_sum)
  }


  # returns number of columns with 3 or more pixels
  cols_with_3p<- function(current_file){
    cols_greater_than_3_sum <- 0
    col_sums<- c(colSums(current_file_image_matrix))
    
    for(current_index in 1:length(col_sums)){
      if( col_sums[current_index] >= 3  ){
        cols_greater_than_3_sum <- cols_greater_than_3_sum + 1
      }
    }
    
    return(cols_greater_than_3_sum)
  }

  # this gets the aspect ratio width/height
  # need to validate aspect ratios are correct a1 calculation and function does not seem to be right
  aspect_ratio <- function(current_file_image_matrix){
    # get the pixel distance between the top most black pixel and bottom
    row_sum_matrix <- rowSums(current_file_image_matrix)
    top_element <- 0
    bottom_element <- 0
    
    current_index <- 1
    while(top_element == 0){
      if(row_sum_matrix[current_index] > 0){
        top_element <- current_index
        break
      }
      current_index <- current_index + 1
    }
    
    # get the pixel index of the bottom most black pixel
    current_index <- length(row_sum_matrix)
    
    while(bottom_element == 0){
      if(row_sum_matrix[current_index] > 0){
        bottom_element <- current_index
        break
      }
      current_index <- current_index - 1
    }
    
    
    height <- bottom_element - top_element
    
    
    # getting the width
    left_most_element <- 0
    right_most_element <- 0
    
    col_sum_matrix <- colSums(current_file_image_matrix)
    
    current_index <- 1
    while(left_most_element == 0){
      if(col_sum_matrix[current_index] > 0){
        left_most_element <- current_index
        break
      }
      current_index <- current_index + 1
    }
    
    # get the pixel index of the bottom most black pixel
    current_index <- length(col_sum_matrix)
    
    while(right_most_element == 0){
      if(col_sum_matrix[current_index] > 0){
        right_most_element <- current_index
        break
      }
      current_index <- current_index - 1
    }
    
    width <- right_most_element - left_most_element
    
    return(width/height)
    
  }
  
  # This returns the number of black pixels with only 1 black pixel neighbour
  neigh_1 <- function(current_file){
  }
  
  # number of black pixels wih no black pixel neighbours
  # "upper left, upper, and "Upper right"
  no_neigh_above <- function(current_file){
  }
  
  # returns the number of elements that have no pixels "below", "below right", or "below left"
  no_neigh_below <- function(current_file){
  }
  
  # returns number of black pixels with no black pixel neighbours in the 
  # "upper left", "left", or "lower left" positions.
  # When comparing checking the upper left  
  no_neigh_left <- function(current_file){
  }
  
  # Returns number of black pixels with no black pixel neighbours in the 
  # 'left" and 'right' positions
  no_neigh_horiz <- function(current_file){
  
  }
  
  # Returns the number of black pixels with no black pixel neighbours in the 
  # "upper" or "lower" positions
  no_neigh_vert <- function(current_file){
  
  }
  
  # 
  connected_areas <- function(current_file){
    rast1 <- raster(current_file)
    plot(rast1)
  }
  
  # checking how many enclosed spaces are in an the image
  # e.g. A has 1, B has 2
  eyes <- function(current_file){
  
  }
  
  # for custom I think I should check for enclosed area.
  # this would allow me to distinguish between letters
  # and exclude an image from being a smliey face or some of the
  # letters
  custom <- function(current_file){
  
  }

  
  for(current_index in 1:length(data_folder))
  {
    current_file_name <- data_folder[current_index]
    current_file_path <- paste(folder_path, current_file_name, sep = "")
    
    current_file <- as.matrix(read.table(file = current_file_path, header = F, sep = ","))
    
    calculated_features = matrix(ncol = 18, nrow = 1)
    colnames(calculated_features) <- c("Label", "Index", "nr_pix", "rows_with_1", "cols_with_1",
                                       "rows_with_3p", "cols_with_3p", "aspect ratio", "neigh_1",
                                       "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                       "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
    
    #This just creates a rater of the current_file so it can be visualised
    # rast1 <- raster(current_file)
    # plot(rast1)
    
    calculated_features[1,1] <- getFileLabel(current_file_name) # works
    calculated_features[1,2] <- getIndex(current_file_name)
    calculated_features[1,3] <- nr_pix(current_file_image_matrix) # works
    calculated_features[1,4] <- rows_with_1(current_file) # works
    calculated_features[1,5] <- cols_with_1(current_file) # works
    calculated_features[1,6] <- rows_with_3p(current_file) # works
    calculated_features[1,7] <- cols_with_3p(current_file) # works
    calculated_features[1,8] <- aspect_ratio(current_file) # works
    
    # creating a matrix containing vertices with each of the neighbours for 
    # pixels in the current_file
    pixel_neighbours_matrix <- matrix(nrow = 18, ncol = 18)
    
    for(current_pixel_row in 1:nrow(current_file)){
      for(current_pixel_col in 1:ncol(current_file)){
        upper <- 0
        upper_left <- 0
        upper_right <- 0
        
        left <- 0
        right <- 0
        
        bottom <- 0
        bottom_left <- 0
        bottom_right <- 0
        
        # assigning upper values
        if(current_pixel_row > 1){
          # upper
          if(current_file[current_pixel_row -1, current_pixel_col] == 1)
            upper <- 1
          # upper left
          if(current_pixel_col > 1){
            if(current_file[current_pixel_row -1, current_pixel_col - 1] == 1)
              upper_left <- 1
          }
          # upper right
          if(current_pixel_col < ncol(current_file)){
            if(current_file[current_pixel_row -1, current_pixel_col + 1] == 1)
              upper_right <- 1
          }
        }
        
        # get right and left neighbours
        # left
        if(current_pixel_col > 1){
          if(current_file[current_pixel_row, current_pixel_col - 1] == 1){
            left <- 1
          }
        }
        
        # right
        if(current_pixel_col < ncol(current_file)){
          if(current_file[current_pixel_row, current_pixel_col + 1] == 1){
            right <- 1
          }
        }
        
        # get the bottom, bottom left, and bottom right neighbours
        if(current_pixel_col < ncol(current_file)){
          # get bottom
          if(current_file[current_pixel_row + 1, current_pixel_col] == 1)
            bottom <- 1
          
          # get bottom left
          if(current_pixel_col > 1){
            if(current_file[current_pixel_row + 1, current_pixel_col - 1] == 1)
              bottom_left <- 1
          }
          
          # get bottom right
          if(current_pixel_col < ncol(current_file)){
            if(current_file[current_pixel_row + 1, current_pixel_col + 1] == 1)
              bottom_right <- 1
          }
        }
        
        
        current_pixel_neighbours <- c(upper_left, upper, upper_right, left, right, bottom_left, bottom, bottom_right)
        names(current_pixel_neighbours) <- c("upper left", "upper", "upper right", "left", "right", "bottom_left", "bottom", "bottom right")
        
        pixel_neighbours_matrix[current_pixel_row, current_pixel_col] <- current_pixel_neighbours
      }
    }
    
    
    
    
    calculated_features[1,9] <- neigh_1(current_file) # not working, needs testing
    calculated_features[1,10] <- no_neigh_above(current_file) # works // for neigh need to ask if pixels that are on top row count as have no pixel neighbours
    calculated_features[1,11] <- no_neigh_below(current_file) # works
    #calculated_features[1,12] <- no_neigh_left(current_file)
    #calculated_features[1,13] <- no_neigh_right(current_file)
    #calculated_features[1,14] <- no_neigh_horiz(current_file)
    #calculated_features[1,15] <- no_neigh_vert(current_file)
   # calculated_features[1,16] <- connected_areas(current_file) <- connected_areas(current_file)
    #calculated_features[1,17] <- eyes(current_file)
    #calculated_features[1,18] <- custom(current_file)
    
    print(calculated_features)
  }
}
  
