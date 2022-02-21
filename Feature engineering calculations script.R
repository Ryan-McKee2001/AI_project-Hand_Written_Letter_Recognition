{
  library(readr)
  library(utile.tables)
  
  # creating a list of all the files in the data_set
  folder_path <- "dataset/csv_images_dataset"
  data_folder <- list.files(path = folder_path)
  
  # updating the file path string so it can be used later
  folder_path <- paste(folder_path, "/", sep="")
  
  # program functions
  # These are the functions for calculating each of the features the dataset files
  
  # function returns the label for the current_file passed in parameters
  getFileLabel <- function(current_file){
    file_name <- current_file
    file_name_split <- strsplit(file_name, "_")
    
    file_names_split_matrix <- matrix(unlist(file_name_split), nrow = 1, byrow = TRUE)
    
    label <- file_names_split_matrix[1,2]
    
    return(label)
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
  
  neigh_1 <- function(current_file){
    
  }
  
  no_neigh_above <- function(current_file){
  
  }
  
  no_neigh_below <- function(current_file){
  
  }
  
  no_neigh_left <- function(current_file){
  
  }
  
  no_neigh_horiz <- function(current_file){
  
  }
  
  no_neigh_vert <- function(current_file){
  
  }
  
  connected_areas <- function(current_file){
  
  }
  
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
    
    calculated_features[1,1] <- getFileLabel(current_file_name)
    calculated_features[1,2] <- current_index
    calculated_features[1,3] <- nr_pix(current_file_image_matrix)
    calculated_features[1,4] <- rows_with_1(current_file)
    calculated_features[1,5] <- cols_with_1(current_file)
    calculated_features[1,6] <- rows_with_3p(current_file)
    calculated_features[1,7] <- cols_with_3p(current_file)
    calculated_features[1,8] <- aspect_ratio(current_file)
    #calculated_features[1,9]
    #calculated_features[1,10]
    #calculated_features[1,11]
    #calculated_features[1,12]
    #calculated_features[1,13]
    #calculated_features[1,14]
    #calculated_features[1,15]
    #calculated_features[1,16]
    #calculated_features[1,17]
    #calculated_features[1,18]
    
    print(calculated_features)
  }
}
  
