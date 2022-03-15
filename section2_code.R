{
  library(readr)
  #library(utile.tables)
  library(raster)
  library(igraph)
  
  # creating a list of all the files in the data_set
  folder_path <- "images"
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
  nr_pix <- function(current_file){
    return(sum(current_file))
  }
  
  # returns how many rows contain only 1 black pixel
  rows_with_1 <- function(current_file){
    rows_with_1_sum <- 0
    row_sums <- c(rowSums(current_file))
    
    for(i in 1:length(row_sums)){
      if(sum(row_sums[i]) == 1)
        rows_with_1_sum <- rows_with_1_sum + 1
    }
    
    return(rows_with_1_sum)
  }
  
  # checks how many columns only contain 1 black pixel
  cols_with_1 <- function(current_file){
    cols_with_1_sum <- 0
    col_sums <- c(colSums(current_file))
    
    for(i in 1:length(col_sums)){
      if(col_sums[i] == 1)
        cols_with_1_sum <- cols_with_1_sum + 1
    }
    
    return(cols_with_1_sum)
  }
  
  # returns number of rows with 3 or more pixels from image
  rows_with_3p <- function(current_file){
    rows_greater_than_3_sum <- 0
    row_sums<- c(rowSums(current_file))
    
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
    col_sums<- c(colSums(current_file))
    
    for(current_index in 1:length(col_sums)){
      if( col_sums[current_index] >= 3  ){
        cols_greater_than_3_sum <- cols_greater_than_3_sum + 1
      }
    }
    
    return(cols_greater_than_3_sum)
  }
  
  # this gets the aspect ratio width/height
  # need to validate aspect ratios are correct a1 calculation and function does not seem to be right
  aspect_ratio <- function(current_file){
    # get the pixel distance between the top most black pixel and bottom
    row_sum_matrix <- rowSums(current_file)
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
    
    col_sum_matrix <- colSums(current_file)
    
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
    num_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0,0,0,0,0,0,0)
          names(neighbours) <- c("upper left", "upper", "upper right", "left", "right", "bottom left", "bottom", "bottom right")
          
          # checking upper region
          if(row_index > 1){
            # upper
            if(current_file[row_index - 1, col_index] == 1)
              neighbours[2] <- 1
            
            # upper left
            if(col_index > 1){
              if(current_file[row_index - 1, col_index -1] == 1)
                neighbours[1] <- 1
            }
            
            # upper right
            if(col_index < ncol(current_file)){
              if(current_file[row_index - 1, col_index + 1] == 1)
                neighbours[3] <- 1
            }
          }
          
          # left
          if(col_index > 1){
            if(current_file[row_index, col_index - 1])
              neighbours[4] <- 1
          }
          
          # right
          if(col_index < ncol(current_file)){
            if(current_file[row_index, col_index +1])
              neighbours[5] <- 1
          }
          
          
          # checking the bottom region
          if(row_index < nrow(current_file)){
            # checking bottom
            if(current_file[row_index + 1, col_index] == 1)
              neighbours[7] <- 1
            
            # bottom left
            if(col_index > 1){
              if(current_file[row_index + 1, col_index -1] == 1)
                neighbours[6] <- 1
            }
            
            # bottom right
            if(col_index < ncol(current_file)){
              if(current_file[row_index + 1, col_index + 1] == 1)
                neighbours[8] <- 1
            }
          }
          
          if(sum(neighbours) == 1)
            num_neighbours <- num_neighbours + 1
        }
      }
    }
    
    return(num_neighbours)
  }
  
  # number of black pixels wiTh no black pixel neighbours
  # "upper left, upper, and "Upper right"
  no_neigh_above <- function(current_file){
    num_no_upper_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0,0)
          names(neighbours) <- c("upper left", "upper", "upper right")
          
          # checking upper region
          if(row_index > 1){
            # upper
            if(current_file[row_index - 1, col_index] == 1)
              neighbours[2] <- 1
            
            # upper left
            if(col_index > 1){
              if(current_file[row_index - 1, col_index -1] == 1)
                neighbours[1] <- 1
            }
            
            # upper right
            if(col_index < ncol(current_file)){
              if(current_file[row_index - 1, col_index + 1] == 1)
                neighbours[3] <- 1
            }
          }
          
          if(sum(neighbours) == 0)
            num_no_upper_neighbours <- num_no_upper_neighbours + 1
        }
      }
    }
    
    return(num_no_upper_neighbours)
  }
  
  # returns the number of black pixels that have no black
  # pixel neighbours "below", "below right", or "below left"
  no_neigh_below <- function(current_file){
    num_no_lower_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0,0)
          names(neighbours) <- c("bottom left", "bottom", "bottom right")
          
          # checking the bottom region
          if(row_index < nrow(current_file)){
            # checking bottom
            if(current_file[row_index + 1, col_index] == 1)
              neighbours[2] <- 1
            
            # bottom left
            if(col_index > 1){
              if(current_file[row_index + 1, col_index -1] == 1)
                neighbours[1] <- 1
            }
            
            # bottom right
            if(col_index < ncol(current_file)){
              if(current_file[row_index + 1, col_index + 1] == 1)
                neighbours[3] <- 1
            }
          }
          
          if(sum(neighbours) == 0)
            num_no_lower_neighbours <- num_no_lower_neighbours + 1
        }
      }
    }
    
    return(num_no_lower_neighbours)
  }
  
  # returns number of pixels that have no black pixel neighbours
  # on the left
  no_neigh_left <- function(current_file){
    num_no_left_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0,0)
          names(neighbours) <- c("upper left", "left", "bottom left")
          
          # checking the left
          if(col_index > 1){
            # checking left
            if(current_file[row_index, col_index - 1] == 1)
              neighbours[2] <- 1
            
            # bottom left
            if(row_index < nrow(current_file)){
              if(current_file[row_index + 1, col_index -1] == 1)
                neighbours[3] <- 1
            }
            
            # top left
            if(col_index < ncol(current_file)){
              if(current_file[row_index - 1, col_index - 1] == 1)
                neighbours[1] <- 1
            }
          }
          
          if(sum(neighbours) == 0)
            num_no_left_neighbours <- num_no_left_neighbours + 1
        }
      }
    }
    
    return(num_no_left_neighbours)
  }
  
  # returns number of pixels that have no black pixel neighbours
  # on the right
  no_neigh_right <- function(current_file){
    num_no_right_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0,0)
          names(neighbours) <- c("upper right", "right", "bottom right")
          
          # checking the right
          if(col_index < ncol(current_file)){
            # checking left
            if(current_file[row_index, col_index + 1] == 1)
              neighbours[2] <- 1
            
            # bottom right
            if(row_index < nrow(current_file)){
              if(current_file[row_index + 1, col_index + 1] == 1)
                neighbours[3] <- 1
            }
            
            # top right
            if(col_index < ncol(current_file)){
              if(current_file[row_index - 1, col_index + 1] == 1)
                neighbours[1] <- 1
            }
          }
          
          if(sum(neighbours) == 0)
            num_no_right_neighbours <- num_no_right_neighbours + 1
        }
      }
    }
    
    return(num_no_right_neighbours)
  }
  
  # returns number of black pixels that have no 
  # black pixel neighbours right or left
  no_neigh_horiz <- function(current_file){
    num_no_horiz_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0)
          names(neighbours) <- c("left", "right")
          
          # check left black pixel
          # neighbour
          if(col_index > 1){
            if(current_file[row_index, col_index - 1])
              neighbours[1] <- 1
          }
          if(col_index < ncol(current_file)){
            if(current_file[row_index, col_index +1])
              neighbours[2] <- 1
          }
          
          
          if(sum(neighbours) == 0)
            num_no_horiz_neighbours <- num_no_horiz_neighbours + 1
        }
      }
    }
    
    return(num_no_horiz_neighbours)
  }
  
  # returns number of black pixels that have no 
  # black pixels neighbours upper or lower
  no_neigh_vert <- function(current_file){
    num_no_vert_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        if(current_file[row_index, col_index] == 1){
          neighbours <- c(0,0)
          names(neighbours) <- c("above", "below")
          
          # check pixels above
          if(row_index < nrow(current_file)){
            if(current_file[row_index + 1, col_index] == 1)
              neighbours[1] <- 1
          }
          
          # check pixels below
          if(row_index < nrow(current_file))
            if(current_file[row_index - 1, col_index] == 1)
              neighbours[2] <- 1
          
          
          if(sum(neighbours) == 0)
            num_no_vert_neighbours <- num_no_vert_neighbours + 1
        }
      }
    }
    
    return(num_no_vert_neighbours)
  }
  
  # returns the number of connected areas in the image
  connected_areas <- function(current_file){
    rast <- raster(current_file)
    clump <- clump(rast, directions=8)
    return(maxValue(clump))
  }
  
  # returns the number of inclosed areas 
  eyes <- function(current_file){
    current_file <- rbind(current_file, 0) # had to extend the matrix to ensure I was not getting false eyes from images hitting borders.
    current_file <- cbind(current_file, 0)
    
    inversed_matrix <- +(!current_file) # ! inverses matrix, + turns values from true and false back to 1 and 0
    rast <- raster(inversed_matrix)
    clump <- clump(rast, direction=4) # orthogonally connected meaning direction = 4
    
    return(maxValue(clump) - 1)
  }
  
  # This custom function is going to check 6x6
  # subset of pixels from the graph and calculate the
  # percentage of pixels from the image that exist in the 
  # subset
  custom <- function(current_file){
    center_subset <- current_file[7:12, 7:12]
    pixel_count <- nr_pix(current_file)
    
    center_subset_pixel_count <- sum(rowSums(center_subset))
    
    return(center_subset_pixel_count/pixel_count*100)
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
    
    
    calculated_features[1,1] <- getFileLabel(current_file_name) # works
    calculated_features[1,2] <- getIndex(current_file_name) # works
    calculated_features[1,3] <- nr_pix(current_file) # works
    calculated_features[1,4] <- rows_with_1(current_file) # works
    calculated_features[1,5] <- cols_with_1(current_file) # works
    calculated_features[1,6] <- rows_with_3p(current_file) # works
    calculated_features[1,7] <- cols_with_3p(current_file) # works
    calculated_features[1,8] <- aspect_ratio(current_file) # works
    calculated_features[1,9] <- neigh_1(current_file) # works
    calculated_features[1,10] <- no_neigh_above(current_file) # works
    calculated_features[1,11] <- no_neigh_below(current_file) # works
    calculated_features[1,12] <- no_neigh_left(current_file) # works
    calculated_features[1,13] <- no_neigh_right(current_file) # works
    calculated_features[1,14] <- no_neigh_horiz(current_file) # works
    calculated_features[1,15] <- no_neigh_vert(current_file) # works
    calculated_features[1,16] <- connected_areas(current_file)  # works
    calculated_features[1,17] <- eyes(current_file) # works
    calculated_features[1,18] <- custom(current_file) # works
    
    write.table(calculated_features, file = "40294886_features.csv", append = T, sep = ",", col.names = F, row.names = F,  quote = F)
  }
}
