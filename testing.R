{
  library(readr)
  library(utile.tables)
  library(raster)
  
  
  # creating a list of all the files in the data_set
  folder_path <- "dataset/csv_images_dataset"
  data_folder <- list.files(path = folder_path)

  # updating the file path string so it can be used later
  folder_path <- paste(folder_path, "/", sep="")

  current_file_name <- data_folder[1]
  current_file_path <- paste(folder_path, current_file_name, sep = "")

  current_file <-matrix(read.table(file = current_file_path, header = F, sep = ",")
  
  print(current_file)
  
  no_neigh_above <- function(current_file){
    num_no_upper_neighbours <- 0
    
    for(row_index in 1:nrow(current_file)){
      for(col_index in 1:ncol(current_file)){
        print("goes in for loop")
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
          
          print(sum(neighbours))
          
          if(sum(neighbours) == 0)
            num_no_upper_neighbours <- num_no_upper_neighbours + 1
        }
      }
      
      return(num_no_upper_neighbours)
    }
    
    
  }
  
  print("testing")
  print(no_neigh_above(current_file))
  
}


