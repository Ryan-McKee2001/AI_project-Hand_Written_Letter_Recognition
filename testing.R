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

  current_file <- as.matrix(read.table(file = current_file_path, header = T, sep = ","))
  
  # print(current_file)
  # 
  # current_file.pad <- rbind(NA, cbind(NA, current_file, NA), NA)
  # 
  # 
  # ind = 2:(18 + 1) # row/column indices of the "middle"
  # neigh = rbind(upper  = as.vector(current_file.pad[ind - 1, ind    ]),
  #               upper_right = as.vector(current_file.pad[ind - 1, ind + 1]),
  #               right  = as.vector(current_file.pad[ind    , ind + 1]),
  #               bottom_right = as.vector(current_file.pad[ind + 1, ind + 1]),
  #               bottom  = as.vector(current_file.pad[ind + 1, ind    ]),
  #               bottom_left = as.vector(current_file.pad[ind + 1, ind - 1]),
  #               left  = as.vector(current_file.pad[ind    , ind - 1]),
  #               upper_left = as.vector(current_file.pad[ind - 1, ind - 1]))
  # 
  # print(neigh[1,1])
  
  df_neigh <- data.frame()
  
  frame <- data
  
}


