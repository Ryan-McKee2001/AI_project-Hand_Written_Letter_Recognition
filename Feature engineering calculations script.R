{
  library(readr)
  library(utile.tables)
  
  # creating a list of all the files in the data_set
  folder_path <- "testing_ai_dataset"
  data_folder <- list.files(path = folder_path)
  
  print(data_folder)
  
  # updating the file path string so it can be used later
  folder_path <- paste(folder_path, "/", sep="")
  
  # program functions
  # These are the functions for calculating each of the features the dataset files
  
  getFileLabel <- function(current_file){
    file_name <- current_file
    file_name_split <- strsplit(file_name, "_")
    
    file_names_split_matrix <- matrix(unlist(file_name_split), nrow = 1, byrow = TRUE)
    
    label <- file_names_split_matrix[1,2]
    
    return(label)
  }

  nr_pix <- function(current_file){
    
  }
   
  # rows_with_1 <- function(current_file){
  #   
  # }
  # 
  # cols_with_1 <- function(current_file){
  #   
  # }
  # 
  # 
  # rows_with_3p <- function(current_file){
  #   
  # }
  # 
  # 
  # cols_with_3p<- function(current_file){
  #   
  # }
  # 
  # 
  # aspect_ratio <- function(current_file){
  #   
  # }
  # 
  # neigh_1 <- function(current_file){
  #   
  # }
  # 
  # no_neigh_above <- function(current_file){
  #   
  # }
  # 
  # no_neigh_below <- function(current_file){
  #   
  # }
  # 
  # no_neigh_left <- function(current_file){
  #   
  # }
  # 
  # no_neigh_horiz <- function(current_file){
  #   
  # }
  # 
  # no_neigh_vert <- function(current_file){
  #   
  # }
  # 
  # connected_areas <- function(current_file){
  #   
  # }
  # 
  # eyes <- function(current_file){
  #   
  # }
  # 
  # custom <- function(current_file){
  #   
  # }
  # 
  
  for(current_index in 1:length(data_folder))
  {
    
    current_file_name <- data_folder[current_index]
    current_file_path <- paste(folder_path, current_file_name, sep = "")
    
    current_file <- read_lines(file = current_file_path)
    
    current_file_image_matrix <- matrix(current_file, nrow = 18, ncol = 18)
    
    print(current_file_image_matrix)
    
    calculated_features = matrix(ncol = 18, nrow = 1)
    
    calculated_features[1,1] <- getFileLabel(current_file_name)
    calculated_features[1,2] <- currentIndex
    #calculated_features[1,3] <- nr_pix(current_file)
    #calculated_features[1,4]
    #calculated_features[1,5]
    #calculated_features[1,6]
    #calculated_features[1,7]
    #calculated_features[1,8]
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
  }
}
  
