{library(readr)
  library(utile.tables)
  
  
  # * To change the folder that contains the .pgm files
  # Just change the path in list.files() belowe to be equal
  # to the file path of the folder, and change the working_folder
  # to be equal to the file path + / *
  data_files <- list.files(path = "ai_dataset")
  working_folder <- "ai_dataset/"
  
  for(index in 1:length(data_files)){
    
    current_file <- data_files[index]
    
    current_file_path <- paste(working_folder, current_file, sep = "")
    
    pgmFile <- read_lines(file = current_file_path, skip = 4)
    imageMatrix <- matrix(pgmFile, nrow = 18, ncol = 18)
    
    for(row in 1:nrow(imageMatrix)) 
    {
      for(col in 1:ncol(imageMatrix)) 
      {
        if(imageMatrix[row,col] < 128)
        {
          imageMatrix[row,col] = 1
        }
        else
        {
          imageMatrix[row,col] = 0
        }
      }
    }
    
    imageMatrix <- t(imageMatrix)
    
    # * To change the folder that the updated dataset gets created in just change the first argument in the paste function to the file path for updated folder + / *
    write.table(imageMatrix, file = paste("csv_ai_dataset/", gsub('.pgm', '.csv', data_files[index]), sep = ""), col.names = F, row.names = F, sep = ",", quote = FALSE)
    
  }
  
  print("csv dataset has been successfully created.")
}
