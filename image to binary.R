{library(readr)
 library(utile.tables)
  
  
  
  data_files <- list.files(path = "ai_dataset")
  
  
  for(index in 1:length(data_files)){
    working_folder <- "ai_dataset/"
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
    
    # need to figure out how to get rid of quotation marks around the values
    write.table(imageMatrix, file = paste("csv_ai_dataset/", current_file, sep = ""), col.names = F, row.names = F, sep = ",", quote = FALSE)
    
  }
}

