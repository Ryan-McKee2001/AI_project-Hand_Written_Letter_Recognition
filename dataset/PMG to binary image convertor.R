library(readr)
  
  pgmFile <- read_lines("ai_datasets/emotes/smiley_face/s2.PGM", skip = 4)
  imageMatrix <- matrix(test, nrow = 18, ncol = 18)
  
  for(row in 1:nrow(imageMatrix)) 
  {
    for(col in 1:ncol(imageMatrix)) 
    {
      if(imageMatrix[row,col] < 128)
      {
        imageMatrix[row,col] = 0
      }
      else
      {
        imageMatrix[row,col] = 255
      }
    }
  }
  
  print(imageMatrix)
  


