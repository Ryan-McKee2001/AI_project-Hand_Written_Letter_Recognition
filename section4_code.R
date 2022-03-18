{
  library(readr)
  library(ggplot2)
  library(tidyverse)
  library(matrixStats)
  library(knitr) 
  
  # This is the setup code for section 3 it just read the feature calculations into a data.frame
  mydata <- read.csv("40294886_features.csv", row.names = NULL)
  calculated_features <- data.frame(mydata)
  calculated_features_col_names <- c("Label", "Index", "nr_pix", "rows_with_1", "cols_with_1",
                                     "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                                     "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                     "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
  colnames(calculated_features) <- calculated_features_col_names
  mydata[ , 3:18] <- as.numeric(mydata[ , 3,18])
  
  
  print(calculated_features)


}