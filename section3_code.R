{
  library(readr)
  library(ggplot2)
  
  mydata <- read.csv("40294886_features.csv", row.names = NULL)
  calculated_features <- data.frame(mydata)
  colnames(calculated_features) <- c("Label", "Index", "nr_pix", "rows_with_1", "cols_with_1",
                                     "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                                     "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                     "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
  
  
  general_histograms <- function(){
    # need to use ggplot to make each of these histograms better.
    nr_pix_hist <- hist(calculated_features$nr_pix, main = "Number of black pixels", xlab="Black pixel number", border = "blue", col = "green", las = 1)
    rows_with_1_hist <- hist(calculated_features$rows_with_1, main = "rows with 1 pixel", xlab = "Rows with 1 number", border = "blue", col = "green")
    cols_with_1_hist <- hist(calculated_features$cols_with_1, main = "columns with 1 pixel", xlab = "columns with 1 number", border = "blue", col = "green")
    rows_with_3p_hist <- hist(calculated_features$rows_with_3p, main = "rows with 3 pixels or more", xlab = "rows with 3 pixels or more", border = "blue", col = "green")
    cols_with_3p_hist <- hist(calculated_features$cols_with_3p, main = "columns with 3 pixels of more", xlab = "columns with 3 pixels or more", border = "blue", col = "green")
    aspect_ratio_hist <- hist(calculated_features$aspect_ratio, main = "aspect ratios", xlab = "aspect ratio", border = "blue", col = "green")
    
    print(nr_pix_hist)
    print(rows_with_1_hist)
    print(cols_with_1_hist)
    print(rows_with_3p_hist)
    print(cols_with_3p_hist)
    print(aspect_ratio_hist)
  }
  
  general_mean_histograms <- function(){
    # need to use ggplot to make each of these histograms better.
    nr_pix_hist <- hist(calculated_features$nr_pix, main = "Number of black pixels", xlab="Black pixel number", border = "blue", col = "green", las = 1)
    rows_with_1_hist <- hist(calculated_features$rows_with_1, main = "rows with 1 pixel", xlab = "Rows with 1 number", border = "blue", col = "green")
    cols_with_1_hist <- hist(calculated_features$cols_with_1, main = "columns with 1 pixel", xlab = "columns with 1 number", border = "blue", col = "green")
    rows_with_3p_hist <- hist(calculated_features$rows_with_3p, main = "rows with 3 pixels or more", xlab = "rows with 3 pixels or more", border = "blue", col = "green")
    cols_with_3p_hist <- hist(calculated_features$cols_with_3p, main = "columns with 3 pixels of more", xlab = "columns with 3 pixels or more", border = "blue", col = "green")
    aspect_ratio_hist <- hist(calculated_features$aspect_ratio, main = "aspect ratios", xlab = "aspect ratio", border = "blue", col = "green")
    
    print(nr_pix_hist)
    print(rows_with_1_hist)
    print(cols_with_1_hist)
    print(rows_with_3p_hist)
    print(cols_with_3p_hist)
    print(aspect_ratio_hist)
  }
  
  #general_histograms()
  
  
  
  # 3.2
  # getting letter and other features
  letters_calculated_features <- calculated_features[1:80,]
  non_letters_calculated_features<- calculated_features[81:140,]
  
  print(mean(letters_calculated_features[,3:18]))

}