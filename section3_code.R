{
  library(readr)
  library(ggplot2)
  # matrix stats has functions for getting colMedian and standard deviation
  library(matrixStats)
  library(knitr) # using this to make a table for my report.
  
  mydata <- read.csv("40294886_features.csv", row.names = NULL)
  calculated_features <- data.frame(mydata)
  colnames(calculated_features) <- c("Label", "Index", "nr_pix", "rows_with_1", "cols_with_1",
                                     "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                                     "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                     "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
  
  mydata[ , 3:18] <- as.numeric(mydata[ , 3,18])
  
  # 3.1
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
  
  #general_histograms()
  
  # 3.2
  letters_calculated_features <- calculated_features[1:80,]
  non_letters_calculated_features<- calculated_features[81:140,]
  letters_calculated_features[, 18] <- as.numeric(letters_calculated_features[ , 18])
  non_letters_calculated_features[ , 18] <- as.numeric(non_letters_calculated_features[ , 18])
  
  # # get the mean of each column
  letters_mean <- c(colMeans(letters_calculated_features[ , 3:18]))
  non_letters_mean <- c(colMeans(non_letters_calculated_features[ , 3:18]))
  
  # get the medians of each column
  letters_median <- c(colMedians(as.matrix(letters_calculated_features[ , 3:18])))
  non_letters_median <- c(colMedians(as.matrix(non_letters_calculated_features[ , 3:18])))

  #get the standard deviation of each column
  letters_sd <- colSds(as.matrix(letters_calculated_features[, 3:18][sapply(letters_calculated_features[ , 3:18 ], is.numeric)]))
  non_letters_sd <- colSds(as.matrix(non_letters_calculated_features[ , 3:18][sapply(non_letters_calculated_features[ , 3:18], is.numeric)]))
  
  # printing tables containing the results for standard_deviation, mean, median for both letters and
  # non letters.
  # source for making tables: https://www.youtube.com/watch?v=hNgeVLotABg
  table = matrix(NA, nrow = 16, ncol = 0)
  rownames(table) = c("nr_pix", "rows_with_1", "cols_with_1",
                                     "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                                     "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                     "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
  
  
  table <- cbind(table, letters_mean)
  table <- cbind(table, non_letters_mean)
  table <- cbind(table, letters_median)
  table <- cbind(table, non_letters_median)
  table <- cbind(table, letters_sd)
  table <- cbind(table, non_letters_sd)
  
  features_table<-kable(table, caption = "This table shows the letter and non letters mean, median and standard deviation for each of the feature calculations")
  
  
  print(table)
  
  # cols with 1, no_neigh_vert and connected_areas seems to be the features with the greates differences and may be the easiest features for
  # discriminating whether an image is a letter or a non letter image.
  
  # density plots for mean, median and standard deviation of cols with 1 for letters and non letters
  
  # box plot for mean cols with 1
  qplot(data = table, x = letters_mean, geom = "boxplot")
  
  
  
  
}
