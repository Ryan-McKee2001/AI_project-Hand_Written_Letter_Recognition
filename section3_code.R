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
  summary_data = matrix(NA, nrow = 16, ncol = 0)
  rownames(summary_data) = c("nr_pix", "rows_with_1", "cols_with_1",
                                     "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                                     "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                                     "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
  
  
  summary_data <- cbind(summary_data, letters_mean)
  summary_data <- cbind(summary_data, non_letters_mean)
  summary_data <- cbind(summary_data, letters_median)
  summary_data <- cbind(summary_data, non_letters_median)
  summary_data <- cbind(summary_data, letters_sd)
  summary_data <- cbind(summary_data, non_letters_sd)
  
  features_table<-kable(summary_data, caption = "This table shows the letter and non letters mean, median and standard deviation for each of the feature calculations")
  # this prints out the table containing all the values for the features for the report
  print(features_table)
  
  #print(summary_data)
  
  # cols with 1, no_neigh_vert and connected_areas seems to be the features with the greates differences and may be the easiest features for
  # discriminating whether an image is a letter or a non letter image.
  
  # density plots for mean, median and standard deviation of cols with 1 for letters and non letters
  # creating box plots I used https://www.datamentor.io/r-programming/box-plot/#:~:text=In%20R%2C%20boxplot%20(and%20whisker,numeric%20vectors%20as%20its%20components.
  
  # box plot for comparing all the summary data, to show in report
  summary_boxplot <- boxplot(letters_mean, non_letters_mean, letters_median, non_letters_median, letters_sd, non_letters_sd,
          main = "summary data comparisons",
          at = c(1,2,3,4,5,6),
          names = c("letter mean", "non letter mean", "letters median", "non letters median", "letters standard deviation", "non letters standard deviation"),
          las = 2,
          col = c("orange","red"),
          border = "brown",
          horizontal = TRUE,
          notch = FALSE
  )
  
  # histogram comparisons for each of the 3 chosen features
  # get a histogram for cols with 1 for letters
  cols_with_1_letters <- letters_calculated_features$cols_with_1
  cols_with_1_non_letters <- non_letters_calculated_features$cols_with_1
  
  # this creates a histogram for the columns with 1 feature for letters and non letters
  hist(cols_with_1_letters, main = "columns with 1", xlab = "Columns with 1", xlim=c(0,10), col="red")
  hist(cols_with_1_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  
  
  # 3.3
  # histogram comparisons for no neigh vert
  no_neigh_vert_letters <- letters_calculated_features$no_neigh_vert
  no_neigh_vert_non_letters <- non_letters_calculated_features$no_neigh_vert
  
  hist(no_neigh_vert_letters, main = "number of pixels with no neighbours vertical per dataset", xlab = "number no neighbours vertical", xlim=c(0,10), col="blue")
  hist(no_neigh_vert_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  
  # histogram comparison of eyes, This is an easy comparison there is a small percentage of letters that have eyes, and non letters have no eyes
  eyes_letters <- letters_calculated_features$eyes
  non_eyes_letters <- non_letters_calculated_features$eyes
  
  hist(eyes_letters, main = "eyes", xlab = "number of eyes", xlim=c(0,10), col="orange")
  hist(non_eyes_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  
  # histogram comparison connected areas 
  connected_areas_letters <- letters_calculated_features$connected_areas
  connected_areas_non_letters <- non_letters_calculated_features$connected_areas
  
  hist(connected_areas_letters, main = "connected_areas_non_letters", xlab = "connected areas", xlim=c(0,10), col="blue")
  hist(connected_areas_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  
  
  # histogram for nr pix
  letters_nr_pix <- letters_calculated_features$nr_pix
  non_letters_nr_pix <- non_letters_calculated_features$nr_pix
  
  hist(letters_nr_pix, main = "letters and non letters number of pixels", xlab = "number of pixels", xlim=c(0,10), col="blue")
  hist(non_letters_nr_pix, add=T, col=rgb(0, 1, 0, 0.5) )
  # going to loop through each of teh calculated features in the vector
  for(x in )


  
  # 3.4 statistical analysis of both letters and non letters
  
  
  
  
  
}
