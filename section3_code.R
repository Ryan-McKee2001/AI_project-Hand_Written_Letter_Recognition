# section 3 code

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
  
  
  
  # 3.1 code
  
  # prints histograms for each of the features
  code_3_1 <- function(calculated_features){
    nr_pix_hist <- ggplot(data = calculated_features, aes(nr_pix))+
    geom_histogram(binwidth = 2, fill = "grey")+
    theme_bw() +
    labs(title = "Number of pixels histogram",
         x = "Number of pixels",
         y = "Frequency")

    rows_with_1_hist <- ggplot(data = calculated_features, aes(rows_with_1)) +
             geom_histogram(binwidth = 1, fill = "red") +
             theme_bw() +
             labs(title = "Rows with 1 pixel histogram",
                  x = "Number of rows with 1 pixel",
                  y = "Frequency")

    cols_with_1_hist <- calculated_features %>%
      ggplot(aes(cols_with_1))+
      geom_histogram(binwidth = 2, fill = "red")+
      theme_bw() +
      labs(title = "Columns with 1 histogram",
           x = "Number of columns with 1",
           y = "Frequency")

    rows_with_3p_hist <- calculated_features %>%
      ggplot(aes(rows_with_3p))+
      geom_histogram(binwidth = 1, fill = "blue")+
      theme_bw() +
      labs(title = "Rows with 3 pixels of more histogram",
           x = "Number of rows with 3 pixels or more",
           y = "Frequency")

    cols_with_3p_hist <- calculated_features %>%
      ggplot(aes(cols_with_3p))+
      geom_histogram(binwidth = 1, fill = "blue")+
      theme_bw() +
      labs(title = "Columns with 3 pixels of more histogram",
           x = "Number of columns with 3 pixels or more",
           y = "Frequency")

    aspect_ratio_hist <- calculated_features %>%
      ggplot(aes(aspect_ratio))+
      geom_histogram(binwidth = 0.1, fill = "orange")+
      theme_bw() +
      labs(title = "Aspect ratio histogram",
           x = "Aspect ratios",
           y = "Frequency")

    plot(nr_pix_hist)
    plot(rows_with_1_hist)
    plot(cols_with_1_hist)
    plot(rows_with_3p_hist)
    plot(cols_with_3p_hist)
    plot(aspect_ratio_hist)
  }
  
  #code_3_1(calculated_features) #uncomment when ready to run

  
  
  
  # 3.2 code
  # creating over lappting bar charts for the 3 features That appear to stand out
  
  #code_3_2(calculated_features)
  
  code_3_2 <- function(calculated_features){
   
    
    stats_df <- get_stats(calculated_features)
    # stats_table(stats_df) # need to fix this
    mean_stats_hist(stats_df)
    median_stats_hist(stats_df)
    standard_deviation_stats_hist(stats_df)
  }
  
  # this function calculated mean, median and standard deviation for each of the features
  # then returns a dataframe containing each of these
  get_stats <- function(calculated_features){
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
    
    stats_df<-data.frame(letters_mean, non_letters_mean, letters_median, non_letters_median, letters_sd, non_letters_sd)
    
    return(stats_df)
  }
  
  # this function prints out a table containing the statistics for the features
  stats_table <- function(stats_df){
    table <- kable(as.matrix(stats_df), caption = "This table shows the letter and non letters mean, median and standard deviation for each of the feature calculations")
    print(table)
  }
  
  
  # this function prints out a grouped bar plot for each of the features stat values
  mean_stats_hist <- function(stats_df){
    mean_df <- data.frame(stats_df[, 1], stats_df[, 2])
    colnames(mean_df) <- c("letters", "non_letters")
    rownames(mean_df) <- c("nr_pix", "rows_with_1", "cols_with_1",
                           "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                           "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                           "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
    mean_df <- data.frame(feature = row.names(mean_df), mean_df) 
    mean_df <- tidyr::pivot_longer(mean_df, cols=c('letters', 'non_letters'), names_to='Symbols',values_to="Mean")
    
    mean_hist <- ggplot(mean_df, aes(x=feature, y=Mean, fill=Symbols))+ 
      theme_bw() +
      geom_bar(stat='identity', position='dodge') + 
      ggtitle("Difference between letter and non letter means") +  xlab("Calculated features") + 
      scale_x_discrete(limits=stats_df$names)
    
    plot(mean_hist)
  }
  
  median_stats_hist <- function(stats_df){
    median_df <- data.frame(stats_df[, 3], stats_df[, 4])
    colnames(median_df) <- c("letters", "non_letters")
    rownames(median_df) <- c("nr_pix", "rows_with_1", "cols_with_1",
                           "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                           "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                           "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
    median_df <- data.frame(feature = row.names(median_df), median_df) 
    median_df <- tidyr::pivot_longer(median_df, cols=c('letters', 'non_letters'), names_to='Symbols',values_to="Median")
    
    print(median_df)
    
    median_hist <- ggplot(median_df, aes(x=feature, y=Median, fill=Symbols))+ 
      theme_bw() +
      geom_bar(stat='identity', position='dodge') + 
      ggtitle("Difference between letter and non letter medians") +  xlab("Calculated features") + 
      scale_x_discrete(limits=stats_df$names)
    
    plot(median_hist)
  }
  
  standard_deviation_stats_hist <- function(stats_df){
    sd_df <- data.frame(stats_df[, 3], stats_df[, 4])
    colnames(sd_df) <- c("letters", "non_letters")
    rownames(sd_df) <- c("nr_pix", "rows_with_1", "cols_with_1",
                             "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                             "no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right",
                             "no_neigh_horiz", "no_neigh_vert","connected_areas","eyes","custom")
    sd_df <- data.frame(feature = row.names(sd_df), sd_df) 
    sd_df <- tidyr::pivot_longer(sd_df, cols=c('letters', 'non_letters'), names_to='Symbols',values_to="Standard deviation")
    
    print(sd_df)
    
    sd_hist <- ggplot(sd_df, aes(x=feature, y=`Standard deviation`, fill=Symbols))+ 
      theme_bw() +
      geom_bar(stat='identity', position='dodge') + 
      ggtitle("Difference between letter and non letter standard deviation") +  xlab("Calculated features") + 
      scale_x_discrete(limits=stats_df$names)
    
    plot(sd_hist)
  }
  
  
  # section 3.3 code 
  code_3_3(calculated_features)
  
  code_3_3 <- function(calculated_features){
    let_calc_feat <- letter_df(calculated_features)
    non_let_calc_feat <- non_letters_df(calculated_features)
    
    letter_non_letter_hist(let_calc_feat, non_let_calc_feat)
    
    
  }
  
  # this function returns a data frame containg letter and non letter columns
  letter_df <- function(calculated_features){
    letters_calculated_features <- calculated_features[1:80,]
    letters_calculated_features[, 18] <- as.numeric(letters_calculated_features[ , 18])
    
    return(letters_calculated_features)
  }
  
  non_letters_df <- function(calculated_features){
    non_letters_calculated_features <- calculated_features[81:140,]
    non_letters_calculated_features[ , 18] <- as.numeric(non_letters_calculated_features[ , 18])
    
    return(non_letters_calculated_features)
  }
  
  # this function will get a comparison of all the image features in the function
  letter_non_letter_hist <- function(let_calc_feat, non_let_calc_feat){
    # compares letters and non letters nr pix
    nr_pix_letter <- let_calc_feat$nr_pix
    nr_pix_non_letter <- non_let_calc_feat$nr_pix
    
    hist(nr_pix_letter, main = "graph for number of pixels: letters vs non letters", xlab = "pixel number", xlim=c(0,100), col="grey")
    hist(nr_pix_non_letter, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compares letters and non letters rows with 1
    letters_rows_with_1 <- let_calc_feat$rows_with_1
    non_letters_rows_with_1 <- non_let_calc_feat$rows_with_1

    hist(letters_rows_with_1, main = "graph for number of rows with 1 pixel: Letter vs non letter", xlab = "Rows with 1", xlim=c(0,15), col="grey")
    hist(non_letters_rows_with_1, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compares letters and non letters cols with 1
    letters_cols_with_1 <- let_calc_feat$cols_with_1
    non_letters_cols_with_1 <- non_let_calc_feat$cols_with_1
    
    hist(letters_cols_with_1, main = "graph for number of columns with 1 pixel: letter vs non letters", xlab = "Cols with 1", xlim=c(0,11), col="grey")
    hist(non_letters_cols_with_1, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare rows with 3p
    letters_rows_with_3p <- let_calc_feat$rows_with_3p
    non_letters_rows_with_3p <- non_let_calc_feat$rows_with_3p
    
    hist(letters_rows_with_3p, main = "graph for number of rows with 3 pixels or more: letters vs non letters", xlab = "Rows with 3p", xlim=c(0,17), col="grey")
    hist(non_letters_rows_with_3p, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare columns with 3p
    letters_cols_with_3p <- let_calc_feat$cols_with_3p
    non_letters_cols_with_3p <- non_let_calc_feat$cols_with_3p
    
    hist(letters_cols_with_3p, main = "graph for number of columns with 3 pixels or more: letters vs non letters", xlab = "Columns with 3p", xlim=c(0,15), col="grey")
    hist(non_letters_cols_with_3p, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare aspect ratios
    letters_aspect_ratio <- let_calc_feat$aspect_ratio
    non_letters_aspect_ratio <- non_let_calc_feat$aspect_ratio
    
    hist(letters_aspect_ratio, main = "graph for aspect ratio: letters vs non letters", xlab = "Aspect ratios", xlim=c(0,2), col="grey")
    hist(non_letters_aspect_ratio, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare neigh_1
    letters_neigh_1 <- let_calc_feat$neigh_1
    non_letters_neigh_1 <- non_let_calc_feat$neigh_1
    
    hist(letters_neigh_1, main = "graph for number of pixels with only 1 neighbour: letters vs non letters", xlab = "neigh 1", xlim=c(0,10), col="grey")
    hist(non_letters_neigh_1, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_above
    letters_no_neigh_above <- let_calc_feat$no_neigh_above
    non_letters_no_neigh_above <- non_let_calc_feat$no_neigh_above
    
    hist(letters_no_neigh_above, main = "graph for number of neighbours above: letters vs non letters", xlab = "Columns with 3p", xlim=c(0,30), col="grey")
    hist(non_letters_no_neigh_above, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_below
    letters_no_neigh_below <- let_calc_feat$no_neigh_below
    non_letters_no_neigh_below <- non_let_calc_feat$no_neigh_below
    
    hist(letters_no_neigh_below, main = "graph for number of neighbours below: letters vs non letters", xlab = "no neigh below", xlim=c(0,30), col="grey")
    hist(non_letters_no_neigh_below, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_left
    letters_no_neigh_left <- let_calc_feat$no_neigh_left
    non_letters_no_neigh_left <- non_let_calc_feat$no_neigh_left
    
    hist(letters_no_neigh_left, main = "graph for number of neighbours left: letters vs non letters", xlab = "No neigh left", xlim=c(0,20), col="grey")
    hist(non_letters_no_neigh_left, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_right
    letters_no_neigh_right <- let_calc_feat$no_neigh_right
    non_letters_no_neigh_right <- non_let_calc_feat$no_neigh_right
    
    hist(letters_no_neigh_right, main = "graph for number of no neighbours right: letters vs non letters", xlab = "No neigh right", xlim=c(0,25), col="grey")
    hist(non_letters_no_neigh_right, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_horiz
    letters_no_neigh_horiz <- let_calc_feat$no_neigh_horiz
    non_letters_no_neigh_horiz <- non_let_calc_feat$no_neigh_horiz
    
    hist(letters_no_neigh_horiz, main = "graph for number of no neighbours horizontal: letters vs non letters", xlab = "no neigh horiz", xlim=c(0,20), col="grey")
    hist(non_letters_no_neigh_horiz, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare no_neigh_vert
    letters_no_neigh_vert <- let_calc_feat$no_neigh_vert
    non_letters_no_neigh_vert <- non_let_calc_feat$no_neigh_vert
    
    hist(letters_no_neigh_vert, main = "graph for number of no neighbours vertical: letters vs non letters", xlab = "no neigh vert", xlim=c(0,35), col="grey")
    hist(non_letters_no_neigh_vert, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare connected_areas
    letters_connected_areas <- let_calc_feat$connected_areas
    non_letters_connected_areas <- non_let_calc_feat$connected_areas
    
    hist(letters_connected_areas, main = "graph for aspect ratio: letters vs non letters", xlab = "aspect ratio", xlim=c(0,5), col="grey")
    hist(non_letters_connected_areas, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare eyes
    letters_eyes <- let_calc_feat$eyes
    non_letters_eyes <- non_let_calc_feat$eyes
    
    hist(letters_eyes, main = "graph for eyes: letters vs non letters", xlab = "Number of eyes", xlim=c(0,2), col="grey")
    hist(non_letters_eyes, add=T, col=rgb(0, 1, 0, 0.5) )
    
    # compare custom
  }
  
  
  # 3.4 code
  
  code_3_4 <- function(calculated_features){
    
  }
  
  # 
  correlation_test <- function(calculated_features){
    
  }
  
  
  # # printing tables containing the results for standard_deviation, mean, median for both letters and
  # # non letters.
  # # source for making tables: https://www.youtube.com/watch?v=hNgeVLotABg
  # summary_data = matrix(NA, nrow = 16, ncol = 0)
  # 
  # summary_data <- cbind(summary_data, letters_mean)
  # summary_data <- cbind(summary_data, non_letters_mean)
  # summary_data <- cbind(summary_data, letters_median)
  # summary_data <- cbind(summary_data, non_letters_median)
  # summary_data <- cbind(summary_data, letters_sd)
  # summary_data <- cbind(summary_data, non_letters_sd)
  # 
  # features_table<-kable(summary_data, caption = "This table shows the letter and non letters mean, median and standard deviation for each of the feature calculations")
  # # this prints out the table containing all the values for the features for the report
  # print(features_table)
  # 
  # non_letters_summary_data <- matrix(NA, nrow = 16, ncol = 0)
  # 
  # non_letters_summary_data <-cbind(letters_summary_data, letters_mean)
  # non_letters_summary_data <- cbind(letters_summary_data, letters_median)
  # non_letters_summary_data <- cbind(letters_summary_data, letters_sd)
  # 
  # non_letters_summary_data <- matrix(NA, nrow = 16, ncol = 0)
  # 
  # 
  # non_letters_summary_data <-cbind(letters_summary_data, letters_mean)
  # non_letters_summary_data <- cbind(letters_summary_data, letters_median)
  # non_letters_summary_data <- cbind(letters_summary_data, letters_sd)
  # 
  # print(letters_summary_data)
  # 
  # letter_comparison_histogram <- ggplot(aes(x=))

  #print(summary_data)

  # cols with 1, no_neigh_vert and connected_areas seems to be the features with the greates differences and may be the easiest features for
  # discriminating whether an image is a letter or a non letter image.

  # density plots for mean, median and standard deviation of cols with 1 for letters and non letters
  # creating box plots I used https://www.datamentor.io/r-programming/box-plot/#:~:text=In%20R%2C%20boxplot%20(and%20whisker,numeric%20vectors%20as%20its%20components.

  # # box plot for comparing all the summary data, to show in report
  # summary_boxplot <- boxplot(letters_mean, non_letters_mean, letters_median, non_letters_median, letters_sd, non_letters_sd,
  #         main = "summary data comparisons",
  #         at = c(1,2,3,4,5,6),
  #         names = c("letter mean", "non letter mean", "letters median", "non letters median", "letters standard deviation", "non letters standard deviation"),
  #         las = 2,
  #         col = c("orange","red"),
  #         border = "brown",
  #         horizontal = TRUE,
  #         notch = FALSE
  # )

  # histogram comparisons for each of the 3 chosen features
  # get a histogram for cols with 1 for letters
  
  
  
  
  # cols_with_1_letters <- letters_calculated_features$cols_with_1
  # cols_with_1_non_letters <- non_letters_calculated_features$cols_with_1
  # 
  # # this creates a histogram for the columns with 1 feature for letters and non letters
  # hist(cols_with_1_letters, main = "columns with 1", xlab = "Columns with 1", xlim=c(0,10), col="red")
  # hist(cols_with_1_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  # 
  # 
  # # 3.3
  # # histogram comparisons for no neigh vert
  # no_neigh_vert_letters <- letters_calculated_features$no_neigh_vert
  # no_neigh_vert_non_letters <- non_letters_calculated_features$no_neigh_vert
  # 
  # hist(no_neigh_vert_letters, main = "number of pixels with no neighbours vertical per dataset", xlab = "number no neighbours vertical", xlim=c(0,10), col="blue")
  # hist(no_neigh_vert_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  # 
  # # histogram comparison of eyes, This is an easy comparison there is a small percentage of letters that have eyes, and non letters have no eyes
  # eyes_letters <- letters_calculated_features$eyes
  # non_eyes_letters <- non_letters_calculated_features$eyes
  # 
  # hist(eyes_letters, main = "eyes", xlab = "number of eyes", xlim=c(0,10), col="orange")
  # hist(non_eyes_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  # 
  # # histogram comparison connected areas
  # connected_areas_letters <- letters_calculated_features$connected_areas
  # connected_areas_non_letters <- non_letters_calculated_features$connected_areas
  # 
  # hist(connected_areas_letters, main = "connected_areas_non_letters", xlab = "connected areas", xlim=c(0,10), col="blue")
  # hist(connected_areas_non_letters, add=T, col=rgb(0, 1, 0, 0.5) )
  # 
  # 
  # # histogram for nr pix
  # letters_nr_pix <- letters_calculated_features$nr_pix
  # non_letters_nr_pix <- non_letters_calculated_features$nr_pix
  # 
  # hist(letters_nr_pix, main = "letters and non letters number of pixels", xlab = "number of pixels", xlim=c(0,10), col="blue")
  # hist(non_letters_nr_pix, add=T, col=rgb(0, 1, 0, 0.5) )
  # # going to loop through each of teh calculated features in the vector
  # 
  # 
  # 
  # # 3.4 statistical analysis of both letters and non letters





}

