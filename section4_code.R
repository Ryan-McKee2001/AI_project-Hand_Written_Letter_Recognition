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
  
  # code for section 4.1
  
  #code_4_1(calculated_features)
  
  code_4_1 <- function(calculated_features){
    aspect_ratio_prediction(calculated_features)
  }
  
  
  # this function will return the predicted aspect ratio based the 3 highest correlations and lowest
  # p value comparisons, custom, no_neigh_above and no_neigh_below
  aspect_ratio_prediction <- function(calculated_features){
    # get the index of the smallest comparison for aspect ratio
    model <- lm(formula = aspect_ratio ~ custom + no_neigh_above + no_neigh_below, data = calculated_features)
    print(summary(model))
    
  }
  
  #code_4_2(calculated_features)
  
  code_4_2<-function(calculated_features){
    # need to add a column to the dataframe which 
    # will signifty if a row is a letter or non letter 
    # row
    
    # 1's are equal to letters
    # 0's are equal to non letters
    calculated_features$letter_non_letter <- c(0)
    calculated_features[1:80,19] <- 1
    calculated_features[81:140,19] <- 0
    
    plot(summary(lm(calculated_features$letter_non_letter ~ calculated_features$no_neigh_below - calculated_features$no_neigh_below)))
  }
  
  # this function will return the logistic regression model 
  # 
  logistic_regession_model <- function(calculated_features){
    logistic <- glm()
  }
  
  code_4_3(calculated_features)
  
  
  # just check if each of letters nr_pixels
  # are greater than the group median value 
  # and returning a 1 if its true
  code_4_3 <- function(calculated_features){
    letters_splits(calculated_features)
  }
  
  letters_splits <- function(calculated_features){
    # letters
    print(calculated_features)
    letters_median <- c(colMedians(as.matrix(letters_calculated_features[ , 3:18])))
    colnames(letters_medians)
    print(letters_median)
    splits_median <- c(letters_median[1], letters_median[], letters_median$neigh_1)
    
    print(splits_matrix)
    
  }
  
  
}