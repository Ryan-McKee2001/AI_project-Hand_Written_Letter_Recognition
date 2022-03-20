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
  # need to get this working
  code_4_2<-function(calculated_features){
    letter_check <- 0
    features_results$is_letter = letter_check
    features_results[81:140,]$is_letter = 1
    
    # make training and test datasets
    # randomly shuffle rows:
    
    results_shuffled <- features_results[sample(nrow(features_results)),]
    head(results_shuffled)
    
    training_data = results_shuffled[1:112,]
    test_data = results_shuffled[112:140,]
    
    test_plot <- ggplot(training_data, aes(x=no_neigh_vert, fill=as.factor(is_letter))) +
      geom_histogram(binwidth=1, alpha=.5, position='identity')
    print(test_plot)
    
    # Lets fit a logistic regression model using this feature:
    glmfit <- glm(is_letter ~ no_neigh_vert, 
                  data = training_data, family = 'binomial') 
    summary(glmfit)
  }
  
  
  # section 4.3
  #code_4_3(calculated_features)
  
  # this function prints out 
  code_4_3 <- function(calculated_features){
    features_medians <- c(colMedians(as.matrix(calculated_features[ , 3:18])))
    splits_medians <- c(features_medians[1], features_medians[6], features_medians[7])
    
    splits_matrix <- matrix(NA, nrow = 3, ncol = 3)
    
    splits_matrix[1, ] <- letters_splits(calculated_features, splits_medians)
    splits_matrix[2, ] <- faces_splits(calculated_features, splits_medians)
    splits_matrix[3, ] <- xclaim_splits(calculated_features, splits_medians)
    
    splits_df <- data.frame(splits_matrix)
    rownames(splits_df) <- c("split1", "split2", "split3")
    colnames(splits_df) <- c("letters", "faces", "exclamation mark")
    print(splits_df)
    
  }
  
  # this returns a vector for the three splits for letters
  letters_splits <- function(calculated_features, splits_median){
    letter_splits <- c(0,0,0)
    
    letters_median <- c(colMedians(as.matrix(calculated_features[1:80 , 3:18])))
    letter_splits_medians <- c(letters_median[1], letters_median[6], letters_median[7])
    
    for(x in 1:3){
      if(letter_splits_medians[x] > splits_median[x] ){
        letter_splits[x] <- 1
      }
    }
    
    return(letter_splits)
  }
  
  # returns a vector for the three splits for faces
  faces_splits <- function(calculated_features, splits_median){
    face_splits <- c(0,0,0)
    
    faces_median <- c(colMedians(as.matrix(calculated_features[81:120 , 3:18])))
    faces_splits_medians <- c(faces_median[1], faces_median[6], faces_median[7])
    
    for(x in 1:3){
      if(faces_splits_medians[x] > splits_median[x] ){
        face_splits[x] <- 1
      }
    }
    return(face_splits)
  }
  
  # retuns a vector for the three splits for exclamation marks
  xclaim_splits <- function(calculated_features, splits_median){
    xclaim_splits <- c(0,0,0)
    
    xclaim_median <- c(colMedians(as.matrix(calculated_features[121:140 , 3:18])))
    xclaim_splits_medians <- c(xclaim_median[1], xclaim_median[6], xclaim_median[7])
    
    for(x in 1:3){
      if(xclaim_splits_medians[x] > splits_median[x] ){
        xclaim_splits[x] <- 1
      }
    }
    
    return(xclaim_splits)
  }
  
}