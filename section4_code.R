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
  
  code_4_1(calculated_features)
  
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
  
  
  code_4_2(calculated_features){
    
  }
  
  # this function will return the logistic regression model 
  # 
  logistic_regession_model <- function(calculated_features){
    logistic <- glm()
  }
  
}