library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(glmnet)
library(caret)


clean_df <- function(df, background_df = NULL){
  
  df <- df %>% 
    mutate(
      age = ifelse(!is.na(birthyear_bg), 
                   2024 - birthyear_bg, 
                   2024 - mean(birthyear_bg, na.rm = TRUE)),
      gender_bg = case_when(
        is.na(gender_bg) ~ 0, # imputation!
        gender_bg == 1 ~ 0,
        gender_bg == 2 ~ 1,
      ),
      age_s = scale(age),
      gender_f = factor(gender_bg)
    ) %>% 
    select(nomem_encr, age_s, gender_f)

  return(df)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  model <- readRDS(model_path)
  df <- clean_df(df, background_df)
  
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  predictions <- predict(model, 
                         subset(df, select = vars_without_id)) 
  predictions <- ifelse(predictions == "no", 0, 1)
  
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  return( df_predict )
}
