library(dplyr)
library(tidyr)
library(glmnet)

clean_df <- function(df, background_df = NULL){
  
  # glmnet requires that outcome is available for all cases
  df <- df %>% filter(outcome_available == 1)
  
  ## Selecting variables
  keepcols = c('nomem_encr', # ID variable required for predictions,
               'birthyear_bg', # birthyear of respondents
               'gender_bg', # gender of respondents, factor
               'oplmet_2020') # highest educational level in 2020
  
  ## Keeping data with variables selected
  df <- df %>% select(all_of(keepcols))
  
  ## function for getting mode
  mode <- function(x) {
    x <- x[ !is.na(x) ]
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    mode <- ux[tab == max(tab)]
    ifelse(length(mode) > 1, sample(mode, 1), mode)
  }
  
  # impute missing values with mode for categorical variables and mean for continuous
  df <- df %>% 
    mutate(
      birthyear_bg = ifelse(is.na(birthyear_bg), 
                            mean(birthyear_bg, na.rm = TRUE), birthyear_bg),
      gender_bg = ifelse(is.na(gender_bg),
                         mode(gender_bg), gender_bg),
      oplmet_2020 = ifelse(is.na(oplmet_2020),
                           mode(oplmet_2020), oplmet_2020)
    ) %>% 
    # standardise continuous variable, create factor for categorical variables
    # needed for glmnet
    mutate(
      birthyear_bg = as.numeric(scale(birthyear_bg)), # z-scores, as.numeric to remove attributes
      gender_bg = factor(gender_bg),
      oplmet_2020 = factor(oplmet_2020)
    )
  
  # turn factors into dummy variables, required for glmnet
  df <- model.matrix(~ ., df)
  
  return(df)
  
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  # Load the model
  model <- readRDS(model_path)
  
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)
  
  # Exclude id
  X_pred <- df[ , !(colnames(df) %in% c("nomem_encr"))]
  
  
  # Generate predictions from model
  predictions <- predict(model, 
                         X_pred, 
                         type = "response") 
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , colnames(df) == "nomem_encr"], 
                           "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
  
}