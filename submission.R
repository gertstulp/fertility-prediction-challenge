library(dplyr)

clean_df <- function(df, background_df = NULL){
  
  # creating average income for all respondents
  income_3months <- background_df |>
    # only select last 3 months
    filter(wave >= 202010 & wave <= 202012) |> 
    # for each respondent
    group_by(nomem_encr) |>
    # calculate average income
    summarise(mean_income = mean(netinc, na.rm = TRUE))
  
  # add income to train data
  data <- left_join(df, income_3months, by = "nomem_encr")
  data <- data |>
    # impute mean income if missing
    mutate(mean_income_imp = if_else(is.na(mean_income), 
                                     mean(mean_income, na.rm = TRUE),
                                     mean_income),
           age = 2024 - data$birthyear_bg)
  
  keepcols = c('nomem_encr', # ID variable required for predictions,
               'age', 
               'mean_income_imp')  
  
  data <- data[ , keepcols ]
  
  return(data)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  model <- readRDS(model_path)
  df <- clean_df(df, background_df)
  
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  predictions <- predict(model, 
                         subset(df, select = vars_without_id), 
                         type = "response") 
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  return( df_predict )
}