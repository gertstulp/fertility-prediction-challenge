clean_df <- function(df, background_df = NULL){
  
  df$age <- 2024 - df$birthyear_bg
  keepcols = c('nomem_encr', # ID variable required for predictions,
               'age', 
               'gender_bg')  

  df <- df[ , keepcols ]
  
  df$gender_bg<- as.factor(df$gender_bg) # 
  
  keepcols = c('nomem_encr', 'age', 'gender_bg')
               
  df <- df[ , keepcols ]

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
                         subset(df, select = vars_without_id), 
                         type = "response") 
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  return( df_predict )
}
