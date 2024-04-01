train_save_model <- function(cleaned_df, outcome_df) {
  set.seed(1) # useful here because penalized regression not deterministic
  
  # Combine cleaned_df and outcome_df to match on ID
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")
  
  # glmnet requires matrix, and also features (X) separately from outcome (y)
  model_df <- as.matrix(model_df) # merged filed into matrix
  
  # features without outcome and identifier
  X <- model_df[ , !(colnames(model_df) %in% c("nomem_encr", "new_child"))]
  y <- model_df[ , colnames(model_df) == "new_child"] # outcome only
  
  ## LASSO regression ##
  # hyperparameter tuning: 10 fold cross-validation to retrieve optimal lambda
  CV <- cv.glmnet(x = X,  y = y, 
                  family = "binomial", nfolds = 10, standardize = FALSE)
  optimal_lambda <- CV$lambda.min
  
  # Run model with optimal lambda
  model <- glmnet(x = X, y = y, 
                  family = "binomial", 
                  lambda = optimal_lambda, standardize = FALSE )
  
  # Save the model
  saveRDS(model, "model.rds")
}