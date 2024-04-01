train_save_model <- function(cleaned_df, outcome_df) {
  
  set.seed(1) # not useful here because logistic regression deterministic
  
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")
  
  model <- glm(new_child ~ age + mean_income_imp, family = "binomial", data = model_df)
  
  # Save the model
  saveRDS(model, "model.rds")
}