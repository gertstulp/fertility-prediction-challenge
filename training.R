
train_save_model <- function(cleaned_df, outcome_df) {

  set.seed(1) # not useful here because logistic regression deterministic

  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")

  # creating average income for all respondents
  respondents_1974 <- supplement %>% # object in environment called supplement
    filter(birthyear_bg == 1974) 
  
  # add respondents from 1974
  data <- bind_rows(model_df, respondents_1974)
    
  model <- glm(new_child ~ age + gender_bg, family = "binomial", data = data)
  
  # Save the model
  saveRDS(model, "model.rds")
}
