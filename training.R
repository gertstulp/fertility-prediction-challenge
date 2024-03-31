
train_save_model <- function(cleaned_df, outcome_df) {

  set.seed(1) # not useful here because logistic regression deterministic

  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")

  model_df$new_child <- factor(clean$new_child, levels = c("no", "yes"))

  caretLogitModel <- train(clean[, 2:3],
                           clean[, 4],
                           method = 'glm',
                           trControl = objControl,
                           metric = "ROC")
  
  # Save the model
  saveRDS(caretLogitModel, "model.rds")
}
