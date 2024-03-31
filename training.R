
train_save_model <- function(cleaned_df, outcome_df) {

  set.seed(1) # not useful here because logistic regression deterministic

  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr")

  model_df$new_child <- factor(model_df$new_child, levels = c(0, 1),
                               labels = c("no", "yes"))

  objControl <- trainControl(method = "none", 
                             summaryFunction = twoClassSummary, 
                             classProbs = TRUE,
                             savePredictions = TRUE)
  
  caretLogitModel <- train(model_df[, 2:3],
                           model_df[, 4],
                           method = 'glm',
                           trControl = objControl,
                           metric = "ROC")
  
  # Save the model
  saveRDS(caretLogitModel, "model.rds")
}
