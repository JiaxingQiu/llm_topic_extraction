# write a function to evaluate the accuracy of using a label_df or a score_df
# to predict a true_label_df

eval_llm <- function(label_df,
                     true_label_df){
  library(pROC)
  library(PRROC)
  library(MLmetrics)
  
  auprc <- list()
  f1_score <- list()
  for(topic in fea_df$fea){
    mdl_df <- data.frame(x = label_df[[topic]],
                         y = true_label_df[[topic]] )
    mdl_df <- mdl_df[complete.cases(mdl_df),]
    mdl <- glm(y~x, data = mdl_df, family = binomial(link = "logit"))
    
    # get PRC curve and AUPRC
    y_prob <- predict(mdl, type = "response")
    pr_curve <- pr.curve(scores.class0 = y_prob, weights.class0 = mdl_df$y == 1, curve = TRUE)
    auprc[[topic]] <- pr_curve$auc.integral
    
    
    # if label_df are 01, also return a F1
    if(length(unique(mdl_df$x))==2){
      # Calculate F1 score
      f1_score[[topic]] <- F1_Score(y_pred = mdl_df$x, y_true = mdl_df$y, positive = "1")
    }else{
      # find the optimal curoff on the PRC curve
      precision <- pr_curve$curve[, 2]  # Precision values
      recall <- pr_curve$curve[, 1]     # Recall values
      cutoffs <- pr_curve$curve[, 3]    # Cutoff values
      # Calculate F1 scores and find optimal cutoff
      f1_scores <- 2 * (precision * recall) / (precision + recall)  # Calculate F1 scores
      optimal_index <- which.max(f1_scores)  # Find index of maximum F1 score
      optimal_cutoff <- cutoffs[optimal_index]  # Get the optimal cutoff value
      # # plot the PRC
      # pr_data <- data.frame(Recall = recall, Precision = precision, Cutoff = cutoffs)
      # p <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
      #   geom_line(color = "blue") +
      #   geom_point(aes(x = recall[optimal_index], y = precision[optimal_index]),
      #              color = "red", size = 3, shape = 8) +
      #   geom_text(aes(x = recall[optimal_index], y = precision[optimal_index],
      #                 label = paste("F1:", round(f1_scores[optimal_index], 4)) ),
      #   hjust = 0.5, vjust = -0.5, color = "red") +
      #   labs(title = paste0("Precision-Recall Curve (", pr_curve$auc.integral,")"), x = "Recall", y = "Precision") +
      #   theme_minimal()
      # print(p)
      f1_score[[topic]] <- f1_scores[optimal_index]
    }
  }
  
  # combine two lists as two columns in a dataframe
  combined_df <- data.frame(auprc = unlist(auprc), f1_score = unlist(f1_score))
  combined_df$topic <- rownames(combined_df)
  
  return(combined_df)
  
}
