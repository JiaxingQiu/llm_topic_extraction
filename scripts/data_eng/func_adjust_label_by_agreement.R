
adjust_label_by_agreement <- function(label_df,
                     score_df,
                     true_label_df,
                     plot_topic = c()){
  library(pROC)
  library(PRROC)
  library(MLmetrics)
  
  auprc <- list()
  f1_score <- list()
  for(topic in fea_df$fea){
    mdl_df <- data.frame(x = score_df[[topic]],
                         y = true_label_df[[topic]] )
    mdl_df <- mdl_df[complete.cases(mdl_df),]
    mdl <- glm(y~x, data = mdl_df, family = binomial(link = "logit"))
    
    # get PRC curve and AUPRC
    y_prob <- predict(mdl, type = "response")
    pr_curve <- pr.curve(scores.class0 = y_prob, weights.class0 = mdl_df$y == 1, curve = TRUE)
    auprc[[topic]] <- pr_curve$auc.integral
    # find the optimal curoff on the PRC curve
    precision <- pr_curve$curve[, 2]  # Precision values
    recall <- pr_curve$curve[, 1]     # Recall values
    cutoffs <- pr_curve$curve[, 3]    # Cutoff values
    # Calculate F1 scores and find optimal cutoff
    f1_scores <- 2 * (precision * recall) / (precision + recall)  # Calculate F1 scores
    optimal_index <- which.max(f1_scores)  # Find index of maximum F1 score
    optimal_cutoff <- cutoffs[optimal_index]  # Get the optimal cutoff value
    
    if (topic %in% plot_topic){
      # plot the PRC
      pr_data <- data.frame(Recall = recall, Precision = precision, Cutoff = cutoffs)
      p <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
        geom_line(color = "blue") +
        geom_point(aes(x = recall[optimal_index], y = precision[optimal_index]),
                   color = "red", size = 3, shape = 8) +
        geom_text(aes(x = recall[optimal_index], y = precision[optimal_index],
                      label = paste("F1:", round(f1_scores[optimal_index], 4)) ),
                  hjust = 0.5, vjust = -0.5, color = "red") +
        labs(title = paste0("Precision-Recall Curve (", pr_curve$auc.integral,")"), x = "Recall", y = "Precision") +
        theme_minimal()
      print(p)
    }
    f1_score[[topic]] <- f1_scores[optimal_index]
    label_df[which(score_df[[topic]]<optimal_cutoff),topic] <- 0 # only remove false positives
  }
  
  return(label_df)
  
}
