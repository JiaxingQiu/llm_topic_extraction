# write a function to evaluate the accuracy of using a label_df or a score_df
# to predict a true_label_df

eval_llm <- function(label_df, #also can be a score_df
                     true_label_df,
                     balance = F,
                     findoptimal=F,
                     byfb=F){
  
  label_df <- label_df[which(label_df$sm_id %in% true_label_df$sm_id),]
  library(pROC)
  library(PRROC)
  library(MLmetrics)
  
  auprc <- list()
  f1_score <- list()
  fb_score <- list()
  sensitivity <- list()
  specificity <- list()
  precision <- list()
  
  for(topic in fea_df$fea){
    mdl_df <- data.frame(x = label_df[[topic]],
                         y = true_label_df[[topic]] )
    mdl_df <- mdl_df[complete.cases(mdl_df),]
    
    if(balance){
      # Filter positive and negative rows
      positive_rows <- mdl_df[mdl_df$y == 1, ]
      negative_rows <- mdl_df[mdl_df$y == 0, ]
      # Number of positive rows
      num_positive <- nrow(positive_rows)
      # Sample the same number of negative rows as positive rows
      sampled_negative_rows <- negative_rows[sample(1:nrow(negative_rows), num_positive), ]
      # Combine positive and sampled negative rows
      mdl_df <- rbind(positive_rows, sampled_negative_rows)
    }
    
    
    # # do a logisitc regression to get a AUPRC
    # mdl <- glm(y~x, data = mdl_df, family = binomial(link = "logit"))
    # # get PRC curve and AUPRC
    # y_prob <- predict(mdl, type = "response")
    if(n_distinct(mdl_df$x)>1){
      pr_curve <- pr.curve(scores.class0 = mdl_df$x, weights.class0 = mdl_df$y == 1, curve = TRUE) #y_prob
      auprc[[topic]] <- pr_curve$auc.integral
    }else{
      auprc[[topic]] <- 0
    }
    
    
    f1_score[[topic]] <- NA
    fb_score[[topic]] <- NA
    sensitivity[[topic]] <- NA
    specificity[[topic]] <- NA
    precision[[topic]] <- NA
    
    if(length(unique(mdl_df$x))>2) {
      if(findoptimal){
        # find the optimal cutoff on the PRC curve
        precisions <- pr_curve$curve[, 2]  # Precision values
        recalls <- pr_curve$curve[, 1]     # Recall values
        cutoffs <- pr_curve$curve[, 3]    # Cutoff values
        # Calculate F1 scores and find optimal cutoff
        f1_scores <- 2 * (precisions * recalls) / (precisions + recalls)  # Calculate F1 scores
        optimal_index <- which.max(f1_scores)  # Find index of maximum F1 score
        optimal_cutoff <- cutoffs[optimal_index]  # Get the optimal cutoff value
        # plot the PRC
        pr_data <- data.frame(Recall = recalls, Precision = precisions, Cutoff = cutoffs)
        p <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
          geom_line(color = "blue") +
          geom_point(aes(x = recalls[optimal_index], y = precisions[optimal_index]),
                     color = "red", size = 3, shape = 8) +
          geom_text(aes(x = recalls[optimal_index], y = precisions[optimal_index],
                        label = paste("F1:", round(f1_scores[optimal_index], 4)) ),
          hjust = 0.5, vjust = -0.5, color = "red") +
          labs(title = paste0("Precision-Recall Curve (", pr_curve$auc.integral,")"), x = "Recall", y = "Precision") +
          theme_minimal()
        print(p)
        # modify mdl_df 
        mdl_df$x <- ifelse(mdl_df$x<optimal_cutoff, 0, 1)
        obj <- get_ML_matrix(mdl_df$x, mdl_df$y)
        f1_score[[topic]] <- obj$f1_score
        fb_score[[topic]] <- obj$fb_score
        sensitivity[[topic]] <- obj$sensitivity
        specificity[[topic]] <- obj$specificity # True Negative Rate
        precision[[topic]] <- obj$precision  # Positive Predictive Value
        
      }
    }
    
    # if label_df are 01, also return a F1
    if(length(unique(mdl_df$x))==2){
      obj <- get_ML_matrix(mdl_df$x, mdl_df$y)
      f1_score[[topic]] <- obj$f1_score
      fb_score[[topic]] <- obj$fb_score
      sensitivity[[topic]] <- obj$sensitivity
      specificity[[topic]] <- obj$specificity # True Negative Rate
      precision[[topic]] <- obj$precision  # Positive Predictive Value
    }
  }
  
  # combine two lists as two columns in a dataframe
  combined_df <- data.frame(auprc = unlist(auprc), 
                            f1_score = unlist(f1_score),
                            fb_score = unlist(fb_score),
                            sensitivity = unlist(sensitivity),
                            specificity = unlist(specificity),
                            precision = unlist(precision) )
  combined_df$topic <- rownames(combined_df)
  
  return(combined_df)
  
}



get_ML_matrix <- function(y_pred, y_true){
  
  # Create a confusion matrix
  conf_matrix <- table(Predicted = y_pred, True = y_true)
  # Extract values from the confusion matrix
  TP <- conf_matrix["1", "1"] # True Positives
  TN <- conf_matrix["0", "0"] # True Negatives
  FP <- conf_matrix["1", "0"] # False Positives
  FN <- conf_matrix["0", "1"] # False Negatives
  prevalence <- sum(y_true==1)/sum(y_true==0)
  
  # Calculate metrics
  sensitivity <- TP / (TP + FN) # True Positive Rate
  specificity <- TN / (TN + FP) # True Negative Rate
  precision <- TP / (TP + FP)   # Positive Predictive Value
  f1_score <- F1_Score(y_true = y_true, y_pred = y_pred, positive = "1")
  fb_score <- FBeta_Score(y_true = y_true, y_pred = y_pred, positive = "1", beta=1/prevalence) 
  f2_score <- FBeta_Score(y_true = y_true, y_pred = y_pred, positive = "1", beta=0.5) 
  # Weight of precision in harmonic mean 
  
  
  return(list(f1_score = f1_score,
              fb_score = fb_score,
              sensitivity = sensitivity,
              specificity = specificity,
              precision = precision))
}
