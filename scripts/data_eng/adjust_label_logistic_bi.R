setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list = ls())
source("./scripts/data_eng/func_x_transform.R")
library(pROC)
library(PRROC)
library(dplyr)
library(ggplot2)
library(pheatmap)

get_score_df <- function(folder, mdl_name = "stella_en_400M_v5"){
  labeled_df <- read.csv(paste0("./",folder,"/results_with_cos_",mdl_name,".csv"))
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), "X")]
  labeled_df <- labeled_df %>% arrange(sm_id)
  scores_df <- readRDS(paste0("./",folder,"/cos_score_",mdl_name,".RDS"))
  scores_df <- scores_df %>% arrange(sm_id)
  for(topic in fea_df$fea){
    # shift a cutoff at 0, then zero out <0
    cutoff <- quantile(scores_df[[topic]][which(scores_df[[topic]]>0)],0)
    a0 <- which(scores_df[[topic]]>=cutoff)
    be0 <- which(scores_df[[topic]]<=cutoff)
    print(min(scores_df[[topic]][a0],na.rm=T))
    scores_df[[topic]][a0] <- (scores_df[[topic]][a0] - min(scores_df[[topic]][a0],na.rm=T) ) / (max(scores_df[[topic]][a0],na.rm=T) - min(scores_df[[topic]][a0],na.rm=T))
    scores_df[[topic]][be0] <- 0
    
    scores_df[[topic]] <- scores_df[[topic]]*labeled_df[[topic]]
  }
  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  info_df <- text_df %>% select(sm_id, group, sr_name, url)
  scores_df <- merge(scores_df, info_df)%>% arrange(sm_id)
  labeled_df <- merge(labeled_df, info_df)%>% arrange(sm_id)
  
  return(list(labeled_df = labeled_df, 
              scores_df = scores_df ))
}


pr_opt <- function(mdl_df, score_col = "llm1_score"){
  
  # to remove false positive
  mdl_df$x <- mdl_df[[score_col]]
  mdl_score <- glm(llm_inter ~ x, data=mdl_df, family = binomial(link = "logit"))
  y_prob <- predict(mdl_score, type = "response")
  # find an optimal cutoff of x using the precision recall curve, you can use the point on the precision-recall curve that maximizes the F1 score (the harmonic mean of precision and recall), or you can identify a cutoff based on a specific precision or recall requirement. 
  # Calculate precision-recall curve
  pr_curve <- pr.curve(scores.class0 = y_prob, weights.class0 = mdl_df$llm_inter == 1, curve = TRUE)
  precision <- pr_curve$curve[, 2]  # Precision values
  recall <- pr_curve$curve[, 1]     # Recall values
  cutoffs <- pr_curve$curve[, 3]    # Cutoff values
  # Calculate F1 scores and find optimal cutoff
  f1_scores <- 2 * (precision * recall) / (precision + recall)  # Calculate F1 scores
  optimal_index <- which.max(f1_scores)  # Find index of maximum F1 score
  optimal_cutoff <- cutoffs[optimal_index]  # Get the optimal cutoff value
  # Prepare data for plotting
  pr_data <- data.frame(Recall = recall, Precision = precision, Cutoff = cutoffs)
  # Plot the precision-recall curve with the optimal cutoff point
  p <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
    geom_line(color = "blue") +
    geom_point(aes(x = recall[optimal_index], y = precision[optimal_index]), 
               color = "red", size = 3, shape = 8) +  # Mark optimal cutoff point
    geom_text(aes(x = recall[optimal_index], y = precision[optimal_index], 
                  label = paste("F1:", round(f1_scores[optimal_index], 4)) #paste("Optimal Cutoff:", round(optimal_cutoff, 4))
    ), 
    hjust = 0.5, vjust = -0.5, color = "red") +
    labs(title = paste0("Precision-Recall Curve (", pr_curve$auc.integral,")"), x = "Recall", y = "Precision") +
    theme_minimal()
  print(p)
  optimal_cutoff_inter <- optimal_cutoff
  
  
  # to remove false positive
  mdl_df$x <- mdl_df[[score_col]]
  mdl_score <- glm(llm_union ~ x, data=mdl_df, family = binomial(link = "logit"))
  y_prob <- predict(mdl_score, type = "response")
  pr_curve <- pr.curve(scores.class0 = y_prob, weights.class0 = mdl_df$llm_inter == 1, curve = TRUE)
  precision <- pr_curve$curve[, 2]  # Precision values
  recall <- pr_curve$curve[, 1]     # Recall values
  cutoffs <- pr_curve$curve[, 3]    # Cutoff values
  f1_scores <- 2 * (precision * recall) / (precision + recall)  # Calculate F1 scores
  optimal_index <- which.max(f1_scores)  # Find index of maximum F1 score
  optimal_cutoff <- cutoffs[optimal_index]  # Get the optimal cutoff value
  pr_data <- data.frame(Recall = recall, Precision = precision, Cutoff = cutoffs)
  # p <- ggplot(pr_data, aes(x = Recall, y = Precision)) +
  #   geom_line(color = "blue") +
  #   geom_point(aes(x = recall[optimal_index], y = precision[optimal_index]), 
  #              color = "red", size = 3, shape = 8) +  # Mark optimal cutoff point
  #   geom_text(aes(x = recall[optimal_index], y = precision[optimal_index], 
  #                 label = paste("F1:", round(f1_scores[optimal_index], 4)) #paste("Optimal Cutoff:", round(optimal_cutoff, 4))
  #   ), 
  #   hjust = 0.5, vjust = -0.5, color = "red") +
  #   labs(title = paste0("Precision-Recall Curve (", pr_curve$auc.integral,")"), x = "Recall", y = "Precision") +
  #   theme_minimal()
  # print(p)
  optimal_cutoff_union <- optimal_cutoff
  
  
  
  
  
  return(list(optimal_cutoff_inter = optimal_cutoff_inter,
              optimal_cutoff_union = optimal_cutoff_union))
}


mdl_name = "stella_en_1.5B_v5"
# mdl_name = "stella_en_400M_v5"
# mdl_name = "all-mpnet-base-v2"

fea_df <- read.csv("./data/fea_df.csv")
# fea_df <- fea_df[which(!fea_df$fea%in%c("nosocialeat", "thinspo", "fearcarb")),]
obj <- get_score_df("gpt_data", mdl_name)
label_df_gpt4omini <- obj$labeled_df
label_df_gpt4omini[is.na(label_df_gpt4omini)] <- 0
score_df_gpt4omini <- obj$scores_df
score_df_gpt4omini[is.na(score_df_gpt4omini)] <- 0
obj <- get_score_df("llama_data", mdl_name)
label_df_llama8b <- obj$labeled_df
label_df_llama8b[is.na(label_df_llama8b)] <- 0
score_df_llama8b <- obj$scores_df
score_df_llama8b[is.na(score_df_llama8b)] <- 0



# find the intersect ture positive tables
label_df_llama8b_new <- label_df_llama8b
label_df_gpt4omini_new <- label_df_gpt4omini
for(topic in fea_df$fea){
  llms_df <- data.frame(llm1 = label_df_llama8b[[topic]], 
                        llm2 = label_df_gpt4omini[[topic]] )
  llms_df$llm_inter <- ifelse(rowSums(llms_df)==ncol(llms_df),1,0)
  llms_df$llm_union <- ifelse(rowSums(llms_df)>0,1,0)
  adjs_df <- data.frame(llm1_score = score_df_llama8b[[topic]], 
                        llm2_score = score_df_gpt4omini[[topic]] )
  table(llms_df[,c("llm1", "llm2")])
  
  for(i in 1:3){
    mdl_df <- cbind(llms_df, adjs_df)
    opt_cuts1 <- pr_opt(mdl_df, score_col = "llm1_score")
    opt_cuts2 <- pr_opt(mdl_df, score_col = "llm2_score")
    
    llm1_01 <- llms_df$llm1 == 0 & llms_df$llm2 == 1
    llm1_10 <- llms_df$llm1 == 1 & llms_df$llm2 == 0
    # llms_df$llm1[llm1_01 & adjs_df$llm1_score>=opt_cuts1$optimal_cutoff_inter] <- 1
    llms_df$llm1[llm1_10 & adjs_df$llm1_score<opt_cuts1$optimal_cutoff_inter] <- 0
    
    llm2_01 <- llms_df$llm2 == 0 & llms_df$llm1 == 1
    llm2_10 <- llms_df$llm2 == 1 & llms_df$llm1 == 0
    # llms_df$llm2[llm2_01 & adjs_df$llm2_score>=opt_cuts2$optimal_cutoff_inter] <- 1
    llms_df$llm2[llm2_10 & adjs_df$llm2_score<opt_cuts2$optimal_cutoff_inter] <- 0
    
    llms_df$llm_inter <- ifelse(rowSums(llms_df)==ncol(llms_df),1,0)
    print(table(llms_df[,c("llm1", "llm2")]))
  }
  
  
  label_df_llama8b_new[[topic]] <- llms_df$llm1
  label_df_gpt4omini_new[[topic]] <- llms_df$llm2
}

# sanity check
plot_distribution <- function(type = "raw"){
  # Load necessary libraries
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(pheatmap)
  
  if(type == "raw"){
    label_df_llm1 <- label_df_gpt4omini
    label_df_llm2 <- label_df_llama8b
  }else{
    label_df_llm1 <- label_df_gpt4omini_new
    label_df_llm2 <- label_df_llama8b_new
  }
  
  distribution_gpt4omini <- label_df_llm1 %>%
    group_by(group) %>%
    summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  distribution_llama8b <- label_df_llm2 %>%
    group_by(group) %>%
    summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  tmp1 <- distribution_gpt4omini
  tmp2 <- distribution_llama8b
  mat1 <- as.matrix(tmp1[, -1])
  rownames(mat1) <- tmp1$group
  pheatmap(mat1,  # Remove the group column, if it's the first column
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           legend_breaks = seq(-1,1,0.1),
           main = "Occurrence rates by GPT 4o-mini")
  mat2 <- as.matrix(tmp2[, -1])
  rownames(mat2) <- tmp2$group
  pheatmap(mat2,  # Remove the group column, if it's the first column
           cluster_rows = F,
           cluster_cols = F,
           legend_breaks = seq(-1,1,0.1),
           main = "Occurrence rates by Llama 8b-instruct (need a second run)")
  
  tmp1 <- distribution_gpt4omini %>% mutate(model = "GPT-4o-mini")
  tmp2 <- distribution_llama8b %>% mutate(model = "Llama 8b-instruct")
  combined_data <- bind_rows(tmp1, tmp2)
  long_data <- combined_data  %>%
    pivot_longer(cols = -c(group, model), names_to = "category", values_to = "occurrence_rate")
  print(ggplot(long_data, aes(x = group, y = occurrence_rate, fill = model)) +
          geom_bar(stat = "identity", position = "dodge") +
          facet_wrap(~category,ncol=4, scales = "free_x") + 
          labs(y = "Occurrence Rate",
               title = type) +
          theme_minimal() +
          scale_fill_manual(values = c("GPT-4o-mini" = "skyblue", "Llama 8b-instruct" = "coral")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "top"))
  
}

plot_distribution("raw")
plot_distribution("adjusted")
