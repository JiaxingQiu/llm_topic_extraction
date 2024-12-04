ensemble_sample <- function(cb, return_label=F){
  llm_list <- llm_full_list[cb]
  
  # ---- ensemble score by PCA ----
  score_df_pca <- score_df_llama  # Initiation
  for (topic in fea_df$fea) {
    pca_df <- NULL
    for (llm_name in names(llm_list)) {
      llm_dfs <- llm_list[[llm_name]]
      pca_df_llm <- data.frame(
        "score" = llm_dfs$score_df[, topic],
        "label" = llm_dfs$label_df[, topic]
      )
      colnames(pca_df_llm) <- paste0(colnames(pca_df_llm), "_", llm_name)
      if (is.null(pca_df)) {
        pca_df <- pca_df_llm
      } else {
        pca_df <- bind_cols(pca_df, pca_df_llm)
      }
    }
    
    # PCA on this dataframe, with dimension = 2, keep the first dimension
    pca_result <- prcomp(pca_df[, startsWith(colnames(pca_df), "score_")], center = TRUE, scale. = TRUE)
    pca_df$pca1 <- pca_result$x[, 1]
    
    # Correction for the sign
    if (cor(pca_df$pca1, score_df_llama[, topic]) < 0) {
      pca_df$pca1 <- -pca_df$pca1
    }
    score_df_pca[, topic] <- (pca_df$pca1 - min(pca_df$pca1)) / (max(pca_df$pca1) - min(pca_df$pca1))
  }
  
  # ---- ensemble agreement label df ----
  label_df_list <- list()
  for (llm_name in names(llm_list)) {
    label_df_list[[llm_name]] <- llm_list[[llm_name]]$label_df
  }
  
  agree_ratio <- 0.1
  label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio)
  label_df_agreed_union <- label_df_ls$agreed
  label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio_inter)
  label_df_agreed_inter <- label_df_ls$agreed
  
  # Find thresholds
  t_df <- explore_thresholds(
    label_df_agreed_union,
    score_df_pca,
    fea_df,
    label_df_agreed_inter
  )
  
  # Adjust union label based on threshold
  threshold_df <- t_df %>%
    group_by(topic) %>%
    summarise(threshold = t[which(f1_score == max(f1_score))][1])
  threshold_df$fea <- threshold_df$topic
  threshold_df <- as.data.frame(threshold_df)
  threshold_df <- merge(threshold_df, fea_df, by = "fea")
  adjust_label_df <- adjust_label_by_threshold_score(
    label_df_agreed_union,
    score_df_pca,
    threshold_df
  )
  score_df_pca <- adjust_score_by_threshold_score(score_df_pca,
                                                  threshold_df)
  
  if(return_label){
    return(list("ensemble_label" = adjust_label_df,
                "ensemble_score" = score_df_pca))
  }else{
    # ---- Evaluation ----
    eval_pca <- eval_llm(score_df_pca, label_true, bal)
    eval_pca$llms <- paste0(names(llm_list), collapse = "; ")
    eval_pca$llm <- paste0("pca_score (",length(cb)," llms)")
    eval_agree_u = eval_llm(label_df_agreed_union, label_true, bal)
    eval_agree_u$llms <- paste0(names(llm_list), collapse = "; ")
    eval_agree_u$llm <- paste0("union_label (",length(cb)," llms)")
    eval_agree_i = eval_llm(label_df_agreed_inter, label_true, bal)
    eval_agree_i$llms <- paste0(names(llm_list), collapse = "; ")
    eval_agree_i$llm <- paste0("inter_label (",length(cb)," llms)")
    eval_agree_adj = eval_llm(adjust_label_df, label_true, bal)
    eval_agree_adj$llms <- paste0(names(llm_list), collapse = "; ")
    eval_agree_adj$llm <- paste0("pca_label (",length(cb)," llms)")
    
    # ---- get improvement from individual evals ----
    eval_label_inds <- list()
    eval_score_inds <- list()
    for(llm_name in names(llm_list)){
      eval_label_inds[[llm_name]] <- eval_ind_ls[[llm_name]]$label[,c("topic", c("auprc", "f1_score", "fb_score", "sensitivity", "specificity", "precision"))]
      eval_score_inds[[llm_name]] <- eval_ind_ls[[llm_name]]$score[,c("topic", c("auprc", "f1_score", "fb_score", "sensitivity", "specificity", "precision"))]
    }
    # Reduce the list of data frames by taking the median for specified columns
    eval_label_median <- eval_label_inds %>%
      purrr::reduce(function(df1, df2) {
        # Ensure consistent structure between data frames
        combined <- bind_rows(df1, df2)
        # Group by all columns except the metrics, if needed
        combined %>%
          group_by(across(-c(auprc, f1_score, fb_score, sensitivity, specificity, precision))) %>%
          summarise(
            across(
              c(auprc, f1_score, fb_score, sensitivity, specificity, precision),
              ~ median(., na.rm = TRUE)
            ),
            .groups = "drop"
          )
      }) %>% as.data.frame()
    eval_score_median <- eval_score_inds %>%
      purrr::reduce(function(df1, df2) {
        # Ensure consistent structure between data frames
        combined <- bind_rows(df1, df2)
        # Group by all columns except the metrics, if needed
        combined %>%
          group_by(across(-c(auprc, f1_score, fb_score, sensitivity, specificity, precision))) %>%
          summarise(
            across(
              c(auprc, f1_score, fb_score, sensitivity, specificity, precision),
              ~ median(., na.rm = TRUE)
            ),
            .groups = "drop"
          )
      }) %>% as.data.frame()
    
    # get the deviation
    eval_score_median <- eval_score_median[which(eval_score_median$topic%in%fea_df$fea), ]
    eval_label_median <- eval_label_median[which(eval_label_median$topic%in%fea_df$fea), ]
    eval_pca_delta <- eval_pca %>% arrange(topic)
    eval_agree_adj_delta <- eval_agree_adj %>% arrange(topic)
    for(e in c("auprc", "f1_score", "fb_score", "sensitivity", "specificity", "precision")){
      eval_pca_delta[,e] <- eval_pca_delta[,e] - eval_score_median[,e]
      eval_agree_adj_delta[,e] <- eval_agree_adj_delta[,e] - eval_label_median[,e]
    }
    
    
    # ---- return ----
    return(list(
      eval_pca = eval_pca,
      eval_pca_delta = eval_pca_delta,
      eval_agree_u = eval_agree_u,
      eval_agree_i = eval_agree_i,
      eval_agree_adj = eval_agree_adj,
      eval_agree_adj_delta = eval_agree_adj_delta
    ))
  }
  
}





# 
# 
# # Display all color palettes
# display.brewer.all()
# get Paired set of colors
library(RColorBrewer)
brewer.pal(12, "Paired")


color_scale <- c(
  # Warm colors (gradient from dark red to orange)
  "Ensemble\n(outlier excluded)" = "#E31A1C",#"brown1", # Dark Red
  # "Ensemble (5 LLMs)" = "#FF6347", # Dark Red
  # "Ensemble (5 LLMs)" = "#FF6347", # Dark Red
  "Ensemble (4 LLMs)" = "#FB9A99",# "#FFA500", # Firebrick
  "Ensemble (3 LLMs)" = "#FDBF6F",#"#D9EF8B",#, # Orange-Red
  "Ensemble (2 LLMs)" = "#FFFF99",#"#FEE08B", # Tomato
  # "ensemble (2 llms)" = "#D9EF8B", # Orange
  # Distinct cold colors for individual models
  # "GPT-4omini" = "pink2",
  "Llama-3.1-8B-Instruct" = "#A6CEE3",
  "Qwen2.5-7B-Instruct" = "#1F78B4",
  "Mistral-7B-Instruct-v0.3" = "#33A02C",
  # "vicuna-13b" = "#A6CEE3",
  "vicuna-7b-v1.5" = "#6A3D9A"
)
plot_evaluation <- function(eval_df, varname = "f1_score"){
  if(varname == "auprc"){
    eval_df$y <- eval_df[,varname]
    eval_df_summary <- eval_df %>%
      filter(grepl("_score", as.character(llm))) %>%
      group_by(topic, llm) %>%
      summarise(
        y_median = quantile(y, 1, na.rm = TRUE),#median(y, na.rm = TRUE),
        y_low = quantile(y, 0, na.rm = TRUE),
        y_q25 = quantile(y, 0.25, na.rm = TRUE),
        y_q75 = quantile(y, 0.75, na.rm = TRUE),
        y_up = quantile(y, 1, na.rm = TRUE)
      )
    eval_df_summary$llm <- factor(eval_df_summary$llm, 
                                  levels = c("pca_score (ex_outlier)",
                                             "pca_score (6 llms)",
                                             "pca_score (5 llms)",
                                             "pca_score (4 llms)",
                                             "pca_score (3 llms)",
                                             "pca_score (2 llms)",
                                             "gpt4omini_score",
                                             "llama_score",
                                             "qwen_score",
                                             "mistral_score",
                                             "vicuna13b_score",
                                             "vicuna7b_score"))
    
  }else{
    eval_df$y <- eval_df[,varname]
    eval_df_summary <- eval_df %>%
      filter(grepl("_label", as.character(llm))) %>%
      group_by(topic, llm) %>%
      summarise(
        y_median = quantile(y, 1, na.rm = TRUE),#median(y, na.rm = TRUE),
        y_low = quantile(y, 0, na.rm = TRUE),
        y_q25 = quantile(y, 0.25, na.rm = TRUE),
        y_q75 = quantile(y, 0.75, na.rm = TRUE),
        y_up = quantile(y, 1, na.rm = TRUE)
      )
    eval_df_summary$llm <- factor(eval_df_summary$llm, 
                                  levels = c("pca_label (ex_outlier)",
                                             "pca_label (6 llms)",
                                             "pca_label (5 llms)",
                                             "pca_label (4 llms)",
                                             "pca_label (3 llms)",
                                             "pca_label (2 llms)",
                                             "gpt4omini_label",
                                             "llama_label",
                                             "qwen_label",
                                             "mistral_label",
                                             "vicuna13b_label",
                                             "vicuna7b_label"))
    
  }
  levels(eval_df_summary$llm) <- c("Ensemble\n(outlier excluded)",
                                   "Ensemble (6 LLMs)",
                                   "Ensemble (5 LLMs)",
                                   "Ensemble (4 LLMs)",
                                   "Ensemble (3 LLMs)",
                                   "Ensemble (2 LLMs)",
                                   "GPT-4omini",
                                   "Llama-3.1-8B-Instruct",
                                   "Qwen2.5-7B-Instruct",
                                   "Mistral-7B-Instruct-v0.3",
                                   "vicuna-13b",
                                   "vicuna-7b-v1.5")
  eval_df_summary[which((!grepl("Ensemble",eval_df_summary$llm)) ),c("y_low", "y_q25","y_q75","y_up")] <- NA ##|(grepl("outlier",eval_df_summary$llm))|(grepl("4 LLMs",eval_df_summary$llm))
  # eval_df_summary <- eval_df_summary[which(!eval_df_summary$llm %in% c("ensemble (6 llms)",
  #                                                                      "ensemble (2 llms)")),]
  
  p <- ggplot(eval_df_summary, aes(x = as.factor(topic), y = y_median, fill = llm)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(
      # data=eval_df_summary[which(grepl("ensemble",eval_df_summary$llm)),],
      aes(ymin = y_low, ymax = y_up),
      position = position_dodge(width = 0.9),
      width = 0.3,
      linewidth = 0.2
    ) +
    geom_errorbar( 
      aes(ymin = y_q25, ymax = y_q75),
      position = position_dodge(width = 0.9),
      width = 0.6,
      linewidth = 0.4
    ) +
    theme_minimal() + scale_fill_manual(values = color_scale) +
    theme(
      axis.title.y = element_text(size=8, face = "bold"),
      axis.ticks.x = element_blank(),# remove
      axis.text.x = element_text(angle =35, hjust = 1, size = 8, margin = margin(t = -5), face="bold"),
      legend.text = element_text(size = 8),  # Smaller font size for legend items
      legend.position = "right",  # Positioning the legend
      legend.box.margin = margin(2, 2, 2, 2),  # Tighten the margin around the entire legend
      legend.spacing.x = unit(0.2, "cm"),  # Reduce horizontal space between legend items
      legend.key.size = unit(0.5, "cm")  
    ) +
    labs(x = NULL, y = varname, fill=NULL, color=NULL)
  if(any(grepl("stigma",as.character(eval_df$topic)) )){
    p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = -2, size=8, face="bold") )
  }
  return(p)
}
plot_improve <- function(eval_df_delta, ename = "f1_score"){
  eval_df_delta <- as.data.frame(eval_df_delta)
  eval_df_delta$yy <- eval_df_delta[,ename]
  if(any(grepl("_score", eval_df_delta$llm))){
    eval_df_delta$llm <- factor(eval_df_delta$llm, 
                                levels = c("pca_score (ex_outlier)",
                                           "pca_score (6 llms)",
                                           "pca_score (5 llms)",
                                           "pca_score (4 llms)",
                                           "pca_score (3 llms)",
                                           "pca_score (2 llms)"))
  }else{
    eval_df_delta$llm <- factor(eval_df_delta$llm, 
                                levels = c("pca_label (ex_outlier)",
                                           "pca_label (6 llms)",
                                           "pca_label (5 llms)",
                                           "pca_label (4 llms)",
                                           "pca_label (3 llms)",
                                           "pca_label (2 llms)"))
  }
  levels(eval_df_delta$llm) <- c("Ensemble\n(outlier excluded)",
                                 "Ensemble (6 LLMs)",
                                 "Ensemble (5 LLMs)",
                                 "Ensemble (4 LLMs)",
                                 "Ensemble (3 LLMs)",
                                 "Ensemble (2 LLMs)")
  # eval_df_delta <- eval_df_delta[which(!eval_df_delta$llm %in% c("ensemble (6 llms)",
  #                                                                "ensemble (2 llms)")),]
  
  p <- ggplot(eval_df_delta, aes(x=as.factor(topic), y = yy, fill = llm)) + # 
    geom_boxplot(alpha = 0.3, size = 0.5, aes(color = llm)) + 
    geom_hline(aes(yintercept=0), color="darkgrey")+
    scale_y_continuous(limits = c(-max(abs(eval_df_delta$yy)), max(abs(eval_df_delta$yy)))) + 
    theme_minimal() +
    theme(
      axis.title.y = element_text(size=8, face = "bold"),
      axis.ticks.x = element_blank(),# remove
      axis.text.x = element_text(angle =35, hjust = 1, size = 8, margin = margin(t = -5), face="bold"),
      legend.text = element_text(size = 8),  # Smaller font size for legend items
      legend.position = "right",  # Positioning the legend
      legend.box.margin = margin(2, 2, 2, 2),  # Tighten the margin around the entire legend
      legend.spacing.x = unit(0.2, "cm"),  # Reduce horizontal space between legend items
      legend.key.size = unit(0.5, "cm")
    ) + scale_fill_manual(values = color_scale) + scale_color_manual(values = color_scale) +
    labs(x = NULL, y = ename, fill=NULL, color=NULL)
  if(any(grepl("stigma",as.character(eval_df_delta$topic)) )){
    p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = -2, size=8, face="bold") )
  }
  return(p)
}
