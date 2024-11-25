setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis2.R")
label_true <- read.csv("./data2/answer_df_raw.csv")
bal <- F
mean(label_true$stigma) # 0.75

# --- individual evals ---
eval_llama <- eval_llm(label_df_llama, label_true, bal)
eval_llama$llm <- "llama_label"
eval_llama_s <- eval_llm(score_df_llama, label_true, bal)
eval_llama_s$llm <- "llama_score"
eval_qwen <- eval_llm(label_df_qwen, label_true, bal)
eval_qwen$llm <- "qwen_label"
eval_qwen_s <- eval_llm(score_df_qwen, label_true, bal)
eval_qwen_s$llm <- "qwen_score"
eval_vicuna7b <- eval_llm(label_df_vicuna7b, label_true, bal)
eval_vicuna7b$llm <- "vicuna7b_label"
eval_vicuna7b_s <- eval_llm(score_df_vicuna7b, label_true, bal)
eval_vicuna7b_s$llm <- "vicuna7b_score"
eval_mistral <- eval_llm(label_df_mistral, label_true, bal)
eval_mistral$llm <- "mistral_label"
eval_mistral_s <- eval_llm(score_df_mistral, label_true, bal)
eval_mistral_s$llm <- "mistral_score"

eval_ind_ls <- list("llama" = list("score" = eval_llama_s,
                                   "label" = eval_llama),
                    "qwen" = list("score" = eval_qwen_s,
                                  "label" = eval_qwen),
                    "mistral" = list("score" = eval_mistral_s,
                                     "label" = eval_mistral),
                    "vicuna7b" = list("score" = eval_vicuna7b_s,
                                      "label" = eval_vicuna7b)
)

# label_df_llama$stigma <- rbinom(1368, size = 1, prob = 0.97)
# label_df_mistral$stigma <- rbinom(1368, size = 1, prob = 0.95)
# label_df_vicuna7b$stigma <- rbinom(1368, size = 1, prob = 0.95)
# label_df_qwen$stigma <-  rbinom(1368, size = 1, prob = 0.05)
llm_full_list <- list("llama" = list("label_df" = label_df_llama,
                                     "score_df" = score_df_llama),
                      "qwen" = list("label_df" = label_df_qwen,
                                    "score_df" = score_df_qwen),
                      "mistral" = list("label_df" = label_df_mistral,
                                       "score_df" = score_df_mistral),
                      "vicuna7b" = list("label_df" = label_df_vicuna7b,
                                        "score_df" = score_df_vicuna7b)
)
# create all combination of c(1,2,3,4,5,6) that has at least 2 elements
vec <- c(1, 2, 3, 4)
combinations_at_least_two <- lapply(2:length(vec), function(k) combn(vec, k, simplify = FALSE))
combinations_at_least_two <- unlist(combinations_at_least_two, recursive = FALSE)

true_set <- ifelse(nrow(label_true)<2000, "human", "gpt4o")
agree_ratio_inter <- 0.5 # at least half
# agree_ratio_inter <- 0.35 # at least 2
res_filename <- paste0("./res2/ensemble_label_comb_",agree_ratio_inter,"_",true_set,".RData") 
if(!file.exists(res_filename)){
  library(doParallel)
  library(foreach)
  library(dplyr)
  
  # Set up the parallel backend
  num_cores <- detectCores() - 3  # Use all available cores except one
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Create the progress bar
  num_combinations <- length(combinations_at_least_two)
  pb <- txtProgressBar(min = 0, max = num_combinations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  # Parallel processing with foreach
  results <- foreach(
    k = seq_along(combinations_at_least_two),
    .packages = c("dplyr"),   # Include required packages
    .options.snow = opts
  ) %dopar% {
    
    
    cb <- combinations_at_least_two[[k]]
    llm_list <- llm_full_list[cb]
    
    
    fea_df <- read.csv("./data2/fea_df.csv") # use full features
    # ---- ensemble score by PCA ----
    score_df_pca <- score_df_llama  # Initiation
    for (topic in fea_df$fea) { # 
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
    # ---- ensemble agreement label df for category labels ----
    label_df_list <- list()
    for (llm_name in names(llm_list)) {
      label_df_list[[llm_name]] <- llm_list[[llm_name]]$label_df
    }
    agree_ratio <- 0.1
    label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio)
    label_df_agreed_union <- label_df_ls$agreed
    label_df_agreed_union$stigma <- ifelse(rowSums(label_df_agreed_union[,fea_df$fea])>0,1,0)
    label_df_ls <- get_label_agreed_by_dfs(label_df_list, agree_ratio_inter)
    label_df_agreed_inter <- label_df_ls$agreed
    label_df_agreed_inter$stigma <- ifelse(rowSums(label_df_agreed_inter[,fea_df$fea])>0,1,0)
    t_df <- explore_thresholds(
      label_df_agreed_union,
      score_df_pca,
      fea_df,
      label_df_agreed_inter
    )
    threshold_df <- t_df %>%
      group_by(topic) %>%
      summarise(threshold = t[which(f1_score == max(f1_score))][1])
    threshold_df$fea <- threshold_df$topic
    threshold_df <- as.data.frame(threshold_df)
    threshold_df <- merge(threshold_df, fea_df, by = "fea")
    adjust_label_df_feas <- adjust_label_by_threshold_score(
      label_df_agreed_union,
      score_df_pca,
      threshold_df
    )
    
    # ---- ensemble final label ----
    adjust_label_df <- adjust_label_df_feas
    adjust_label_df$stigma <- ifelse(rowSums(adjust_label_df[,fea_df$fea])>0,1,0)
    fea_df <- data.frame("fea" = "stigma", "description" = NA)
    label_df_ls <- get_label_agreed_by_dfs(list(label_df_agreed_union, 
                                                adjust_label_df), 0.1)
    label_df_agreed_union <- label_df_ls$agreed
    label_df_ls <- get_label_agreed_by_dfs(list(label_df_agreed_inter, 
                                                adjust_label_df), 0.7)
    label_df_agreed_inter <- label_df_ls$agreed
    t_df <- explore_thresholds(
      label_df_agreed_union,
      score_df_pca,
      fea_df,
      label_df_agreed_inter
    )
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
    
    
    
    # ---- Evaluation ----
    eval_pca = eval_llm(score_df_pca, label_true, bal)
    eval_pca$llms <- paste0(names(llm_list), collapse = "; ")
    eval_pca$llm <- paste0("pca_score (",length(cb)," llms)")
    eval_agree_u = eval_llm(label_df_agreed_union, label_true, bal)
    eval_agree_u$llms <- paste0(names(llm_list), collapse = "; ")
    eval_agree_u$llm <- paste0("union_label (",length(cb)," llms)")
    eval_agree_i = eval_llm(label_df_agreed_inter, label_true, bal)
    eval_agree_i$llms <- paste0(names(llm_list), collapse = "; ")
    eval_agree_i$llm <- paste0("inter_label (",length(cb)," llms)")
    eval_agree_adj <- eval_llm(adjust_label_df, label_true, bal)
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
    eval_pca_delta <- eval_pca
    eval_agree_adj_delta <- eval_agree_adj
    for(e in c("auprc", "f1_score", "fb_score", "sensitivity", "specificity", "precision")){
      eval_pca_delta[,e] <- eval_pca_delta[,e] - eval_score_median[,e]
      eval_agree_adj_delta[,e] <- eval_agree_adj_delta[,e] - eval_label_median[,e]
    }
    
    
    # ---- return ----
    list(
      eval_pca = eval_pca,
      eval_pca_delta = eval_pca_delta,
      eval_agree_u = eval_agree_u,
      eval_agree_i = eval_agree_i,
      eval_agree_adj = eval_agree_adj,
      eval_agree_adj_delta = eval_agree_adj_delta
    )
  }
  # Close the progress bar and cluster
  close(pb)
  stopCluster(cl)
  
  # Combine results
  eval_pca <- lapply(results, `[[`, "eval_pca")
  eval_pca_delta <- lapply(results, `[[`, "eval_pca_delta")
  eval_agree_u <- lapply(results, `[[`, "eval_agree_u")
  eval_agree_i <- lapply(results, `[[`, "eval_agree_i")
  eval_agree_adj <- lapply(results, `[[`, "eval_agree_adj")
  eval_agree_adj_delta <- lapply(results, `[[`, "eval_agree_adj_delta")
  # save(eval_pca, eval_pca_delta, eval_agree_u, eval_agree_i, eval_agree_adj, eval_agree_adj_delta,
  #      file = res_filename)
}else{
  load(res_filename)
}


# ---- plot eval ----
color_scale <- c(
  # Warm colors (gradient from dark red to orange)
  "ensemble (4 llms)" = "#FF6347", # Firebrick
  "ensemble (3 llms)" = "#FFA500", # Orange-Red
  "ensemble (2 llms)" = "#FEE08B", # Tomato
  # Distinct cold colors for individual models
  "llama-8b" = "green3",
  "qwen-7b" = "steelblue2",
  "mistral-7b" = "green4",
  "vicuna-7b" = "purple1"
)

rm_topics <- c()#c("fearfood", "feargain")
eval_df_pca_final <- do.call(rbind, eval_pca)
eval_df_agree_adj <- do.call(rbind, eval_agree_adj)
eval_df <- rbind(eval_df_agree_adj[,intersect(colnames(eval_df_agree_adj), colnames(eval_llama))],
                 eval_df_pca_final[,intersect(colnames(eval_df_pca_final), colnames(eval_llama))],
                 eval_llama, eval_llama_s,
                 eval_qwen, eval_qwen_s,
                 eval_vicuna7b, eval_vicuna7b_s,
                 eval_mistral, eval_mistral_s)
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
                                  levels = c("pca_score (6 llms)",
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
                                  levels = c("pca_label (6 llms)",
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
  levels(eval_df_summary$llm) <- c("ensemble (6 llms)",
                                   "ensemble (5 llms)",
                                   "ensemble (4 llms)",
                                   "ensemble (3 llms)",
                                   "ensemble (2 llms)",
                                   "gpt-4omini",
                                   "llama-8b",
                                   "qwen-7b",
                                   "mistral-7b",
                                   "vicuna-13b",
                                   "vicuna-7b")
  eval_df_summary[which(!grepl("ensemble",eval_df_summary$llm)),c("y_low", "y_q25","y_q75","y_up")] <- NA
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
      width = 0.7,
      linewidth = 0.5
    ) +
    theme_minimal() + scale_fill_manual(values = color_scale) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8, margin = margin(t = -5)),
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 8)
    ) +
    labs(x = NULL, y = varname) + ylim(0,1)
  
  return(p)
}
eval_df  <- eval_df[which(!eval_df$topic%in%rm_topics),]
p1 <- plot_evaluation(eval_df, "f1_score") 
p2 <- plot_evaluation(eval_df, "precision")
p3 <- plot_evaluation(eval_df, "sensitivity")
p4 <- plot_evaluation(eval_df, "auprc")+ ylab("auprc (by score)")
p_eval <- ggarrange(p4, p1,p2,p3, ncol=1, common.legend = T, legend = "right")
p_eval <- annotate_figure(
  p_eval,
  top = text_grob("Performance Evaluation", size = 12, face = "bold", hjust=0, x=0)
)

# ---- plot improvement ----
plot_improve <- function(eval_df_delta, ename = "f1_score"){
  eval_df_delta <- as.data.frame(eval_df_delta)
  eval_df_delta$yy <- eval_df_delta[,ename]
  if(any(grepl("_score", eval_df_delta$llm))){
    eval_df_delta$llm <- factor(eval_df_delta$llm, 
                                levels = c("pca_score (6 llms)",
                                           "pca_score (5 llms)",
                                           "pca_score (4 llms)",
                                           "pca_score (3 llms)",
                                           "pca_score (2 llms)"))
  }else{
    eval_df_delta$llm <- factor(eval_df_delta$llm, 
                                levels = c("pca_label (6 llms)",
                                           "pca_label (5 llms)",
                                           "pca_label (4 llms)",
                                           "pca_label (3 llms)",
                                           "pca_label (2 llms)"))
  }
  levels(eval_df_delta$llm) <- c("ensemble (6 llms)",
                                 "ensemble (5 llms)",
                                 "ensemble (4 llms)",
                                 "ensemble (3 llms)",
                                 "ensemble (2 llms)")
  # eval_df_delta <- eval_df_delta[which(!eval_df_delta$llm %in% c("ensemble (6 llms)",
  #                                                                "ensemble (2 llms)")),]
  
  p <- ggplot(eval_df_delta, aes(x=as.factor(topic), y = yy, fill = llm)) + # 
    geom_boxplot(alpha = 0.3, size = 0.5, aes(color = llm)) + 
    geom_hline(aes(yintercept=0), color="darkgrey")+
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8, margin = margin(t = -5)),
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 8)
    ) + scale_fill_manual(values = color_scale) + scale_color_manual(values = color_scale) +
    labs(x = NULL, y = ename)
  return(p)
}
eval_df_pca_delta <- do.call(rbind, eval_pca_delta)
eval_df_agree_adj_delta <- do.call(rbind, eval_agree_adj_delta)

eval_df_pca_delta  <- eval_df_pca_delta[which(!eval_df_pca_delta$topic%in%rm_topics),]
eval_df_agree_adj_delta  <- eval_df_agree_adj_delta[which(!eval_df_agree_adj_delta$topic%in%rm_topics),]

p1 <- plot_improve(eval_df_pca_delta, "auprc") + ylab("auprc (by score)")
p2 <- plot_improve(eval_df_agree_adj_delta, "f1_score")
p3 <- plot_improve(eval_df_agree_adj_delta, "precision")
p4 <- plot_improve(eval_df_agree_adj_delta, "sensitivity")
p_improve <- ggarrange(p1, p2, p3, p4, ncol=1, common.legend = T, legend = "right")
# add text "Performance Increase" on the top of p_improve
p_improve <- annotate_figure(
  p_improve,
  top = text_grob("Performance Increase", size = 12, face = "bold", hjust=0, x=0)
)

ggarrange(p_eval, p_improve, nrow=1) %>% ggsave(filename = paste0("./res2/",true_set,"_evaluation.png"), width = 8, height = 8, bg="white")
