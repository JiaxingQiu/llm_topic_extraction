setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
label_true <- read.csv("./human_data/results.csv")
# human_df <- read.csv("./human_data/sampled_posts.csv")
# label_true <- label_df_gpt4o
bal <- F
rm_ls <- readRDS("./res/1/outliers.RDS")

# --- individual evals ---
eval_gpt <- eval_llm(label_df_gpt4o, label_true, bal)
eval_gpt$llm <- "gpt4o_label"
eval_gpt_s <- eval_llm(score_df_gpt4o, label_true, bal)
eval_gpt_s$llm <- "gpt4o_score"
eval_mini <- eval_llm(label_df_mini, label_true, bal)
eval_mini$llm <- "gpt4omini_label"
eval_mini_s <- eval_llm(score_df_mini, label_true, bal)
eval_mini_s$llm <- "gpt4omini_score"
eval_llama <- eval_llm(label_df_llama, label_true, bal)
eval_llama$llm <- "llama_label"
eval_llama_s <- eval_llm(score_df_llama, label_true, bal)
eval_llama_s$llm <- "llama_score"
eval_qwen <- eval_llm(label_df_qwen, label_true, bal)
eval_qwen$llm <- "qwen_label"
eval_qwen_s <- eval_llm(score_df_qwen, label_true, bal)
eval_qwen_s$llm <- "qwen_score"
eval_vicuna13b <- eval_llm(label_df_vicuna13b, label_true, bal)
eval_vicuna13b$llm <- "vicuna13b_label"
eval_vicuna13b_s <- eval_llm(score_df_vicuna13b, label_true, bal)
eval_vicuna13b_s$llm <- "vicuna13b_score"
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
                    # ,
                    # "mini" = list("score" = eval_mini_s,
                    #               "label" = eval_mini)#,
                    # "vicuna13b" = list("score" = eval_vicuna13b_s,
                    #                    "label" = eval_vicuna13b)
                    )


llm_full_list <- list("llama" = list("label_df" = label_df_llama,
                                     "score_df" = score_df_llama),
                      "qwen" = list("label_df" = label_df_qwen,
                                    "score_df" = score_df_qwen),
                      "mistral" = list("label_df" = label_df_mistral,
                                       "score_df" = score_df_mistral),
                      "vicuna7b" = list("label_df" = label_df_vicuna7b,
                                        "score_df" = score_df_vicuna7b)
                      # ,
                      # "mini" = list("label_df" = label_df_mini,
                      #               "score_df" = score_df_mini)
                      # "vicuna13b" = list("label_df" = label_df_vicuna13b,
                      #                    "score_df" = score_df_vicuna13b)
                      )
# create all combination of c(1,2,3,4,5,6) that has at least 2 elements
vec <- c(1, 2, 3, 4)#, 5) 
combinations_at_least_two <- lapply(2:length(vec), function(k) combn(vec, k, simplify = FALSE))
combinations_at_least_two <- unlist(combinations_at_least_two, recursive = FALSE)


true_set <- ifelse(nrow(label_true)<2000, "human", "gpt4o")
agree_ratio_inter <- 0.5 # at least half
outlier_cut <- 0.1 # percentage of increase
res_filename <- paste0("./res/1/ensemble_label_comb_",agree_ratio_inter,"_",true_set,"_",outlier_cut,".RData") 
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
    ensemble_sample(cb)
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
  
  # get outlier_removed combination
  fea_df_full <- fea_df
  eval_pca_opt <- data.frame()
  eval_pca_delta_opt <- data.frame()
  eval_agree_u_opt <- data.frame()
  eval_agree_i_opt <- data.frame()
  eval_agree_adj_opt <- data.frame()
  eval_agree_adj_delta_opt <- data.frame()
  
  for(topic in fea_df_full$fea){
    fea_df <- fea_df_full[which(fea_df_full$fea==topic),]
    os <- (rm_ls[[topic]][["label"]]$alpha_inc_ratio + rm_ls[[topic]][["score"]]$alpha_inc_ratio)/2
    est_avg <- (rm_ls[[topic]][["label"]]$alpha_est + rm_ls[[topic]][["score"]]$alpha_est)/2
    os <- os/est_avg # percentage of increase
    opt_cb <- setdiff(names(llm_full_list), c(names(os)[which(os>outlier_cut)])) # remove outlier
    if(topic == "protein"){
      opt_cb <- names(llm_full_list)
    }
    opt_res <- ensemble_sample(opt_cb)
    opt_res$eval_pca$llm <- paste0("pca_score (ex_outlier)")
    opt_res$eval_agree_u$llm <- paste0("union_label (ex_outlier)")
    opt_res$eval_agree_i$llm <- paste0("inter_label (ex_outlier)")
    opt_res$eval_agree_adj$llm <- paste0("pca_label (ex_outlier)")
    opt_res$eval_pca_delta$llm <- paste0("pca_score (ex_outlier)")
    opt_res$eval_agree_adj_delta$llm <- paste0("pca_label (ex_outlier)")
    
    eval_pca_opt <- bind_rows(eval_pca_opt, opt_res$eval_pca)
    eval_pca_delta_opt <- bind_rows(eval_pca_delta_opt, opt_res$eval_pca_delta)
    eval_agree_u_opt <-bind_rows(eval_agree_u_opt, opt_res$eval_agree_u)
    eval_agree_i_opt <- bind_rows(eval_agree_i_opt, opt_res$eval_agree_i)
    eval_agree_adj_opt <- bind_rows(eval_agree_adj_opt, opt_res$eval_agree_adj)
    eval_agree_adj_delta_opt <- bind_rows(eval_agree_adj_delta_opt, opt_res$eval_agree_adj_delta)
    
  }
  eval_pca[[length(eval_pca)+1]] <- eval_pca_opt # append to the last element
  eval_pca_delta[[length(eval_pca)+1]] <- eval_pca_delta_opt
  eval_agree_u[[length(eval_pca)+1]] <- eval_agree_u_opt
  eval_agree_i[[length(eval_pca)+1]] <- eval_agree_i_opt
  eval_agree_adj[[length(eval_pca)+1]] <- eval_agree_adj_opt
  eval_agree_adj_delta[[length(eval_pca)+1]] <- eval_agree_adj_delta_opt
  fea_df <- fea_df_full
  
  # save(eval_pca, eval_pca_delta, eval_agree_u, eval_agree_i, eval_agree_adj, eval_agree_adj_delta,
  #      file = res_filename)
}else{
  load(res_filename)
}



# ---- plot eval ----
rm_topics <- c("leanbody","nosocialeat","meal")# "fearfood", "feargain",  c("fearcarb", "fearfood", "feargain")#c("fearfood", "feargain")
eval_df_pca_final <- do.call(rbind, eval_pca)
eval_df_agree_adj <- do.call(rbind, eval_agree_adj)
eval_df_pca_delta <- do.call(rbind, eval_pca_delta)
eval_df_agree_adj_delta <- do.call(rbind, eval_agree_adj_delta)
eval_df <- rbind(eval_df_agree_adj[,intersect(colnames(eval_df_agree_adj), colnames(eval_mini))],
                 eval_df_pca_final[,intersect(colnames(eval_df_pca_final), colnames(eval_mini))],
                 # eval_mini, eval_mini_s,
                 eval_llama, eval_llama_s,
                 eval_qwen, eval_qwen_s,
                 # eval_vicuna13b, eval_vicuna13b_s,
                 eval_vicuna7b, eval_vicuna7b_s,
                 eval_mistral, eval_mistral_s)
eval_df  <- eval_df[which(!eval_df$topic%in%rm_topics),]
eval_df$topic[which(eval_df$topic=="thinspo")] <- "idealbody"
eval_df_pca_delta  <- eval_df_pca_delta[which(!eval_df_pca_delta$topic%in%rm_topics),]
eval_df_agree_adj_delta  <- eval_df_agree_adj_delta[which(!eval_df_agree_adj_delta$topic%in%rm_topics),]
eval_df_pca_delta$topic[which(eval_df_pca_delta$topic=="thinspo")] <- "idealbody"
eval_df_agree_adj_delta$topic[which(eval_df_agree_adj_delta$topic=="thinspo")] <- "idealbody"


eval_gpt <- eval_gpt[which(!eval_gpt$topic%in%rm_topics),]
eval_gpt_s <- eval_gpt_s[which(!eval_gpt_s$topic%in%rm_topics),]
p0 <- plot_evaluation(eval_df, "auprc") + ylab("AUPRC\n(by score)")
p1 <- plot_evaluation(eval_df, "f1_score") + ylab("F1-score\n")
p2 <- plot_evaluation(eval_df, "precision") + ylab("Precision\n")
p3 <- plot_evaluation(eval_df, "specificity") + ylab("Specificity\n")
p4 <- plot_evaluation(eval_df, "sensitivity") + ylab("Sensitivity\n")
if(true_set=="human"){
  eval_gpt_s$model <- "GPT-4o"
  p0 <- p0 + 
    geom_errorbar(data=eval_gpt_s,  
                  aes(y = auprc,  ymin = auprc, ymax = auprc, linetype = model), 
                  width = 0.8, color = "#585858") +
    scale_linetype_manual(values = "dashed", labels = "GPT-4o") + 
    labs(linetype = NULL)
  p1 <- p1 + geom_errorbar(data=eval_gpt,  aes(y = f1_score,  ymin = f1_score, ymax = f1_score), width = 0.8, color = "#585858",  linetype = "dashed" )
  p2 <- p2 + geom_errorbar(data=eval_gpt,  aes(y = precision,  ymin = precision, ymax = precision), width = 0.8, color = "#585858",  linetype = "dashed" )
  p3 <- p3 + geom_errorbar(data=eval_gpt,  aes(y = specificity,  ymin = specificity, ymax = specificity), width = 0.8, color = "#585858",  linetype = "dashed" )
  p4 <- p4 + geom_errorbar(data=eval_gpt,  aes(y = sensitivity,  ymin = sensitivity, ymax = sensitivity), width = 0.8, color = "#585858",  linetype = "dashed" )
  
}
p_eval_ls <- list(p0,p1,p2,p3,p4)
p_eval <- ggarrange(p0,p1,p2,p4, ncol=1, common.legend = T, legend = "right")
p_eval <- annotate_figure(
  p_eval,
  top = text_grob(paste0("A. Predicting ",ifelse(true_set=="human", "Human", "GPT-4o")," Annotation"), 
                  size = 10, face = "bold", 
                  hjust=0, x=0)
)

# ---- plot improvement ----
p1 <- plot_improve(eval_df_pca_delta, "auprc") + ylab("AUPRC\n(by score)")
p2 <- plot_improve(eval_df_agree_adj_delta, "f1_score") + ylab("F1-score")
p3 <- plot_improve(eval_df_agree_adj_delta, "precision") + ylab("Precision")
p4 <- plot_improve(eval_df_agree_adj_delta, "specificity") + ylab("Specificity")
p5 <- plot_improve(eval_df_agree_adj_delta, "sensitivity") + ylab("Sensitivity")
p_improve_ls <- list(p1,p2,p3,p4,p5)
p_improve <- ggarrange(p1, p2, p3, p5, ncol=1, common.legend = T, legend = "right")
# add text "Performance Increase" on the top of p_improve
p_improve <- annotate_figure(
  p_improve,
  top = text_grob("B. Performance Increase Relative to Individual LLMs", 
                  size = 10, face = "bold", 
                  hjust=0, x=0)
)
# save(p_eval, p_eval_ls, p_improve, p_improve_ls, file = paste0("./res/eval_plots_",true_set,"_A.RDS"))
p_final <- ggarrange(p_eval, p_improve, ncol=1) # widths=c(1.3,1),
p_final <- annotate_figure(
  p_final,
  top = text_grob("Data 1: ED and Dieting Reddit Posts", 
                  size = 12, face = "bold", 
                  hjust=0, x=0)
)
p_final %>% ggsave(filename = paste0("./res/",true_set,"_evaluation1.png"), width=8,height=12, bg="white") # width = 13, height = 8, 

