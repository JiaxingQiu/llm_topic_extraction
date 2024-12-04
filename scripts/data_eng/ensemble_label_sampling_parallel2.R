setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis2.R")
label_true <- read.csv("./data2/answer_df_raw.csv")
bal <- F
rm_ls <- readRDS("./res/2/outliers.RDS")
table(label_true$stigma) # 0.75

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
vec <- c(1,2,3,4)#4
combinations_at_least_two <- lapply(2:length(vec), function(k) combn(vec, k, simplify = FALSE))
combinations_at_least_two <- unlist(combinations_at_least_two, recursive = FALSE)


true_set <- ifelse(nrow(label_true)<2000, "human", "gpt4o")
agree_ratio_inter <- 0.5 # at least half
outlier_cut <- 0.1 # round to 1 decimal
# agree_ratio_inter <- 0.35 # at least 2
res_filename <- paste0("./res/2/ensemble_label_comb_",agree_ratio_inter,"_",true_set,"_",outlier_cut,".RData") 
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

# ---- plot data ----
rm_topics <- c()
eval_df_pca_final <- do.call(rbind, eval_pca)
eval_df_agree_adj <- do.call(rbind, eval_agree_adj)
eval_df <- rbind(eval_df_agree_adj[,intersect(colnames(eval_df_agree_adj), colnames(eval_llama))],
                 eval_df_pca_final[,intersect(colnames(eval_df_pca_final), colnames(eval_llama))],
                 eval_llama, eval_llama_s,
                 eval_qwen, eval_qwen_s,
                 eval_vicuna7b, eval_vicuna7b_s,
                 eval_mistral, eval_mistral_s)
eval_df  <- eval_df[which(!eval_df$topic%in%rm_topics),]
eval_df$topic[which(eval_df$topic=="stigma")] <- "weightstigma"
eval_df_pca_delta <- do.call(rbind, eval_pca_delta)
eval_df_agree_adj_delta <- do.call(rbind, eval_agree_adj_delta)
eval_df_pca_delta  <- eval_df_pca_delta[which(!eval_df_pca_delta$topic%in%rm_topics),]
eval_df_agree_adj_delta  <- eval_df_agree_adj_delta[which(!eval_df_agree_adj_delta$topic%in%rm_topics),]
eval_df_agree_adj_delta$topic[which(eval_df_agree_adj_delta$topic=="stigma")] <- "weightstigma"
eval_df_pca_delta$topic[which(eval_df_pca_delta$topic=="stigma")] <- "weightstigma"


# ---- plot eval ----
p0 <- plot_evaluation(eval_df, "auprc")+ ylab("AUPRC\n(by score)") + coord_cartesian(ylim = c(0.5, 1)) 
p1 <- plot_evaluation(eval_df, "f1_score") + ylab("F1-score\n") + coord_cartesian(ylim = c(0.5, 1)) 
p2 <- plot_evaluation(eval_df, "precision") + ylab("Precision\n") + coord_cartesian(ylim = c(0.5, 1)) 
p3 <- plot_evaluation(eval_df, "specificity") + ylab("Specificity\n") 
p4 <- plot_evaluation(eval_df, "sensitivity") + ylab("Sensitivity\n")  + coord_cartesian(ylim = c(0.5, 1)) 
p_eval_ls <- list(p0,p1,p2,p3,p4)
p_eval <- ggarrange(p0,p1,p2,p3,p4, ncol=2, nrow=3, common.legend = T, legend = "right")
p_eval <- annotate_figure(
  p_eval,
  top = text_grob("A. Predicting Human Annotation", 
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
p_improve <- ggarrange(p1, p2, p3, p4, p5, ncol=2, nrow=3, common.legend = T, legend = "right")
# add text "Performance Increase" on the top of p_improve
p_improve <- annotate_figure(
  p_improve,
  top = text_grob("B. Performance Increase Relative to Individual LLMs", 
                 size = 10, face = "bold", 
                 hjust=0, x=0)
)
# save(p_eval,p_eval_ls, p_improve, p_improve_ls, file = paste0("./res/eval_plots_",true_set,"_B.RDS"))
# ggarrange(p_eval, p_improve, nrow=1) %>% ggsave(filename = paste0("./res/2/",true_set,"_evaluation.png"), width = 7, height = 7, bg="white")
p_final <- ggarrange(p_eval, p_improve, nrow=1) 
p_final <- annotate_figure(
  p_final,
  top = text_grob("Data 2: ED Patients Experiences", 
                  size = 12, face = "bold", 
                  hjust=0, x=0)
)
p_final %>% ggsave(filename = paste0("./res/",true_set,"_evaluation2.png"), width = 10, height = 5, bg="white")

