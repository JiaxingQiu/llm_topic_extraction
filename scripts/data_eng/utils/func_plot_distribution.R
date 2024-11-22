# 
# label_df_ls = list(labeled_df_gpt4omini,
#                    labeled_df_llama8b,
#                    labeled_df_gpt4o,
#                    labeled_df_qwen)
# llm_name_ls = list("GPT4o-mini",
#                    "Llama8b",
#                    "GPT4o",
#                    "Qwen7b")
plot_distribution <- function(label_df_ls, llm_name_ls, ret=F){
  library(pheatmap)
  library(gridExtra)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(grid)
  
  # fea_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction/data/fea_df.csv")
  
  names(label_df_ls) <- llm_name_ls
  distribution_ls <- list()
  pheat_ls <- list()
  for(llm_name in names(label_df_ls) ){
    distribution_ls[[llm_name]] <- label_df_ls[[llm_name]] %>%
      group_by(group) %>%
      summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
    mat <- as.matrix(distribution_ls[[llm_name]][, -1])
    rownames(mat) <- distribution_ls[[llm_name]]$group
    pheat_ls[[llm_name]] <- pheatmap(mat,  
                                     cluster_rows = FALSE,
                                     cluster_cols = FALSE,
                                     legend_breaks = seq(-1,1,0.1),
                                     fontsize=7.5,
                                     angle_col = "45",
                                     main = llm_name)[[4]]
    distribution_ls[[llm_name]]  <- distribution_ls[[llm_name]] %>% mutate(model = llm_name)
  }
  # convert to a plot object grid.arrange(grobs=pheat_ls, ncol = 1)
  p_heat <- arrangeGrob(grobs = pheat_ls, nrow = 4)
  grid.arrange(p_heat)
  
  
  combined_data <- do.call(bind_rows, distribution_ls)
  long_data <- combined_data  %>% 
    pivot_longer(cols = -c(group, model), names_to = "category", values_to = "occurrence_rate")
  p_hist <- ggplot(long_data, aes(x = group, y = occurrence_rate, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~category, ncol=4, scales = "free_x") + 
    labs(y = "Occurrence Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  print(p_hist)
  
  
  if(ret){
    return(list(p_heat = p_heat,
                p_hist = p_hist))
  }
  
}
 



plot_distribution_old <- function(score_df_gpt4omini, 
                              score_df_llama8b,
                              score_df_gpt4o,
                              label_df_agreed=NULL){
  distribution_gpt4omini <- score_df_gpt4omini %>%
    group_by(group) %>%
    summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  distribution_llama8b <- score_df_llama8b %>%
    group_by(group) %>%
    summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  distribution_gpt4o <- score_df_gpt4o %>%
    group_by(group) %>%
    summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  if(!is.null(label_df_agreed)){
    distribution_agree <- label_df_agreed %>%
      group_by(group) %>%
      summarise(across(all_of(c(fea_df$fea)), ~ mean(.x, na.rm = TRUE)))
  }
  
  library(pheatmap)
  library(gridExtra)
  tmp1 <- distribution_gpt4omini
  tmp2 <- distribution_llama8b
  tmp3 <- distribution_gpt4o
  if(!is.null(label_df_agreed)){
    tmp4 <- distribution_agree
  }
  
  mat1 <- as.matrix(tmp1[, -1])
  rownames(mat1) <- tmp1$group
  p1 <- pheatmap(mat1,  # Remove the group column, if it's the first column
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           legend_breaks = seq(-1,1,0.1),
           main = "Occurrence rates by GPT 4o-mini")
  mat2 <- as.matrix(tmp2[, -1])
  rownames(mat2) <- tmp2$group
  p2 <- pheatmap(mat2,  # Remove the group column, if it's the first column
           cluster_rows = F,
           cluster_cols = F,
           legend_breaks = seq(-1,1,0.1),
           main = "Occurrence rates by Llama 8b-instruct (need a second run)")
  mat3 <- as.matrix(tmp3[, -1])
  rownames(mat3) <- tmp3$group
  p3 <- pheatmap(mat3,  # Remove the group column, if it's the first column
           cluster_rows = F,
           cluster_cols = F,
           legend_breaks = seq(-1,1,0.1),
           main = "Occurrence rates by GPT 4o")
  if(!is.null(label_df_agreed)){
    mat4 <- as.matrix(tmp4[, -1])
    rownames(mat4) <- tmp4$group
    p4 <- pheatmap(mat4,  # Remove the group column, if it's the first column
                   cluster_rows = F,
                   cluster_cols = F,
                   legend_breaks = seq(-1,1,0.1),
                   main = "Agreement by GPT 4o mini, Llama 8b-instruct")
  }
  
  
  # Use grid.arrange to concatenate them by column
  print(grid.arrange(p3[[4]], p1[[4]], p2[[4]], ncol = 1))
  if(!is.null(label_df_agreed)){
    print(grid.arrange(p3[[4]], p4[[4]], p1[[4]], p2[[4]], ncol = 1))
  }
  
  
  # Load necessary libraries
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  tmp1 <- distribution_gpt4omini %>% mutate(model = "GPT-4o-mini")
  tmp2 <- distribution_llama8b %>% mutate(model = "Llama 8b-instruct")
  tmp3 <- distribution_gpt4o %>% mutate(model = "GPT-4o")
  combined_data <- bind_rows(tmp1, tmp2, tmp3)
  long_data <- combined_data  %>%
    pivot_longer(cols = -c(group, model), names_to = "category", values_to = "occurrence_rate")
  print(ggplot(long_data, aes(x = group, y = occurrence_rate, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~category,ncol=4, scales = "free_x") + 
    labs(y = "Occurrence Rate") +
    theme_minimal() +
    scale_fill_manual(values = c("GPT-4o-mini" = "green3", 
                                 "Llama 8b-instruct" = "coral", 
                                 "GPT-4o" = "blue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top"))
  if(!is.null(label_df_agreed)){
    tmp4 <- distribution_agree %>% mutate(model = "Agreed by small LLMs")
    combined_data <- bind_rows(tmp1, tmp2, tmp3, tmp4)
    long_data <- combined_data  %>%
      pivot_longer(cols = -c(group, model), names_to = "category", values_to = "occurrence_rate")
    print(ggplot(long_data, aes(x = group, y = occurrence_rate, fill = model)) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~category,ncol=4, scales = "free_x") + 
            labs(y = "Occurrence Rate") +
            theme_minimal() +
            scale_fill_manual(values = c("GPT-4o-mini" = "green3", 
                                         "Llama 8b-instruct" = "coral", 
                                         "GPT-4o" = "blue",
                                         "Agreed by small LLMs" = "skyblue")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "top"))
  }
  
}
