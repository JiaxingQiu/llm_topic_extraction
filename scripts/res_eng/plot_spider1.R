setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

source("./scripts/data_eng/prepare_analysis.R")
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)  %>% select(sm_id, group, sr_name, url)
info_df$group <- ifelse(tolower(info_df$group) == "ed", "ED", "Dieting")
label_true <- read.csv("./human_data/results.csv")
label_true <- merge(label_true, info_df, by="sm_id",all.x=T)

if(!"group" %in% colnames(label_df_gpt4o)){
  label_df_gpt <- merge(label_df_gpt4o, info_df, by="sm_id",all.x=T)
  score_df_gpt <- merge(score_df_gpt4o, info_df, by="sm_id",all.x=T)
}else{
  label_gpt <- label_df_gpt4o
  score_gpt <- score_df_gpt4o
  
  label_llama <- label_df_llama
  score_llama <- score_df_llama
  
  label_qwen <- label_df_qwen
  score_qwen <- score_df_qwen
  
  label_vicuna <- label_df_vicuna7b
  score_vicuna <- score_df_vicuna7b
  
  label_mistral <- label_df_mistral
  score_mistral <- score_df_mistral
  
  label_gpt$group <- ifelse(tolower(label_gpt$group) == "ed", "ED", "Dieting")
  score_gpt$group <- ifelse(tolower(score_gpt$group) == "ed", "ED", "Dieting")
  label_llama$group <- ifelse(tolower(label_llama$group) == "ed", "ED", "Dieting")
  score_llama$group <- ifelse(tolower(score_llama$group) == "ed", "ED", "Dieting")
  label_qwen$group <- ifelse(tolower(label_qwen$group) == "ed", "ED", "Dieting")
  score_qwen$group <- ifelse(tolower(score_qwen$group) == "ed", "ED", "Dieting")
  label_vicuna$group <- ifelse(tolower(label_vicuna$group) == "ed", "ED", "Dieting")
  score_vicuna$group <- ifelse(tolower(score_vicuna$group) == "ed", "ED", "Dieting")
  label_mistral$group <- ifelse(tolower(label_mistral$group) == "ed", "ED", "Dieting")
  score_mistral$group <- ifelse(tolower(score_mistral$group) == "ed", "ED", "Dieting")
}
load("./res/1/ensemble_dfs.RData")
label_ens <- merge(ens_obj$label, info_df, by="sm_id",all.x=T)
score_ens <- merge(ens_obj$score, info_df, by="sm_id",all.x=T)



# reorder fea in fea_df
rownames(fea_df) <- fea_df$fea
df_tmp <- label_df_gpt4o %>%
  pivot_longer(cols = all_of(fea_df$fea), names_to = "feature", values_to = "value")  %>%
  filter(group%in%c("ed","ED")) %>%
  group_by(feature) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(mean_value))
fea_df <- fea_df[df_tmp$feature,]
rownames(fea_df) <- NULL



df_ls <- list("Human label" = label_true,
              "Ensemble of labels" = label_ens,
     "Ensemble of scores" = score_ens,
     "Label by GPT-4o" = label_gpt,
     "Score by GPT-4o" = score_gpt,
     "Label by Llama-3.1-8B-Instruct" = label_llama,
     "Score by Llama-3.1-8B-Instruct" = score_llama,
     "Label by Qwen2.5-7B-Instruct" = label_qwen,
     "Score by Qwen2.5-7B-Instruct" = score_qwen,
     "Label by Mistral-7B-Instruct-v0.3" = label_mistral,
     "Score by Mistral-7B-Instruct-v0.3" = score_mistral,
     "Label by Vicuna-7b-v1.5" = label_vicuna,
     "Score by Vicuna-7b-v1.5" = score_vicuna
     )

pl <- list()
for(dn in names(df_ls)){
  pl[[dn]] <- spider_plot(df_ls[[dn]], dn)
}
p1 <- ggarrange(plotlist = pl[c(4,5,1)], heights = c(1.1,1,1.1), nrow=3, ncol=1, common.legend = T, legend = "left")
p2 <- ggarrange(plotlist = pl[c(2,3)], heights = c(1,1,1), nrow=3, ncol=1, common.legend = T, legend = "none")
p2 <- annotate_figure(p2, top = text_grob("Ensemble of Open LLMs", face = "bold", size = 12.5, hjust=0.4, color="red")) # add bold text "Ensemble" to the top, center it 
p3 <- ggarrange(plotlist = pl[c(6,7)], heights = c(1,1,1), nrow=3, ncol=1, common.legend = T, legend = "none")
p3 <- annotate_figure(p3, top = text_grob("Llama-3.1-8B-Instruct", face = "bold", size = 12.5, hjust=0.4))  # add bold text "Llama-3.1-8B-Instruct" to the top
p4 <- ggarrange(plotlist = pl[c(8,9)], heights = c(1,1,1), nrow=3, ncol=1, common.legend = T, legend = "none")
p4 <- annotate_figure(p4, top = text_grob("Qwen2.5-7B-Instruct", face = "bold", size = 12.5, hjust=0.4))  # add bold text "Qwen2.5-7B-Instruct" to the top
p5 <- ggarrange(plotlist = pl[c(10,11)], heights = c(1,1,1), nrow=3, ncol=1, common.legend = T, legend = "none")
p5 <- annotate_figure(p5, top = text_grob("Mistral-7B-Instruct-v0.3", face = "bold", size = 12.5, hjust=0.4)) # add bold text "Mistral-7B-Instruct-v0.3" to the top
p6 <- ggarrange(plotlist = pl[c(12,13)], heights = c(1,1,1), nrow=3, ncol=1, common.legend = T, legend = "none")
p6 <- annotate_figure(p6, top = text_grob("Vicuna-7b-v1.5", face = "bold", size = 12.5, hjust=0.4)) # add bold text "Vicuna-7b-v1.5" to the top

ggarrange(p1,p2,p3,p4,p5,p6, widths = c(1.2,1,1,1,1,1), nrow=1) %>% ggsave(filename = paste0("./res/spider1.png"), 
                                             width=18, height=8, units = "in", bg="white") # width = 13, height = 8, 

