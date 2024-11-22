setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

library(dplyr)
library(parallel)

path = paste0("./scripts/data_eng/utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

# prepare label_df, score_df
fea_df <- read.csv("./data/fea_df.csv")
# folder <- "./gpt_data" # indicate llm models
# topic <- "bodyhate" # indicate topic

get_label_df_adj <- function(folder){
  
  obj <- get_score_df(folder, "stella_en_1.5B_v5")
  obj[['label_df_adj']] <- as.data.frame(obj$labeled_df)
  if(folder == "./gpt4o_data" ){
    return(obj)
  }
  for(topic in fea_df$fea){
    
    print(table(obj$label_df_adj[[topic]]))
    cutoff <- quantile(obj$scores_df[[topic]][which(obj$scores_df[[topic]]>0)],0.025)
    obj$label_df_adj[which(obj$label_df[[topic]]==1 & obj$scores_df[[topic]]<cutoff), topic] <- 0
    print(table(obj$label_df_adj[[topic]]))
    
  }
  return(obj)
}

obj_gpt4omini <- get_label_df_adj("./gpt_data")
obj_llama <- get_label_df_adj("./llama_data")
obj_gpt4o <- get_label_df_adj("./gpt4o_data")

text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
info_df <- text_df %>% select(sm_id, group, sr_name, url)
labeled_df_gpt4o <- merge(obj_gpt4o$label_df_adj, info_df, by = "sm_id", all.x = TRUE)
labeled_df_gpt4omini <- merge(obj_gpt4omini$label_df_adj, info_df, by = "sm_id", all.x = TRUE)
labeled_df_llama8b <- merge(obj_llama$label_df_adj, info_df, by = "sm_id", all.x = TRUE)
rm(text_df)

# sanity check
plot_distribution(labeled_df_gpt4omini,
                  labeled_df_llama8b,
                  labeled_df_gpt4o)



