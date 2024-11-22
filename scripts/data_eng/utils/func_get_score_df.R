get_score_df <- function(folder, mdl_name = "all-mpnet-base-v2",
                         rm_feas = c("fearcarb")){ # c("feargain", "fearcarb", "feargain")
  orgwd <- getwd()
  setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
  library(dplyr)
  
  fea_df <- read.csv(paste0("./data/fea_df_with_cos",mdl_name,".csv"))
  labeled_df <- read.csv(paste0("./",folder,"/results_with_cos_",mdl_name,".csv"))
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), "X")]
  labeled_df <- labeled_df %>% arrange(sm_id)
  scores_df <- readRDS(paste0("./",folder,"/cos_score_",mdl_name,".RDS"))
  scores_df <- scores_df %>% arrange(sm_id)
  
  # cos_v <- as.numeric(unlist(scores_df[,setdiff(colnames(scores_df),"sm_id")]))
  # cos_v <- cos_v[cos_v>-1]
  # score_cut = median(cos_v)
  # par(mfrow=c(4,5))
  for(topic in fea_df$fea){
    # hist(scores_df[[topic]])
    # shift a cutoff at 0, then zero out <0
    # cutoff <- quantile(scores_df[[topic]][which(scores_df[[topic]]>0)],0)
    cutoff <- fea_df$baseline_cosine[which(fea_df$fea==topic)]
    a0 <- which(scores_df[[topic]]>cutoff)
    be0 <- which(scores_df[[topic]]<=cutoff)
    print(min(scores_df[[topic]][a0],na.rm=T))
    scores_df[[topic]][a0] <- scores_df[[topic]][a0] - cutoff#(scores_df[[topic]][a0] - min(scores_df[[topic]][a0],na.rm=T) ) / (max(scores_df[[topic]][a0],na.rm=T) - min(scores_df[[topic]][a0],na.rm=T))
    scores_df[[topic]][be0] <- 0
    scores_df[[topic]] <- scores_df[[topic]]*labeled_df[[topic]]
    # hist(scores_df[[topic]])
  }
  # par(mfrow=c(1,1))
  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  info_df <- text_df %>% select(sm_id, group, sr_name, url)
  labeled_df <- merge(labeled_df[,c("sm_id",setdiff(colnames(labeled_df), colnames(info_df)))], info_df) %>% arrange(sm_id)
  scores_df <- merge(scores_df[,c("sm_id",setdiff(colnames(scores_df), colnames(info_df)))], info_df) %>% arrange(sm_id)
  
  labeled_df[is.na(labeled_df)] <- 0
  scores_df[is.na(scores_df)] <- 0
  
  # ---- special engineering ---- 
  # lump thinspo and leanbody
  labeled_df$thinspo <- ifelse(labeled_df$thinspo + labeled_df$leanbody > 0, 1, 0)
  scores_df$thinspo <- ifelse(scores_df$thinspo >= scores_df$leanbody, scores_df$thinspo, scores_df$leanbody)
  # lump fearfood, fearcarb
  labeled_df$fearfood <- ifelse(labeled_df$fearfood + labeled_df$fearcarb > 0, 1, 0)
  scores_df$fearfood <- ifelse(scores_df$fearfood >= scores_df$fearcarb, scores_df$fearfood, scores_df$fearcarb)
  
  # remove feargain, fearcarb, fearfood (provided in rm_feas)
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), rm_feas)]
  scores_df <- scores_df[,setdiff(colnames(scores_df), rm_feas)]
  
  
  setwd(orgwd)
  return(list(labeled_df = labeled_df, 
              scores_df = scores_df ))
}

