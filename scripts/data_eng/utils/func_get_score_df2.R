get_score_df2 <- function(llm_name,
                         mdl_name = "all-mpnet-base-v2",
                         rm_feas = c("feargain"),
                         useweightstigma = T){ # c("feargain", "fearcarb", "feargain")
  orgwd <- getwd()
  setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
  library(dplyr)
  
  fea_df <- read.csv(paste0("./data2/fea_df_with_cos",mdl_name,".csv"))
  labeled_df <- read.csv(paste0("./data2/",llm_name,"_results_with_cos_",mdl_name,".csv"))
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), "X")]
  labeled_df <- labeled_df %>% arrange(sm_id)
  scores_df <- readRDS(paste0("./data2/",llm_name,"_cos_score_",mdl_name,".RDS"))
  scores_df <- scores_df %>% arrange(sm_id)
  
  # cos_v <- as.numeric(unlist(scores_df[,setdiff(colnames(scores_df),"sm_id")]))
  # cos_v <- cos_v[cos_v>-1]
  # score_cut = median(cos_v)
  # par(mfrow=c(4,5))
  for(topic in fea_df$fea){
    # correct label for phrases reasoning
    labeled_df[which(labeled_df[,paste0(topic,"_phrases")] == ""),topic] <- 0
    labeled_df[,topic] <- as.numeric(labeled_df[,topic])
    
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
  
  
  labeled_df[is.na(labeled_df)] <- 0
  scores_df[is.na(scores_df)] <- 0
  
  # ---- special engineering ---- 
  if(useweightstigma){
    labeled_df$stigma <- labeled_df$weightstigma
    scores_df$stigma <-scores_df$weightstigma
  }else{
    labeled_df$stigma <- ifelse(rowSums(labeled_df[,fea_df$fea])>0,1,0)
    scores_df$stigma <- apply(scores_df[, fea_df$fea], 1, max, na.rm=T)
  }
  
  # remove (provided in rm_feas)
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), rm_feas)]
  scores_df <- scores_df[,setdiff(colnames(scores_df), rm_feas)]
  
  
  setwd(orgwd)
  return(list(labeled_df = labeled_df, 
              scores_df = scores_df ))
}

