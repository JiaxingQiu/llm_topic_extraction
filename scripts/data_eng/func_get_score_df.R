get_score_df <- function(folder, mdl_name = "stella_en_400M_v5"){
  labeled_df <- read.csv(paste0("./",folder,"/results_with_cos_",mdl_name,".csv"))
  labeled_df <- labeled_df[,setdiff(colnames(labeled_df), "X")]
  labeled_df <- labeled_df %>% arrange(sm_id)
  scores_df <- readRDS(paste0("./",folder,"/cos_score_",mdl_name,".RDS"))
  scores_df <- scores_df %>% arrange(sm_id)
  
  # cos_v <- as.numeric(unlist(scores_df[,setdiff(colnames(scores_df),"sm_id")]))
  # cos_v <- cos_v[cos_v>-1]
  # score_cut = median(cos_v)
  
  par(mfrow=c(4,5))
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
  par(mfrow=c(1,1))
  
  text_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE)
  info_df <- text_df %>% select(sm_id, group, sr_name, url)
  scores_df <- merge(scores_df, info_df)%>% arrange(sm_id)
  
  return(list(labeled_df = labeled_df, 
              scores_df = scores_df ))
}

