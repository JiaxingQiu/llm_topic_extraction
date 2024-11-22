# get the over-"agree_ratio"-agreed "yes" from the labels from multple llms 
get_label_agreed <- function(llm_ls = c("gpt_data", "llama_data"),
                             agree_ratio = 0.5){
  
  label_df_ls <- list()
  for(llm in llm_ls){
    obj <- get_score_df(llm, "stella_en_1.5B_v5")
    label_df_ls[[llm]] <- obj$labeled_df[,c("sm_id",fea_df$fea)] %>% arrange(sm_id)
  }
  
  # return 
  label_df_sum <- Reduce(function(df1, df2) {
    intersected_df <- df1[, fea_df$fea, drop = FALSE] + df2[, fea_df$fea, drop = FALSE]
    as.data.frame(intersected_df)
  }, label_df_ls)
  # sum(label_df_sum$ed == length(label_df_ls),na.rm=T)
  label_df_sum <- label_df_sum / length(label_df_ls)
  label_df <- as.data.frame(ifelse(label_df_sum>agree_ratio,1,0))
  # sum(label_df$ed == 1,na.rm=T)
  # sum(label_df_ls$gpt_data$ed==1 & label_df_ls$llama_data$ed==1,na.rm = T)
  label_df$sm_id <- label_df_ls[[1]]$sm_id
  label_df_ls[["agreed"]] <- label_df
  
  return(label_df_ls)
}

get_label_agreed_by_dfs <- function(label_df_ls,
                             agree_ratio = 0.5){
  
  # return 
  label_df_sum <- Reduce(function(df1, df2) {
    intersected_df <- df1[, fea_df$fea, drop = FALSE] + df2[, fea_df$fea, drop = FALSE]
    as.data.frame(intersected_df)
  }, label_df_ls)
  # sum(label_df_sum$ed == length(label_df_ls),na.rm=T)
  label_df_sum <- label_df_sum / length(label_df_ls)
  label_df <- as.data.frame(ifelse(label_df_sum>agree_ratio,1,0))
  # sum(label_df$ed == 1,na.rm=T)
  # sum(label_df_ls$gpt_data$ed==1 & label_df_ls$llama_data$ed==1,na.rm = T)
  label_df$sm_id <- label_df_ls[[1]]$sm_id
  label_df_ls[["agreed"]] <- label_df
  
  return(label_df_ls)
}
