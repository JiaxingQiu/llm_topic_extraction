

adjust_label_by_threshold_score <- function(label_df, 
                                            score_df,
                                            threshold_df){
  label_df_adjust <- label_df
  for(topic in threshold_df$fea){
    # print(topic)
    threshold <- threshold_df$threshold[which(threshold_df$fea==topic)]
    FP <- label_df[[topic]]==1 & score_df[[topic]]<threshold
    label_df_adjust[which(FP),topic]<-0
    # print(sum(FP))
  }
  
  return(label_df_adjust)
}

adjust_score_by_threshold_score <- function(score_df,
                                            threshold_df){
  score_df_adjust <- score_df
  for(topic in threshold_df$fea){
    threshold <- threshold_df$threshold[which(threshold_df$fea==topic)]
    score_df_adjust[which(score_df[[topic]]<threshold),topic]<-0
  }
  return(score_df_adjust)
}



explore_thresholds <- function(label_df, 
                               score_df, 
                               fea_df,
                               true_label_df,
                               plot=T){
  
  threshold_df <- fea_df
  eval_df_all <- data.frame()
  for(t in seq(0,0.5,0.05)){
    threshold_df$threshold <- t
    adjusted_df <- adjust_label_by_threshold_score(label_df, 
                                    score_df,
                                    threshold_df)
    eval_df <- eval_llm(adjusted_df, true_label_df)
    eval_df$t <- t
    eval_df_all <- bind_rows(eval_df_all, eval_df)
  }
  
  if(plot){
    library(ggplot2)
    library(ggpubr)
    pl <- list()
    for(topic in fea_df$fea){
      plot_df <- eval_df_all[which(eval_df_all$topic==topic),]
      plot_df$f1_max = max(plot_df$f1_score, na.rm=T)
      plot_df$x_f1_max = plot_df$t[which(plot_df$f1_score==max(plot_df$f1_score))][1]
      pl[[topic]] <- ggplot(plot_df) +
        geom_point(aes(x = x_f1_max, y = f1_max), color='black') +
        geom_line(aes(x = t, y = f1_score),color='red') +
        geom_line(aes(x = t, y = sensitivity),color='orange') +
        geom_line(aes(x = t, y = precision),color='green3') +
        geom_line(aes(x = t, y = specificity),color='lightblue3') +
        labs(y=NULL, x=topic) + ylim(0.2, 1)
    }
    print(ggpubr::ggarrange(plotlist = pl, ncol=4, nrow = 5))
  }
  
  return(eval_df_all)
}

