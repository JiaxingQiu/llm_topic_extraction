
# ------------- calibration plot -------------
cali_plot <- function(y_prob, y_true, plot=F){

  library(ggplot2)
  df <- data.frame(y_true, y_prob)
  
  breaks <- seq(0, 1, by = 0.01)
  obs_rate <- c()
  for(b in breaks){
    obs_rate_b <- 0
    if(sum(df$y_prob<=b)>0) obs_rate_b <- mean(df$y_true[which(df$y_prob<=b)],na.rm=T)
    obs_rate <- c(obs_rate, obs_rate_b)
  }
  df_cali <- data.frame(y_cali_predicted = breaks,
                        y_cali_observed = obs_rate/max(obs_rate,na.rm=T))
  p_cali <- ggplot(df_cali, aes(x=y_cali_predicted, y=y_cali_observed)) +
    geom_point() +
    geom_line(color="grey") +
    geom_abline(intercept=0, slope = 1, size=0.3, linetype="dotted") +
    ylim(0,1) + #max(obs_rate)
    xlim(0,1)
  if(plot){
    print(p_cali)
  }
  
  
  return(list(df_cali = df_cali,
              p_cali = p_cali))
}


