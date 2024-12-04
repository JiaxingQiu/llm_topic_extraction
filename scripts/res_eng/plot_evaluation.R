setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(gridtext)

# human A
load(file = paste0("./res/eval_plots_human_A.RDS"))
p_eval_legend <- cowplot::get_legend(p_eval_ls[[1]])
p_eval <- ggarrange(plotlist=p_eval_ls[c(1,2,3,5)], ncol=1, common.legend = T, legend = "none")
p_eval_a <- annotate_figure( p_eval,
                             top = richtext_grob(
                               "<b>Data 1.</b> ED and Dieting Reddit Posts", 
                               gp = gpar(fontsize = 10),
                               hjust=0, x=0)
)
p_improve_legend <- cowplot::get_legend(p_improve_ls[[1]])
p_improve <- ggarrange(plotlist=p_improve_ls[c(1,2,3,5)], ncol=1, common.legend = T, legend = "none")
p_improve_a <- annotate_figure( p_improve,
                                top = richtext_grob(
                                  "<b>Data 1.</b>ED and Dieting Reddit Posts", 
                                  gp = gpar(fontsize = 10),
                                  hjust=0, x=0)
)
# human B
load(file = paste0("./res/eval_plots_human_B.RDS"))
for(i in 1:length(p_eval_ls)){
  p_eval_ls[[i]] <- p_eval_ls[[i]] + labs(y=NULL)
}
p_eval <- ggarrange(plotlist=p_eval_ls[c(1,2,3,5)], ncol=1, common.legend = T, legend = "none")
p_eval_b <- annotate_figure( p_eval,
                             top = richtext_grob(
                               "<b>Data 2.</b> Patient Notes", 
                               gp = gpar(fontsize = 10),
                               hjust = 0, 
                               x = 0
                             )
)
for(i in 1:length(p_improve_ls)){
  p_improve_ls[[i]] <- p_improve_ls[[i]] + labs(y=NULL)
}
p_improve <- ggarrange(plotlist=p_improve_ls[c(1,2,3,5)], ncol=1, common.legend = T, legend = "none")
p_improve_b <- annotate_figure( p_improve,
                                top = richtext_grob(
                                  "<b>Data 2.</b> Patient Notes", 
                                  gp = gpar(fontsize = 10),
                                  hjust=0, x=0)
)
p_eval_combined <- ggarrange(p_eval_a, p_eval_b, ncol=3, widths=c(8,1.2,0.7), legend.grob = p_eval_legend, legend = "bottom")
p_eval_combined <- annotate_figure(p_eval_combined,
                                   top = text_grob("A. Performance Evaluation by Human Annotation", 
                                                   size = 12, face = "bold", 
                                                   hjust=0, x=0))
# p_eval_combined %>% ggsave(filename = paste0("./res/human_performance.png"), width = 10, height = 8, bg="white")

p_improve_combined <- ggarrange(p_improve_a, p_improve_b, ncol=3, widths=c(8,1.2,0.9), legend.grob = p_improve_legend, legend = "bottom")
p_improve_combined <- annotate_figure(p_improve_combined,
                                      top = text_grob("B. Performance Increase of Ensembles Relative to Individual LLMs", 
                                                      size = 12, face = "bold", 
                                                      hjust=0, x=0))
# p_improve_combined %>% ggsave(filename = paste0("./res/human_improvement.png"), width = 10, height = 8, bg="white")

# add some white padding to the right for p_improve_combined and p_eval_combined

# ggarrange(p_eval_combined, p_improve_combined, ncol=1, nrow=2) %>% ggsave(filename = paste0("./res/human_performance.png"), width = 8, height = 12, bg="white")
ggarrange(p_eval_combined, p_improve_combined, widths=c(1.4,1), ncol=2, nrow=1) %>% ggsave(filename = paste0("./res/human_performance.png"), width = 16, height = 8, bg="white")

