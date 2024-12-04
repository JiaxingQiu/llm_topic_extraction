setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")

fea_df <- read.csv("./data/fea_df.csv")
rownames(fea_df) <- fea_df$fea
fea_df <- fea_df[setdiff(fea_df$fea, c("nosocialeat", "thinspo", "leanbody", "fearcarb","meal")),]
fea_df <- fea_df[sort(fea_df$fea),]
rownames(fea_df) <- NULL
fea_df <- bind_rows(fea_df,data.frame("fea" = "idealbody",
                                      "feature" = "Ideal body image",
                                      "description" = "Ideal body image including thinness, skinny body, low body fat and lean body mass"))
fea_df$description <- sub("\\.$", "", fea_df$description)
fea_df$description <- paste0(fea_df$description, ".")

fea_df[,c("feature", "fea", "description")] %>% 
  kable(caption="Case study 2 -- Topic Descriptions",
        format = "latex", 
        col.names = c("Topic", "Short Name", "Description"),
        booktabs = T,
        align="l") %>% 
  column_spec(column = 3, width = "3.5in")%>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro") 

