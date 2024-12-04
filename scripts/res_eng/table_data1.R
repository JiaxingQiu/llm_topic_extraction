setwd("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/LLMTopicExtraction/llm_topic_extraction")
rm(list=ls())
info_df <- read.csv("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/script/llm/sm_eos.csv", stringsAsFactors = FALSE) 
colnames(info_df)


library(knitr)
library(kableExtra)
table1 <- info_df %>%
  dplyr::mutate(topic = group) %>%
  dplyr::group_by(topic) %>%
  dplyr::summarize(
    number_of_communities = n_distinct(sr_id),
    number_of_authors = n_distinct(author_id),
    number_of_submissions = n_distinct(sm_id) ) 
library(jsonlite)
json_sr <- fromJSON("/Users/joyqiu/Documents/Documents JoyQiu Work/Research/ED Media/network/data/searchdict_result.json")
g_keywords <- c()
for(g in names(json_sr)){
  g_keywords <- c(g_keywords,paste0(names(json_sr[[g]]$keyword), collapse = ", "))
}
table1 <-  merge(table1, data.frame(topic = names(json_sr), searched_keywords = g_keywords), all.x=TRUE) 
table1$topic <- factor(table1$topic, levels=c("ed", "diet","fitness","control"))
levels(table1$topic) <- c("ED", "Dietary", "Fitness", "Control")
table1 <- table1 %>% arrange(topic) %>% 
  select(topic,number_of_communities,number_of_authors,number_of_submissions,searched_keywords)
table1$npost_nuser <- paste0(table1$number_of_submissions," (",table1$number_of_authors,")")
table1 <- table1 %>% select(topic,number_of_communities,npost_nuser,searched_keywords)
table1$searched_keywords[which(table1$topic=="Dietary")] <- paste0("diet, ",table1$searched_keywords[which(table1$topic=="Dietary")])
table1 %>% 
  kable(caption="Data Description of ED and Dieting Reddit Posts",
        format = "latex", 
        col.names = c("", "Forums", "Posts (Authors)", "Search by"),
        booktabs = T,
        align="l") %>% 
  column_spec(column = 4, width = "2.5in")%>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro") 


info_df %>%
  dplyr::summarize(
    number_of_communities = n_distinct(sr_id),
    number_of_authors = n_distinct(author_id),
    number_of_submissions = n_distinct(sm_id) ) 
