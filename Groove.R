#packages and functions and loading data
pacman::p_load(tidyverse,rvest,tm,tidytext,ggplot2,quanteda,stm,dplyr,readability,mice)

stop <- rbind(tibble(text = stopwords(source = "smart")),"ill","im","yeah","dont","hey","back","lets")

nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(-score, -lexicon)%>%
  filter(sentiment %in% c("joy","fear","disgust","anger","sadness"))


webpage <- read_html('http://transcripts.wikia.com/wiki/The_Emperor%27s_New_Groove')
web_text <- html_nodes(webpage,'#mw-content-text') %>%
             html_text() %>%
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]

webdat <- data.frame(web_text) %>% 
  mutate(web_text = trimws(str_replace(web_text, "\\[.*?\\]", ""))) %>%
  filter(web_text != "" & web_text != "Transcript Edit") %>% 
  separate(web_text,into = c("speaker","text"), sep=":")

webdat$text<- gsub("\\[[^\\]]*\\]", "", webdat$text, perl=TRUE);
webdat$text<- gsub("\\([^\\]]*\\)", "", webdat$text, perl=TRUE);
webdat$text <- gsub('"', '', webdat$text)   



text_cleanish<-webdat%>%
  filter(text != " ") %>%
  mutate(linenumber = row_number())
