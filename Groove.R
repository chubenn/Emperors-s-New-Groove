#packages and functions and loading data
pacman::p_load(tidyverse,rvest,tm,tidytext,ggplot2,quanteda,stm,dplyr,readability,mice)

library(textclean)

stop <- rbind(tibble(text = stopwords("smart")),"ill","im","yeah","dont","hey","back","lets")

webpage <- read_html('http://transcripts.wikia.com/wiki/The_Emperor%27s_New_Groove')
web_text <- html_nodes(webpage,'#mw-content-text') %>%
             html_text() %>%
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]

webdat <- tibble(web_text) %>% 
  mutate(web_text = trimws(str_replace(web_text, "\\[.*?\\]", ""))) %>%
  filter(web_text != "" & web_text != "TranscriptEdit") %>% 
  separate(web_text,into = c("speaker","text"), sep=":")

webdat$text <- gsub("\\[[^\\]]*\\]", "", webdat$text, perl=TRUE);
webdat$text <- gsub("\\([^\\]]*\\)", "", webdat$text, perl=TRUE);
webdat$text <- gsub('"', '', webdat$text)   
webdat$text <- textclean::replace_non_ascii(webdat$text)

text_cleanish <- webdat%>%
  filter(text != " ") %>%
  mutate(linenumber = row_number()) %>%
  as.tibble() %>%
  janitor::clean_names() %>%
  mutate(text = removePunctuation(text)) %>%
  anti_join(stop) %>%
  group_by(speaker) %>%
  mutate(speaker_count = n()) %>%
  ungroup() %>%
  filter(speaker %in% c("Kuzco","Pacha","Yzma","Kronk"))
