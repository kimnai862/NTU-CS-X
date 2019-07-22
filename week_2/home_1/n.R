```{r}
library(tidyverse)
library(tidytext)
library(rvest)
library(lubridate)
library(topicmodels)
library(jiebaR)
library(extrafont)
library(grDevices)
rm(list = ls())
# loadfonts(device="win")
windowsFonts(BL = windowsFont("微軟正黑體"))
options(stringsAsFactors = F)
#Sys.setlocale(locale = "UTF-8")
assign("last.warning", NULL, envir = baseenv())
```

```{r}
part1 = "https://www.ptt.cc/bbs/KoreaDrama/index"
part2 = ".html"
# to crawl the articles needed
abc <- paste0(part1, 1950:2025 , part2) %>%
  map(read_html) 
# to get all titles
abc_title <- abc %>%
  map(html_nodes,css= ".title a") %>%
  map(html_text)
# to check which article has the needed title
check <- str_detect(unlist(abc_title), pattern = "\\LIVE] tvN 和遊記")
sum(check)
```

```{R}
abc_link <- abc %>%
  lapply(html_nodes , css= ".title a") %>%
  lapply(html_attr, "href") %>%
  unlist()
urls <- abc_link[check]
article_links <- paste0("https://www.ptt.cc", urls) 
```

```{R}
ptt_article_crawler <- function(url) {
  #main content
  main_author = ".article-metaline:nth-child(1) .article-meta-value"
  main_title = ".article-metaline-right+ .article-metaline .article-meta-value"
  main_datetime = ".article-metaline+ .article-metaline .article-meta-value"
  main_content = "#main-content"
  main_link = ".f2 a"
  #push content
  push_tag = ".push-tag"
  push_id = ".push-userid"
  push_text = ".push-content"
  push_datetime = ".push-ipdatetime"
  # css argument 
  main_column = c(main_author, main_title, main_datetime, main_content, main_link)
  push_column = c(push_tag, push_id, push_text, push_datetime)
  # main and push as two empty lists
  main = list()
  push = list()
  raw_html <- url %>% read_html()
  # for loop
  for(i in 1:length(main_column)){
    content <- raw_html %>% html_nodes(css = main_column[i]) %>% html_text()
    main[[i]] <- content
  }
  main <- as.data.frame(matrix(unlist(main), nrow = 1))
  colnames(main) <- c("main_author", "main_title", "main_datetime", "main_content", "main_link")
  
  for(i in 1:length(push_column)){
    content <- raw_html %>% html_nodes(css = push_column[i]) %>% html_text()
    push[[i]] <- content
  }
  push <- as.data.frame(push)
  colnames(push) <-  c("push_tag", "push_id", "push_text", "push_datetime")
  # clean
  main$main_content <- main$main_content %>%
    str_replace_all("\n", replacement = "") %>% # 清理斷行符號
    str_replace_all("--※.+", replacement = "") %>% # 去尾
    str_replace_all("作者.+:[0-9]{2}\\s[0-9]{4}", replacement = "") %>% #sentfromy
    str_replace_all("-----Sent from.+", replacement = "")
  main$weekday <- main$main_datetime %>% str_sub(1,4)
  main$datetime <- main$main_datetime %>% str_sub(5,-1) %>% parse_datetime("%b %d %H:%M:%S %Y")
  # main$authorip <- main$authorip %>% str_sub(str_locate(., "來")[1] + 4 , -2)
  #######
  push$push_text <- push$push_text%>% str_replace(pattern = ":", replacement = "") %>% str_replace_all(pattern = "\\s", replacement = "")
  #2018/04/0100:42
  push$push_datetime <- toString(year(main$datetime)) %>% str_c(push$push_datetime, sep = "/" ) %>% str_replace_all(" ", "")
  push$date <- push$push_datetime %>% str_sub(1,10) %>% parse_date("%Y/%m/%d", locale = locale(tz = "Asia/Taipei"))
  push$time <- push$push_datetime %>% str_sub(11,-1) %>% parse_time("%H:%M")
  push$weekday <- wday(push$date, label = T)
  push$month <- month(push$date)
  push$day <- day(push$date)
  push$hour <- hour(push$time)
  push$minute <- minute(push$time)
  #######
  article <- list("main" = main, "push" = push)
  return(article)
}
# use the function
article_Korean_Odyssey = list()
for(i in 1:length(article_links)) {
  article_Korean_Odyssey[[i]] = ptt_article_crawler(article_links[i])
}
names(article_Korean_Odyssey)= str_c("article", c(1:22))
```

```{R}
article_Korean_Odyssey_push = tibble()
for(i in 1:22) {
  article_Korean_Odyssey_push <- article_Korean_Odyssey[[i]]$push %>%
    mutate(article = i) %>% rbind(article_Korean_Odyssey_push)
}
```

```{R}
article_descriptive <- select(article_Korean_Odyssey_push, article, date, time, month, day, hour, minute)
article_descriptive <- as_tibble(article_descriptive)
article_descriptive
```

```{R}
library(dplyr)
library(tidyr)
library(ggplot2)
for(i in 1:22) {
  print(article_descriptive %>%
          filter(article == i) %>%
          filter(date == date[1]) %>%
          mutate(mingroup = factor(floor(minute/10)), hourgroup = factor(hour, ordered = T)) %>%
          select(-date, -time) %>%
          group_by(month, day, hourgroup, mingroup) %>%
          count() %>%
          ungroup %>%
          mutate(minute = as.numeric(mingroup)*10, hour = hourgroup) %>%
          unite("HM", c("month", "day", "hour", "minute"), sep = ":") %>%
          ggplot() +
          geom_col(aes(HM, n, fill = hourgroup), show.legend = FALSE) +  geom_line(aes(HM, n, group = 1), alpha = 1, col = "blue") +
          labs(x = "時間", y = "出現次數", title = paste0("episode", i)) +
          theme(plot.title = element_text(family = "BL", color = "black"), axis.title.y = element_text(angle = 90, family = "BL", color = "black"),
                axis.text.x = element_text(angle = 60, family = "BL", hjust = 1, color = "black"),
                axis.title.x = element_text(family = "BL", color = "black", face = "bold"))
  )
}
```