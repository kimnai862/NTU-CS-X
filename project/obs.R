library(rvest)
library(tidyverse)
url <- "https://e-service.cwb.gov.tw/wdps/obs/state.htm"
html <- read_html(url)
tag <- character()
pos <- character()
for (i in c(c(3 : 596), c(601 : 867))) {
  tag_node <- html_nodes(html, paste0("tr:nth-child(", i, ") td:nth-child(1) .MsoNormal"))
  pos_node <- html_nodes(html, paste0("tr:nth-child(", i, ") td:nth-child(6) .MsoNormal"))
  tag <- c(tag, html_text(tag_node))
  pos <- c(pos, html_text(pos_node))
}

data <- data.frame(tag = tag, pos = pos)
write.csv(data, "obs.csv")