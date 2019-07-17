library(jiebaR)
library(tidyverse)
library(rvest)
main_html<-read_html("http://www.b111.net/novel/13/13753/index.html")
main_node<-html_nodes(main_html, "td > a")
main_web<-html_attr(main_node, "href")
n = length(main_web)
res=character()
for (i in c(1 : n)) {
  web<-paste0("http://www.b111.net/novel/13/13753/", main_web[i]);
  html<-read_html(web)
  node<-html_nodes(html, "#content")
  content<-html_text(node)
  res<-c(res, content)
}
write.csv(res, "gold.txt")

gold<-readChar("gold.txt", 10000000)
cutter <- worker(user = "pre.txt")
res<-cutter[gold]
write.csv(freq(res) %>% arrange(freq), "res.csv")