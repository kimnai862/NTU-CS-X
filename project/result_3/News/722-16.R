library(rvest)
library(tm)
library(jiebaR)
library(jiebaRD)
library(Matrix)

library(wordcloud2)
library(text2vec)
library(stringr)

# ----------------------------------------------------------------------------------------抓網址
data <- c()
data1 <- c()
links <- character()
front <- "https://news.ltn.com.tw/search/?keyword=%E9%A2%B1%E9%A2%A8%20%E5%BD%B1%E9%9F%BF%20%E5%9C%B0%E5%8D%80&conditions=and&SYear=2016&SMonth=7&SDay=1&EYear=2016&EMonth=9&EDay=30&page="

for (i in 1:45) { #------------------------------------------------------------------------改頁數
  pages <- paste0(front, i)
  print(pages)
  
  web <- read_html(pages) %>%
    html_nodes("a.tit") %>%     
    html_attr("href")
  web <- paste0("https://news.ltn.com.tw/", web)
  links <- c(links, web)
}

# -------------------------------------------------------------------------------------------抓資料
for (i in 1:length(links)) {
  data1 <- read_html(links[i])%>%
    html_nodes(".text p+ p , .viewtime+ p , .boxTitle+ p")%>%
    html_text()
  data <-paste0(data, data1)
}

# ----------------------------------------------------------------------------------------開始清洗
data <- as.list(data)    ##轉成list
d.corpus <- Corpus(VectorSource(data)) %>% # Corpus(VectorSource())的input是list
  tm_map(removePunctuation) %>%  ## 標點符號
  tm_map(removeNumbers) %>%     ##數字
  tm_map(function(word) { # Regular Expression 把英文&數字的內容拿掉
    gsub("[A-Za-z0-9]", "", word)    ##把 function(word) 裡面的"[A-Za-z0-9]" 換成 ""
  })

for (i in 2:143)#------------------------------------------------------------------------看d.corpus的elements有幾個
{
  d.corpus <- paste0(d.corpus[1], d.corpus[i])
}

# ----------------------------------------------------------------------------------------開始斷詞
cutter <- worker()
cutter <- worker(stop_word = "C:/Users/jeff6/Desktop/Github/alan/颱風專題/722/a.txt")
new_user_word(cutter, c("西半部","東半部","中南部","南部","北部","東部","西部","東北部","基隆",
                        "台北","新北","桃園","新竹","苗栗","台中","澎湖","金門","連江","馬祖",
                        "彰化","南投","雲林","嘉義","台南","高雄","屏東","台東","花蓮","宜蘭",
                        "外圍環流",
                        "丹娜絲","山竹","瑪莉亞","泰利","谷超","天鴿","珊瑚","馬莎",
                        "海棠","尼莎","艾利","梅姬","馬勒卡","莫蘭蒂","尼伯特","杜鵑","天鵝",
                        "蘇迪勒","蓮花","昌鴻","紅霞","鳳凰","麥德姆","哈吉貝","菲特","天兔",
                        "康芮","潭美","西馬隆","蘇力","杰拉華","天秤","啟德","海葵","蘇拉",
                        "杜蘇芮","南瑪都","梅花","米雷","桑達","凡那比","南修","萊羅克",
                        "芭瑪","莫拉克","莫拉菲","薔蜜","哈格比","辛樂克","如麗","鳳凰",
                        "卡玫基","米塔","柯羅莎","韋帕","聖帕","梧提","帕布","珊珊","寶發",
                        "桑美","凱米","碧利斯","艾維尼","珍珠","龍王","丹瑞","卡努"))
cloud <- cutter[d.corpus]

#---------------------------------------------------------------------------------------------看關聯性
tokens=list(cloud)
class(tokens)

# 建構詞彙
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5L)#刪除出現小於5次的詞
tail(vocab,20)#查看最高频的詞
vectorizer = vocab_vectorizer(vocab)
tcm = create_tcm(it, vectorizer,skip_grams_window = 5L)# 考慮詞的前後5個詞
glove = GlobalVectors$new(word_vectors_size = 40, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
dim(wv_main)
wv_context = glove$components
dim(wv_context)
wv_main[1,1]
t(wv_context)[1,1]
word_vectors = wv_main + t(wv_context)

#建造“影響+地區”向量
relation = word_vectors["地區", , drop = FALSE] +
  word_vectors["影響", , drop = FALSE]

#看和 “影響+地區” 相關的10個詞
cos_sim = sim2(x = word_vectors, y = relation, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
# ----------------------------------------------------------------------------------------文字雲
z <- sort(table(cloud),decreasing = T)[1:50]
wordcloud2(z,
           size = 0.5,
           color = "random-light",
           backgroundColor = "black",
           shape = "triangle")

