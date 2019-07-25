# 檔案匯入
getwd()
setwd("/Users/zhaowen/Desktop/Data-Science/Week3_期末專題")

library(readr)

r_t <- read.csv("/Users/zhaowen/Desktop/Data-Science/Week3_期末專題/降雨量-颱風.csv")
hurt <- read.csv("/Users/zhaowen/Desktop/Data-Science/Week3_期末專題/颱風傷亡.csv")

library(readxl)

sp <- read_excel("/Users/zhaowen/Desktop/Data-Science/Week3_期末專題/2000.xlsx")

sp1 <- as.data.frame(sp)


# 總計df
library(dplyr)

levels(hurt$name)[63] <- "山竹"

#改col名稱
colnames(r_t)[2]="name"
colnames(r_t)[4]="strength"

levels(r_t$strength)[1] = "strong"
levels(r_t$strength)[2] = "light"
levels(r_t$strength)[3] = "medium"


rt.hurt <- merge(r_t, hurt, by="name")
rh <- arrange(rt.hurt, X)
rh1 <- rh[-c(3, 5, 9, 11, 12, 16, 19, 22, 26, 34,35, 40, 41, 46, 47, 49, 53, 56, 57, 59, 61, 66, 68, 72, 75, 83, 87, 92, 93, 97, 99, 102, 103, 108,111, 116, 121, 124, 129, 133, 135, 137),]

rh2 <- rh1[-c(1,2),]

colnames(rh2)[5]="wind"

rh2$cas.total <- as.numeric(rh2$cas.total)

##整理路徑
colnames(sp1)[1]="name"
colnames(sp1)[2]="path"
colnames(sp1)[3]="date"

sp2 <- select(sp1, "name", "path", "date")

sp2$path <- as.factor(sp2$path)

levels(sp2$path)[12]="special"

rt.hurt1 <- merge(sp2, rh2, by="name")
rth <- arrange(rt.hurt1, X)

rth1 <- rth[-c(2, 3, 7, 8, 10, 11, 14, 17, 21, 23, 26, 34, 35, 40, 45, 47, 48, 51, 55, 56, 58, 61, 65, 66, 70, 74, 82, 86, 90, 91, 94, 97, 100, 102, 103, 109, 111, 113, 118, 124, 129, 133, 135, 137),]


rth2 <- select(rth1, name, path, strength, wind, tart_time:level_1, cas.total:"equi.其他" )

# 畫圖
library(ggplot2)

## 一變量
### 雨量
ggplot(data = rth2) + 
  geom_density(aes(x = average), fill = "#003366") 

### 風速
ggplot(data = rth2) + 
  geom_density(aes(x = wind), fill = "#003366") +
  geom_vline(aes(xintercept=33), colour="#990000", linetype="dashed") #風速33m/s

## 二變量
#### 雨量-風速
ggplot(rth2, aes(x = average, y = wind)) + #scattperplot
  geom_point() + 
  stat_smooth(method=lm, level=0.99) + #回歸直線
  geom_rug()+ #邊際地毯
  geom_hline(aes(yintercept=33), colour="#990000", linetype="dashed") #風速33m/s

### 路徑
#### 路徑-風速
ggplot(rth2, aes(x=path, y=wind))+ #boxplot
  geom_boxplot(aes(group=path)) +
  geom_hline(aes(yintercept=33), colour="#990000", linetype="dashed") #風速33m/s

#### 路徑-雨量
ggplot(rth2, aes(x=path, y=average))+ geom_boxplot(aes(group=path)) #boxplot

#### 路徑-傷亡
ggplot(rth2, aes(x=path, y=cas.total))+ geom_boxplot(aes(group=path)) #boxplot

## 三變量
### 雨量平均-風速-傷亡
qplot(average, wind, data = rth2, color = strength, size = cas.total, alpha = I(0.7))+ #scattperplot
  geom_hline(aes(yintercept=33), colour="#990000", linetype="dashed")+ #風速33m/s
  geom_vline(aes(xintercept=1000), colour="#990000", linetype="dashed")

### 路徑-風速-傷亡
qplot(path, wind, data = rth2, color = strength, size = cas.total, alpha = I(0.7)) + #scattperplot
  geom_hline(aes(yintercept=33), colour="#990000", linetype="dashed") #風速33m/s

### 高於平均雨量-風速-傷亡
qplot(better_than_average, wind, data = rth2, color = strength, size = cas.total, alpha = I(0.7))#scattperplot

### 警戒值level1-風速-傷亡
qplot(level_1, wind, data = rth2, color = strength, size = cas.total, alpha = I(0.7))#scattperplot

### 警戒值level2-風速-傷亡
qplot(level_2, wind, data = rh2, color = strength, size = cas.total, alpha = I(0.7))#scattperplot
