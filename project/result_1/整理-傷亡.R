setwd("/Users/zhaowen/Desktop/Data-Science/data")
library(readxl)
Data <- read_excel("傷亡.xls")

Data1 <- as.data.frame(Data)

library(dplyr)

colnames(Data1)[2]="year"

Data2 <- filter(Data1, year>=2000)

colnames(Data2)[4]="type"

Data3 <- filter(Data2, type %in% c("Typhoon"))

row <- Data1[195:198,]
colnames(row)[4]="type"

Data4 <- rbind(row, Data3)

colnames(Data4)[5]="name"
colnames(Data4)[6]="month"
colnames(Data4)[7]="day"

date <- select(Data4, year, month, day)

Data4 %>% select(year, name,)

library(lubridate)

date1 <- date %>% 
  mutate(date = (paste(year, month, day, sep="/")))
head(date1)

date2 <- date1$date

Data5 <- cbind(date2, Data4)%>%
  select(date2, name, ...8:...27)

colname <- c("date", "name", "cas-total", "cas-death", "cas-miss", "cas-重傷", "cas-輕傷", "房損-全倒-棟", "房損-全倒-戶", "房損-半倒-棟", "房損-半倒-戶", "rescued victims", "res-total", "res-消防", "res-義消", "res-警察", "res-駐軍", "res-其他", "equi-車", "equi-船", "equi-直升機", "equi-其他")
colnames(Data5)=colname

write.table(Data5, file = "/Users/zhaowen/Desktop/Data-Science/data/颱風傷亡.csv",row.names = FALSE, sep = ",")