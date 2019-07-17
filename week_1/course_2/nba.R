library(tidyverse)
library(downloader)
library(ggplot2)
library(cluster)
library(factoextra)
data_1_origin<-read.csv("data1.csv")
data_1_need <- data_1_origin %>% mutate(Throw = FGA*G)%>%select(Salary, Height, FG., Throw, Name, Team) %>% filter(Team != "")
data_1_need<-data_1_need%>%group_by(Team)%>%mutate(sum_throw = sum(Throw))
data_1_need<-data_1_need%>%mutate(Percentage = Throw / sum_throw)%>%select(-Throw, -sum_throw)
t1<-as.vector(data_1_need$Name)
t2<-as.vector(data_1_need$Team)
n = nrow(data_1_need)
for (i in c(1: n)) {
  tt<-t1[i]
  tmp<-strsplit(tt, split = " ")
  #print(tmp[[1]][2])
  t1[i] = paste0(substring(tmp[[1]][1], 1, 1), ". ", tmp[[1]][2])
  if (t2[i] == "Boston Celtics") {
    t1[i] = paste0(t1[i], " - BOS")
  } else if (t2[i] == "Brooklyn Nets") {
    t1[i] = paste0(t1[i], " - BRK")
  } else if (t2[i] == "New York Knicks") {
    t1[i] = paste0(t1[i], " - NY")
  } else if (t2[i] == "Philadelphia 76ers") {
    t1[i] = paste0(t1[i], " - PHI")
  } else if (t2[i] == "Toronto Raptors") {
    t1[i] = paste0(t1[i], " - TOR")
  } else if (t2[i] == "Chicago Bulls") {
    t1[i] = paste0(t1[i], " - CHI")
  } else if (t2[i] == "Cleveland Cavaliers") {
    t1[i] = paste0(t1[i], " - CLE")
  } else if (t2[i] == "Detroit Pistons") {
    t1[i] = paste0(t1[i], " - DET")
  } else if (t2[i] == "Indiana Pacers") {
    t1[i] = paste0(t1[i], " - IND")
  } else if (t2[i] == "Milwaukee Bucks") {
    t1[i] = paste0(t1[i], " - MIL")
  } else if (t2[i] == "Atlanta Hawks") {
    t1[i] = paste0(t1[i], " - ATL")
  } else if (t2[i] == "Charlotte Hornets") {
    t1[i] = paste0(t1[i], " - CHO")
  } else if (t2[i] == "Miami Heat") {
    t1[i] = paste0(t1[i], " - MIA")
  } else if (t2[i] == "Orlando Magic") {
    t1[i] = paste0(t1[i], " - ORL")
  } else if (t2[i] == "Washington Wizards") {
    t1[i] = paste0(t1[i], " - WAS")
  } else if (t2[i] == "Dallas Mavericks") {
    t1[i] = paste0(t1[i], " - DAL")
  } else if (t2[i] == "Houston Rockets") {
    t1[i] = paste0(t1[i], " - HOU")
  } else if (t2[i] == "Memphis Grizzlies") {
    t1[i] = paste0(t1[i], " - MEM")
  } else if (t2[i] == "New Orleans Pelicans") {
    t1[i] = paste0(t1[i], " - NO")
  } else if (t2[i] == "San Antonio Spurs") {
    t1[i] = paste0(t1[i], " - SAS")
  } else if (t2[i] == "Denver Nuggets") {
    t1[i] = paste0(t1[i], " - DEN")
  } else if (t2[i] == "Minnesota Timberwolves") {
    t1[i] = paste0(t1[i], " - MIN")
  } else if (t2[i] == "Oklahoma City Thunder") {
    t1[i] = paste0(t1[i], " - OKC")
  } else if (t2[i] == "Portland Trail Blazers") {
    t1[i] = paste0(t1[i], " - POR")
  } else if (t2[i] == "Utah Jazz") {
    t1[i] = paste0(t1[i], " - UTA")
  } else if (t2[i] == "Golden State Warriors") {
    t1[i] = paste0(t1[i], " - GSW")
  } else if (t2[i] == "Los Angeles Clippers") {
    t1[i] = paste0(t1[i], " - LAC")
  } else if (t2[i] == "Los Angeles Lakers") {
    t1[i] = paste0(t1[i], " - LAL")
  } else if (t2[i] == "Phoenix Suns") {
    t1[i] = paste0(t1[i], " - PHO")
  } else if (t2[i] == "Sacramento Kings") {
    t1[i] = paste0(t1[i], " - SAC")
  }
}
t1<-factor(t1)
data_1_need<-data.frame(data_1_need, name = t1)%>%select(Salary, Height, FG., Percentage, name)
write.csv(data_1_need, "res1.csv")
data_2_origin<-read.csv("data2.csv")
data_2_t1<-data_2_origin%>%filter(GameType == "regular")%>%mutate(Pt = abs(HomeScore - AwayScore)) %>% select(Quarter, SecLeft, Shooter, Pt) %>% filter(Quarter==4, SecLeft <=60, Shooter != "")
data_2_need<-data_2_t1%>%filter( (SecLeft > 30 & Pt <= 10) | (SecLeft > 20 & Pt <= 8) | (Pt <= 7) )%>%select(Shooter)%>%group_by(Shooter)%>%mutate(Times = n())%>%distinct()
t1<-as.vector(data_2_need$Shooter)
n = nrow(data_2_need)
for (i in c(1 : n)) {
  tmp = strsplit(t1[i], split = " ")
  t1[i] = tmp[[1]][4]
}
t1<-factor(t1)
data_2_need<-data.frame(data_2_need, Team = t1)%>%group_by(Team)%>%mutate(sum_times = sum(Times))
data_2_need<-data_2_need%>%mutate(sp_percentage = Times / sum_times)%>%ungroup()
data_2_need<-data_2_need%>%rename(name = Shooter)%>%select(name, sp_percentage)
data_final = merge(data_1_need, data_2_need, by = "name")
data_final<-data_final%>%filter(Percentage > 0.065)
write.csv(data_final, "res.csv")
ggplot(data = data_final) + geom_boxplot(mapping = aes(x = Salary, y = pp, group = cut_width(Salary, 5000000, boundary = 0)))
ggplot(data = data_final) + geom_boxplot(mapping = aes(x = Salary, y = pp, group = cut_number(Salary, 8))) 
ggplot(data = data_final) + geom_boxplot(mapping = aes(x = Height, y = pp, group = cut_width(Height, 2.8, boundary = 72)))
ggplot(data = data_final) + geom_boxplot(mapping = aes(x = FG., y = pp, group = cut_width(FG., 0.05, boundary = 0.386)))
ggplot(data = data_final) + geom_boxplot(mapping = aes(x = Salary, y = FG., group = cut_width(Salary, 5000000, boundary = 0)))
ggplot(data = data_final) + geom_point(mapping = aes(x = Percentage, y = sp_percentage)) + geom_smooth(mapping = aes(x = Percentage, y = sp_percentage))
cc<-kmeans(data_final%>%select(Percentage, sp_percentage), centers=3)
fviz_cluster(cc, data = data_final%>%select(Percentage, sp_percentage),            # 資料
             geom = c("point", "text"),     # 點 (point)
             frame.type = "norm")
cc<-kmeans(data_final%>%select(Salary, pp), centers=4)
fviz_cluster(cc, data = data_final%>%select(Salary, pp),            # 資料
             geom = c("point", "text"),     # 點 (point)
             frame.type = "norm")
ggplot(data = data_final) + geom_point(mapping = aes(x = Salary, y = pp))

data_final<-read.csv("res.csv") %>% mutate(pp = sp_percentage / Percentage)

ggplot(data = data_final) + geom_bar(mapping = aes(x = floor(Salary/5000000), fill = (pp > 1)), position = "fill")


