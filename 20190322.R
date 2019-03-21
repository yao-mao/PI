#資料：台中市長候選人粉絲頁，林佳龍代號GL，盧秀燕代號Air
library(readr)
library(dplyr)
X201801 <- read_csv("201801_data.csv")
X201802 <- read_csv("201802_data.csv")
X201803 <- read_csv("201803_data.csv")
X201804 <- read_csv("201804_data.csv")
X201805 <- read_csv("201805_data.csv")
X201806 <- read_csv("201806_data.csv")
X201807 <- read_csv("201807_data.csv")
X201808 <- read_csv("201808_data.csv")
X201809 <- read_csv("201809_data.csv")
X201810 <- read_csv("201810_data.csv")
X201811 <- read_csv("201811_data.csv")
X201812 <- read_csv("201812_data.csv")
X201901 <- read_csv("201901_data.csv")
#資料清理：提出候選人並清除非候選人粉絲頁
#2018.01月
GL1 <-filter(X201801,grepl("林佳龍", X201801$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201801$Page_Name)==FALSE&grepl("台中市長林佳龍", X201801$Page_Name)==FALSE)
Air1 <-filter(X201801,grepl("盧秀燕", X201801$Page_Name)==TRUE)
#2018.02月
GL2 <-filter(X201802,grepl("林佳龍", X201802$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201802$Page_Name)==FALSE)
Air2 <-filter(X201802,grepl("盧秀燕", X201802$Page_Name)==TRUE)
#2018.03月
GL3 <-filter(X201803,grepl("林佳龍", X201803$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201803$Page_Name)==FALSE)
Air3 <-filter(X201803,grepl("盧秀燕", X201803$Page_Name)==TRUE)
#2018.04月
GL4 <-filter(X201804,grepl("林佳龍", X201804$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201804$Page_Name)==FALSE)
Air4 <-filter(X201804,grepl("盧秀燕", X201804$Page_Name)==TRUE)
#2018.05月
GL5 <-filter(X201805,grepl("林佳龍", X201805$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201805$Page_Name)==FALSE)
Air5 <-filter(X201805,grepl("盧秀燕", X201805$Page_Name)==TRUE)
#2018.06月
GL6 <-filter(X201806,grepl("林佳龍", X201806$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201806$Page_Name)==FALSE)
Air6 <-filter(X201806,grepl("盧秀燕", X201806$Page_Name)==TRUE)
#2018.07月
GL7 <-filter(X201807,grepl("林佳龍", X201807$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201807$Page_Name)==FALSE)
Air7 <-filter(X201807,grepl("盧秀燕", X201807$Page_Name)==TRUE)
#2018.08月
GL8 <-filter(X201808,grepl("林佳龍", X201808$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201808$Page_Name)==FALSE)
Air8 <-filter(X201808,grepl("盧秀燕", X201808$Page_Name)==TRUE)
#2018.09月
GL9 <-filter(X201809,grepl("林佳龍", X201809$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201809$Page_Name)==FALSE)
Air9 <-filter(X201809,grepl("盧秀燕", X201809$Page_Name)==TRUE)
#2018.10月
GL10 <-filter(X201810,grepl("林佳龍", X201810$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201810$Page_Name)==FALSE)
Air10 <-filter(X201810,grepl("盧秀燕", X201810$Page_Name)==TRUE)
#2018.11月
GL11 <-filter(X201811,grepl("林佳龍", X201811$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201811$Page_Name)==FALSE)
Air11 <-filter(X201811,grepl("盧秀燕", X201811$Page_Name)==TRUE)
#2018.12月
GL12 <-filter(X201812,grepl("林佳龍", X201812$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201812$Page_Name)==FALSE&grepl("反林佳龍粉絲團", X201812$Page_Name)==FALSE)
Air12 <-filter(X201812,grepl("盧秀燕", X201812$Page_Name)==TRUE&grepl("侯友宜 盧秀燕 韓國瑜 北中南連線", X201812$Page_Name)==FALSE)
#2019一月
GL13 <-filter(X201901,grepl("林佳龍", X201901$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201901$Page_Name)==FALSE&grepl("反林佳龍粉絲團", X201901$Page_Name)==FALSE)
Air13 <-filter(X201901,grepl("盧秀燕", X201901$Page_Name)==TRUE&grepl("侯友宜 盧秀燕 韓國瑜 北中南連線", X201901$Page_Name)==FALSE)

#合併201801~201901月的資料

GLalldata <- rbind(GL1,GL2,GL3,GL4,GL5,GL6,GL7,GL8,GL9,GL10,GL11,GL12,GL13)
Airalldata <- rbind(Air1,Air2,Air3,Air4,Air5,Air6,Air7,Air8,Air9,Air10,Air11,Air12,Air13)
all <- rbind(GLalldata,Airalldata)

#時間處理
GLalldata$Date=as.POSIXct(GLalldata$Date,format="%Y/%m/%d %H:%M:%S")
Airalldata$Date=as.POSIXct(Airalldata$Date,format="%Y/%m/%d %H:%M:%S")
all$Date=as.POSIXct(all$Date,format="%Y/%m/%d %H:%M:%S")

# PO文類型統計
#林佳龍
library(ggplot2)
GLdatatype=GLalldata%>%group_by(Type)%>%count()
label_value <- paste('(', round(GLdatatype$n/sum(GLdatatype$n) * 100, 1), '%)', sep = '')
label=paste(GLdatatype$Type,label_value,sep = "")
ggplot(GLdatatype, aes(x="", y=n, fill=Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+
  labs(x = '', y = '', title = '')+
  theme(axis.text = element_blank())+
  scale_fill_discrete(labels = label)

#盧秀燕
Airdatatype=Airalldata%>%group_by(Type)%>%count()
label_value <- paste('(', round(Airdatatype$n/sum(Airdatatype$n) * 100, 1), '%)', sep = '')
label=paste(Airdatatype$Type,label_value,sep = "")
ggplot(Airdatatype, aes(x="", y=n, fill=Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+
  labs(x = '', y = '', title = '')+
  theme(axis.text = element_blank())+
  scale_fill_discrete(labels = label)
#總PO文
alldatatype=all%>%group_by(Type)%>%count()
label_value <- paste('(', round(alldatatype$n/sum(alldatatype$n) * 100, 1), '%)', sep = '')
label=paste(alldatatype$Type,label_value,sep = "")
ggplot(alldatatype, aes(x="", y=n, fill=Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+
  labs(x = '', y = '', title = '')+
  theme(axis.text = element_blank())+
  scale_fill_discrete(labels = label)

# 至2019年一月的PO文變化
#林佳龍
GLpostcount=GLalldata%>%group_by(month=format(Date,"%Y/%m"))%>%count()
ggplot(GLpostcount,aes(x=month,y=n))+
  geom_bar(stat = "identity")
#盧秀燕
Airpostcount=Airalldata%>%group_by(month=format(Date,"%Y/%m"))%>%count()
ggplot(Airpostcount,aes(x=month,y=n))+
  geom_bar(stat = "identity")
#加總
allpostcount=all%>%group_by(month=format(Date,"%Y/%m"))%>%count()
ggplot(allpostcount,aes(x=month,y=n))+
  geom_bar(stat = "identity")

Aircount=Airalldata%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(group="Air")
GLcount=GLalldata%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(group="GL")
monthcount=rbind(Aircount,GLcount)%>%arrange((month))
## han chen 每月PO文次數
ggplot(monthcount,aes(x=month,y=n,fill=group))+
  geom_bar(stat="identity",position = "dodge")

## Type&SHARE COUNT BOXPLOT
#林佳龍
ggplot(GLalldata,aes(x=Type,y=Share_Count))+
  geom_boxplot()
#盧秀燕
ggplot(Airalldata,aes(x=Type,y=Share_Count))+
  geom_boxplot()
#全部
ggplot(all,aes(x=Type,y=Share_Count))+
  geom_boxplot()

#Line
ggplot(all, aes(x=Page_Name, y=Share_Count))+geom_line()+theme(text=element_text(family="Heiti TC Light"))
#直方圖
ggplot(all, aes(x=WOW_COUNT))+geom_histogram()

#point

ggplot(all, aes(x=Type,y=Share_Count))+geom_point()+theme(text=element_text(family="Heiti TC Light"))
#常態分佈
library(ggpubr)
ggqqplot(GLalldata$All_Reaction_Count)       

#觀察數值
library(corrplot)
cor(GLalldata[c(6:14)])%>%corrplot.mixed(lower = "pie",tl.cex=0.6)
## 挑出correlation中相關係數較顯著的，觀察其圖型
ggscatter(GLalldata,x="All_Reaction_Count",y="LOVE_COUNT", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")
