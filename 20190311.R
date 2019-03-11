# 789月台中市長候選人，林佳龍代號GL，盧秀燕代號Air
Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
library(readr)
library(dplyr)
X201807 <- read_csv("201807_data.csv")
X201808 <- read_csv("201808_data.csv")
X201809 <- read_csv("201809_data.csv")
View(X201807)
#七月
GL7 <-filter(X201807,grepl("林佳龍", X201807$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201807$Page_Name)==FALSE)
Air7 <-filter(X201807,grepl("盧秀燕", X201807$Page_Name)==TRUE)
#八月
GL8 <-filter(X201808,grepl("林佳龍", X201808$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201808$Page_Name)==FALSE)
Air8 <-filter(X201808,grepl("盧秀燕", X201808$Page_Name)==TRUE)
#九月
GL9 <-filter(X201809,grepl("林佳龍", X201809$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201809$Page_Name)==FALSE)
Air9 <-filter(X201809,grepl("盧秀燕", X201809$Page_Name)==TRUE)
#合併
GLdata789 <- rbind(GL7,GL8,GL9)
Airdata789 <- rbind(Air7,Air8,Air9)

#GLAir789 <- rbind(GLdata789,Airdata789) #不需要合併，會讓線合在一起
#製圖

GLdata789$Date=as.POSIXct(GLdata789$Date, format="%Y/%m/%d %H:%M:%S")
Airdata789$Date=as.POSIXct(Airdata789$Date, format="%Y/%m/%d %H:%M:%S")

plot(GLdata789$Date, GLdata789$LIKE_COUNT, type = "p", col="blue")
lines(Airdata789$Date, Airdata789$LIKE_COUNT,type="p", col="red")
?lines     
#############

X201810 <- read_csv("201810_data.csv")
X201811 <- read_csv("201811_data.csv")
X201812 <- read_csv("201812_data.csv")
X201901 <- read_csv("201801_data.csv")


#10月
GL10 <-filter(X201810,grepl("林佳龍", X201810$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201810$Page_Name)==FALSE)
Air10 <-filter(X201810,grepl("盧秀燕", X201810$Page_Name)==TRUE)
#11月
GL11 <-filter(X201811,grepl("林佳龍", X201811$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201811$Page_Name)==FALSE)
Air11 <-filter(X201811,grepl("盧秀燕", X201811$Page_Name)==TRUE)
#12月
GL12 <-filter(X201812,grepl("林佳龍", X201812$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201812$Page_Name)==FALSE)
Air12 <-filter(X201812,grepl("盧秀燕", X201812$Page_Name)==TRUE)
# 1月
GL13 <-filter(X201901,grepl("林佳龍", X201901$Page_Name)==TRUE&grepl("叮住台中市長林佳龍", X201901$Page_Name)==FALSE)
Air13 <-filter(X201901,grepl("盧秀燕", X201901$Page_Name)==TRUE)

#合併
GLdata4 <- rbind(GL10,GL11,GL12,GL13)
Airdata4 <- rbind(Air10,Air11,Air12,Air13)

#GLAir4 <- rbind(GLdata4,Airdata4)

#製圖

GLdata4$Date=as.POSIXct(GLdata4$Date, format="%Y/%m/%d %H:%M:%S")
Airdata4$Date=as.POSIXct(Airdata4$Date, format="%Y/%m/%d %H:%M:%S")

plot(GLdata4$Date, GLdata4$LIKE_COUNT, type = "p", col="blue")
lines(Airdata4$Date, Airdata4$LIKE_COUNT,type="p", col="red")
#盧秀燕的發文數逐月上升，於12月和隔年一月下降
#林佳龍的發文數亦有此趨勢
