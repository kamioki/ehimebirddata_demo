#==ehime birdchecklist map===
library(readr)
library(tidyverse)
library(dplyr)
library(sf)
library(ggforce)
library(ggplot2)
library(rmapshaper)
library(jpmesh)
library(tibble)
library(stringr)
library(openxlsx)


##############データフレームコピペ
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

################元データ読み込み
df <- read.xlsx("YOURDATA.xlsx") 

###############データ補完
#3年
df<-df %>%
  mutate(year3 = case_when(
    between(year,1988,1990) ~ "yt01",
    between(year,1991,1993) ~ "yt02",
    between(year,1994,1996) ~ "yt03",
    between(year,1997,1999) ~ "yt04",
    between(year,2000,2002) ~ "yt05",
    between(year,2003,2005) ~ "yt06",
    between(year,2006,2008) ~ "yt07",
    between(year,2009,2011) ~ "yt08",
    between(year,2012,2014) ~ "yt09",
    between(year,2015,2017) ~ "yt10",
    between(year,2018,2020) ~ "yt11")) 

#5年
df<-df %>%
  mutate(year5 = case_when(
    between(year,1950,1974) ~ "1950s",
    between(year,1975,1979) ~ "1975s",
    between(year,1980,1984) ~ "1980s",
    between(year,1985,1989) ~ "1985s",
    between(year,1990,1994) ~ "1990s",
    between(year,1995,1999) ~ "1995s",
    between(year,2000,2004) ~ "2000s",
    between(year,2005,2009) ~ "2005s",
    between(year,2010,2014) ~ "2010s",
    between(year,2015,2019) ~ "2015s")) 
#10年
df<- df %>%
  mutate(year10 = case_when(
    between(year,1954,1989) ~ "1950-80年代",
    between(year,1990,1999) ~ "1990年代",
    between(year,2000,2009) ~ "2000年代",
    between(year,2010,2019) ~ "2010年代"))

##############2次メッシュ、geometry追加
df$mesh10km <- substring(df$mesh,1,6)
mesh10km <- administration_mesh(code = 38, to_mesh_size = 10) %>% rename(mesh10km=meshcode)
df<-left_join(df,mesh10km,"mesh10km")

################重複抽出
duplicate <- df %>% 
  group_by(sp,num,year,month,date,city,mesh,notes,time) %>% 
  filter(n()>1)
duplicate <- duplicate[order(duplicate$spID,duplicate$year,duplicate$month,duplicate$date,duplicate$num,duplicate$city),]

################位置情報全補完
latlng <- df[c("latlng","mesh")] %>% 
#緯度経度コンマで分割
          separate(latlng, c("lat", "lng"), sep=",") %>% 
#空白削除
          mutate(lng = str_squish(lng))
latlng$lat <- as.numeric(latlng$lat)
latlng$lng <- as.numeric(latlng$lng)

#メッシュが揃ってて緯度経度情報埋める
umesh <- unique(na.omit(latlng[c("mesh")]))
#メッシュ緯度経度変換
umesh <- mesh_to_coords(as.numeric(umesh$mesh))
umesh <- lapply(umesh, unlist)
umesh <- as.data.frame(umesh)
umesh <- tibble::rownames_to_column(umesh,"mesh_code")
umesh <- umesh[!grepl('mesh_size', umesh$mesh_code),] 
umesh <- umesh[-c(1,5,6)] 
umesh <- umesh[c(3,2,1)] %>% rename(lat=lat_center,lng=lng_center,mesh=meshcode)
umesh$mesh <- as.numeric(umesh$mesh)

#緯度経度が揃ってて1kmメッシュ情報埋める
#ulat <- unique(na.omit(latlng[c("lat","lng")]))
#緯度経度メッシュ変換
#ulat2 <- coords_to_mesh(as.numeric(ulat$lng),as.numeric(ulat$lat))
#ulat <- cbind(ulat,ulat2)
#ulat <- lapply(ulat, unlist)
#ulat <- as.data.frame(ulat)
#ulat <- tibble::rownames_to_column(ulat,"meshcode")
#ulat <- ulat[!grepl('mesh_size', ulat$meshcode),] 
#ulat <- ulat[,c(-1)] %>% rename(mesh1km=ulat2)
#latlng <- left_join(latlng, ulat, by=c("lat","lng"))

#元データと結合
latlng <- left_join(latlng, umesh, by="mesh",copy = TRUE) 
#左詰め shift to left
library(hacksaw)
latlng <- latlng[c(1,2,4,5,3)]
latlng <- latlng %>% shift_row_values()
latlng <- tibble::rowid_to_column(latlng, "id")
df <- left_join(df, latlng, by=c("id")) 
df <- df[-c(12,27:29)]
df <- df %>% rename(mesh=mesh.x,lng=lng.x,lat=lat.x)

################同一人物が同じ種を同じ月に複数回以上報告しているのを省略
#結合
df$obrec <- paste(df$observer,df$recorder,sep = ",")
#置換
df$obrec <- str_replace_all(df$obrec,c('・' = ',', '、' = ',','，'=',',' '=''))
#不要文字列削除
df$obrec <- trimws(gsub("NA,|,NA|探鳥会：|（個人記録）", "\\1", df$obrec))
#重複文字列削除
df$obrec <- sapply(df$obrec, function(x) paste(unique(unlist(str_split(x,","))), collapse = ","))

df <- df %>% 
  add_count(sp,year,month,mesh,obrec) %>% 
  filter(n<2)

#必要データ抜き出し
df2<-df[c(1,2,3,5,6,19,20,21,7,10,22,24,25,13,23)]
#必要データ抜き出し
df2<-df[c('spID','sp','num','year','month','date','city','place','notes','obrec')]

###############地図読み込み
#download shape file from https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v2_4.html#prefecture38
ehime_original <- read_sf("ehime/N03-20_38_200101.shp", options="ENCODING=SHIFT_JIS")
#extract unique city names and keep city_id,city_name,geometry
ehime <- aggregate(ehime_original, list(ehime_original$N03_004), unique) %>%
  select(N03_004,geometry) %>%
  rename(city_name = N03_004) %>%
  #simplify and make the shape file light to read
  ms_simplify(keep = 0.005)
#plot(ehime)


###############全記録地点描画
#描画
latyear<-unique(df[c("year","lat","lng")])
mapcheck<-ggplot() +
  #geom_sf(data =mesh10km,alpha=0)+
  geom_sf(data=ehime,alpha=0)+
  geom_point(data=latyear,aes(x=lng,y=lat),
             size=1,
             color="black",
             alpha=0.25,
             #group=year_ten
             shape=15,
            )+
  theme_void()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=15))
#    facet_wrap_paginate(~ year_ten,
#                        nrow=4,
#                        ncol=1, 
#                        page=1) 
mapcheck


#マウスオーバーでデータ表示
library(plotly)
ggplotly(mapcheck)

##############市町村別記録数
#ユニーク数
citycount <- df %>% group_by(city)%>%summarize(count=n())
citycount2 <- ggplot(data=citycount,aes(x=reorder(city,-count), y=count))+geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0,20000,10000),
                     #limits = c(0,50000),
                     expand = expansion(mult = c(-0.01, .1),
                                        add = c(0, 0)))
citycount2
#write.csv(citycount,file="citycount.csv")
#boxplot(count~city, citycount)

################年別最大個体数グラフ
maxnum <- df[c("sp","year","num")] 
maxnum$num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", maxnum$num))
maxnum <- unique(na.omit(maxnum)) %>%
  group_by(sp, year) %>% 
  filter(num == max(num))

#spID付ける
spid <- unique(na.omit(df2[c(2,3)]))
spid <- spid[order(nchar(spid$spID), spid$spID),]
rownames(spid) <- NULL
spid$spIDnum <- as.numeric(rownames(spid))
spid$spIDnum <- sprintf("%03d", spid$spIDnum)
maxnum <- left_join(maxnum,spid, by =c("sp"))
maxnum$IDsp <- paste(maxnum$spIDnum,maxnum$sp)

#記録数5以上,1990年以降抽出
maxnum <- maxnum %>% 
  filter(year>1989) %>% 
  group_by(sp)%>% 
  mutate(count=n()) %>%
  filter(count>5)


#pdf("maxnum_all.pdf", family="Japan1GothicBBB")
#for(i in 1:14){print(

maxnumline <- ggplot()+geom_line(data=maxnum, size=1.5,
                   aes(x=year,y=num,group=IDsp),stat='identity') +
  facet_wrap_paginate(~IDsp, nrow=5, ncol=5, page=i,scales="free_y",
                      strip.position="bottom")+
  guides(colour=FALSE) +
  theme_bw()
#    theme(panel.border = element_blank(), 
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
#          axis.line = element_line(colour = "black"))+ 
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#  theme_void()
#maxnumline

#)}
#dev.off()


################記録傾向2軸グラフ
records <- df[c("sp","year","mesh")] 
#年別記録数
yearcount <- records %>% group_by(year)%>%summarize(count=n())
#年別種数
spyearcount <- records %>% group_by(year,sp)%>%summarize(count=n())%>% group_by(year)%>%summarize(count=n())
allcount<-left_join(spyearcount,yearcount, by =c("year"))

countg<-ggplot()+
  geom_bar(data=allcount,aes(x=year,y=count.y),stat='identity')
countg
countg2<-countg+geom_line(data=allcount,aes(x=year,y=count.x/200*10000),stat='identity')+
  scale_y_continuous(breaks = seq(0,15000,2500),
    sec.axis = sec_axis(~./10000*200,name="種数",
                                         breaks=seq(0,300,50)),
                     expand = expansion(mult = c(-0.01, .1),
                                        add = c(0, 0)))+
  ylab("記録件数")+
  scale_x_continuous(breaks = seq(1954, 2021, by = 10))+
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
countg2

write.excel(yearcount)

##############メッシュ地図
mesh1km <- administration_mesh(code=38,to_mesh_size = 1)

ggplot() +
  geom_sf(data=ehime,alpha=0.5)+
  geom_sf(data=mesh1km,alpha=0,size=0.05)+
  #geom_sf(data=mesh10km,alpha=0,size=0.8)+
  theme_void()


##############記録少ないの抜き出し
less10 <- df2 %>% 
  add_count(sp) %>% 
  filter(n<25) %>% 
  arrange(spID)
less10 <- less10[!(less10$spID=="unknown"),]  

less10$detail = suppressWarnings(paste0(less10$year,".",sprintf("%02d", as.numeric(less10$month)),".",sprintf("%02d", as.numeric(less10$date))," (",less10$num,") ",less10$city," ",less10$place,". ",less10$notes,". "))
less10<-less10[c('spID','sp','detail')]
less10$detail<-gsub("NA.", "", less10$detail)
write.csv(less10,file="less10.csv",fileEncoding = "UTF-8")

##############増減傾向
#unknown削除
df3 <- df2[!(df2$spID=="unknown"),]
#記録数100以上抽出
trend <- df3 %>% add_count(sp) %>% filter(n>99)

#3年ごとの記録数とメッシュ、種数をカウントしてそれぞれ割る

#  group_by(mesh10km) %>% add_count(mesh10km,name="num_mesh10km") %>% ungroup() %>%
#  group_by(year_three,mesh10km) %>% add_count(year_three,mesh10km,name="yaer_allmesh")　%>% ungroup() %>%
#  group_by(year_three,sp) %>% add_count(year_three,sp,name="year_allsp")　%>% ungroup()

yachocount <- trend %>% group_by(sp,year3,mesh10km)%>%summarize(count=n())
yachocount2 <- yachocount %>% summarize(mesh10km_u=n_distinct(mesh10km))
yachocount3 <- aggregate(x=yachocount[c("count")], by=list(yachocount$sp,yachocount$year3), sum)
yachocount3 <- dplyr::rename(yachocount3, sp = 1, year3 = 2)
yachocount4 <- left_join(yachocount2,yachocount3, by =c("sp","year3"))
gridcount <- trend %>% group_by(year3,mesh10km)%>%summarize(count=n())
gridcount <- gridcount %>% summarize(mesh10km_u=n_distinct(mesh10km,year3))
yearcount <- aggregate(x=yachocount[c("count")], by=list(yachocount$year3), sum)
yearcount <- dplyr::rename(yearcount, year3 = 1)

yeargrid <-  left_join(gridcount,yearcount, by =c("year3"))

countratio <- left_join(yeargrid,yachocount4, by =c("year3"))
countratio$meshratio <- (countratio$mesh10km_u.y / countratio$mesh10km_u.x)*100
#length(unique(df2$mesh10km)
countratio$recordratio <-  (countratio$count.y / countratio$count.x)*100
countratio <- countratio[,-c(2,3,5,6)]
countratiodata<-countratio[order(countratio$sp),]
#spID付ける
countratiodata <- left_join(countratiodata,spid, by =c("sp"))
countratiodata$IDsp <- paste(countratiodata$spID,countratiodata$sp)

#傾き求める
slope<-countratiodata %>%
  group_by(sp) %>% 
  do(mod_lin = lm(meshratio~year3, data = .)) %>% 
  mutate(intercept = mod_lin$coefficients[1],
         slope = mod_lin$coefficients[2])
slope<-slope[order(slope$slope),]
slope<-tibble::rownames_to_column(slope,"id")
slope$order<-paste(slope$id, slope$sp, sep=". ")
countratiodata <- left_join(countratiodata,slope, by =c("sp"))
countratiodata <- countratiodata[,-c(5:7)]


df3<-ggplot(data=countratiodata, aes(year3, meshratio, group=IDsp)) +
  theme(legend.position="none")+ 
  geom_bar(aes(group=IDsp),stat='identity')
  #facet_wrap_paginate(~IDsp, nrow=5, ncol=5, page=2, scales="free", strip.position="bottom")
#df3

#pdf("recordratio_all.pdf", family="Japan1GothicBBB")
#for(i in 1:10){print(

df4<-df3+geom_line(data=countratiodata, size=1.5,
                   aes(x=year3,y=recordratio/0.03*0.5,
                       group=IDsp),stat='identity') +
  scale_y_continuous(sec.axis = sec_axis(~./0.5*0.03,
                                         breaks=seq(0,3,0.5)),
                     expand = expansion(mult = c(-0.01, .1),
                                        add = c(0, 0)))+
  facet_wrap_paginate(~IDsp, nrow=5, ncol=5, page=i,scales="free_y",
           strip.position="bottom")+
  guides(colour=FALSE) +
    theme_bw()
#    theme(panel.border = element_blank(), 
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
#          axis.line = element_line(colour = "black"))+ 
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#  theme_void()
df4
#)}
#dev.off()

##############種毎の分布変化
spyear <- df %>% group_by(sp,year_five,mesh)%>%summarize(count=n())
#種の抜き出し
spyear<-unique(na.omit(spyear[c("year_five","sp","mesh","count")]))
spyear<- spyear %>% filter(str_detect(sp, "ヒゲガビチョウ"))
                           # str_detect(sp, "サンジャク")|
                           # str_detect(sp, "コアジサシ"))
#メッシュ
spyear<- na.omit(left_join(latcheck, spyear, by=c("mesh")))
spyear$mesh <- as.numeric(substr(spyear$mesh,1,6))
spyear<- na.omit(left_join(mesh10km, spyear, by=c("mesh")))
#描画
ggplot() +
  geom_sf(data=ehime,alpha=0.5)+
  geom_sf(data=mesh10km,alpha=0,color="grey")+
  geom_sf(data=spyear,aes(fill=count,geometry=geometry))+
  facet_wrap_paginate(sp~year_five, nrow=2, ncol=2,page = 1)+
  theme_void()+
  scale_fill_continuous(high = "#26B107",
                        low = "#ffff00",
                        name = "記録数",
                        limits = c(0, 10),
                        breaks = seq(0, 10, by = 5)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=10))


############記述から繁殖記録抽出
breeding <- df %>% filter(str_detect
                          (notes, "巣|交尾|雛|孵化|羽化|孵|コロニー|ビナ|ヒナ|巣立|雌雄が|オスメスが|オスとメスが|雄と雌が|
                            幼鳥|幼い|卵|抱卵|抱雛|エサ|給餌|餌運び|求愛|巣材|間もない|頻繁に出入り|繁殖|囀|さえず|ディスプレイ"))
breedingsp <- unique(na.omit(breeding[c("sp","notes")]))
write.csv(breeding,file="breeding.csv",fileEncoding = "UTF-8")
write.excel(breeding)

