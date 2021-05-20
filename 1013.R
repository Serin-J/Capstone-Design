library(stringr)
library(dplyr)
library(data.table)
library(lubridate)
setwd("C:/Users/Serin/Desktop/캡스톤/생활인구")

month_local = list()
for(i in 1:12){
  if(i < 10){
    month = paste0(0, i)
  }else{
    month = as.character(i)
  }
  month_file = list.files()[str_detect(list.files(), paste0("^LOCAL_PEOPLE_2019", month))]
  
  local = list()
  for(j in 1:length(month_file)){
    local[[j]] = read.csv(month_file[j], skip = 1, header = F)[,1:5]
  }
  local = rbindlist(local)
  weekdays = weekdays(as.Date(as.character(local$V1), "%Y%m%d"))
  local$weekend[weekdays %in% paste0(c("월", "화", "수", "목", "금"), "요일")] = "평일"
  local$weekend[weekdays %in% paste0(c("토", "일"), "요일")] = "주말"
  month_local[[i]] = local %>% group_by(V2, weekend, V4) %>%
    summarize(V5 = mean(V5)) %>%
    data.frame() %>%
    mutate(month = as.character(month))
}

tot_month_local = rbindlist(month_local)
colnames(tot_month_local)[1:4] = c("hour", "weekend", "id", "pop")
tot_month_local = data.frame(tot_month_local)
tot_month_local$id = as.character(tot_month_local$id)
#
library(sf)
library(maptools)
library(ggplot2)
library(ggmap)
library(plotly)
library(gganimate)
library(dplyr)

map = st_read("./집계구.shp", stringsAsFactors = F)
local_map = left_join(tot_month_local, map[1], by = c("id" = "TOT_REG_CD"))
save(local_map, file = "./생활인구.rdata")

# new ver. (평일 주말 구분..인데 위에랑 따로 구분해야될듯)
local_weekend_map = left_join(tot_month_local, map[1], by = c("id" = "TOT_REG_CD"))
local_weekend_map = local_map
save(local_weekend_map, file = "./생활인구주말구분.rdata")
#
library(dplyr)
library(sf)
library(maptools)
library(ggplot2)
library(ggmap)
setwd("C:/Users/Serin/Desktop/캡스톤/생활인구")
load("./생활인구.rdata")
map = st_read("./집계구.shp")



local = local_map[,1:4]

local$time[local$hour %in% 0:2] = "00 ~ 02"
local$time[local$hour %in% 3:5] = "03 ~ 05"
local$time[local$hour %in% 6:8] = "06 ~ 08"
local$time[local$hour %in% 9:11] = "09 ~ 11"
local$time[local$hour %in% 12:14] = "12 ~ 14"
local$time[local$hour %in% 15:17] = "15 ~ 17"
local$time[local$hour %in% 18:20] = "18 ~ 20"
local$time[local$hour %in% 21:23] = "21 ~ 23"

by_hour = local %>%
  group_by(hour, time, id) %>%
  summarize(pop = mean(pop)) %>%
  group_by(time, id) %>%
  summarize(pop = sum(pop))
by_hour = left_join(by_hour, map[,1:2], by = c("id" = "TOT_REG_CD"))
by_hour = st_as_sf(by_hour)

seoul = st_read("./서울지도/seoul.shp")
Encoding(seoul$SIG_KOR_NM) = "CP949"
seoul = seoul %>% select(SIG_KOR_NM)
seoulmap_sp = as(seoul, "Spatial")
seoulmap_longlat = spTransform(seoulmap_sp,  
                               CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"))
seoullonglat = fortify(seoulmap_longlat)
library(plotly)
memory.size()
memory.limit(10000)

p1 = ggplot(local_map %>%
         filter(month == "01",
                hour == 12)) +
  geom_sf(aes(fill = pop), col = NA) +
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), col = "white", fill = NA) +
  scale_fill_gradient(low = "midnightblue", high = "lightgoldenrod") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13)) 

p2 = ggplot(local_map %>%
              filter(month == "01",
                     hour == 12)) +
  geom_sf(aes(fill = pop), col = NA) +
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), col = "white", fill = NA) +
  scale_fill_gradient(low = "midnightblue", high = "lightgoldenrod", trans = "sqrt") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13)) 
library(patchwork)
rm(list = "p2")
p1 + p2
library(plotly)
rm()
data = local_map[,1:4] %>%
  filter(month == "01"&
         hour == 12)
library(reshape2)
ggplot(data %>%
         mutate(original = pop,
                sqrt = sqrt(pop)) %>%
         melt(id.vars = )) +
  geom_boxplot(aes(x = ==))
plot_ly(data) %>%
  add_boxplot(y = ~pop) 
plot_ly(data) %>%
  add_boxplot(y = ~sqrt(pop), marker = list(line = list(colors = "steelblue"), colors = NA), fillcolor = "white",
              name = "sqrt") 
# 시간별 유동인구의 월평균 이용
ggplot(by_hour) +
  geom_sf(aes(fill = pop), col = NA) +
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), col = "white", fill = NA) +
  scale_fill_gradient(low = "midnightblue", high = "lightgoldenrod", trans = "sqrt") +
  facet_wrap(~time, ncol = 4) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13)) 

by_month = local %>%
  group_by(month, id) %>%
  summarize(pop = mean(pop))
by_month = left_join(by_month, map[,1:2], by = c("id" = "TOT_REG_CD"))
by_month = st_as_sf(by_month)

ggplot(by_month) +
  geom_sf(aes(fill = pop), col = NA) +
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), col = "white", fill = NA) +
  scale_fill_gradient(low = "midnightblue", high = "lightgoldenrod") +
  facet_wrap(~month, ncol = 4) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13)) 

local_map$time = local$time





unique(tot_month_local$month)
dat = tot_month_local%>% group_by(month, weekend) %>%
  summarize(pop = mean(pop))
p1 = plot_ly(data = data.frame(dat), 
             x = ~ month, y = ~pop, color = ~factor(weekend), showlegend = F) %>%
  add_lines() %>%
  layout(xaxis = list(autorange = F,
                      range = c("00", "12")))
dat = tot_month_local%>% group_by(hour, weekend) %>%
  summarize(pop = mean(pop))

p2 = plot_ly(data = data.frame(dat), x = ~ hour, y = ~pop, color = ~factor(weekend)) %>%
  add_lines() %>%
  layout(xaxis = list(zeroline = F)) %>%
  layout(xaxis = list(autorange = F,
                      range = c("00", "23")))

subplot(p1, p2, titleX = T, margin = 0.05)
#평일은 낮 두시, 주말은 낮 네시 피크

tot_month_local$time[tot_month_local$hour %in% 0:2] = "00 ~ 02"
tot_month_local$time[tot_month_local$hour %in% 3:5] = "03 ~ 05"
tot_month_local$time[tot_month_local$hour %in% 6:8] = "06 ~ 08"
tot_month_local$time[tot_month_local$hour %in% 9:11] = "09 ~ 11"
tot_month_local$time[tot_month_local$hour %in% 12:14] = "12 ~ 14"
tot_month_local$time[tot_month_local$hour %in% 15:17] = "15 ~ 17"
tot_month_local$time[tot_month_local$hour %in% 18:20] = "18 ~ 20"
tot_month_local$time[tot_month_local$hour %in% 21:23] = "21 ~ 23"

by_hour = tot_month_local %>%
  group_by(hour, time, weekend, id) %>%
  summarize(pop = mean(pop)) %>%
  group_by(time, weekend, id) %>%
  summarize(pop = sum(pop))
by_hour = left_join(by_hour, map[,1:2], by = c("id" = "TOT_REG_CD"))
by_hour = st_as_sf(by_hour)

ggplot(by_hour %>%
         filter(time == "12 ~ 14")) +
  geom_sf(aes(fill = pop), col = NA) +
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), col = "white", fill = NA) +
  scale_fill_gradient(low = "midnightblue", high = "lightgoldenrod", trans = "sqrt") +
  facet_grid(~ weekend) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15)) 
save(tot_month_local, file = "./생활인구주평.rdata")
