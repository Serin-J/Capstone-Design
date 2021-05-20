cycle = read.csv("./대여소_결합(0716).csv",
                 stringsAsFactor = T)
cycle$field_1 = cycle$field_1_1 = NULL
cycle
str(cycle)
colnames(cycle)
age = cycle[,c(2, 3, 31:36, 40:47)]

str %>% group_by(STATION_GR) %>%
  summarize(평균대여 = mean(대여),
            평균유동 = m)

sapply(age[,9:16], function(x) which(is.na(x)))
age_notna = age[-c(323, 749),]
cor(age_notna)
colnames(age_notna)
plot(age[,1:2])
plot(age[,3:8])
plot(age[,9:16])
age_tot = data.frame(age[,1:2], living = apply(age[,3:8], 1, sum), flow = apply(age[,9:16], 1, sum))
age_tot = age_tot[-c(323, 749),]
plot(flow~living, age_tot)
cor(age_tot$flow, age_tot$living)
library(ggExtra)

p = ggplot(age_tot) +
  geom_point(aes(x = living, y = flow), col = "orange") +
  scale_x_continuous(name = "거주인구") +
  scale_y_continuous(name = "유동인구") +
  ggtitle("거치대 주위의 거주인구와 유동인구") +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        title = element_text(size = 20),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank())
ggMarginal(p, type = "histogram", col = "white", fill = "lightsteelblue")
summary(age_tot)
colnames(age_tot)[3:4] = c("거주인구", "유동인구")

ggplot(age_tot[,1:2]) +
  geom_point(aes(x = 대여, y = 반납), col = "orange") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        title = element_text(size = 20)) +
  ggtitle("거치대별 일평균 대여량과 반납량")

ggpairs(age_tot[,c(1, 3:4)],
        lower = list(continuous = wrap("points", colour = "orange", size = 0.7)),
        diag = list(continuous = wrap("barDiag",colour = "white", fill = "lightsteelblue")),
        upper = list(continuous = wrap("cor", size=6)),
        switch = "both") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        title = element_text(size = 20)) +
  ggtitle("거치대 이용량과 주변 거주인구, 유동인구")
?ggpairs
library(ggplot2)
library(GGally)
?ggpairs
library(reshape2)
boxdat = melt(age_notna[,3:16])
boxdat$gubun = unlist(lapply(strsplit(as.character(boxdat$variable), "[^가-힣]"), head, 1))
boxdat$new_variable = boxdat$variable
levels(boxdat$new_variable)[7:8] = "유동10대"
levels(boxdat$new_variable)[12:13] = "유동60대"

library(stringr)

boxdat$age = paste0(unlist(str_extract_all(as.character(boxdat$new_variable), "[0-9]+")), "대")
ggplot(boxdat) +
  geom_boxplot(aes(x = age, y = value, col = age)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  facet_wrap(~gubun, scales = "free_y") +
  ggtitle("연령대별 거주인구, 유동인구 양상 비교") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        title = element_text(size = 20))

ggpairs(age[,c(3:8)],
        lower = list(continuous = wrap("points", colour = "orange", size = 0.7)),
        diag = list(continuous = wrap("barDiag",colour = "white", fill = "steelblue")),
        switch = "both")  +
  ggtitle("연령대별 거치대 주변 거주인구") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        title = element_text(size = 20))
?ggpairs

ggpairs(age[,c(9:16)],
        lower = list(continuous = wrap("points", colour = "orange", size = 0.7)),
        diag = list(continuous = wrap("barDiag",colour = "white", fill = "steelblue")),
        upper = list(continuous = wrap("cor", size = 3.5)),
        axisLabels = "inner",
        switch = "both")  +
  ggtitle("연령대별 거치대 주변 유동인구") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 20))


S = eigen(cov(age[,3:8]))
as.matrix(age[,3:8] - sapply(age[,3:8], mean)) %*% S$vectors
cumsum(S$value/sum(S$value))

pca_1 = data.frame(- as.matrix(age_notna[,3:8] - sapply(age_notna[,3:8], mean)) %*% S$vectors)
pca_1 = cbind(cycle[-c(323, 749),2:8], pca_1[,1:2])
colnames(pca_1)[8:9] = c("live_1", "live_2")
plot(pca_1)
ggpairs(pca_1[,8:9])

S = eigen(cov(age_notna[,9:16]))
as.matrix(age_notna[,9:16] - sapply(age_notna[,9:16], mean)) %*% S$vectors
cumsum(S$value/sum(S$value))
pca_2 = data.frame(- as.matrix(age_notna[,9:16] - sapply(age_notna[,9:16], mean)) %*% S$vectors)
pca_2 = cbind(cycle[-c(323, 749),2:8], pca_2[,1:2])
colnames(pca_2)[8:9] = c("flow_1", "flow_2")
plot(pca_2)
ggpairs(pca_2[,8:9])


ggpairs(cbind(pca_1[,8:9], pca_2[,8:9]))


S = eigen(cov(age_notna[,3:16]))
as.matrix(age_notna[,3:16] - sapply(age_notna[,3:16], mean)) %*% S$vectors
cumsum(S$value/sum(S$value))
plot(cumsum(S$value/sum(S$value)), type = "b", ylab = "분산설명량", 
     main = "주성분 개수에 따른 분산설명량")

# 결론 : 주성분분석으로 14개의 인구 관련 변수를 처리하면 좋을 것이다.
# 선형회귀 이용 시에 다중 공선성의 문제가 발생할 가능성이 크고, 주성분 분석 등을 이용하여 이러한 문제를 해결하면
# 과적합문제를 해결하고, 모형 단순화에 도움이 될 것이다.

colnames(age_notna[,3:16])
a = -S$vectors[,1:3]
rownames(a) = colnames(age_notna[,3:16])
colnames(a) = c("p1", "p2", "p3")
a
pca = data.frame(- as.matrix(age_notna[,3:16] - sapply(age_notna[,3:16], mean)) %*% S$vectors)
colnames(pca) = paste0("p", 1:14)
plot(pca[,1:3])
pca_r = cbind(cycle[-c(323, 749),2:8], pca[,1:3])
str(pca_r)
ggpairs(pca_r[,c(1, 8:10)],
        lower = list(continuous = wrap("points", colour = "orange", size = 0.7)),
        diag = list(continuous = wrap("barDiag",colour = "white", fill = "steelblue")),
        upper = list(continuous = wrap("cor", size = 5)),
        axisLabels = "inner",
        switch = "both")  +
  ggtitle("인구 관련 변수들에 대한 주성분") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 20))

fit1 = lm(대여~p1+p2+p3, data = pca_r)
summary(fit1)
library(car)
vif(fit1)

fit2 = lm(대여~.-반납, data = age_notna)
summary(fit2)
vif(fit2)


summary(lm(대여~p1+p2+p3, data = pca_r))
summary(lm(대여~.-반납, data = age))
library(plotly)
plot_ly(data = pca_r, x = ~p1, y = ~p2, z = ~p3, color = ~대여, size = 0.7) %>%
  add_markers()
plot_ly(data = pca_r, x = ~p1, y = ~p2, color = ~대여, size = 1, text = ~STATION_NA) %>%
  add_markers()
