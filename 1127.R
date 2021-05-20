# data = read.csv("./대여소_결합(0716).csv")
# real = read.csv("./대여소후보_결합(0717).csv")
# 
# data[,1:2] = NULL
# real[,1] = NULL
# 
# na1 = unique(apply(data, 2, function(x) which(is.na(x))))[[2]]
# na2 = unique(apply(real, 2, function(x) which(is.na(x))))[[2]]
# 
# data = data[-na1, ]
# real = real[-na2, ]
# 
# 
# data_y = data$대여 + data$반납
# data_x = data[,9:47]
# 
# data = data.frame(data_x, y = data_y)
# real_x = real[,8:46]

z_data = data
z_data[,1:39] = scale(data[,1:39])

t_t = sample(1:nrow(data), 1049)
train = data[t_t, ]
test = data[-t_t,]
z_train = z_data[t_t, ]
z_test = z_data[-t_t, ]


fit1 = lm(y~., z_train)
summary(fit1)

hist(predict(fit1), xlab = "lm_prediction", main = "중회귀모형 예측값 분포")
mean((z_train$y-predict(fit1))^2)
library(car)
vif(fit1)

lm_test_mse = mean((z_test$y - predict(fit1, z_test))^2)


# 
library(leaps)
regfit_f = regsubsets(y~., data = z_train, method = "forward")
summary(regfit_f)
reg.summary = summary(regfit_f)

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

mat.train = model.matrix(y~., data = z_train)
mat.test =  model.matrix(y~., data = z_test)

coef_8 = coef(regfit_f, id = 8)
mean((z_train$y - mat.train[,names(coef_8)]%*% coef_8 )^2)
lm_f_test_mse = mean((z_test$y - mat.test[,names(coef_8)]%*% coef_8 )^2)


#
library(leaps)
regfit_b = regsubsets(y~., data = z_train, method = "backward")
predict(regfit_b)
summary(regfit_b)
reg.summary = summary(regfit_b)

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

coef_8 = coef(regfit_b, id = 8)
mean((z_train$y - mat.train[,names(coef_8)]%*% coef_8 )^2)
lm_b_test_mse = mean((z_test$y - mat.test[,names(coef_8)]%*% coef_8 )^2)

#
fit_inter = lm(y~.^2, data = z_train)

mean((z_train$y - predict(fit_inter))^2)
lm_inter_test_mse = mean((z_test$y - predict(fit_inter, z_test))^2)

# 
library(leaps)
regfit_f = regsubsets(y~.^2, data = z_train, method = "forward")
summary(regfit_f)
reg.summary = summary(regfit_f)

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

mat.train.sq = model.matrix(y~.^2, data = z_train)
mat.test.sq =  model.matrix(y~.^2, data = z_test)

coef_8 = coef(regfit_f, id = 8)
mean((z_train$y - mat.train.sq[,names(coef_8)]%*% coef_8 )^2)
lm_inter_f_test_mse = mean((z_test$y - mat.test.sq[,names(coef_8)]%*% coef_8 )^2)


#
library(leaps)
regfit_b = regsubsets(y~.^2, data = z_train, method = "backward")
predict(regfit_b)
summary(regfit_b)
reg.summary = summary(regfit_b)

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

coef_8 = coef(regfit_b, id = 8)
mean((z_train$y - mat.train.sq[,names(coef_8)]%*% coef_8 )^2)
lm_inter_b_test_mse = mean((z_test$y - mat.test.sq[,names(coef_8)]%*% coef_8 )^2)


library(tree)
fit2 = tree(y~., train)
plot(fit2)
text(fit2)


mean((train$y-predict(fit2, train))^2)
tree_test_mse = mean((test$y-predict(fit2, test))^2)

library(randomForest)

fit3 = randomForest(y~., train, ntree = 100, importance = T)
mean((train$y-predict(fit3, train))^2)
rf_test_mse = mean((test$y-predict(fit3, test))^2)


###############################################
a = data.frame(name = c("lm", "lm_pca", "lasso", "tree", "random_forest"),
           cv_error = c(lm_test_mse, lm_f_test_mse, lm_b_test_mse, lm_inter_test_mse, lm_inter_f_test_mse, lm_inter_b_test_mse, tree_test_mse, rf_test_mse))
ggplot(a) +
  geom_bar(stat = "identity", aes(x = reorder(name, -cv_error), y = cv_error, fill = name)) +
  coord_cartesian(ylim = c(1500, 2500)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = c("tomato", "orange","lightgoldenrod", "steelblue", "lightblue")) +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.text = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 10)))

colnames(test_x)
pca = data.frame(as.matrix(test_x[,c(23:28, 32:39)] - sapply(train[,c(23:28, 32:39)], mean)) %*% a)
colnames(pca) = paste0("age", 1:3)
pca
test = data.frame(test, pca)
fit

random_forest = randomForest(y~., data, ntree = 100)

importance(random_forest)
varImpPlot(random_forest, pch = 16, cex = 1.3)


real$pred = predict(random_forest, real)
colnames(real)
plot(pred~예측총이용량, real)

apply(real, 2, function(x) which(is.na(x)))

write.table(real, file = "./predict.txt", fileEncoding = "UTF-8", row.names = F)




pred = read.table("./predict.txt", fileEncoding = "UTF-8", header = T)
pred
plot(Y~X, data = test)
library(ggplot2)
plot()
p1 = ggplot(test) +
  geom_point(aes(X, Y, col = pred)) +
  scale_color_continuous(low = "white", high = "black")
p1
ggplot(train)
str(test)
library(dplyr)
library(reshape)

quantile(test$pred, 0.95)
test %>%
  filter(pred > 122.6736) %>%
  group_by(구) %>%
  tally() %>%
  arrange(-desc(n)) %>%
  data.frame()

udong_pred = test %>%
  group_by(구) %>%
  summarize(mean_age1 = mean(age1),
            mean_pred = mean(pred)) %>%
  arrange(desc(mean_age1)) %>%
  data.frame()
udong_pred
barplot(mean_age1~구, head(udong_pred, 10))



plot(pred~I(유동20대+유동30대+유동10대), test, pch = 16)
plot(pred~평균_경사, test, pch = 16)
plot(pred~I(버스_승객 + 지하철_승), test, pch = 16)

udong_pred = test %>%
  group_by(구) %>%
  summarize(mean_udong = mean(유동20대+유동30대+유동10대),
            mean_grad = mean(평균_경사),
            mean_dae = mean(버스_승객 + 지하철_승),
            mean_river = mean(거리_하천)) %>%
  data.frame()
udong_pred
barplot(mean_udong~구, 
        data = udong_pred %>% 
          arrange(desc(mean_udong)) %>%
          head(10))

ggplot(udong_pred %>% 
         arrange(desc(mean_grad)) %>%
         tail(10)) +
  geom_bar(stat = "identity", aes(x = reorder(구, mean_grad), y = mean_grad, fill = 구)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL)  +
  ggtitle("일정 반경 내의 평균 경사도")+
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.text.x = element_text(size = 15),
        axis.ticks = element_blank(),
        title = element_text(size = 20),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 10)))

ggplot(udong_pred %>% 
         arrange(desc(mean_udong)) %>%
         head(10)) +
  geom_bar(stat = "identity", aes(x = reorder(구, - mean_udong), y = mean_udong, fill = 구)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  ggtitle("일정 반경 내의 30대 이하 유동인구 수") +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        title = element_text(size = 20),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.text.x = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 10)))

ggplot(udong_pred %>% 
         arrange(desc(mean_dae)) %>%
         head(10)) +
  geom_bar(stat = "identity", aes(x = reorder(구, -mean_dae), y = mean_dae, fill = 구)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  ggtitle("일정 반경 내의 평균 대중교통 이용자 수") +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        title = element_text(size = 20),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.text.x = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 10)))

