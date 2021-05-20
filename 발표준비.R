rm(list = ls())
gc()

data = read.csv("./대여소_결합(0716).csv")
real = read.csv("./대여소후보_결합(0717).csv")

data[,1:2] = NULL
real[,1] = NULL

na1 = unique(apply(data, 2, function(x) which(is.na(x))))[[2]]
na2 = unique(apply(real, 2, function(x) which(is.na(x))))[[2]]

data = data[-na1, ]
real = real[-na2, ]


y = data$대여 + data$반납
x = data[,9:47]

data = data.frame(x, y)
real_x = real[,8:46]

str(data)

set.seed(1)
k = 10
fold = sample(1:k, nrow(data), replace = T)

# 중회귀모형(1차항만)
fit = lm(y ~., data)
summary(fit)

library(car)
vif(fit)

train_err = c()
test_err = c()
for(i in 1:k){
  fit = lm(y ~., data = data[fold != i,])
  train_err[i] = mean((y[fold != i] - 
                         ifelse(predict(fit, data[fold != i,]) >= 0, predict(fit, data[fold != i,]), 0) )^2)
  test_err[i] = mean((y[fold == i] - 
                        ifelse(predict(fit, data[fold == i,]) >= 0, predict(fit, data[fold == i,]), 0))^2)
  
}

train_err_lm = mean(train_err)
test_err_lm = mean(test_err)
c(train_err_lm, test_err_lm)


# 중회귀모형 stepwise selection(1차)

fit = lm(y ~., data)
summary(fit)
step_aic = step(fit, direction = "both", k = 2)
summary(step_aic)
coef(step_aic)

step_bic = step(fit, direction = "both", k = log(nrow(data)))
summary(step_bic)
coef(step_bic)

library(car)
vif(step_aic)
vif(step_bic)


train_err_a = c()
test_err_a = c()
train_err_b = c()
test_err_b = c()

for(i in 1:k){
  fit = lm(y ~ 1, data = data[fold != i,])
  biggest = formula(lm(y~., data[fold != i,]))
  
  step_aic = step(fit, direction = "both", k = 2, scope = biggest)
  step_bic = step(fit, direction = "both", k = log(nrow(data[fold != i,])), scope = biggest)
  
  train_err_a[i] = mean((y[fold != i] - 
                           ifelse(predict(step_aic, data[fold != i,]) >= 0, predict(step_aic, data[fold != i,]), 0) )^2)
  test_err_a[i] = mean((y[fold == i] - 
                          ifelse(predict(step_aic, data[fold == i,]) >= 0, predict(step_aic, data[fold == i,]), 0) )^2)

  train_err_b[i] = mean((y[fold != i] - 
                           ifelse(predict(step_bic, data[fold != i,]) >= 0, predict(step_bic, data[fold != i,]), 0) )^2)
  test_err_b[i] = mean((y[fold == i] -  
                          ifelse(predict(step_bic, data[fold == i,]) >= 0, predict(step_bic, data[fold == i,]), 0) )^2)
  
}

train_err_step_aic = mean(train_err_a)
test_err_step_aic = mean(test_err_a)

train_err_step_bic = mean(train_err_b)
test_err_step_bic = mean(test_err_b)

c(train_err_step_aic, test_err_step_aic)
c(train_err_step_bic, test_err_step_bic)



# 중회귀모형(2차)
fit_quad = lm(y ~.^2, data)
summary(fit_quad)

train_err = c()
test_err = c()
for(i in 1:k){
  fit_quad = lm(y ~.^2, data = data[fold != i,])
  train_err[i] = mean((y[fold != i] -  
                         ifelse(predict(fit_quad, data[fold != i,]) >= 0, predict(fit_quad, data[fold != i,]), 0) )^2)
  test_err[i] = mean((y[fold == i] -  
                        ifelse(predict(fit_quad, data[fold == i,]) >= 0, predict(fit_quad, data[fold == i,]), 0) )^2)
  
}

train_err_quad = mean(train_err)
test_err_quad = mean(test_err)
c(train_err_quad, test_err_quad)


# 중회귀모형 stepwise selection(2차)
fit_min = lm(y ~ 1, data)
biggest = formula(lm(y~.^2, data))
step_aic = step(fit_min, direction = "both", k = 2, scope = biggest)
step_bic = step(fit_min, direction = "both", k = log(nrow(data)), scope = biggest)


train_err_a = c()
test_err_a = c()
train_err_b = c()
test_err_b = c()


for(i in 1:k){
  fit = lm(y ~ 1, data = data[fold != i,])
  biggest = formula(lm(y ~ .^2, data[fold != i,]))
  
  step_aic = step(fit, direction = "both", k = 2, scope = biggest)
  step_bic = step(fit, direction = "both", k = log(nrow(data[fold != i,])), scope = biggest)
  
  train_err_a[i] = mean((y[fold != i] - 
                           ifelse(predict(step_aic, data[fold != i,]) >= 0, predict(step_aic, data[fold != i,]), 0) )^2)
  test_err_a[i] = mean((y[fold == i] - 
                          ifelse(predict(step_aic, data[fold == i,]) >= 0, predict(step_aic, data[fold == i,]), 0) )^2)
  
  train_err_b[i] = mean((y[fold != i] - 
                           ifelse(predict(step_bic, data[fold != i,]) >= 0, predict(step_bic, data[fold != i,]), 0) )^2)
  test_err_b[i] = mean((y[fold == i] -  
                          ifelse(predict(step_bic, data[fold == i,]) >= 0, predict(step_bic, data[fold == i,]), 0) )^2)
  
}

train_err_step_q_aic = mean(train_err_a)
test_err_step_q_aic = mean(test_err_a)

train_err_step_q_bic = mean(train_err_b)
test_err_step_q_bic = mean(test_err_b)

c(train_err_step_q_aic, test_err_step_q_aic)
c(train_err_step_q_bic, test_err_step_q_bic)





# 중회귀모형(2차, 교호작용항 x)

library(dplyr)

formula = paste0(names(data)[-40]," + I(", names(data)[-40], "^2) +", collapse="") %>%
  paste("y ~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
fit_quad_noint = lm(formula, data)

train_err = c()
test_err = c()
for(i in 1:k){
  fit_quad_noint = lm(formula, data = data[fold != i,])
  train_err[i] = mean((y[fold != i] -  
                         ifelse(predict(fit_quad_noint, data[fold != i,]) >= 0, predict(fit_quad_noint, data[fold != i,]), 0) )^2)
  test_err[i] = mean((y[fold == i] -  
                        ifelse(predict(fit_quad_noint, data[fold == i,]) >= 0, predict(fit_quad_noint, data[fold == i,]), 0) )^2)
  
}

train_err_quad_noint = mean(train_err)
test_err_quad_noint = mean(test_err)
c(train_err_quad_noint, test_err_quad_noint)

# 중회귀모형(2차, 교호작용항 x), stepwise selection

train_err_a = c()
test_err_a = c()
train_err_b = c()
test_err_b = c()


for(i in 1:k){
  fit = lm(y ~ 1, data = data[fold != i,])

  step_aic = step(fit, direction = "both", k = 2, scope = formula)
  step_bic = step(fit, direction = "both", k = log(nrow(data[fold != i,])), scope = formula)
  
  train_err_a[i] = mean((y[fold != i] - 
                           ifelse(predict(step_aic, data[fold != i,]) >= 0, predict(step_aic, data[fold != i,]), 0) )^2)
  test_err_a[i] = mean((y[fold == i] - 
                          ifelse(predict(step_aic, data[fold == i,]) >= 0, predict(step_aic, data[fold == i,]), 0) )^2)
  
  train_err_b[i] = mean((y[fold != i] - 
                           ifelse(predict(step_bic, data[fold != i,]) >= 0, predict(step_bic, data[fold != i,]), 0) )^2)
  test_err_b[i] = mean((y[fold == i] -  
                          ifelse(predict(step_bic, data[fold == i,]) >= 0, predict(step_bic, data[fold == i,]), 0) )^2)
  
}

train_err_step_q_noint_aic = mean(train_err_a)
test_err_step_q_noint_aic = mean(test_err_a)

train_err_step_q_noint_bic = mean(train_err_b)
test_err_step_q_noint_bic = mean(test_err_b)

c(train_err_step_q_noint_aic, test_err_step_q_noint_aic)
c(train_err_step_q_noint_bic, test_err_step_q_noint_bic)


# 선형회귀 결과
c(train_err_lm, train_err_step_aic, train_err_step_bic, train_err_quad, train_err_step_q_aic, train_err_step_q_bic,
  train_err_quad_noint, train_err_step_q_noint_aic, train_err_step_q_noint_bic)
c(test_err_lm, test_err_step_aic, test_err_step_bic, test_err_quad, test_err_step_q_aic, test_err_step_q_bic,
  test_err_quad_noint, test_err_step_q_noint_aic, test_err_step_q_noint_bic)



# randomforest
?randomForest
library(randomForest)

fit_tree = randomForest(y~., data, importance = T)
varImpPlot(fit_tree)

train_err = c()
test_err = c()
for(i in 1:k){
  fit_tree = randomForest(y ~., data = data[fold != i,])
  train_err[i] = mean((y[fold != i] -  
                         ifelse(predict(fit_tree, data[fold != i,]) >= 0, predict(fit_tree, data[fold != i,]), 0) )^2)
  test_err[i] = mean((y[fold == i] -  
                        ifelse(predict(fit_tree, data[fold == i,]) >= 0, predict(fit_tree, data[fold == i,]), 0) )^2)
  
}

train_err_rf = mean(train_err)
test_err_rf = mean(test_err)
c(train_err_rf, test_err_rf)



# random forest가 가장 성능이 좋은 것으로 나타남

upper_20 = data$y >= quantile(data$y, 0.8)

train_err = c()
test_err = c()
for(i in 1:k){
  fit_tree = randomForest(y ~., data = data[fold != i,])
  train_pred = predict(fit_tree, data[fold != i,])
  test_pred = predict(fit_tree, data[fold == i,])
  train_upper = train_pred > quantile(train_pred, 0.8)
  test_upper = test_pred > quantile(test_pred, 0.8)
  train_err[i] = mean(train_upper != upper_20[fold != i])
  test_err[i] = mean(test_upper != upper_20[fold == i])
}

train_class_err_rf = mean(train_err)
test_class_err_rf = mean(test_err)
c(train_class_err_rf, test_class_err_rf)


library(dplyr)
formula = paste0(names(data)[-40]," + I(", names(data)[-40], "^2) +", collapse="") %>%
  paste("y ~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula


train_err = c()
test_err = c()
for(i in 1:k){
  fit_quad_noint = lm(formula, data = data[fold != i,])
  train_pred = predict(fit_quad_noint, data[fold != i,])
  test_pred = predict(fit_quad_noint, data[fold == i,])
  train_upper = train_pred > quantile(train_pred, 0.8)
  test_upper = test_pred > quantile(test_pred, 0.8)
  train_err[i] = mean(train_upper != upper_20[fold != i])
  test_err[i] = mean(test_upper != upper_20[fold == i])
}

train_class_err_lm = mean(train_err)
test_class_err_lm = mean(test_err)
c(train_class_err_lm, test_class_err_lm)

##################

now = read.csv("./대여소_결합(0716).csv")
now = now[- unique(apply(now, 2, function(x) which(is.na(x))))[[2]], ]
fit_rf = randomForest(y~., data, importance = T)

library(dplyr)

now$pred = predict(fit_rf)
now$y = now$대여 + now$반납
now$res = now$y - now$pred





plot(pred~y, now)
plot(now$y - now$pred)
plot(res~y, now)
plot(res~pred, now)

now[head(order(abs(now$res), decreasing = T)), ]
now[head(order(abs(now$res))), ]


hist(now$res)


now
real$pred = predict(fit_rf, real)
real
rf_imp = importance(fit_rf)
rf_imp = as.data.frame(rf_imp)
colnames(rf_imp)[1] = "IncMSE"

rf_imp = rf_imp %>% 
  arrange(desc(IncMSE))
rf_imp$variable = rownames(rf_imp)
library(ggplot2)
library(stringr)
colors()[str_detect(colors(), "green")]

ggplot(head(rf_imp, 10)) +
  geom_segment(aes(x = reorder(variable, IncMSE), y = IncMSE, xend = reorder(variable, IncMSE), yend = 0), 
               col = "seagreen", linetype = "longdash") +
  geom_point(aes(x = reorder(variable, IncMSE), y = IncMSE), 
             col = "seagreen", size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), name = "%IncMSE") +
  scale_x_discrete(name = NULL) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.y = element_line(color = "white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "grey", size = 1),
        strip.background = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 15),
        title = element_text(size = 20))


?scale_y_continuous
colnames(now)
str(real)
write.table(now[,- c(1:2)], file = "./현거치대.txt", fileEncoding = "UTF-8", row.names = F)
write.table(real, file = "./신규거치대.txt", fileEncoding = "UTF-8", row.names = F)


mean_bar = function(var, value, i = 0, length = 11){
  library(dplyr)
  library(ggplot2)
  a = now
  
  breaks = quantile(a[,var], seq(0, 1, length = length))
  a$cut = cut(a[,var], breaks = breaks, include.lowest = T)
  
  if(i == 0){
    i = which.max(breaks>value) - 1
  }

  
  a = a %>% 
    group_by(cut) %>%
    summarize(pred = mean(pred))
  # a = rbind(a, NA)
  # 
  # a$cut = factor(formatC(breaks, format = "e", digit = 2), levels = formatC(breaks, format = "e", digit = 2))
  
  
  fill = rep("white", length)
  fill[i] = "mediumseagreen"
  col = rep("mediumseagreen", length)
  col[i] = NA
    
  ggplot(a) +
    geom_bar(stat = "identity", aes(x = cut, y = pred, fill = cut, col = cut))+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), name = "평균 예측값", limits = c(0, 110)) +
    scale_x_discrete(expand = c(0.1, 0)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.grid.major.y = element_line(color = "lightgrey"),
          panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
          axis.ticks = element_blank(),
          axis.line.x = element_line(color = "grey", size = 1,
                                     arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "both")),
          strip.background = element_blank(),
          axis.text.x = element_blank(),
          # axis.text.x = element_text(hjust = 3, vjust = 3, angle = 60),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 15),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_fill_manual(values = fill) + 
    scale_color_manual(values = col) +
      labs(x = "낮음                                                  높음",
         title = var)

  
}
summary(now)

mean_bar("평균_경사", i = 1:10)
mean_bar("거리_특화", i = 1:10)
mean_bar("거리_관광", i = 1:10)
str(now)
str(real)

# 885 1678 
colnames(now)
now[now$rent_place == "ST-885",]
mean_bar("평균_경사", 2.079963)
mean_bar("유동20대", 2475.802)
mean_bar("유동10대", 1076.615)

now[now$rent_place == "ST-1678",]
mean_bar("평균_경사", 2.320095)
mean_bar("유동20대", 2475.802)
mean_bar("유동10대", 1076.615)


# 840 1681 891

now[now$rent_place == "ST-840",]
mean_bar("평균_경사", 0.4377)
mean_bar("유동20대", 10516)

now[now$rent_place == "ST-891",]

now[now$rent_place == "ST-1681",]
mean_bar("평균_경사", 0.8331)
mean_bar("유동20대", 4623)
# 관광지와의 거리에 영향


# EDA
colnames(now)
cor(now[,c(51, 42:49)])[,1]

colnames(now)
cor(now$y, apply(now[,33:38], 1, sum))
cor(now$y, apply(now[,42:49], 1, sum))
a = now
a$총주거 = apply(now[,33:38], 1, sum)
a$총유동 = apply(now[,42:49], 1, sum)

write.table(a, file = "./a.txt", fileEncoding = "UTF-8", row.names = F)
colnames(now)
cor(now$pred, now$거리_특화)
cor(now$pred, now$평균_경사)
cor(now$pred, now$유동20대)
