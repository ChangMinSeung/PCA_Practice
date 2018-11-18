library(psych) #PCA패키지
library(ggplot2)
library(data.table)

train <- fread("NHLtrain.csv")

#standarduzed(평균_0, 표준편차_1)
train.scale <- scale(train[, -1:-2])

#상관분석
#1.상관분석 그래프
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(train.scale, histogram = TRUE, pch = 19)
#2.상관분석 그래프
install.packages("corrr")
library(corrr)
install.packages("dplyr")
library(dplyr)
train.scale %>% 
  #focus(mpg:drat, mirror = TRUE) %>% 
  correlate() %>% 
  #network_plot()
  rplot()

pca <- principal(train.scale, rotate = "none") #성분추출 

plot(pca$values, type = "b", ylab = "Eigenvalues", xlab = "Component") #성분의 숫자 / 성분의 고윳값

pca.rotate <- principal(train.scale, nfactors = 5, rotate = "varimax"); pca.rotate

pca.scores <- data.frame(pca.rotate$scores) #회전된 성분들의 기여도를 각 팀의 요인 점수로 변환
head(pca.scores)

pca.scores$ppg <- train$ppg

#회귀분석
nhl.lm <- lm(ppg ~ ., data = pca.scores)
summary(nhl.lm)

nhl.lm2 <- lm(ppg ~ RC1 + RC2, data = pca.scores)
summary(nhl.lm2)

sqrt(mean(nhl.lm2$residuals^2)) #Root Mean Squared

test <- fread("NHLtest.csv")
test.scores <- data.frame(psych::predict.psych(pca.rotate, test[, c(-1:-2)])) #테스트 데이터의 비율을 자동으로 맞춤
test.scores$pred <- predict(nhl.lm2, test.scores)

test.scores$ppg <- test$ppg
test.scores$team <- test$Team

  #시각화
P <- ggplot(test.scores, aes(x = pred, y = ppg, label = team))
P + geom_point() +
  geom_text(size=3.5, hjust=0.4, vjust = -0.9, angle = 35) +
  xlim(0.75, 1.5) + ylim(0.5, 1.6) +
  stat_smooth(method = "lm", se = FALSE)

resid <- test.scores$ppg - test.scores$pred
sqrt(mean(resid^2))