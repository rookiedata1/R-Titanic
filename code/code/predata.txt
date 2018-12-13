# https://blog.csdn.net/xpexia/article/details/79102377
# https://mirrors.tuna.tsinghua.edu.cn/CRAN/
#数据的导入和整理
library(ggplot2)

setwd("E:/Titanic/data/")
train <- read.csv("train.csv", stringsAsFactors = F) # 891 obs, 11 variables
test <- read.csv("test.csv", stringsAsFactors = F)   # 418 obs, 10 variables
#查看数据的概述
str(train)
str(test)
#什么因素决定船上人的生与死
#头等舱的重要性
library(ggplot2)
pclass_s<-table(train$survived, train$pclass)
pclass_s_prop<-prop.table(pclass_s,2)
ggplot(data = train, aes(x = pclass, fill = factor(survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(1:3)) + labs(x = 'Pclass')
#女士优先
sex_s<-table(train$survived,train$sex)
sex_s_prop<-prop.table(sex_s,2)
ggplot(data = train, aes(x = sex, fill = factor(survived)))+geom_bar(stat='count', position='dodge')
#年龄的影响
agedata<-as.numeric(unlist(train$age))
age_s<-table(train$survived,cut(agedata,breaks = c(0, 15, 30, 45, 60, 75, 90), labels = c('kids', 'teenagers', 'prime', 'middle', 'agedness', 'senium' )))
age_s_prop<-prop.table(age_s,2)
ggplot(data = data.frame(train$survived, train$age), aes(x = cut(train$age, breaks = c(0, 15, 30, 45, 60, 75, 90)), fill = factor(train.survived)))+geom_bar(stat='count', position='dodge') + labs(x = 'Age') +  scale_x_discrete(labels = c('kids', 'teenagers', 'prime', 'middle', 'agedness', 'senium'))
#亲情的好坏
sibsp_s<-table(train$survived,train$sibsp)
parch_s<-table(train$survived,train$parch)
sibsp_s_prop <- prop.table(sibsp_s, 2)
parch_s_prop <- prop.table(parch_s, 2)
ggplot(data = train, aes(x = train$sibsp, fill = factor(survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:8)) + labs(x = 'Sibsp')
ggplot(data = train, aes(x = train$parch, fill = factor(survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:6)) + labs(x = 'Parch')
families<-train$sibsp+train$parch
ggplot(data = train, aes(x = families, fill = factor(survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:10)) + labs(x = 'Families')
#现金和港口
Faredata <- as.numeric(unlist(train$fare))
Fare_S <- table(train$survived, cut(Faredata, breaks = c(0, 50, 100, 600), labels = c('poor', 'middle', 'rich')))
Fare_S_prop <- prop.table(Fare_S, 2)
ggplot(data = data.frame(train$survived, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(traindata.Survival)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich'))
Embarked_S <- table(train$survived, train$embarked)
Embarked_S_prop <- prop.table(Embarked_S, 2)
ggplot(data = train, aes(x = embarked, fill = factor(survived)))+geom_bar(stat='count', position='dodge')

# #对Fare这个变量进行分析
# plot_FP <- ggplot(data = data.frame(traindata$Pclass, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(traindata.Pclass)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_FS <- ggplot(data = data.frame(traindata$Sex, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(traindata.Sex)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_FA <- ggplot(data = data.frame(Agedata, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(Age_Fare)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_FF <- ggplot(data = data.frame(Families, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(Families)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich')) + scale_fill_brewer(palette = "Blues") + theme_bw()

# library(grid)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(plot_FP, vp = vplayout(1, 1))
# print(plot_FS, vp = vplayout(1, 2))
# print(plot_FA, vp = vplayout(2, 1))
# print(plot_FF, vp = vplayout(2, 2))

# plot_EP <- ggplot(data = traindata, aes(x = (Embarked), fill = factor(Pclass)))+geom_bar(stat='count', position='dodge') + labs(x = 'Embarked') + scale_x_discrete(labels = c('Cherbourg-Octeville', 'Queenstown', 'Southampton')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_ES <- ggplot(data = traindata, aes(x = (Embarked), fill = factor(Sex)))+geom_bar(stat='count', position='dodge') + labs(x = 'Embarked') + scale_x_discrete(labels = c('Cherbourg-Octeville', 'Queenstown', 'Southampton')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_EA <- ggplot(data = data.frame(Agedata, traindata$Embarked), aes(x = (traindata.Embarked), fill = factor(Age_cut)))+geom_bar(stat='count', position='dodge') + labs(x = 'Embarked') + scale_x_discrete(labels = c('Cherbourg-Octeville', 'Queenstown', 'Southampton')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# plot_EF <- ggplot(data = data.frame(Families, traindata$Embarked), aes(x = (traindata.Embarked), fill = factor(Families)))+geom_bar(stat='count', position='dodge') + labs(x = 'Embarked') + scale_x_discrete(labels = c('Cherbourg-Octeville', 'Queenstown', 'Southampton')) + scale_fill_brewer(palette = "Blues") + theme_bw()
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(plot_EP, vp = vplayout(1, 1))
# print(plot_ES, vp = vplayout(1, 2))
# print(plot_EA, vp = vplayout(2, 1))
# print(plot_EF, vp = vplayout(2, 2))


