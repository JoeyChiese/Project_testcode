#导入数据
data("iris")
# 特征矩阵
iris.data <- iris[, -length(iris)]
# 目标向量
iris.targer <- iris[, length(iris)]


#数据预处理
#1标准化（要求数据符合正态性）
scale(iris.data, center = TRUE, scale = TRUE)
# 或者运用BBmisc包中的normalize函数
library(BBmisc)
normalize(iris.data)#中心化
#2区间放缩法
# 依据公式构建区间放缩函数
maxmin <- function(col) {
  maxmin <- (col - min(col))/(max(col) - min(col))
  return(maxmin)}
maxmin(iris.data)

#3归一化
#此处的归一化是指依照特征矩阵的行处理数据，
#其目的在于样本向量在点乘运算或其他核函数计算相似性时，
#拥有统一的标准，也就是说都转化为“单位向量”.归一化后样本各属性的平方和为1.
norm <- function(data) {
  norm = apply(data, 1, function(x) {
    x/sqrt(sum(x^2))
  })
  norm = t(norm)
  return(norm)}

norm(iris.data)

#标准化是依照特征矩阵的列处理数据，其通过求z-score的方法，转换为标准正态分布。
#而归一化是将样本的特征值转换到同一量纲下把数据映射到[0,1]区间内，因此区间放缩法是归一化的一种

#对定量特征二值化
#以某个值为阈值，转换为0，1变量。
bina <- function(data, threshold) {
  ifelse(data > threshold, 1, 0)}
bina(iris.data, threshold = 3)

#对定性特征哑编码
library(caret)
var <- dummyVars(~Species, data = iris)#离散变量哑变量处理
predict(var, iris["Species"])

#缺失值填补
#新增一个样本，缺失值为NAN，并对缺失值进行填补
new = rep(NA, 4)
iris.data <- rbind(new, iris.data)
library(Hmisc)
impute(iris.data,fun= mean)

#数据变换
# 多项式转换
library(dplyr)
iris.data <- iris.data %>% 
  mutate(x3 = Sepal.Length * Sepal.Width)# 对数函数转换 创建新的列和变量
iris.data <- iris.data %>% 
  mutate_each(funs(log1p))


#特征选择
#Filter法(过滤法)
#按照变量内部特征或者相关性对各个特征进行评分，
#设定阈值或者待选择阈值的个数选择特征.与特定的学习算法无关，
#因此具有较好的通用性，作为特征的预筛选器非常合适。
#缺点主要是由于算法的评价标准独立于特定的学习算法，
#所选的特征子集在分类准确率方面通常低于Wrapper方法。

#1.方差选择法
#计算各个特征的方差，然后根据阈值，选择方差大于阈值的特征.
library(mlr)
# 创建task
train.task <- makeClassifTask(data = iris, target = "Species")
# 查看变量选择可选方法listFilterMethods()
# 选择计算方差，进行特征选择
var_imp <- generateFilterValuesData(train.task, method = "variance", nselect = 3)
var_imp
# 对衡量特征指标进行绘图
plotFilterValues(var_imp, feat.type.cols = TRUE, n.show = 3)

#相关系数法
library(mlr)
library(Rfast)
# 创建task
iris.data<- iris.data[-1,]
train.task <- makeRegrTask(data = iris.data, target = "Sepal.Width")
# 查看变量选择可选方法
listFilterMethods()
# 选择pearson相关系数，进行特征选择 也可以选择秩相关系数(method =# rank.correlation)
var_imp <- generateFilterValuesData(train.task, method = "linear.correlation")
var_imp
# 对相关系数进行绘图
library(corrplot)
corrplot(cor(iris.data), order = "hclust")


#计算卡方值
#经典的卡方检验是检验定性自变量对定性因变量的相关性，
#考虑自变量等于i且因变量等于j的样本频数的观察值与期望的差距,
#这个统计量的含义简而言之就是自变量对因变量的相关性.

library(mlr)
# 创建task
train.task <- makeClassifTask(data = iris, target = "Species")
# 查看变量选择可选方法
listFilterMethods()
# 选择计算卡方值，进行特征选择
var_imp <- generateFilterValuesData(train.task, method = "chi.squared")
var_imp
# 对衡量特征指标进行绘图
plotFilterValues(var_imp, feat.type.cols = TRUE)


#互信息
#经典的互信息也是评价定性自变量对定性因变量的相关性的，
#可以看成是一个随机变量中包含的关于另一个随机变量的信息量.
library(mlr)
# 创建task
train.task <- makeClassifTask(data = iris, target = "Species")
# 查看变量选择可选方法
listFilterMethods()# 选择计算信息增益，进行特征选择
var_imp <- generateFilterValuesData(train.task, method = "information.gain")
var_imp
# 对衡量特征指标进行绘图
plotFilterValues(var_imp, feat.type.cols = TRUE)



#Wrapper法（封装法）
#封装式特征选择是利用学习算法的性能来评价特征子集的优劣。
#因此，对于一个待评价的特征子集，Wrapper方法需要训练一个分类器,
#根据分类器的性能对该特征子集进行评价，
#学习算法包括决策树、神经网络、贝叶斯分类器、近邻法以及支持向量机等。
#Wrapper方法缺点主要是特征通用性不强，当改变学习算法时，需要针对该学习算法重新进行特征选择。
#1.递归特征消除法
#递归消除特征法使用一个模型来进行多轮训练，
#每轮训练后，消除若干权值系数的特征，再基于新的特征集进行下一轮训练。
#Logistic回归的R实现详见本微信公众号历史文章：Logistic回归详解（三）——变量选择部分。
# 构建Logistic回归模型
library(MASS)
m <- glm(Species ~ ., data = iris, family = "binomial")
# 运用step函数进行变量选择
selecting <- step(m, direction = "backward")


#Embedded（集成法）
#在集成法特征选择中，特征选择算法本身作为组成部分嵌入到学习算法里。
#最典型的即决策树算法。包括基于惩罚项的特征选择法和基于树模型的特征选择法
#1.基于惩罚项的特征选择法
#其中R实现详见本微信公众号历史文章:正则化及其R实现。
# 转换变量类型
iris.matrix <- as.matrix(iris.data)
target <- as.numeric(iris.targer)
# Lasso回归
library(glmnet)
# alpha = 1为Lasso回归，alpha=0为岭回归
r2 <- glmnet(iris.matrix, target, family = "multinomial", alpha = 1)
# 通过10fold交叉验证获得最优lambda参数
r2.cv <- cv.glmnet(iris.matrix, target, family = "multinomial", alpha = 1, nfolds = 10)
plot(r2.cv)
# 根据获得的lambda.min值，拟合最优模型r2.min
r2.min <- glmnet(iris.matrix, target, family = "multinomial", alpha = 1, lambda = r2.cv$lambda.min)
r2.min_coef <- coef(r2.min)#


#基于树模型的特征选择法
library(mlr)
train.task <- makeClassifTask(data = iris, target = "Species")
# 查看可选模型参数
listLearners("classif", "multiclass")[c("class", "package")]
# 创建机器学习模型
gbm_learner <- makeLearner("classif.gbm", predict.type = "response")
# 设定模型参数
gbm_learner$par.vals <- list(laplace = 1)
# 训练和预测
nB_models <- mlr::train(gbm_learner, train.task)
nB_predict <- predict(nB_models, train.task)
# 模型评估
nB_prediction <- nB_predict$data$response
# 混淆矩阵
dCM <- confusionMatrix(iris$Species, nB_prediction)


#降维
#降维方法除了以上提到的基于L1惩罚项的模型以外，另外还有主成分分析法（PCA）和线性判别分析（LDA），
#线性判别分析本身也是一个分类模型。PCA和LDA有很多的相似点，
#其本质是要将原始的样本映射到维度更低的样本空间中，但是PCA和LDA的映射目标不一样：
#PCA是为了让映射后的样本具有最大的发散性；而LDA是为了让映射后的样本有最好的分类性能。
#所以说PCA是一种无监督的降维方法，而LDA是一种有监督的降维方法。

#主成分分析（PCA）
# 主成分分析
iris.data <- iris.data[-1,]
iris.pc <- prcomp(iris.data)
summary(iris.pc)
# 主成分分析陡坡图
plot(iris.pc, type = "l", main = "PCA陡坡图")

#线性判别分析（LDA）
library(MASS)
# 以公式格式进行线性判别
fit_lda = lda(Species ~ ., data = iris)
names(fit_lda)
fit_lda$means

