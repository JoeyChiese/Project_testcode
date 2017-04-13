#http://mp.weixin.qq.com/s?src=3&timestamp=1491877995&ver=1&signature=bIMSYKqdZ-4csqmJmB-4zitG21JoUnpPEkuPB7aa8ZmNdZ1HsReksQUTANH2O1MI9LY4CrMylPYB8AsORUIGUVxGKJ8kdCpDthJ70kydY2J*8kedFJ2xQByCkk3qzrs-0iUcEfbCkXFT-g2WAeuVvTLbDYix2*67WjdeSgxa92Y=

library(devtools)
#install_github("tomasgreif/riv",force = TRUE)
library(woe)
#install.packages("drat", repos="https://cran.rstudio.com")
library(drat)
#drat:::addRepo("dmlc")
#install.packages("mxnet")
library(mxnet)
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)

#install.packages("xgboost")
#require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
str(train)
dim(train$data)
dim(test$data)

#数据被存储为“dgCMatrix”格式，这是一种稀疏的矩阵
class(train$data)[1]
class(train$label)

bstSparse <- xgboost(data =train$data, label = train$label, max.depth = 2, 
                     eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")

#稠密矩阵Dense matrix
bstDense <- xgboost(data = as.matrix(train$data),label = train$label, max.depth = 2, eta = 1, 
                    nthread = 2, nround = 2, objective= "binary:logistic")
#xgb.DMatrix
dtrain <- xgb.DMatrix(data =train$data, label = train$label)
bstDMatrix <- xgboost(data =dtrain, max.depth = 2, eta = 1, nthread = 2,
                      nround = 2, objective = "binary:logistic")
#繁琐的选项Verbose option
## verbose = 0, no message
# verbose = 1, print evaluationmetric
# verbose = 2, also printinformation about tree
bst <- xgboost(data = dtrain,max.depth = 2, eta = 1, nthread = 2, nround = 2, 
               objective = "binary:logistic",verbose = 0)

#prediction
pred <- predict(bst, test$data)
# size of the prediction vector
print(length(pred))
# limit display of predictions tothe first 10
print(head(pred))
#将回归转换成二分类形式Transform the regression in abinary classification
prediction <-as.numeric(pred >0.5)
print(head(prediction))
#衡量模型的性能Measuring model performance
#度量标准：平均误差（average error）
err <-mean(as.numeric(pred >0.5) != test$label)
print(paste("test-error=", err))

#高级特征Advanced features
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
#调参
watchlist <-list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread =2, 
                  nround=6, watchlist=watchlist, objective ="binary:logistic")

#特定的标准，或者使用多种评估指标
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread =2, nround=5, 
                 watchlist=watchlist, eval.metric ="logloss",eval.metric="error")

#线性提升Linear boosting “ booster = "gblinear"”参数（并且去掉“eta”参数）
bst <- xgb.train(data=dtrain, booster ="gblinear", max.depth=2, nthread =2, nround=5, 
                 watchlist=watchlist, eval.metric ="error", eval.metric ="logloss")
#warning 决策树在匹配预测和结果的非线性关系上效果更好,没有比用线性算法去匹配线性关系更好的方式,对比

#操作xgb.DMatrix Manipulating xgb.DMatrix
#保存/加载Save / Load
xgb.DMatrix.save(dtrain, "dtrain.buffer")
# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")
bst <- xgb.train(data=dtrain2, max.depth=2, eta=1, nthread =2, nround=5, 
                 watchlist=watchlist, objective ="binary:logistic")

#信息萃取Information extraction
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <-as.numeric(sum(as.integer(pred >0.5)!= label))/length(label)
#err <-mean(as.integer(pred >0.5)!= label)
print(paste("test-error=", err))
#View feature importance/influence fromthe learnt model
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#从一个模型中查看树 View the trees from a model
xgb.dump(bst, with.stats =T)
#xgb.plot.tree”将你模型中的树画出来
#install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(model = bst,feature_names = colnames(agaricus.train$data),fname="路径")

# save model to binary local file
xgb.save(bst, "xgboost.model")

# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)
# And now the test
print(paste("sum(abs(pred2-pred))=",sum(abs(pred2-pred))))

# save model to R's raw vector
rawVec <- xgb.save.raw(bst)
# print class
print(class(rawVec))

# load binary model to R
bst3 <- xgb.load(rawVec)
pred3 <- predict(bst3, test$data)
# pred2 should be identical to pred
print(paste("sum(abs(pred3-pred))=",sum(abs(pred2-pred))))

# do cross validation with prediction values for each fold   验证交叉
res <- xgb.cv( data = dtrain, nrounds =2 , nfold = 5,
               max_depth = 3, eta = 1, objective = "binary:logistic",
               metrics = list("rmse","auc"),prediction = TRUE)  
res$evaluation_log  
length(res$pred)  

#线性模型替代树模型
# you can also set lambda_bias which is L2 regularizer on the bias term  
param <- list(objective = "binary:logistic", booster = "gblinear",  
              nthread = 2, alpha = 0.0001, lambda = 1)  









