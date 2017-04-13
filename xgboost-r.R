#http://mp.weixin.qq.com/s?src=3&timestamp=1491877945&ver=1&signature=iwXdAj0wsAYDFMtg*d7qIgRU7lIRt7DkAmBT438xiyhzJNEcaYa8SRvJutWdVHURI*jEN9o1szrZs0lOimbCuP7cjoYvLUFsmOoWi5h1ZcXxBokk9UXCDUGUbG4erx73bsBaNdsdwP9Ug1rn53seuj3b52UVsQVGZSRV000PEGU=
library(xgboost)
setwd('C:/Users/admin/Desktop/DataCastle-Solution-master/code/data')
# read data
train=read.csv('train_x.csv')
test=read.csv('test_x.csv')
train.y=read.csv('train_y.csv')
ft=read.csv('features_type.csv')
fn.cat=as.character(ft[ft[,2]=='category',1])#分类index
fn.num=as.character(ft[ft[,2]=='numeric',1])#num index

# create dummy variables
temp.train=data.frame(rep(0,nrow(train)))
temp.test=data.frame(rep(0,nrow(test)))
for(f in fn.cat){
  levels=unique(train[,f])
  col.train=data.frame(factor(train[,f],levels=levels))#转成factor
  col.test=data.frame(factor(test[,f],levels=levels))
  colnames(col.train)=f
  colnames(col.test)=f
  temp.train=cbind(temp.train,model.matrix(as.formula(paste0('~',f,'-1')),data=col.train))#转成sparse matrix
  temp.train[,paste0(f,'-1')]=NULL #去掉本身
  temp.test=cbind(temp.test,model.matrix(as.formula(paste0('~',f,'-1')),data=col.test))
  temp.test[,paste0(f,'-1')]=NULL
}
temp.train[,1]=NULL#去掉第一列
temp.test[,1]=NULL
train.new=as.matrix(data.matrix(cbind(train[,c('uid',fn.num)],temp.train)),sparse=T)#合并原始数据和sparse matrix
test.new=as.matrix(data.matrix(cbind(test[,c('uid',fn.num)],temp.test)),sparse=T)


# fit xgboost model

dtrain=xgb.DMatrix(data=train.new[,-1],label=1-train.y$y)#去掉uid,label opposite
dtest= xgb.DMatrix(data=test.new[,-1])

model=xgb.train(booster='gbtree',
                objective='binary:logistic',
                scale_pos_weight=8.7,#Control the balance of positive and negative weights, useful for unbalanced classes
                gamma=0,#minimum loss reduction
                lambda=700,#regularization term on weights
                subsample=0.7,#subsample ratio of the training instance
                colsample_bytree=0.30,#subsample ratio of columns when constructing each tree
                min_child_weight=5,#minimum sum of instance weight (hessian) needed in a child
                max_depth=8,#maximum depth of a tree
                eta=0.02,#the learning rate
                data=dtrain,
                nrounds=1520,#the max number of iterations
                metrics='auc',
                nthread=8#number of parallel threads used to run xgboost 
                )

# predict probabilities
pred=1-predict(model,dtest)
#write.csv(data.frame('uid'=test.new[,1],'score'=pred),file='R_7199the max number of iterations.csv',row.names=F)
#让我们开始寻找实际的树是什么样子吧
model_tree <- xgb.dump(model, with.stats = T)
model_tree[1:10] #This statement prints top 10 nodes of the model
# 获得特征的真实名称
names <- dimnames(dtrain)[[2]]
# 计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = model)
# 制图
barplot(head(importance_matrix[,1]))#版本过低


#View feature importance/influence fromthe learnt model
#importance_matrix <- xgb.importance(model = bst)
#print(importance_matrix)
#xgb.plot.importance(importance_matrix = importance_matrix)

#卡方检验
test <- chisq.test(train$Age, flag)
print(test)





