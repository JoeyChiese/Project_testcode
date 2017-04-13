#R语言数据可视化
library(ggplot2)
ggplot() #画布

#使用geom_abline、geom_hline、geom_vline画直线
#横线
ggplot() + geom_hline(yintercept = 5)
#画一条竖线
ggplot() + geom_vline(xintercept = 5)
#画斜线
ggplot()+ geom_abline(intercept = 2.5, slope=1)

#使用geom_point画点
x <- c(1,2,3)
y <- c(1,3,4)
data <- data.frame(x,y)
str(data)
ggplot(data, aes(x = x, y = y)) + geom_point()+geom_abline(intercept = 2.5, slope=1)


#开始调整geom_point的参数，比如展示不同的颜色(左)，和展示不同的形状(右)
ggplot(data, aes(x, y)) + geom_point(aes(colour = factor(y))) #colour
ggplot(data, aes(x, y)) + geom_point(aes(shape = factor(y))) # shape


#按factor区分，而是按连续值来区分，那么就是渐变形式
ggplot(data, aes(x, y)) + geom_point(aes(colour = y))

#展示不同的大小，可以固定大小(左)，也可以根据数据确定大小(右)
ggplot(data, aes(x, y)) + geom_point(aes(size = 3))
ggplot(data, aes(x, y)) + geom_point(aes(size = y))

#aes的作用，看下面两个用法(如图左、右)：左边的含义就是画红色点，右边是按照指定的一个维度展示不同的颜色
ggplot(data, aes(x, y)) + geom_point(colour="red")
ggplot(data, aes(x, y)) + geom_point(aes(colour="red")) #aes备注标注信息

#上面划线一节，我们在已经画了点的画布上再画一条斜线：一条斜率为1，
#截距为1的直线，也就是y=x+1，那么一定是经过(2,3),(3,4)两个点的
ggplot(data, aes(x, y)) + geom_point(aes(colour = y)) + geom_abline(slope = 1, intercept = 1)

#geom_bar来画直方图
ggplot(data, aes(x)) + geom_bar()
ggplot(data, aes(x<2)) + geom_bar()

#指定直方图的高度的计算方法
ggplot(data, aes(x)) + geom_bar(aes(weight=y))
ggplot(data, aes(x,y)) + geom_bar(stat = "identity")

#如果y是数字，那么想把他当成类别，需要转成factor
x <- rep(c(1,2), c(2,3))
y <- rep(c(3,2), c(1,4))
data <- data.frame(x,y)
ggplot(data, aes(x)) + geom_bar(aes(fill=factor(y)))

#也可以不简单堆叠起来，比如扁平放置(左)，或拉伸至顶部(右)
ggplot(data, aes(x)) + geom_bar(aes(fill=factor(y)), position="dodge")
ggplot(data, aes(x)) + geom_bar(aes(fill=factor(y)), position="fill")

#利用geom_density画概率密度曲线
x <- rep(c(1,3,7,11,23,50,60),c(1,30,400,60,4,55,11))
y <- rep(c(1,3,7,11,23,50,60),c(1,30,400,60,4,55,11))
data <- data.frame(x,y)
ggplot(data, aes(x)) + geom_density()

#调整平滑的宽度
ggplot(data, aes(x)) + geom_density(adjust = 1/5)

#按照不同的y值来分开画密度图，并且用不同颜色来表示不同的y值，那么我们可以用描边的方式(左)，
#也可以用填充的方式(中)，当然也可以两者结合
ggplot(data, aes(x, colour = factor(y))) + geom_density(adjust = 1/5)
ggplot(data, aes(x, fill = factor(y))) + geom_density(adjust = 1/5)
ggplot(data, aes(x, colour = factor(y), fill = factor(y))) + geom_density(adjust = 1/5, alpha = 0.1)

#柱状图一样，我们也可以通过geom_density的position参数来显示累计情况
ggplot(data, aes(x, fill = factor(y))) + geom_density(adjust = 1/5, position='fill')
ggplot(data, aes(x, fill = factor(y))) + geom_density(adjust = 1/5, position='stack')

#用geom_text和geom_label写标注文本
ggplot(data, aes(x, y, label=rownames(data))) + geom_point(aes(colour = y)) + geom_abline(slope = 1, intercept = 1) + geom_text(check_overlap = TRUE)
ggplot(data, aes(x, y, label=rownames(data))) + geom_point(aes(colour = y)) + geom_abline(slope = 1, intercept = 1) + geom_label()



