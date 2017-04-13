#应用一：XGBoost用来做预测
#http://mp.weixin.qq.com/s?src=3&timestamp=1491900391&ver=1&signature=wY1ir8cJnYOrJaEuqSpwGPmtZl0LoKYSaXiV8UTZ3ce4gFqmLbwpb9DLd-Shg4e2giWKnnRN-524SWEvFeOyfdT5tEIh17HwxvBW9K1UZXlsdCFlDBfFpKLL3kn14Z1vuc29jOv3UUskMUwXokK6gQ4Pjy3s5DGRRpvIAKWR9A8=
devtools::install_github("ellisp/forecastxgb-r-package/pkg")  
library(forecastxgb)  
model <- xgbar(gas)  
summary(model)



#建好模之后就是进行预测：
fc <- forecast(model, h = 12)  
plot(fc)  

#如果有额外的自变量需要加入：
library(fpp)  
consumption <- usconsumption[ ,1]  
income <- matrix(usconsumption[ ,2], dimnames = list(NULL, "Income"))  
consumption_model <- xgbar(y = consumption, xreg = income)  
#Stopping. Best iteration: 20  

#预测以及画图：
income_future <- matrix(forecast(xgbar(usconsumption[,2]), h = 10)$mean,   
                        dimnames = list(NULL, "Income"))  
#Stopping. Best iteration: 1  
plot(forecast(consumption_model, xreg = income_future))  

#