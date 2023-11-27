library(dlnm)
library(splines)


#  read dataset
data = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020-2023 carTemperRh.csv')

data$date = as.Date(data$date, '%Y-%m-%d')
data$year = format(data$date, '%Y')
data$yday = format(data$date, '%j')
data$dow = format(data$date, '%w')

#  define variable

## percentiles for temperature
per = quantile(data$temp_mean, c(2.5, 10, 25, 50, 75, 90, 97.5) / 100, na.rm=T)
## degree of freedom for seasonality
dfseas = 8
## lag days
lag = 21
## number of knots for lag
lagnk = 3
## the funciton to exposure reaction(argvar in crossbasis)
varfun = 'bs'
## the df for exposure function
vardegree = 2
##  the knots for exposure function
varper = c(10, 75, 90)



#  define the crossbasis
argvar = list(fun=varfun, knots = quantile(data$temp_mean, varper/100, na.rm=T), degree = vardegree)
arglag = list(knots = logknots(lag, lagnk))
cb = crossbasis(data$temp_mean, lag=lag, argvar = argvar, arglag = arglag)




#  define model formula used in prediction
formula = count ~ cb + ns(date, df = dfseas*length(unique(data$year))) + dow + bs(yday, df=3)
model = glm(formula, data, family = quasipoisson, na.action = 'na.exclude')
# define censoring
cen = mean(data$temp_mean, na.rm=T)

pred = crosspred(cb, model, cen=cen)




png( 
    filename = "温度-滞后期-相对风险_季节性+长期趋势+星期.png", # 文件名称
    width = 1440,           # 宽
    height = 1440,          # 高
    units = "px",          # 单位
    bg = "white",          # 背景颜色
    res = 300) 
plot(pred, "contour", xlab="MeanTemp", key.title=title("RR"),
# cex.axis=2,
    #  plot.axes={axis(1,cex.axis=2)
    #    axis(2,cex.axis=2)},
    #  key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="MeanTemp (°C)",ylab="Lag (days)"))
dev.off()

png( 
    filename = "年平均温度不同滞后期的相对风险_季节性+长期趋势+星期.png", # 文件名称
    width = 1440,           # 宽
    height = 1000,          # 高
    units = "px",          # 单位
    bg = "white",          # 背景颜色
    res = 300) 
plot(pred, "slices", var = 0, col=3, ylab='RR',
    ci.arg = list(density=15, lwd=2),
    main = '年平均温度不同滞后期的相对风险')     
dev.off()


