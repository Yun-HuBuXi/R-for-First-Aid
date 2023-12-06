# IMPORT PACKAGES
library(dlnm)
library(splines)
library(dplyr)
library(tidyr)





#  READ DATA
data_dwn = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\DWWN2020to2023.CSV')
data_car = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020-2023 carTemperRhDowHoliday.csv')





#  PERPAED DATA
data_dwn = data_dwn[grep('全市陆地', data_dwn$alarmArea), ]
data_car$date = as.Date(data_car$date, format = '%Y-%m-%d')
data_dwn$date = as.Date(data_dwn$recdate, format = '%Y-%m-%d')

  ## chose type
dwn_typhoon = subset(data_dwn, data_dwn$alarmType == '台风')

  ## get all days in duration
generate_datas = function(rec, end){
    seq.Date(from = as.Date(rec), to = as.Date(end), by = 'day')
}
dwn_typhoon = dwn_typhoon %>%
    rowwise() %>%
    mutate(date = list(generate_datas(recdate, endDate))) %>%
    unnest(date)

  ##  get unique day in duration
date_typhoon = data.frame(date = unique(dwn_typhoon$date), is_typhoon = 1)

  ## merge and supply
car_typhoon = merge(data_car, date_typhoon, by='date', all.x = TRUE)
car_typhoon$is_typhoon[is.na(car_typhoon$is_typhoon)] = 0





# MODEL AND PERD
  ## degree of freedom for seasonality
dfseas = 9

  ## lag days
lag_temp = 21
lag_typhoon = 60

  ## the funciton to exposure reaction(argvar in crossbasis)
varfun = 'ns'

  ##  the arg function
argvar_temp = list(fun=varfun, df = 4)
argvar_typhoon= list(fun='strata', breaks=c(0.5))
arglag_temp = list(knots = logknots(lag_temp, df = 5))
arglag_typhoon = list(fun = 'ns', df=3)

  ##  the crossbasis
cb_temp = crossbasis(car_typhoon$temp_mean, lag=lag_temp, argvar = argvar_temp, arglag = arglag_temp)
cb_typhoon = crossbasis(car_typhoon$is_typhoon, lag=lag_typhoon, argvar = argvar_typhoon, arglag = arglag_typhoon)

  ##  other splines 
time = ns(car_typhoon$date, df = dfseas*4)
rh = ns(car_typhoon$rh_mean, df = 3)

  ##  model formula used in prediction
formula = count ~ cb_typhoon + cb_temp + time + rh + dow + holiday
model = glm(formula, car_typhoon, family = quasipoisson, na.action = 'na.exclude')
  ##  censoring
cen = 0

  ## perd
pred = crosspred(cb_typhoon, model, cen=cen, from=0, to=1.1)





# REMOVE VARIABLE EXCEPT PERD OUTCOME
rm(list = setdiff(ls(), 'pred'))





#PLOT
plot(pred, 'overall', xlab='预警等级', ylab= 'Cumulative RR', main='累计相对风险', ylim=c(0.2, 2.1))
plot(pred, 'slices', var = '1' , xlab='滞后天数', ylab='Cumulative RR', main = '预警相对风险滞后效应', lwd= 2)