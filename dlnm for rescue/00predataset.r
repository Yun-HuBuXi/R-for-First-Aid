library(dplyr)

data = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020-2023 disease&area.csv')

climate = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\climate_data.csv')


data$time = as.Date(data$time, "%Y-%m-%d")



climate$DDATETIME = as.Date(climate$DDATETIME, format = '%Y-%m-%d')

temperature = data.frame(date = climate$DDATETIME, temper = climate$T / 10)
temperature = subset(temperature, date >= as.Date('2020-01-01') & date <= as.Date("2023-06-30"))

relhumidity = data.frame(date = climate$DDATETIME, rh = climate$U)
relhumidity = subset(relhumidity, date >= as.Date('2020-01-01') & date <= as.Date("2023-06-30"))


diseases = as.character(unique(data$disease))
#  [1] "理化中毒"                "呼吸系统疾病"
#  [3] "创伤-暴力事件"           "心血管系统疾病-其他疾病"
#  [5] "创伤-跌倒"               "精神病"
#  [7] "神经系统疾病-其他疾病"   "其他-其他症状"
#  [9] "心血管系统疾病-胸痛"     "消化系统疾病"           
# [11] "创伤-其他原因"           "创伤-交通事故"
# [13] "妇产科"                  "儿科"
# [15] "其他-死亡"               "内分泌系统疾病"
# [17] "泌尿系统疾病"            "神经系统疾病-脑卒中"
# [19] "其他-胸闷"               "创伤-高处坠落"
# [21] "其他-昏迷"               "心脏骤停"
# [23] "感染性疾病"
dlist_disease = lapply(diseases, function(x) data[data$disease == x, ] )

# 选择心血管疾病的信息
cardio_disease = rbind(data.frame(dlist_disease[[9]]), data.frame(dlist_disease[[4]]))
cardio_disease = subset(cardio_disease, time >= as.Date('2020-01-01') & time <= as.Date("2023-06-30"))


cardio_daily = cardio_disease %>%
  group_by(time) %>%
  summarise(count = n())
cardio_daily$date = as.Date(cardio_daily$time, format = '%Y-%m-%d')
cardio_daily$time = NULL

temperature = temperature %>%
  group_by(date) %>%
  mutate(temper = ifelse(is.na(temper), mean(temper, na.rm=TRUE), temper))
daliy_temp = temperature %>%
  group_by(date) %>%
  summarise(
    temp_max = max(temper),
    temp_min = min(temper),
    temp_mean = mean(temper),
    temp_q1 = quantile(temper, 0.25),
    temp_median = quantile(temper, 0.5),
    temp_q3 = quantile(temper, 0.75),
    temp_ep = temp_max - temp_min
  )


relhumidity = relhumidity %>%
  group_by(date) %>%
  mutate(rh = ifelse(is.na(rh), mean(rh, na.rm=TRUE), rh))
daliy_rh = relhumidity %>%
  group_by(date) %>%
  summarise(
    rh_max = max(rh),
    rh_min = min(rh),
    rh_mean = mean(rh),
    rh_q1 = quantile(rh, 0.25),
    rh_median = quantile(rh, 0.5),
    rh_q3 = quantile(rh, 0.75),
    rh_ep = rh_max - rh_min
  )

temper_rh = merge(daliy_temp, daliy_rh, by='date', all=TRUE)
cardio_temper_rh = merge(cardio_daily, temper_rh, by='date', all=TRUE)

cardio_temper_rh[is.na(cardio_temper_rh)] = 0

write.csv(cardio_temper_rh, file = "2020-2023 carTemperRh.csv", row.names = FALSE)
