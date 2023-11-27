library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)

# 读取原始数据
data_2017 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2017.xlsx')
data_2018 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2018.xlsx')
data_2019 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2019.xlsx')
data_2020 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020.xlsx')
data_2021 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2021.xlsx')
data_2022 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2022.xlsx')
data_2023 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2023.xlsx')
data_2020to2023 = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020-2022 disease&area.csv')
# 提取时间、疾病类型、地址
data_2017_disease = data.frame(data_2017[, c(8, 112, 23)])
colnames(data_2017_disease) = c('time', 'disease', 'address')
data_2018_disease = data.frame(data_2018[, c(5, 112, 23)])
colnames(data_2018_disease) = c('time', 'disease', 'address')
data_2019_disease = data.frame(data_2019[, c(5, 112, 23)])
colnames(data_2019_disease) = c('time', 'disease', 'address')
data_2020_disease = data.frame(data_2020[, c(23, 119, 7)])
colnames(data_2020_disease) = c('time', 'disease', 'address')
data_2021_disease = data.frame(data_2021[, c(50, 119, 7)])
colnames(data_2021_disease) = c('time', 'disease', 'address')
data_2022_disease = data.frame(data_2022[, c(22, 119, 6)])
colnames(data_2022_disease) = c('time', 'disease', 'address')
data_2023_disease = data.frame(data_2023[, c(22, 119, 6)])
colnames(data_2023_disease) = c('time', 'disease', 'address')



data_2023_disease$time =  as.POSIXct(data_2023_disease$time, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")

data_2020to2023$time =  as.POSIXct(data_2020to2023$time, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")


data_2020to2023_disease = rbind(data_2020to2023, data_2023_disease)
View(data_2020to2023_disease)

# 获取区域
data_2023_disease$district = str_extract(data_2023_disease$address, '.*?区')
View(data_2017to2022_disease)

# 保存
write.csv(data_2017to2022_disease, file = "2017-2022 disease&area.csv", row.names = FALSE)
write.csv(data_2020to2022_disease, file = "2020-2022 disease&area.csv", row.names = FALSE)
write.csv(data_2020to2023_disease, file = "2020-2023 disease&area.csv", row.names = FALSE)

# na_rows = data_2017to2022_disease[data_2017to2022_disease$district %in% NA, ]

# View(na_rows)


# 得到所有疾病类型
diseases = as.character(unique(data_2017to2022_disease$disease))
#  [1] "其他-其他症状"           "理化中毒"
#  [3] "神经系统疾病-其他疾病"   "创伤-交通事故"
#  [5] "消化系统疾病"            "创伤-跌倒"
#  [7] "呼吸系统疾病"            "神经系统疾病-脑卒中"    
#  [9] "儿科"                    "创伤-其他原因"
# [11] "创伤-高处坠落"           "妇产科"
# [13] "创伤-暴力事件"           "内分泌系统疾病"
# [15] "心血管系统疾病-其他疾病" "精神病"
# [17] "其他-死亡"               "泌尿系统疾病"
# [19] "其他-胸闷"               "心血管系统疾病-胸痛"
# [21] "其他-昏迷"               "心脏骤停"

#根据疾病类型分为不同数据框
dlist_disease = lapply(diseases, function(x) data_2017to2022_disease[data_2017to2022_disease$disease == x, ] )

# 选择心血管疾病的信息
cardio_disease = rbind(data.frame(dlist_disease[[15]]), data.frame(dlist_disease[[20]]))

# 格式化时间
cardio_disease$time = as.POSIXct(cardio_disease$time, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")
cardio_disease$day = format(cardio_disease$time, format = '%Y-%m-%d')
cardio_disease$day = as.Date(cardio_disease$day)
# 按天统计每日发病数
cardio_daily = cardio_disease %>%
  group_by(day) %>%
  summarise(count = n())

View(cardio_daily)


## 绘图

# 提取每年第一天的数据
cardio_daily$day = as.POSIXct(cardio_daily$day, '%Y-%m-%d', tz = "Asia/Shanghai")
cardio_daily$day = as.Date(cardio_daily$day)
cardio_firstday = cardio_daily %>%
  filter(format(day, "%j") == '001')

View(cardio_firstday)
# 制图
daliy_cardio = ggplot(cardio_daily, aes(x=day, y=count))+
  geom_point(size = 1) +
  geom_vline(xintercept=as.numeric(as.Date(paste0(c(2018, 2019, 2020, 2021, 2022),"-01-01"))), linetype="dashed", color="grey") +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white",colour = 'white')) +
  labs(x = '日期', y = '呼叫数')
ggsave('每日呼叫.png', plot = daliy_cardio, width = 20, height = 10, dpi = 300)
