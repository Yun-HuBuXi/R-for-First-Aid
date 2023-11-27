library(dplyr)
library(ggplot2)


## 读取数据并格式化

data = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020-2023 disease&area.csv')

data$time = as.Date(data$time, "%Y-%m-%d")


## 根据疾病类型划分dataframe
diseases = as.character(unique(data$disease))
print(diseases)
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
# 选择呼吸系统疾病
respi_disease = data.frame(dlist_disease[[2]])


# 按天统计每日发病数
cardio_daily = cardio_disease %>%
  group_by(time) %>%
  summarise(count = n())
cardio_daily$time = as.Date(cardio_daily$time, format = '%Y-%m-%d')

respi_daily = respi_disease %>%
  group_by(time) %>%
  summarise(count = n())
respi_daily$day = as.Date(cardio_daily$day, format = '%Y-%m-%d')

all_daily = data %>%
  group_by(time) %>%
  summarise(count = n())
all_daily$day = as.Date(all_daily$time, format = '%Y-%m-%d')


## 绘图
daliy_cardio = ggplot(cardio_daily, aes(x=time, y=count))+
  geom_point(size = 1) +
  geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white",colour = 'white')) +
  labs(x = '日期', y = '呼叫数')
ggsave('心血管疾病日呼叫变化.png', plot = daliy_cardio, width = 20, height = 10, dpi = 300)

daliy_respi = ggplot(respi_daily, aes(x=day, y=count))+
  geom_point(size = 1) +
  geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white",colour = 'white')) +
  labs(x = '日期', y = '呼叫数')
ggsave('呼吸系统疾病日呼叫变化.png', plot = daliy_respi, width = 20, height = 10, dpi = 300)

daliy_all = ggplot(all_daily, aes(x=time, y=count))+
  geom_point(size = 1) +
  geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white",colour = 'white')) +
  labs(x = '日期', y = '呼叫数')
ggsave('全因日呼叫变化.png', plot = daliy_all, width = 20, height = 10, dpi = 300)



scatter_plot_fn = function(i) {
    type = diseases[[i]]
    plot_title = paste(type, '日紧急呼叫变化', sep = '——')
    name = paste(type, '日呼叫 变化.png', sep = '——')
    data_disease = data.frame(dlist_disease[[i]])
    disease_daily = data_disease %>%
      group_by(time) %>%
      summarise(count = n())
    disease_daily$time = as.Date(disease_daily$time, format = '%Y-%m-%d')
    daliy_disease = ggplot(disease_daily, aes(x=time, y=count))+
      geom_point(size = 1) +
      geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white",colour = 'white')) +
      labs(x = '日期', y = '呼叫数',  title = plot_title)
    ggsave(name, plot = daliy_disease, width = 20, height = 10, dpi = 300)
} 

for ( i in 1:length(dlist_disease)) {
    scatter_plot_fn(i)
}






### 查看均值和方差的关系

View(cardio_disease)
meanVarCardio = cardio_disease %>%
  group_by(time) %>%
  summarise(
    count = n()
  )

meanCardio = mean(meanVarCardio$count)
varCardio = var(meanVarCardio$count)
meanCardio
varCardio
# > meanCardio
# [1] 23.63956
# > varCardio
# [1] 57.09142
