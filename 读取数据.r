library(readxl)

# 读取整个excel
data_2019 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2019.xlsx')
data_2020 = read_excel('C:\\Users\\16521\\Desktop\\文件夹\\数据\\2020.xlsx')

# 读取需要分析的数据
data_2019_time_sex_disease = data.frame(data_2019[, c(5, 58,112)])
colnames(data_2019_time_sex_disease) = c('时间', '性别', '疾病类型')
data_2020_time_sex_disease = data.frame(data_2020[, c(50, 58, 119)])
colnames(data_2020_time_sex_disease) = c('时间', '性别', '疾病类型')

# View(data_2019_time_sex_disease)
# View(data_2020_time_sex_disease)

# data_time_sex_disease = rbind(data_2019_time_sex_disease, data_2020_time_sex_disease)
data_time_sex_disease = data_2020_time_sex_disease

# 时间格式化 提取月份
data_2019_time_sex_disease$时间 = as.POSIXct(data_2019_time_sex_disease$时间, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")
data_2020_time_sex_disease$时间 = as.POSIXct(data_2020_time_sex_disease$时间, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")
data_time_sex_disease$时间 = as.POSIXct(data_time_sex_disease$时间, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")

data_2019_time_sex_disease$month = format(data_2019_time_sex_disease$时间, format = '%m')
data_2020_time_sex_disease$month = format(data_2020_time_sex_disease$时间, format = '%m')


# 2019.12 - 2020.04每个月的数据
library(lubridate)  
library(glue)

data_YM = function(year, month){
    startdate = as.POSIXct(glue("{year}-{month}-01"), tz = "Asia/Shanghai")
    if (month==2){
        enddate = as.POSIXct(glue("{year}-{month}-29"), tz = "Asia/Shanghai")
    } else if (month==4) {
       enddate = as.POSIXct(glue("{year}-{month}-30"), tz = "Asia/Shanghai")
    } else {
       enddate = as.POSIXct(glue("{year}-{month}-31"), tz = "Asia/Shanghai")
    }
    sex_disease = subset(data_time_sex_disease, data_time_sex_disease$时间 >= startdate & data_time_sex_disease$时间 <= enddate)
    sex_disease$日 = format(sex_disease$时间, '%d')
    return(sex_disease)
}
sex_disease_2019.12 = data_YM(2019, 12)
sex_disease_2020.01 = data_YM(2020, 01)
sex_disease_2020.02 = data_YM(2020, 02)
sex_disease_2020.03 = data_YM(2020, 03)
sex_disease_2020.04 = data_YM(2020, 04)
View(sex_disease_2019.12)

# 2019。12-2020.04每七天的变化

startdate = as.POSIXct('2019-12-01', tz = "Asia/Shanghai")
enddate = as.POSIXct('2020-04-30', tz = "Asia/Shanghai")
data_time_sex_disease = subset(data_time_sex_disease, data_time_sex_disease$时间 >= startdate & data_time_sex_disease$时间 <= enddate)
data_time_sex_disease$week_start = cut(data_time_sex_disease$时间, breaks = '7 days')


# 计算各月份性别、疾病类型占比
library(dplyr)

# sex_persent = data_time_sex_disease %>%
#   group_by(年月, 性别) %>%
#   summarise(count = n()) %>%
#   group_by(年月) %>%
#   mutate(Percentage = count / sum(count) * 100)
# sex_persent$性别[is.na(sex_persent$性别)] = '未统计' 
# sex_persent = data.frame(sex_persent)
# # View(sex_persent)

sex_persent_fn = function(data, time) {
  sex_persent = data %>%
  group_by(!!rlang::sym(time), 性别) %>%
  summarise(count = n()) %>%
  group_by(!!rlang::sym(time)) %>%
  mutate(Percentage = count / sum(count) * 100)
  sex_persent$性别[is.na(sex_persent$性别)] = '未统计' 
  sex_persent = data.frame(sex_persent)
  return(sex_persent)
}

sex_persent_2019.12 = sex_persent_fn(data = sex_disease_2019.12, '日')
sex_persent_2020.01 = sex_persent_fn(data = sex_disease_2020.01, '日')
sex_persent_2020.02 = sex_persent_fn(data = sex_disease_2020.02, '日')
sex_persent_2020.03 = sex_persent_fn(data = sex_disease_2020.03, '日')
sex_persent_2020.04 = sex_persent_fn(data = sex_disease_2020.04, '日')
# disease_persent = data_time_sex_disease %>%
#   group_by(年月, 疾病类型) %>%
#   summarise(count = n()) %>%
#   group_by(年月) %>%
#   mutate(Percentage = count / sum(count) * 100)
# disease_persent = data.frame(disease_persent)
# print(disease_persent)

disease_percent_fn = function(data, time) {
  disease_percent = data %>%
  group_by(!!rlang::sym(time), 疾病类型) %>%
  summarise(count = n()) %>%
  group_by(!!rlang::sym(time)) %>%
  mutate(Percentage = count / sum(count) * 100)
  disease_percent = data.frame(disease_percent)
  return(disease_percent)
}

disease_persent_2019.12 = disease_percent_fn(sex_disease_2019.12, '日')
disease_persent_2020.01 = disease_percent_fn(sex_disease_2020.01, '日')
disease_persent_2020.02 = disease_percent_fn(sex_disease_2020.02, '日')
disease_persent_2020.03 = disease_percent_fn(sex_disease_2020.03, '日')
disease_persent_2020.04 = disease_percent_fn(sex_disease_2020.04, '日')

#  计算每周的占比
weekly_sex = data_time_sex_disease %>%
  group_by(week_start, 性别) %>%
  summarise(count = n()) %>%
  group_by(week_start) %>%
  mutate(Percentage = count / sum(count) * 100)
weekly_sex = data.frame(weekly_sex)

weekly_disease = data_time_sex_disease %>%
  group_by(week_start, 疾病类型) %>%
  summarise(count = n()) %>%
  group_by(week_start) %>%
  mutate(Percentage = count / sum(count) * 100)
weekly_disease = data.frame(weekly_disease)

#  将疾病类型补充为22项
library(tidyverse)
all_disease = c('创伤-交通事故','其他-其他症状','创伤-暴力事件','神经系统疾病-其他疾病','理化中毒','创伤-跌倒','消化系统疾病','妇产科','创伤-其他原因','呼吸系统疾病','心血管系统疾病-其他疾病','泌尿系统疾病','神经系统疾病-脑卒中','精神病','儿科','内分泌系统疾病','其他-死亡','心血管系统疾病-胸痛','其他-胸闷','创伤-高处坠落','其他-昏迷','心脏骤停')
weekly_disease_filled = weekly_disease %>%
  complete(week_start, 疾病类型 = all_disease, fill = list(count=0, Percentage=0))
View(weekly_disease_filled)

# 画图
library(ggplot2)

# time_sex_pic = ggplot(sex_persent, aes(x = 年月, y = Percentage,group = 性别, color = 性别, shape= 性别))+
#   geom_point() +
#   geom_line() +
#   labs(x = "年月", y = "百分比") +
#   theme_bw() +
#   theme(panel.grid.major=element_line(colour=NA),
#         panel.background = element_rect(fill = "white",colour = 'white'),
#         plot.background = element_rect(fill = "white",colour = 'white'),
#         panel.grid.minor = element_blank(),
#         legend.position = c(1.,1.),
#         legend.justification = c(1, 1),
#         legend.box.background = element_rect(color="black"),
#         plot.margin = unit(c(0.1,.1,.1,.1), 'in')) +
#   scale_y_continuous(limits = c(0,70),breaks = seq(0,65,5))
# ggsave('时间-性别.png', plot = time_sex_pic, width = 20, height = 10, dpi = 300)


# time_disease_pic = ggplot(disease_persent, aes(x = 年月, y = Percentage, group = 疾病类型, color = 疾病类型, shape=疾病类型))+
#   scale_shape_manual(values = c(1:24))+
#   geom_point() +
#   geom_line() +
#   labs(x = "年月", y = "百分比") +
#   theme_bw() +
#   theme(panel.grid.major=element_line(colour=NA),
#         panel.background = element_rect(fill = "white",colour = 'white'),
#         plot.background = element_rect(fill = "white",colour = 'white'),
#         panel.grid.minor = element_blank(),
#         legend.position = c(1.,1.),
#         legend.justification = c(1, 1),
#         legend.box.background = element_rect(color="black"),
#         plot.margin = unit(c(0.1,.1,.1,.1), 'in'))
# #   scale_y_continuous(limits = c(0,70),breaks = seq(0,65,5))
# ggsave('月份-疾病类型.png', plot = time_disease_pic, width = 20, height = 10, dpi = 300)

time_sex_pic_fn = function(data, name){
  time_sex_pic = ggplot(data, aes(x = 日, y = Percentage, group = 性别, color = 性别, shape= 性别))+
  geom_point() +
  geom_line() +
  labs(x = "日期", y = "百分比") +
  theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "white",colour = 'white'),
        plot.background = element_rect(fill = "white",colour = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = c(1.,1.),
        legend.justification = c(1, 1),
        legend.box.background = element_rect(color="black"),
        plot.margin = unit(c(0.1,.1,.1,.1), 'in')) +
  scale_y_continuous(limits = c(0,80),breaks = seq(0,75,5))
ggsave(glue('{name}.png'), plot = time_sex_pic, width = 20, height = 10, dpi = 300)
}

time_sex_pic_fn(sex_persent_2019.12, '2019.12-性别')
time_sex_pic_fn(sex_persent_2020.01, '2020.01-性别')
time_sex_pic_fn(sex_persent_2020.02, '2020.02-性别')
time_sex_pic_fn(sex_persent_2020.03, '2020.03-性别')
time_sex_pic_fn(sex_persent_2020.04, '2020.04-性别')

time_sex_pic = ggplot(weekly_sex, aes(x = week_start, y = Percentage, group = 性别, color = 性别, shape= 性别))+
    geom_point() +
    geom_line() +
    labs(x = "日期", y = "百分比") +
    theme_bw() +
    theme(panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "white",colour = 'white'),
          plot.background = element_rect(fill = "white",colour = 'white'),
          panel.grid.minor = element_blank(),
          legend.position = c(1.,1.),
          legend.justification = c(1, 1),
          legend.box.background = element_rect(color="black"),
          plot.margin = unit(c(0.1,.1,.1,.1), 'in')) +
    scale_y_continuous(limits = c(0,70),breaks = seq(0,70,5))
ggsave('性别-周.png', plot = time_sex_pic, width = 20, height = 10, dpi = 300)


time_disease_pic_fn = function(data, name) {
  time_disease_pic = ggplot(data, aes(x = 日, y = Percentage, group = 疾病类型, color = 疾病类型, shape=疾病类型))+         
  scale_shape_manual(values = c(1:24))+
  geom_point() +
  geom_line() +
  labs(x = "日", y = "百分比") +
  theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "white",colour = 'white'),
        plot.background = element_rect(fill = "white",colour = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = c(1.,1.),
        legend.justification = c(1, 1),
        legend.box.background = element_rect(color="black"),
        plot.margin = unit(c(0.1,.1,.1,.1), 'in'))
  # scale_y_continuous(limits = c(0,30),breaks = seq(0,25,5))
  ggsave(glue('{name}.png'), plot = time_disease_pic, width = 20, height = 10, dpi = 300)
}
time_disease_pic_fn(disease_persent_2019.12, '2019.12-疾病类型')
time_disease_pic_fn(disease_persent_2020.01, '疾病类型-2020.01-')
time_disease_pic_fn(disease_persent_2020.02, '疾病类型-2020.02-')
time_disease_pic_fn(disease_persent_2020.03, '疾病类型-2020.03-')
time_disease_pic_fn(disease_persent_2020.04, '疾病类型-2020.04-')


time_disease_pic = ggplot(weekly_disease_filled, aes(x = week_start, y = Percentage, group = 疾病类型, color = 疾病类型, shape=疾病类型))+         
  scale_shape_manual(values = c(1:24))+
  geom_point() +
  geom_line() +
  labs(x = "日期", y = "百分比") +
  theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "white",colour = 'white'),
        plot.background = element_rect(fill = "white",colour = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = c(1.,1.),
        legend.justification = c(1, 1),
        legend.box.background = element_rect(color="black"),
        plot.margin = unit(c(0.1,.1,.1,.1), 'in')) +
  scale_y_continuous(limits = c(0,45),breaks = seq(0,45,5))
  ggsave('疾病-周.png', plot = time_disease_pic, width = 20, height = 10, dpi = 300)


View(weekly_disease)



View(weekly_disease_filled)
