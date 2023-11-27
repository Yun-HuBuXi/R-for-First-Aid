library(dplyr)
library(ggplot2)

climate = read.csv('C:\\Users\\16521\\Desktop\\文件夹\\数据\\climate_data.csv')

climate$DDATETIME = as.Date(climate$DDATETIME, format = '%Y-%m-%d')

temperature = data.frame(date = climate$DDATETIME, temper = climate$T / 10)
temperature = subset(temperature, date >= as.Date('2020-01-01'))

relhumidity = data.frame(date = climate$DDATETIME, rh = climate$U)
relhumidity = subset(relhumidity, date >= as.Date('2020-01-01'))





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

temper_type = colnames(daliy_temp)[-1]
rh_type = colnames(daliy_rh)[-1]



scatter_temper_fn = function(i) {
    type = temper_type[[i]]
    plot_title = paste(type, '日变化', sep = '——')
    name = paste(type, '日变化.png', sep = '——')
    
    temp_daily = ggplot(daliy_temp, aes(x=date, y=!!rlang::sym(type)))+
      geom_point(size = 1) +
      geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white",colour = 'white')) +
      labs(x = '日期', y = '温度',  title = plot_title)
ggsave(name, plot = temp_daily, width = 20, height = 10, dpi = 300)
}

scatter_rh_fn = function(i) {
    type = rh_type[[i]]
    plot_title = paste(type, '日变化', sep = '——')
    name = paste(type, '日变化.png', sep = '——')
    
    temp_daily = ggplot(daliy_rh, aes(x=date, y=!!rlang::sym(type)))+
      geom_point(size = 1) +
      geom_vline(xintercept=as.numeric(as.Date(paste0(c(2020, 2021, 2022, 2023),"-01-01"))), linetype="dashed", color="#3f3f3f") +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white",colour = 'white')) +
      labs(x = '日期', y = '相对湿度',  title = plot_title)
ggsave(name, plot = temp_daily, width = 20, height = 10, dpi = 300)
}


for ( i in 1:length(temper_type)) {
    scatter_temper_fn(i)
}


for ( i in 1:length(rh_type)) {
    scatter_rh_fn(i)
}
