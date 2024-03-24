library(readxl)
library(vegan)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
#install.packages("tseries")
library(tseries)
#install.packages("forecast")
library(forecast)
library(lmtest)
library(ggpubr)
library(forcats)

data = read.delim("data/env.csv", sep = ",")
data = read_excel("data/env.xlsx", sheet = 1)
data$date <- as.POSIXct(data$date, format= "%Y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
data2 = subset(data, month %in% c(12,1,2,3))

data.avg = group_by(data2, year, month)

tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
pg_env_avg$int = paste(pg_env_avg$month,".",pg_env_avg$year)
pg_env_avg$int = gsub(" ", "", pg_env_avg$int)

#write.csv(pg_env_avg, file = "data/env_avg.csv")

###import the saved filed with averages 
x = read.csv("data/env_avg.csv", sep = ",")

x$month = as.factor(x$month)

p1 = ggplot(x, aes(fct_inorder(month), at, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Air temperature (°C)")
p1

p2 = ggplot(x, aes(fct_inorder(month), st, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Soil temperature (°C)")
p2

p3 = ggplot(x, aes(fct_inorder(month), rh, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Relative humidity (%)")
p3

p4 = ggplot(x, aes(fct_inorder(month), sm2, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Soil Moisture (%)")
p4

p5 = ggplot(x, aes(fct_inorder(month), ppt, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Precipitation (mm)")
p5

p6 = ggplot(x, aes(fct_inorder(month), par, group = growth_season)) + 
  geom_point(size = 2, aes(shape = as.factor(growth_season)), stat = "identity") + 
  geom_line(linetype="dotted") + scale_shape_manual(values = 9:18) + theme_classic() +
  xlab("Month") + ylab("Photosynthetic Active Radiation (mol/m2/s1)")
p6

ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
