rm(list=ls());gc()
setwd("C:/Users/USER/Desktop/MT Project/BigData/BDF20190923")
library(tidyverse)
library(ggplot2)
library(data.table)
library(imager)
library(spectral)

ga=fread(file="Analytics All Web Site Data 目標對象總覽 20180130-20190922.csv",data.table = F)
colnames(ga)=c("date","hits") 
ga$date<-as.Date(ga$date)
ga$hits <-ga$hits %>% as.numeric() 
ga<-ga%>% na.omit()
ga<-ga %>% mutate(wk=week(date),yr=year(date)) %>%
  mutate(yr_wk=paste0(as.character(yr),'-',as.character(wk))) %>%
  group_by(yr_wk) %>% summarise(wkly_hits=sum(hits))






y=ga$wkly_hits
x=1:nrow(ga)
 FT <- spec.fft(y, x)
  par(mfrow = c(1, 1))
  plot(x, y, type = "l", main = "Signal")
  abline(v=26*3)
  
  abline(v=16.12903*1+10)
  abline(v=0.062)
  abline(v=0.145)
plot(
  FT,
  ylab = "Amplitude",
  xlab = "Frequency",
  type = "l",
  xlim = c(-0.5,0.5),
  main = "Spectrum"
)


abline(v=0.03846154)
abline(v=0.062)
abline(v=0.145)
abline(v=0.21)
