library(tidyverse)
library(data.table)
library(gtsummary)
library(MplusAutomation)

minmax <- function(x){
  
  c_n <- (5 / (8*1000)) * sd(x, na.rm = T)
  c_d <- (10/ (8*1000)) * sd(x, na.rm = T)
  
  (x - min(x, na.rm = T) + c_n) / (max(x, na.rm = T) - min(x, na.rm = T) + c_d)
  
}