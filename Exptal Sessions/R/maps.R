###########################
### Data Analysis
############################
library(stargazer)

library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)



rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


load(paste0(path_datos, "/Datos_islitas.Rdata"))