#############################
#### Data Management
#############################


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

datos_csv<-"datos_piloto_islitas.csv"

# List all CSV files in the specified folder
file_list <- list.files(path = path_datos, pattern = "\\.csv$", full.names = TRUE)

# Read each CSV file and combine them into one data frame
df<- do.call(rbind, lapply(file_list, read.csv))


##################################################
############ recode ##############################
##################################################

df$gid.treat<-df$participant.zonaT2

df$gid.amerb<-paste0(df$participant.caleta, ".",df$participant.zonaT2, ".",df$participant.id_caleta)
#df$gid.amerb <- gsub(" ", ".", df$ugid)

save(df, file = paste0(path_datos, "/Datos_islitas.Rdata"))




