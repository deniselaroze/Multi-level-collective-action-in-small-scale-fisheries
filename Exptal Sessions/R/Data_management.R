#############################
#### Data Management
#############################


library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(readr)
library(haven)
library(R.matlab)


rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"


path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"


setwd(path_github)

datos_csv<-"datos_piloto_islitas.csv"

# List all CSV files in the specified folder
file_list <- list.files(path = path_datos, pattern = "\\.csv$", full.names = TRUE)

# Read each CSV file and combine them into one data frame
df<- do.call(rbind, lapply(file_list, read.csv))


##################################################
############ recode ##############################
##################################################

#Each person person participants in two group /matching groups at the same time
# This is the group ID in the Open Access area
df$gid.treat<-df$participant.zonaT2

#This is the group ID in the TURF
df$gid.amerb<-paste0(df$participant.caleta, ".",df$participant.zonaT2, ".",df$participant.id_caleta)
#df$gid.amerb <- gsub(" ", ".", df$ugid)

# 



#########################
### Export Raw Data 
#########################
save(df, file = paste0(path_datos, "/Datos_islitas.Rdata"))

write.csv(df, paste0(path_datos, "/Datos_islitas_base.csv"), row.names = FALSE)
write.table(df, paste0(path_datos, "/Datos_islitas_tab_base.txt"), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

