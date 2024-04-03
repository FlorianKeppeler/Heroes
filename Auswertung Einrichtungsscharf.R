source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


library(randomForest)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")


# Leute entfernen die nicht mit jM arbeiten:

data = data[-unique(c(which(data$C207_07 == 2), which(data$B101_04 == 2))), ]



for(i in unique(data$B107)){
  
  dir.create(paste("C:/Heroes/Auswertung_Einrichtung/", imp_names[["B107"]][i]), recursive = T)
  
  descriptive_anal_plot(data = data[data$B107 == i, ],
                   path = paste("C:/Heroes/Auswertung/Einrichtung/", imp_names[["B107"]][i]))
}

