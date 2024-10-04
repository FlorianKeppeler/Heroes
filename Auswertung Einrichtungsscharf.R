source("C:/Heroes/Funktionen_nachEinrichtungen.R")
source("C:/Heroes/keys_nachEinrichtungen.R")


library(randomForest)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")

Einrichtung = "eva Stuttgart"

# Einrichtung = "Jugendhilfe Hochdorf"

# nach Einrichtungsnamen suchen
i = which(imp_names$B107 == Einrichtung)

# subdatensatz der ausgewählten Einrichtung erzeugen
sub_data = data[data$B107 == i, ]

meta_path = paste0("C:/Heroes/Ergebnisse/nachEinrichtungen/",Einrichtung)


descriptive_anal_plots(plot_data = sub_data,
                       data = data,
                       type="binom",
                       path=paste0(meta_path,"/Grafiken/Deskriptive Analyse Plots/mitFehler"),
                       se= T)



# wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft für verschiedene Gruppierungen

group_list = list("Gesamt" = 1:5, "SBBZ" = 2, "Leitung" = 1, "HZE" = 3:5)

ums_data = create_ums_data(data, umsetzung)


if(!dir.exists(paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung"))) {
  
  dir.create(paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung"))
}



# wichtigste Haltekraft nach Oberskala -------------------------------------------------


# Berechnung der Skalen

data_skalen = skalen3
data_skalen_ohne = skalen3_ohne


skalen_scores = get_skalen_scores(data = sub_data,
                                  skalen = data_skalen_ohne,
                                  skalen_names =  names(data_skalen_ohne))

plot_combined_imp(skalen = skalen3,
                  data = sub_data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(16, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit"), 
                  combined=FALSE)

plot_combined_imp(skalen = skalen3,
                  data = sub_data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(16, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit_Umsetzung"), 
                  combined=TRUE)


#.....................................................................

# Vergleiche der Skalen unter den Gruppen:

plot_group_diff(skalen=skalen3,
                umsetzung=umsetzung,
                ums_data=ums_data,
                mar=c(16, 6, 3,3),
                group_list = list("SBBZ" = 2,"HZE" = 3:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_")

plot_group_diff(skalen = skalen3,
                umsetzung=umsetzung,
                ums_data=ums_data,
                mar=c(16, 6, 3,3),
                group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_")

