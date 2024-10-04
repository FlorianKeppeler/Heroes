#------------------------------------------------
# Heroes: Auswertung nach Einrichtung
#----------------------------------------------

source("C:/Heroes/Funktionen_nachEinrichtungen.R")
source("C:/Heroes/keys_nachEinrichtungen.R")


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")

which(imp_names[["B107"]] == "Weraheim Stuttgart")

imp_names[["B107"]][unique(data$B107)]

data$B107[data$B107 %in% c(34:36)] = 50 # Stiftung Jugendhilfe aktiv zusammenführen

data$B107[data$B107 %in% c(11, 12, 40)] = 51 # Eva Stuttgart, Heidenheim und Weraheim zusammenführen

imp_names[["B107"]]


Einrichtungen = c("EVA", "Scout am Löwentor Stuttgart", "Mutpol Tuttlingen",
                  "Jugendhilfe Hochdorf", "Stiftung Tragwerk KirchheimTeck", "Stiftung Jugendhilfe aktiv",
                  "Diasporahaus Bietenhausen", "Jugendhilfeverbund Paulinenpflege Winnenden",
                  "Jugendhilfe Hoffmannhaus Wilhelmsdorf", "Jugendhilfeverbund Kinderheim Rodt")



for(Einrichtung in Einrichtungen){
  
  # data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")
  
  # nach Einrichtungsnamen suchen
  Index_einrichtung = which(imp_names$B107 == Einrichtung)
  
  # subdatensatz der ausgewählten Einrichtung erzeugen
  sub_data = data[data$B107 == Index_einrichtung, ]
  
  meta_path = paste0("C:/Heroes/Ergebnisse/nachEinrichtungen/",Einrichtung)
  
  
  descriptive_anal_plots(plot_data = sub_data,
                         data = data,
                         type="binom",
                         path=paste0(meta_path,"/Grafiken/Deskriptive Analyse Plots/mitFehler"),
                         se= T)
  
  
  
  # wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft für verschiedene Gruppierungen
  
  # group_list = list("Gesamt" = 1:5, "SBBZ" = 2, "Leitung" = 1, "HZE" = 3:5)
  
  group_list = list("Gesamt" = 1:5)
  
  
  ums_data = create_ums_data(sub_data, umsetzung)
  
  
  if(!dir.exists(paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung"))) {
    
    dir.create(paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung"))
  }
  
  
  
  # wichtigste Haltekraft nach Oberskala -------------------------------------------------
  
  
  # Berechnung der Skalen
  
  data_skalen = skalen2
  data_skalen_ohne = skalen2_ohne
  
  
  skalen_scores = get_skalen_scores(plot_data = sub_data,
                                    skalen = data_skalen_ohne,
                                    skalen_names =  names(data_skalen_ohne))
  
  skalen_scores_andere = get_skalen_scores(plot_data = data,
                                           skalen = data_skalen_ohne,
                                           skalen_names =  names(data_skalen_ohne))
  
  
  plot_combined_imp(skalen = skalen2,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Oberskala_Wichtigkeit"), 
                    combined=FALSE)
  
  plot_combined_imp(skalen = skalen2,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Oberskala_Wichtigkeit_Umsetzung"), 
                    combined=TRUE)
  
  
  #-----------------------------------------------------
  # Vergleiche der Skalen unter den Gruppen:
  
  plot_group_diff(plot_data = sub_data,
                  skalen=skalen2,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3, 3),
                  group_list = list("SBBZ" = 2,"HZE" = 3:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Oberskala_"))
  
  
  plot_group_diff(plot_data = sub_data,
                  skalen = skalen2,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3,3),
                  group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Oberskala_"))
  
  
  # plot_most_important(data,
  #                     skalen_scores,
  #                     mar=c(12,4,5,3),
  #                     main="Wichtigste nach MA",
  #                     file=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Wichtigste_nachMA_Einrichtungen.pdf"),
  #                     best_var = 0)
  
  
  
  data_skalen = skalen3
  data_skalen_ohne = skalen3_ohne
  
  
  skalen_scores = get_skalen_scores(plot_data = sub_data,
                                    skalen = data_skalen_ohne,
                                    skalen_names =  names(data_skalen_ohne))
  
  skalen_scores_andere = get_skalen_scores(plot_data = data,
                                           skalen = data_skalen_ohne,
                                           skalen_names =  names(data_skalen_ohne))
  
  
  plot_combined_imp(skalen = skalen3,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit"), 
                    combined=FALSE)
  
  plot_combined_imp(skalen = skalen3,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit_Umsetzung"), 
                    combined=TRUE)
  
  
  #-----------------------------------------------------
  # Vergleiche der Skalen unter den Gruppen:
  
  plot_group_diff(plot_data = sub_data,
                  skalen=skalen3,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3, 3),
                  group_list = list("SBBZ" = 2,"HZE" = 3:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_"))
  
  
  plot_group_diff(plot_data = sub_data,
                  skalen = skalen3,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3,3),
                  group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_"))
  
  
  # nach Skalen
  
  data_skalen = skalen
  data_skalen_ohne = skalen_ohne
  
  
  skalen_scores = get_skalen_scores(plot_data = sub_data,
                                    skalen = data_skalen_ohne,
                                    skalen_names =  names(data_skalen_ohne))
  
  skalen_scores_andere = get_skalen_scores(plot_data = data,
                                           skalen = data_skalen_ohne,
                                           skalen_names =  names(data_skalen_ohne))
  
  
  plot_combined_imp(skalen = skalen,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Wichtigkeit"), 
                    combined=FALSE)
  
  plot_combined_imp(skalen = skalen,
                    plot_data = sub_data,
                    skalen_scores = skalen_scores,
                    skalen_scores_andere = skalen_scores_andere,
                    umsetzung = umsetzung,
                    var_name_group= "B102",
                    mar=c(16, 9, 3,3),
                    ums_data = ums_data,
                    group_list = group_list,
                    path=paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/Wichtigkeit_Umsetzung"), 
                    combined=TRUE)
  
  
  #-----------------------------------------------------
  # Vergleiche der Skalen unter den Gruppen:
  
  plot_group_diff(plot_data = sub_data,
                  skalen=skalen,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3, 3),
                  group_list = list("SBBZ" = 2,"HZE" = 3:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/"))
  
  
  plot_group_diff(plot_data = sub_data,
                  skalen = skalen,
                  umsetzung=umsetzung,
                  ums_data=ums_data,
                  mar=c(16, 6, 3,3),
                  group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                  path = paste0(meta_path,"/Grafiken/Wichtigkeit und Umsetzung/"))
  
  
  # Density Diagramm subjektive Haltekrafte
  
  if(!dir.exists(paste0(meta_path,"/Grafiken/Dichte"))){
    
    dir.create(paste0(meta_path,"/Grafiken/Dichte"))
  }
  
  ums_scores = get_ums_scores(ums_data = ums_data,
                              skalen = skalen,
                              skalen_names = names(skalen))
  
  
  plot_rel_density(plot_data=sub_data,
                   ums_scores=ums_scores,
                   mar=c(11,4,4,3),
                   main = "subjektive Haltekraft der Einrichtung",
                   file=paste0(meta_path,"/Grafiken/Dichte/Haltekraft_gesamt.pdf"),
                   ylim=c(0, 0.9))
  
  
}

