source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


library(randomForest)
library(openxlsx)
library(betareg)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")



#--------------------------------------
# Stichprobe:
nrow(data)
#  -> 397 Versuchspersonen gesamt


# SBBZ
sum(data$B102==2)
# 74


# HZE stationär
sum(data$B102==3)
#154

#  -> 228 Versuchspersonen aus SBBZ und HZE stationär

# Demographie ---------------------------
sum(!is.na(data$B103))
sum(!is.na(data$B105))
sum(!is.na(data$B110))
#  --> alle geben 228 aus: insgesamt 228 Versuchspersonen zu Demographie befragt

# Geschlecht
table(data$B103)
# -> 151: w;    75: m;   2: d

# Berufserfahrung:
table(data$B105)
#  -> 34: < 3;    99: 4 - 10;     95: > 11


# Alter:
table(data$B110)
#  -> 61: < 30;    126:  31 - 50;    41: > 51 


# Alter der jM in den Gruppen:
tmp = data[,keys[["AlterGruppen"]]]
tmp = tmp[complete.cases(tmp),]

tmp = tmp - 1

apply(tmp, 2, sum)/nrow(tmp)
# < 6-Jährige   6 bis 10-Jährige   11 bis 14-Jährige   15 bis 17-Jährige   18 bis 21-Jährige   > 21-Jährige
# -> 3.6%           36.3%             63.1%                65.7%                34.3%               2.9%;


# Arbeitsbereiche:
table(data$B102)
# -> 88: Leitung;    74: SBBZ;    154: HZE.station;     20: HZE.teilst;     61: HZE.ambul



sum(!is.na(data$B107))
#  -> Rücklauf sind 397 nach Ausschluss der Personen die nicht mit jM arbeiten



write_einrichtungen_sbbz_hze(data, file= "C:/Heroes/Ergebnisse/Excel/Einrichtungen/Sbbz_Hze.xlsx")





#-----------------------------------------------------------------------------
# alle offenen Fragen ausgeben:

# write.table(unique(data$C202_01), file = "C:/Heroes/Sammlung/C202_01_sonstige_Begriffe.txt", row.names = F, col.names = F)
# write.table(unique(data$C206_01), file = "C:/Heroes/Sammlung/C206_01_sonstige_Verhaltensweisen.txt", row.names = F, col.names = F)
# write.table(unique(data$D334_01), file = "C:/Heroes/Sammlung/D334_01_sonstige_Info_Anmerkung.txt", row.names = F, col.names = F)
# write.table(unique(data$D305_01), file = "C:/Heroes/Sammlung/D305_01_sonstige_Dokumente.txt", row.names = F, col.names = F)
# write.table(unique(data$D306_11a), file = "C:/Heroes/Sammlung/D306_11a_sonstige_weitereInfos.txt", row.names = F, col.names = F)
# write.table(unique(data$D311_01), file = "C:/Heroes/Sammlung/D311_01_sonstige_VerbesserungAnfrage.txt", row.names = F, col.names = F)
# write.table(unique(data$D334_01), file = "C:/Heroes/Sammlung/D334_01_sonstige_Anmerkung_VerbesserungAnfrage.txt", row.names = F, col.names = F)
# write.table(unique(data$E406_09a), file = "C:/Heroes/Sammlung/E406_09a_sonstige_Vetorecht_Aufnahme.txt", row.names = F, col.names = F)
# write.table(unique(data$E407_09a), file = "C:/Heroes/Sammlung/E407_09a_sonstige_Vetorempfehlung_Aufnahme.txt", row.names = F, col.names = F)
# write.table(unique(data$E403_01), file = "C:/Heroes/Sammlung/E403_01_sonstige_Verb_Aufn_Bez.txt", row.names = F, col.names = F)
# write.table(unique(data$F505_01), file = "C:/Heroes/Sammlung/F505_01_sonstige_Verb_Setting.txt", row.names = F, col.names = F)
# write.table(unique(data$G603_01), file = "C:/Heroes/Sammlung/G603_01_sonstige_Verb_Alltag.txt", row.names = F, col.names = F)
# write.table(unique(data$H704_01), file = "C:/Heroes/Sammlung/H704_01_sonstige_Verb_Krisenbew.txt", row.names = F, col.names = F)
# write.table(unique(data$H803_01), file = "C:/Heroes/Sammlung/H803_011_sonstige_Verb_Krisenauf.txt", row.names = F, col.names = F)
# write.table(unique(data$I903_01), file = "C:/Heroes/Sammlung/I903_01_sonstige_Verb_Entlassung.txt", row.names = F, col.names = F)
# write.table(unique(data$I904_09a), file = "C:/Heroes/Sammlung/I904_09a_sonstige_Vetorecht_Entlassung.txt", row.names = F, col.names = F)
# write.table(unique(data$J103_01), file = "C:/Heroes/Sammlung/J103_01_sonstige_Verbesserung_Haltung.txt", row.names = F, col.names = F)
# write.table(unique(data$K204_01), file = "C:/Heroes/Sammlung/K204_01_sonstige_Interdisz_Team.txt", row.names = F, col.names = F)
# write.table(unique(data$K209_01), file = "C:/Heroes/Sammlung/K209_01_sonstige_Verbesserung_Teamqual_Qualifikation.txt", row.names = F, col.names = F)
# write.table(unique(data$N503_01), file = "C:/Heroes/Sammlung/N503_01_sonstige_Anmerkungen_Abschluss.txt", row.names = F, col.names = F)



#-----------------------------------------------------------------------------------
# Deskriptive Analyse

# Plots

descriptive_anal_plots(data=data,
                       type="binom",
                       path="C:/Heroes/Ergebnisse/Grafiken/Deskriptive Analyse Plots")


descriptive_anal_plots(data=data,
                       type="binom",
                       path="C:/Heroes/Ergebnisse/Grafiken/Deskriptive Analyse Plots/mitFehler",
                       se= T)


# Mittelwerte von Items und Umsetzung

get_item_summary(data,
                 file.excel = "C:/Heroes/Ergebnisse/Excel/Mittelwerte/Items.xlsx",
                 path.plot="C:/Heroes/Ergebnisse/Grafiken/Items nach Skala")







# Faktorladungen pro Skala

# skalen_loadings = check_loadings(skalen_scores)



# skalen ordnen -> wichtigste Haltekräfte nach Umfragewert

# plot_skalen_imp(create_skalen_imp(score_type = "scores_mean",
#                                   skalen_scores = skalen_scores,
#                                   group_index=get_index_by_group(data,
#                                                                  "B102",
#                                                                  group_keys = c(1:5))),
#                 mar=c(8,3,1,1),
#                 ylim = c(0,6),
#                 main = "",
#                 plot=T,
#                 file="C:/Heroes/Ergebnisse/PDF/aktuell/Deskriptive Analyse Plots/Wichtigkeit_gesamt.pdf")
# 







# wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft für verschiedene Gruppierungen

group_list = list("Gesamt" = 1:5, "SBBZ" = 2, "Leitung" = 1, "HZE" = 3:5)


ums_data = create_ums_data(data, umsetzung)



# wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft für verschiedene Gruppierungen




# wichtigste Haltekraft nach Oberskala -------------------------------------------------


# Berechnung der Skalen

data_skalen = skalen2
data_skalen_ohne = skalen2_ohne


skalen_scores = get_skalen_scores(data = data,
                                  skalen = data_skalen_ohne,
                                  skalen_names =  names(data_skalen_ohne))

plot_combined_imp(skalen = skalen2,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(11, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Oberskala_Wichtigkeit", 
                  combined=FALSE)

plot_combined_imp(skalen = skalen2,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(11, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Oberskala_Wichtigkeit_Umsetzung", 
                  combined=TRUE)


# Vergleiche der Skalen unter den Gruppen:

plot_group_diff(skalen=skalen2,
                umsetzung=umsetzung,
                ums_data=ums_data,
                mar=c(11, 6, 3,3),
                group_list = list("SBBZ" = 2,"HZE" = 3:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Oberskala_")

plot_group_diff(skalen2,
                umsetzung,
                ums_data,
                mar=c(11, 6, 3,3),
                group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Oberskala_")




# wichtigste Haltekraft nach Oberskala -------------------------------------------------


# Berechnung der Skalen

data_skalen = skalen3
data_skalen_ohne = skalen3_ohne


skalen_scores = get_skalen_scores(data = data,
                                  skalen = data_skalen_ohne,
                                  skalen_names =  names(data_skalen_ohne))

plot_combined_imp(skalen = skalen3,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(16, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit", 
                  combined=FALSE)

plot_combined_imp(skalen = skalen3,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(16, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Haltekraft_Wichtigkeit_Umsetzung", 
                  combined=TRUE)



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




# nach Skalen

data_skalen = skalen
data_skalen_ohne = skalen_ohne


skalen_scores = get_skalen_scores(data = data,
                                  skalen = data_skalen_ohne,
                                  skalen_names =  names(data_skalen_ohne))



plot_combined_imp(skalen = skalen,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(11, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Wichtigkeit", 
                  combined=FALSE)

plot_combined_imp(skalen = skalen,
                  data = data,
                  skalen_scores = skalen_scores,
                  umsetzung = umsetzung,
                  var_name_group= "B102",
                  mar=c(11, 9, 3,3),
                  ums_data = ums_data,
                  group_list = group_list,
                  path="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Wichtigkeit_Umsetzung", 
                  combined=TRUE)



# Vergleiche der Skalen unter den Gruppen:

plot_group_diff(skalen=skalen,
                umsetzung=umsetzung,
                ums_data=ums_data,
                mar=c(11, 6, 3,3),
                group_list = list("SBBZ" = 2,"HZE" = 3:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/")

plot_group_diff(skalen,
                umsetzung,
                mar=c(11, 6, 3,3),
                ums_data,
                group_list = list("Leitung" = 1,"Mitarbeitende" = 2:5),
                path = "C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/")




plot_most_important(data,
                    skalen_scores,
                    mar=c(12,4,5,3),
                    main="Wichtigste nach MA",
                    file="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Wichtigste_nachMA_Einrichtungen.pdf",
                    best_var = 0)




# RandomForest Modell ------------------------------------------------------

m = fit_randomForest(data = data,
                     ums_data = ums_data,
                     skalen = data_skalen,
                     skalen_names = names(data_skalen),
                     main="Potentielle Stellschrauben um Haltekraft zu erhöhen",
                     file="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Wichtigkeit nach Modell.pdf")



most_imp_model = dimnames(importance(m))[[1]][order(importance(m), decreasing = T)][1:13]


plot_most_important(data,
                    skalen_scores,
                    mar=c(12,4,5,3),
                    main="Wichtigste nach Modell",
                    file="C:/Heroes/Ergebnisse/Grafiken/Wichtigkeit und Umsetzung/Wichtigste_nachModell_Einrichtungen.pdf",
                    best_var = most_imp_model)


# Density Diagramm subjektive Haltekrafte


ums_scores = get_ums_scores(ums_data = ums_data, skalen = skalen, skalen_names = names(skalen))

plot_rel_density(data=data,
                 ums_scores=ums_scores,
                 mar=c(11,4,4,3),
                 main = "subjektive Haltekraft der Einrichtung",
                 file="C:/Heroes/Ergebnisse/Grafiken/Dichte/Haltekraft_gesamt.pdf")
  



#----------------------------------------------------------------------------
# Bis hierher
#---------------------------------------------------------------------------


# Dichteplots für Einrichtungen

for(i in unique(ums_df_agg$Einrichtung)){
  
  if(i == 11 | i == 40) next
  
  data_slice = ums_df_agg[get_index_by_group(data = data, var_name_group = "B107",group_keys = i), ]
  
  a = length(get_index_by_group(data_slice, "Gruppe", 1)) == 1
  b = length(get_index_by_group(data_slice, "Gruppe", 2)) == 1
  c = length(get_index_by_group(data_slice, "Gruppe", c(3:5))) == 1
  
  if(a) next
  if(b) next
  if(c) next
  
  lim_y = max(max(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", c(3:5))]))$y),
              max(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 2)]))$y),
              max(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 1)]))$y))
  
  pdf(file=paste("C:/Heroes/Test_", i,".pdf"), width=14, height = 12, paper = "a4r")
  
  par(xpd=F, mar=c(5,4.5,3,2))
  
  plot(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", c(3:5))])),
       col="#1b9e77", 
       main=imp_names[["B107"]][i],
       xlim=c(1, 6),
       ylim=c(0, lim_y), lwd=2, xlab="geschätzte Haltekraft")
  
  if(length(get_index_by_group(data_slice, "Gruppe", 2)) > 2){
    lines(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 2)])),
          col="#e6ab02", lwd=2)
  }
  if(length(get_index_by_group(data_slice, "Gruppe", 1)) > 2){
    lines(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 1)])),
          col="#e7298a", lwd=2)
  }
  
  points(jitter(data_slice$HK.subj.Einrichtung, 3), rep(0, nrow(data_slice)),
         pch="|", col=ifelse(data_slice$Gruppe == 1, "#1b9e77",
                             ifelse(data_slice$Gruppe == 2, "#e6ab02", "#e7298a")),
         cex=2)
  
  # c("#e7298a", "#e6ab02", "#1b9e77")
  
  # legend("topleft", fill = c("#1b9e77", "#e6ab02", "#e7298a"),
  #        legend = c("Leitung","SBBZ","HZE"), bty="n")
  
  legend("topleft", fill = c( "#1b9e77", "#e6ab02", "#e7298a"),
         legend = c("HZE","SBBZ","Leitung"), bty="n")
  
  dev.off()
}










# Was ist mit der Transparenz.jM los?

hist(ums_scores[["Transparenz.jM"]]$scores_mean)
hist(skalen_scores[["Transparenz.jM"]]$scores_mean)

plot(ums_scores[["Haltekraft"]]$scores_mean ~ jitter(skalen_scores[["Transparenz.jM"]]$scores_mean),
     ylab="Haltekraft", xlab ="Transparenz.jM")

plot(ums_scores[["Haltekraft"]]$scores_mean ~ jitter(ums_scores[["Transparenz.jM"]]$scores_mean),
     ylab="Haltekraft", xlab ="Transparenz.jM")

# -> wahrscheinlich finden das eben alle wichtig und bereits gut umgesetzt


skalen_imp = create_skalen_imp(score_type = "scores_mean", skalen_scores = ums_scores,
                               group_index = get_index_by_group(data, "B102", c(1:5)))

skalen_imp = skalen_imp[-1,]

plot_skalen_imp(skalen_imp,
                mar=c(8,3,4,1),
                ylim=c(0,1),
                main="bereits umgesetzt")

# jap! Transparenz.jM ist die die Variable die bereits am meisten in den Einrichtungen umgesetzt wird.









nrow(data_slice)


plot(1:5, col=c("#1b9e77", "#e7298a","#7570b3","#66a61e", "#e6ab02"), pch=20)

c("#1b9e77", "#e7298a","#7570b3","#66a61e", "#e6ab02")

index = get_index_by_group(data = data, "B102", group_keys = c(3:5))

ums_df_agg = ums_df_agg[index, ]

ums_df_agg = aggregate(ums_df_agg, by = list(ums_df_agg$Einrichtung), mean)  


rfm_agg = randomForest(Haltekraft ~ ., data = ums_df_agg[, -ncol(ums_df_agg)], ntree=1500)

par(mar=c(4,3,4,1))
varImpPlot(rfm_agg)
rfm_agg

par(mfrow=c(2,2), mar=c(2,2,2,2))
for(i in names(ums_df_agg)){
  plot(ums_df_agg[,"Haltekraft"] ~ ums_df_agg[,i], main=i)
}



rfm = randomForest(Haltekraft ~ ., data = ums_df, ntree=1500)

par(mar=c(4,3,4,1))
varImpPlot(rfm)
rfm


# gibt es unterschiede zwischen den Beschäftigten?

group_list = list("Alle" = 1:5, "SBBZ" = 2, "HZE" = 3:5, "Leitung" = 1, "Mitarbeiter"= 2:5)

for(i in 1:length(group_list)){
  
  index = get_index_by_group(data = data, "B102", group_keys = group_list[[i]])
  
  rfm = randomForest(Haltekraft ~ .,
                     data = ums_df[index,],
                     ntree=1500)
  
  par(mar=c(4,3,4,1))
  varImpPlot(rfm, main=names(group_list)[i])
  
}

# a = importance(rfm, type = 2)
# 
# imp = data.frame("Name"=rownames(a), "Importance"=a, row.names = NULL)
# 
# tmp_df = list()
# tmp_df[]







# plot_skalen_imp(compare_skalen_ums(skalen_scores, ums_scores, score_type = "scores_mean"))


# Random Forest Analyse

ums_tmp = create_model_df(ums_scores=get_skalen_scores(data = data,
                                                       skalen = data_skalen,
                                                       skalen_names =  names(data_skalen)),
                          score_type = "scores_mean")



ums_df = ums_tmp[[1]]

rfm = randomForest(Haltekraft ~ ., data = ums_df, ntree=1500)

# m1 = lm(Haltekraft ~ ., data = ums_df)

# summary(m1)
# anova(m1)

# was sind die wichtigsten Haltekräfte?

par(mar=c(4,3,4,1))
rfm_import = varImpPlot(rfm)


# was sind die wichtigsten 10?

tmp_names = names(rfm_import[,1])

tmp_names[order(rfm_import, decreasing = T)]

best = tmp_names[order(rfm_import, decreasing = T)][1:10]


par(mar=c(7,7,4,2), mfrow=c(2,2))

for(i in best){
  
  check_skalen_by_group(data=data, skalen_scores=skalen_scores, pred_var=pred_var,
                        score_type = "scores_mean", score_name = i,
                        group_type = "B102", group_index = list(1,2,c(3,4,5),3,c(4,5)), group_names = imp_names[["B102_edit"]])
}

# gibt es negative Zusammenhänge?

# par(mfrow=c(2,2))
# 
# for(i in best){
# 
#   partialPlot(rfm, x.var = paste(i), pred.data = ums_df)
# }



# nein!

# Scatterplots

index = ums_tmp[[2]]

names(data)

FB =  data[,"B102"]

FB[FB >= 3] = 3

table(FB)

ums_df$FB = FB[index]

variables = names(ums_df)[!names(ums_df) %in% c("Haltekraft", "FB")]


col = c("red", "blue", "black")

par(mfrow=c(2,2), mar=c(4,4,4,4))

for(i in 1:length(variables)){
  
  plot(jitter(ums_df$Haltekraft, amount = 0.05) ~ ums_df[,variables[i]],
       # plot(ums_df$Haltekraft ~ ums_df[,variables[1]],
       pch = 20,
       col = ifelse(ums_df$FB == 1, col[1], ifelse(ums_df$FB == 2, col[2], col[3])),
       main= paste("Haltekraft ~ ", variables[i]),
       xlab= variables[i],
       ylab= "Haltekraft")
  
  spline = smooth.spline(ums_df[,variables[i]], jitter(ums_df$Haltekraft, amount = 0.05), spar = 0.5)
  
  pred = partialPlot(rfm, x.var = paste(variables[i]), pred.data = ums_df, plot=F)
  
  lines(spline, lwd = 2, lty = 2)
  lines(pred$x, pred$y, lwd=2, lty=2, col="red")
  
  if(i %% 3 == 0){
    plot(1:10, 1:10, type = "n", xaxt="n", yaxt="n", xlab="", ylab="")
    legend("topleft",
           legend = c("Leitung","SBBZ", "HZE.Gesamt"),
           fill= col, cex = 1, bty = "n")
    
  }
}

# Welche Einrichtungen haben bei den wichtigsten ein Umsetzungsproblem?

for(i in best){
  
  agg_ums = create_agg_ums(skalen_tmp=data_skalen, umsetzung = umsetzung, skala=i)
  
  plot_agg_ums(agg_ums = agg_ums, skala=i)
}


# Für die Einrichtungen aufschlüsseln

agg_df = create_agg_df(skalen_tmp = data_skalen, umsetzung = umsetzung, variables = best)

par(mfrow=c(2,2))
plot_einrichtungen(agg_df)



check_skalen(data=data,
             skalen_scores=skalen_scores,
             variables=best,
             pred_var=pred_var,
             score_type = "scores_mean",
             group_names = imp_names)




ums_df




fact_data = data[,c(data_skalen[["Paed.Halt.1"]], data_skalen[["Paed.Halt.2"]])]

fact_data = apply(fact_data, 2, jitter, amount=0.01)

factanal(fact_data, factors = 2)








# --------------------------------------------------
# nicht so wichtig?

check_items_by_group(data = data,
                     variables = "Begriffe",
                     group_index = list(1,2,c(3,4,5),3,c(4,5)),
                     group_names = imp_names[["B102_edit"]][1:5])
