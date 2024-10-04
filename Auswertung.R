source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


library(randomForest)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")





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

descriptive_anal_plots(data=data, path="C:/Heroes/Ergebnisse/PDF/aktuell/Deskriptive Analyse Plots")



# check_items_by_group(data = data,
#                      variables = "Begriffe",
#                      group_index = list(1,2,c(3,4,5),3,c(4,5)),
#                      group_names = imp_names[["B102_edit"]][1:5])



data_skalen = skalen
data_skalen_ohne = skalen_ohne


skalen2



# check_items_by_group(data = data,
#                      variables = names(keys),
#                      group_index = list(1,2,c(3,4,5),3,c(4,5)),
#                      group_names = imp_names[["B102_edit"]][1:5])
# 
# 
# check_items_by_group(data = data,
#                      variables = "Krisenauf",
#                      group_index = list(1,2,c(3,4,5),3,c(4,5)),
#                      group_names = imp_names[["B102_edit"]][1:5])

# -> auf Skalenebene ists interessanter



skalen_scores = get_skalen_scores(data = data,
                                  skalen = data_skalen_ohne,
                                  skalen_names =  names(data_skalen_ohne))



# skalen ordnen -> wichtigste Haltekräfte nach Umfragewert

plot_skalen_imp(create_skalen_imp(score_type = "scores_mean",
                                  skalen_scores = skalen_scores,
                                  group_index=get_index_by_group(data,
                                                                 "B102",
                                                                 group_keys = c(1:5))),
                mar=c(8,3,1,1), ylim = c(0,6), main = "")



# 
# check_skalen(data=data, 
#              skalen_scores=skalen_scores,
#              variables=names(skalen),
#              pred_var=pred_var,
#              score_type = "scores_mean",
#              group_names = imp_names)
# 
# check_skalen(data=data, 
#              skalen_scores=skalen_scores,
#              variables="Paed.Halt.2",
#              pred_var=pred_var,
#              score_type = "scores_mean",
#              group_names = imp_names)



# Faktorladungen pro Skala

check_loadings(skalen_scores)


# wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft für verschiedene Gruppierungen

group_list = list("Alle" = 1:5, "SBBZ" = 2, "Leitung" = 1, "HZE" = 3:5)


ums_data = create_ums_data(data, umsetzung)

plot_combined_imp(skalen, data, skalen_scores,
                             umsetzung, ums_data, group_list)



# Vergleiche der Skalen unter den Gruppen:

plot_group_diff(skalen, umsetzung, ums_data, group_list = list("SBBZ" = 2,"HZE" = 3:5))

plot_group_diff(skalen, umsetzung, ums_data, group_list = list("Leitung" = 1,"Mitarbeiter" = 2:5))



# Umsetzungsscores für RF generieren:

ums_scores = get_ums_scores(ums_data, skalen = data_skalen, names(data_skalen))




Halte_scores = get_skalen_scores(data = data,
                                 skalen = data_skalen,
                                 skalen_names = "Haltekraft")


ums_scores[["Haltekraft"]] = Halte_scores[["Haltekraft"]]

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



ums_tmp = create_model_df(ums_scores = ums_scores, score_type = "scores_mean")

ums_df = ums_tmp[[1]]


ums_df_agg = ums_df

ums_df_agg$Einrichtung = data$B107
ums_df_agg$Gruppe = data$B102


for(i in unique(ums_df_agg$Einrichtung)){
  
  if(i == 11 | i == 40) next
  
  data_slice = ums_df_agg[get_index_by_group(data, "B107", i), ]
  
  plot(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", c(3:5))])),
       col="#e7298a", 
       main=imp_names[["B107"]][i],
       xlim=c(1, 6),
       ylim=c(0, 1), lwd=2, xlab="Haltekraft")
  
  if(length(get_index_by_group(data_slice, "Gruppe", 2)) > 2){
    lines(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", 2)])),
          col="#e6ab02", lwd=2)
  }
  if(length(get_index_by_group(data_slice, "Gruppe", 1)) > 2){
    lines(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", 1)])),
          col="#1b9e77", lwd=2)
  }
  
  points(jitter(data_slice$Haltekraft, 3), rep(0, nrow(data_slice)),
         pch="|", col=ifelse(data_slice$Gruppe == 1, "#1b9e77",
                             ifelse(data_slice$Gruppe == 2, "#e6ab02", "#e7298a")),
         cex=2)
  
  legend("topleft", fill = c("#1b9e77", "#e6ab02", "#e7298a"),
         legend = c("Leitung","SBBZ","HZE"), bty="n")
  
}


data_slice = ums_df_agg

plot(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", c(3:5))])),
     col="#e7298a", 
     main="Alle Einrichtungen",
     xlim=c(1, 6),
     ylim=c(0, 0.7), lwd=2, xlab="Haltekraft")

if(length(get_index_by_group(data_slice, "Gruppe", 2)) > 2){
  lines(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", 2)])),
        col="#e6ab02", lwd=2)
}
if(length(get_index_by_group(data_slice, "Gruppe", 1)) > 2){
  lines(density((data_slice$Haltekraft[get_index_by_group(data_slice, "Gruppe", 1)])),
        col="#1b9e77", lwd=2)
}

points(jitter(data_slice$Haltekraft, 3), rep(0, nrow(data_slice)),
       pch="|", col=ifelse(data_slice$Gruppe == 1, "#1b9e77",
                           ifelse(data_slice$Gruppe == 2, "#e6ab02", "#e7298a")),
       cex=2)

legend("topleft", fill = c("#1b9e77", "#e6ab02", "#e7298a"),
       legend = c("Leitung","SBBZ","HZE"), bty="n")


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
