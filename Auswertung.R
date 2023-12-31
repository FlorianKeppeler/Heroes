source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


library(randomForest)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")




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
                                  skalen = skalen2_ohne,
                                  skalen_names =  names(skalen2_ohne))



# skalen ordnen -> wichtigste Haltekräfte nach Umfragewert

plot_skalen_imp(create_skalen_imp(score_type = "scores_mean",
                                  skalen_scores = skalen_scores), mar=c(8,3,1,1))



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


# wichtigste Haltekraft nach Abgleich mit tatsächlicher Haltekraft

ums_data = create_ums_data(data, umsetzung)


ums_proz = create_ums_proz(skalen2_ohne, umsetzung, ums_data)

skalen_imp = create_skalen_imp(score_type = "scores_mean",
                               skalen_scores = skalen_scores)


merged_imp = merge(skalen_imp, ums_proz, by.x = "Name", by.y="Skalen")

merged_imp = merged_imp[order(merged_imp[,"Wert"], decreasing = T),]



# Gemeinsame Darstellung: Skalen_scores und Umsetzung

plot_skalen_imp(merged_imp, mar=c(8,3,1,3))

offset = 1

arrows(x0 = 1:nrow(merged_imp)+0.05, x1 = 1:nrow(merged_imp)+0.05,
      y0 = rep(offset, nrow(merged_imp)),  y1 = offset + merged_imp[,3]*max(merged_imp[,2]-offset),
      length=0, col="grey20", lwd=3)

axis(4, at = c(offset, (max(merged_imp[,2]) + offset)/2, max(merged_imp[,2])), labels = c(0, 0.5, 1))
abline(h=offset, lty=2)




ums_scores = get_skalen_scores(data = ums_data,
                               skalen = skalen2,
                               skalen_names =  names(skalen2))


plot_skalen_imp(create_skalen_imp(score_type = "scores_mean",
                                  skalen_scores = ums_scores), mar=c(8,3,1,1))


# plot_skalen_imp(compare_skalen_ums(skalen_scores, ums_scores, score_type = "scores_mean"))


# Random Forest Analyse

ums_df = create_model_df(ums_scores, score_type = "scores_mean")
  
rfm = randomForest(Haltekraft ~ ., data = ums_df)

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

par(mfrow=c(2,2))

for(i in best){
  
  partialPlot(rfm, x.var = paste(i), pred.data = ums_df)
}

# nein!


# Welche Einrichtungen haben bei den wichtigsten ein Umsetzungsproblem?

for(i in best){
  
  agg_ums = create_agg_ums(skalen_tmp=skalen2, umsetzung = umsetzung, skala=i)
  
  plot_agg_ums(agg_ums = agg_ums, skala=i)
}


# Für die Einrichtungen aufschlüsseln

agg_df = create_agg_df(skalen_tmp = skalen2, umsetzung = umsetzung, variables = best)

par(mfrow=c(2,2))
plot_einrichtungen(agg_df)



check_skalen(data=data,
             skalen_scores=skalen_scores,
             variables=best,
             pred_var=pred_var,
             score_type = "scores_mean",
             group_names = imp_names)




fact_data = data[,c(skalen2[["Paed.Halt.1"]], skalen2[["Paed.Halt.2"]])]

fact_data = apply(fact_data, 2, jitter, amount=0.01)

factanal(fact_data, factors = 2)
