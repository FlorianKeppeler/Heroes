source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


library(randomForest)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")

data_skalen = skalen
data_skalen_ohne = skalen_ohne






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
                                  group_index=get_index_by_group(data, group_index = c(1:5))),
                mar=c(8,3,1,1))



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





ums_scores = get_skalen_scores(data = ums_data,
                               skalen = data_skalen,
                               skalen_names =  names(data_skalen))


plot_skalen_imp(create_skalen_imp(score_type = "scores_mean",
                                  skalen_scores = ums_scores), mar=c(8,3,1,1), main="")


# plot_skalen_imp(compare_skalen_ums(skalen_scores, ums_scores, score_type = "scores_mean"))


# Random Forest Analyse

ums_tmp = create_model_df(ums_scores, score_type = "scores_mean")

ums_df = ums_tmp[[1]]


  
rfm = randomForest(Haltekraft ~ ., data = ums_df, ntree=10000)

m1 = lm(Haltekraft ~ ., data = ums_df)

summary(m1)
anova(m1)

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
