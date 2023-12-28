source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")


check_items_by_group(data = data,
                     variables = names(keys),
                     group_index = list(1,2,c(3,4,5),3,c(4,5)),
                     group_names = imp_names[["B102_edit"]][1:5])


skalen_scores = get_skalen_scores(data = data,
                                  skalen = skalen,
                                  skalen_names =  names(skalen))



# skalen_subsets
score_type = "scores_mean"
score_name = "Beziehung.2"
group_index = c(2,3)
group_type = "B102"
group_names = imp_names


check_skalen(data=data, 
             skalen_scores=skalen_scores,
             variables=skalen_names,
             pred_var=pred_var,
             score_type = "scores_fact",
             group_names = imp_names)
  


imp_names


tmp = data[,pred_var]

tmp[!complete.cases(tmp),] = NA

scores = skalen_scores$Beziehung.2$scores_fact

tmp_scores = cbind(scores, tmp)


dist_plot(test, n=1, m = 2)


group_index = c(2,3)

ls = list()

m = length(group_index)

for(i in 1:m){
  
  ls[[i]] = res$t[which(res$B102 %in% group_index[i])]
}

mat = matrix(0, m, m)


for(i in 1:m){
  for(j in 1:m){
    if(i!=j){
      mat[i,j] = round(t.test(ls[[i]], ls[[j]])$p.value,3)
    }else{
      mat[i,j] = round(mean(ls[[i]]),3)
    }
  }
}


# NA_vec = rep(NA, nrow(tmp))
# 
# NA_vec[tmp$B102 == 2] = 2
# NA_vec[tmp$B102 %in% c(3,4,5)] = 3

# tmp$B102_edited = NA_vec

get_pred_variables 



# check_skalen_by_groups


sum(is.na(data[,pred_var[2]]))



skalen_scores[[2]]$scores_mean



names(skalen_scores)

plot(scores_fact ~ scores_mean)

a = 0
k = 1

while(a <= 0.05){
  fact = factanal(tmp_jittered, k, scores = "regression")
  a = fact$PVAL
  k = k + 1
}

fact$loadings

hist(fact$scores[,1])


hist(jitter(tmp[,1]))
hist(tmp[,1])



names(skalen)

tmp = data[,skalen[["Paed.Halt.1"]]]

tmp_var = data[,pred_var]

tmp


names(umsetzung)

umsetzung[["Paed.Halt.1"]]





plot(density(group[[1]][,n], bw = 0.5), xlim=c(1,m))
lines(density(group[[2]][,n], bw = 0.5))
lines(density(group[[3]][,n], bw = 0.5))
lines(density(group[[4]][,n], bw = 0.5))
lines(density(group[[5]][,n], bw = 0.5))
mean(group[[5]][,1])


# Arbeitsbereiche -> B102

data_sub <- data[which(data$B102 == 3),]


names(keys)


data_item = data_sub[,l[["Begriffe"]]]

data_item = data_item[complete.cases(data_item),]


groups = list()

groups[[1]] = data_item 



names(data)






