source("C:/Heroes/Funktionen.R")
source("C:/Heroes/keys.R")
library(MASS)


data = import_data_surv(Path="C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv")

for(var_str in names(keys)){
  
  group = create_subsets(data = data, var_str = var_str,
                         group_index = list(1,2,c(3,4,5),3,c(4,5)),
                         # group_index = list(2,c(3,4,5)),
                         group_names = imp_names[["B102_edit"]][1:5])
  
  n_group = length(group)
  
  for(n in 1:ncol(group[[1]])){
    tmp = c()
    for(i in 1:n_group){
      tmp = c(tmp, group[[i]][,n])
    }
    m = max(tmp)
    dist_plot(group = group, n = n, m = m, n_group=n_group)
  }
}



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






