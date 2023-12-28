

import_data_surv <- function(Path){
  
  data <- read.csv("C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv",
                   skipNul = T, na.strings = "-9", stringsAsFactors = FALSE)
  
  
  data_compl <- data[!(is.na(as.numeric(data$A003)) | as.numeric((data$A003) == 2)),]
  
  
  data_compl <- data_compl[,-c(598:680)]
  
  # CASE umbenennen
  names(data_compl)[1] <- "CASE"
  
  # Eva Stuttgart rausnehmen
  data_compl <- data_compl[-which(data_compl$CASE==1164), ]
  
  ## Umkodieren Soziale Erwünschtheit
  
  data_compl$M401_01 <- 5 - as.numeric(data_compl$M401_01)
  
  #beide items mit 4 bewertet
  index <- which(data_compl$M401_01 == 4 & data_compl$M401_02 == 4)
  
  data_compl$sozialErw <- FALSE
  
  data_compl$sozialErw[index] <- TRUE
  
  #Soziale Erwünschtheit rausschmeißen
  data_compl <- data_compl[!data_compl$sozialErw,]
  
  
  # items in numeric transformieren
  
  test <- apply(data_compl, 2, FUN=function(x){ mean(nchar(x), na.rm=T)})
  
  col_index <- test < 3
  
  col_index[c(1:6, 598:ncol(data_compl))] <- FALSE # die Spaltenzahlen muessen im original Format behalten werden
  
  for(i in 1:ncol(data_compl)){
    
    if(col_index[i] == TRUE){
      
      data_compl[,i] <- as.numeric(data_compl[,i])
    }
  }
  
  return(data_compl)
}


create_simple_key = function(string, number){
  
  res = c()
  
  for(i in 1:length(number)){
    if(number[i] < 10) res = c(res, paste0("0",number[i]))
    if(number[i] >= 10) res = c(res, as.character(number[i]))
  }
  
  return(paste0(string,res))
  
}


create_simple_key2 = function(string1, number, string2){
  
  res = c()
  
  for(i in 1:length(number)){
    if(number[i] < 10) res = c(res, paste0("0",number[i]))
    if(number[i] >= 10) res = c(res, as.character(number[i]))
  }
  
  return(paste0(string1,res,string2))
  
}


create_complex_key = function(string, number1, number2){
  
  res1 = c()
  res2 = c()
  
  for(i in 1:length(number1)){
    if(number1[i] < 10) res1 = c(res1, paste0("0",number1[i]))
    if(number1[i] >= 10) res1 = c(res1, as.character(number1[i]))
  }
  
  for(i in 1:length(number2)){
    if(number2[i] < 10) res2 = c(res2, paste0("0",number2[i]))
    if(number2[i] >= 10) res2 = c(res2, as.character(number2[i]))
  }
  
  return(paste0(string,res1,"_",res2))
}


add_missing = function(vec, m){
  
  check = 1:m %in% names(vec) 
  tmp = numeric(m)
  tmp[check] = vec
  return(tmp)
}


item_dist = function(group1, group2, n, m){
  
  
  vec1 = table(group1[,n])/length(group1[,n])
  vec2 = table(group2[,n])/length(group2[,n])
  
  vec1 = add_missing(vec1, 6)
  vec2 = add_missing(vec2, 6)
  
  vec3 = vec2 - vec1
  
  
  
  return(sqrt(sum(vec3^2)))
}


item_abs = function(group1, group2, n, m){
  
  
  vec1 = table(group1[,n])/length(group1[,n])
  vec2 = table(group2[,n])/length(group2[,n])
  
  vec1 = add_missing(vec1, m)
  vec2 = add_missing(vec2, m)
  
  vec3 = vec2 - vec1
  return(sum(abs(vec3))/2) # weil maximal |1|+|-1| sein kann
  # return(sum(abs(vec3))) 
  
  
}


dist_plot = function(group, n, m, n_group){
  
  res = matrix(0, n_group, n_group)
  
  for(i in 1:n_group){
    for(j in 1:n_group){
      if(i!=j){
        if( i < j){ # unteres dreieck
          res[i,j] = round(item_abs(group1 = group[[i]], group2 = group[[j]], n, m), 3)
        }
        if(i > j){ # oberes dreieck
          if(length(unique(group[[i]][,n]))>1 | length(unique(group[[j]][,n]))>1){
            res[i,j] = round(t.test(group[[i]][,n], group[[j]][,n])$p.value,3)
          }else{
            if(length(unique(group[[i]][,n]))<=1 & length(unique(group[[j]][,n]))<=1){
              if(unique(group[[i]][,n]) != unique(group[[j]][,n])){
                res[i,j] = 0 # komplett unterschiedlich wenn nur eine Kategorie gewählt, aber unterschiedliche
              }else{
                res[i,j] = 1 # komplett gleich
              }
            }
          }
        }
      }else{ # diagonale
        res[i,j] = round(mean(group[[i]][,n]),3)
        
        # res[i,j] = median(group[[i]][,n])
      }
    }
  }
  
  # Betrag der Differenz
  res2 = res
  diag(res2) = NA
  res2[lower.tri(res2)] = NA
  tmp = res2[upper.tri(res2)]
  tmp = ifelse(tmp <= 0.25, 1, ifelse(tmp <= 0.5, 2, ifelse(tmp <= 0.75, 3, 4)))
  res2[upper.tri(res2)] = tmp
  
  # p-wert
  res3 = res
  diag(res3) = NA
  res3[upper.tri(res3)] = NA
  tmp = res3[lower.tri(res3)]
  tmp = ifelse(tmp <= 0.01, 4, ifelse(tmp <= 0.05, 3, ifelse(tmp <= 0.1, 2, 1)))
  res3[lower.tri(res3)] = tmp
  
  par(mar=c(6,6,4,2))
  
  image(res3, breaks = c(0, 1, 2, 3, 4),
        zlim=c(0,1),
        col=rev(c("grey30","grey50","grey80","white")),
        useRaster = FALSE,
        xaxt="n", yaxt="n",
        main=names(group[[1]])[n])
  image(res2, breaks = c(0, 1, 2, 3, 4), add =T,
        zlim=c(0,1),
        col=rev(c("grey30","grey50","grey80","white")))
  axis(1, at = seq(0,1, length.out=n_group), labels=names(group), las=3)
  axis(2, at = seq(0,1, length.out=n_group), labels=names(group), las=2)
  
  for(i in 1:n_group){
    for(j in 1:n_group){
      # if(i == j) text((i-1)/4,(j-1)/4, diag(res)[i])
      # if(i < j) text((i-1)/4,(j-1)/4, res[i,j])
      text(seq(0,1, length.out=n_group)[i], seq(0,1, length.out=n_group)[j], res[i,j])
    }
    
    abline(v=(i-1)/(n_group-1)-(1/((n_group-1)*2)))
    abline(h=(i-1)/(n_group-1)+(1/((n_group-1)*2)))
  }
}







t_plot = function(group, n, m, main){
  
  res = matrix(0, m, m)
  
  for(i in 1:m){
    for(j in 1:m){
      if(i!=j){
        res[i,j] = round(t.test(group[[i]][,n], group[[j]][,n])$p.value,3)
      }else{
        res[i,j] = round(mean(group[[i]][,n], na.rm=T),3)
      }
    }
  }
  
  res2 = res
  diag(res2) = NA
  # res2[lower.tri(res2)] = NA
  # tmp = res2[upper.tri(res2)]
  # tmp = ifelse(tmp <= 0.01, 4, ifelse(tmp <= 0.05, 3, ifelse(tmp <= 0.1, 2, 1)))
  # res2[upper.tri(res2)] = tmp
  
  res2 = ifelse(res2 <= 0.01, 4, ifelse(res2 <= 0.05, 3, ifelse(res2 <= 0.1, 2, 1)))
  
  
  image(res2, breaks = c(0, 1, 2, 3, 4),
        zlim=c(0,1),
        col=rev(c("grey10","grey50","grey80","white")),
        useRaster = FALSE,
        xaxt="n", yaxt="n",
        main=main)
  axis(1, at = seq(0,1, length.out=m), labels=names(group), las=3)
  axis(2, at = seq(0,1, length.out=m), labels=names(group), las=2)
  
  for(i in 1:m){
    for(j in 1:m){
      # if(i == j) text((i-1)/4,(j-1)/4, diag(res)[i])
      # if(i < j) text((i-1)/4,(j-1)/4, res[i,j])
      text(seq(0,1, length.out=m)[i], seq(0,1, length.out=m)[j], res[i,j])
    }
    
    abline(v=(i-1)/(m-1)-(1/((m-1)*2)))
    abline(h=(i-1)/(m-1)+(1/((m-1)*2)))
  }
}


create_subsets = function(data, var_str, group_index, group_names){
  
  group = list()
  
  for(i in 1:length(group_index)){
    data_sub = data[which(data$B102 %in% group_index[[i]]), keys[[var_str]]]
    data_sub = data_sub[complete.cases(data_sub),]
    group[[group_names[i]]] = data_sub
  }
  
  return(group)
}



check_items_by_group = function(data, variables, group_index, group_names){
  
  for(var_str in variables){
    
    group = create_subsets(data = data, var_str = var_str,
                           group_index = group_index,
                           group_names = group_names)
    
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
}


get_skalen_scores = function(data, skalen, skalen_names){
  
  skalen_scores = list()
  
  
  for(i in 1:length(skalen_names)){
    
    tmp_ls = list()
    
    
    tmp = data[,skalen[[skalen_names[i]]]]
    
    na_index = complete.cases(tmp)
    
    
    if(is.null(dim(tmp))){
      
      tmp_ls[["scores_mean"]] = tmp
      
      tmp_ls[["scores_fact"]] = rep(NA, length(tmp))
      
      tmp_ls[["loadings"]] = rep(NA, 1)
      
      tmp_ls[["p"]] = NA
      
      skalen_scores[[skalen_names[i]]] = tmp_ls
      
    }else{
      
      tmp = tmp[na_index,]
      
      tmp_na = rep(NA, length(na_index))
      
      tmp_na[na_index] = as.numeric(apply(tmp, 1, mean, na.rm=T))
      
      tmp_ls[["scores_mean"]] = tmp_na
      
      if(ncol(tmp) < 3){
        tmp_ls[["scores_fact"]] = rep(NA, nrow(tmp))
        
        tmp_ls[["loadings"]] = rep(NA, ncol(tmp))
        
        tmp_ls[["p"]] = NA
        
        skalen_scores[[skalen_names[i]]] = tmp_ls
        
      }else{
        
        tmp_jittered = apply(tmp, 2, jitter, factor = 0.01)
        
        fact = factanal(tmp_jittered, 1, scores = "regression")
        
        tmp_na[na_index] = fact$scores
        
        tmp_ls[["scores_fact"]] = tmp_na
        
        tmp_ls[["loadings"]] = fact$loadings
        
        tmp_ls[["p"]] = fact$PVAL
        
        
        skalen_scores[[skalen_names[i]]] = tmp_ls
      }
    }
  }
  
  return(skalen_scores)
}


check_skalen_by_group = function(data, skalen_scores, pred_var,
                                 score_type, score_name,
                                 group_type, group_index, group_names){
  
  
  if(is.na(skalen_scores[[score_name]][["p"]])){
    
    return(NA)
  }
  
  group_names = group_names[[group_type]][group_index]
  
  tmp = data[,pred_var]
  
  tmp[!complete.cases(tmp),] = NA
  
  scores = skalen_scores[[score_name]][[score_type]]
  
  tmp_scores = cbind(scores, tmp)
  
  m = length(group_index)
  
  group = list()
  
  for(i in 1:m){
    
    group[[group_names[i]]] = data.frame(tmp_scores[tmp_scores[,group_type] == group_index[i], 1])
    
  }
  
  
  t_plot(group = group, n=1, m=length(group_index), main=paste(score_name, "-", group_type, "-", score_type))
  
}


check_skalen = function(data, skalen_scores, variables, pred_var,
                        score_type, score_name, group_names){
  
  for(var_str in variables){
    
    par(mfrow=c(2,2))
    
    check_skalen_by_group(data=data, skalen_scores=skalen_scores, pred_var=pred_var,
                          score_type = score_type, score_name = var_str,
                          group_type = "B102", group_index = c(2,3), group_names = imp_names)
    
    
    check_skalen_by_group(data=data, skalen_scores=skalen_scores, pred_var=pred_var,
                          score_type = score_type, score_name = var_str,
                          group_type = "B103", group_index = c(1,2,3), group_names = imp_names)
    
    check_skalen_by_group(data=data, skalen_scores=skalen_scores, pred_var=pred_var,
                          score_type = score_type, score_name = var_str,
                          group_type = "B105", group_index = c(1,2,3), group_names = imp_names)
    
    
    check_skalen_by_group(data=data, skalen_scores=skalen_scores, pred_var=pred_var,
                          score_type = score_type, score_name = var_str,
                          group_type = "B110", group_index = c(1,2,3), group_names = imp_names)
    
  }
}

