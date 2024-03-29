
#  noch nicht fertig
##################################################


get_codebook_varnames = function(var_name){
  
  tmp = c()
  
  for (var in keys[[var_name]]){
    
    a = codebook[codebook$Variable == var, "Variable Label"][1]
    print(a)
    c = unlist(strsplit(a, ":")); c = c[length(c)]
    
    c = unlist(strsplit(c, " "))[-1]
    
    c = paste(c, collapse = " ")
    
    tmp = c(tmp, c)
  }
  return(tmp)
}


####################################################


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



get_ranking = function(data, var_name){
  
  df_begr = data.frame("mean"=apply(data[,keys[[var_name]]], 2, mean, na.rm=T), "name"=imp_names[[var_name]])
  
  return (df_begr[order(df_begr$mean, decreasing = T),])
}


plot_ranked = function(data, var_name, yx, mar, main, file){
  
  ranked = get_ranking(data, var_name)
  pdf(file=file, width=14, height = 12, paper = "a4r")
  par(mar=mar)
  plot(ranked$mean, xaxt = "n", xlab = "", pch = 20,
       ylab="", yaxt="n", main = main, ylim=c(1, length(yx)))
  arrows(x0 = 1:nrow(ranked), y0 = rep(0, nrow(ranked)),
         y1 = ranked[,1], col="grey50", length = 0, lwd=2)
  points(ranked$mean,  pch = 20)
  abline(h=1:length(yx), lwd=0.5, col="grey60")
  axis(1, las=2, at = 1:nrow(ranked), labels = ranked$name)
  axis(2, las=2, at = 1:length(yx), labels=yx)
  dev.off()
}

plot_binary = function(data, var_name, main, mar, ylab, file, ranked=FALSE){
  
  
  tmp = apply(data[,keys[[var_name]]] - 1, 2, sum, na.rm=T)
  
  if(ranked == TRUE){
    tmp_labels = imp_names[[var_name]][order(tmp, decreasing = T)]
    tmp = sort(tmp, decreasing = T)
    
  }
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
    par(mar=mar)
    
  
    plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp)))
    
    arrows(x0 = 1:length(tmp), y0 = rep(0, length(tmp)),
           y1 = tmp, col="grey50", length = 0, lwd=2)
    points(1:length(tmp), tmp, pch = 20)
    
    if(ranked == TRUE){
      
      axis(side=1, at=1:length(tmp), labels = tmp_labels, las=2)
    }else{
    
      axis(side=1, at=1:length(tmp), labels = imp_names[[var_name]], las=2)
    }
  
  dev.off()
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
      
      tmp_ls[["scores_mean"]] = as.numeric(tmp)
      
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


get_index_by_group = function(data, var.name, group_keys){
  
  return(which(data[,var.name] %in% group_keys))
}


check_skalen_by_group = function(data, skalen_scores, pred_var,
                                 score_type, score_name,
                                 group_type, group_index, group_names){
  
  if(score_type == "scores_fact"){
    
    if(is.null(skalen_scores[[score_name]][["p"]])){
      
      return(NA)
    } 
    
    if(is.na(skalen_scores[[score_name]][["p"]])){
      
      return(NA)
    }
  }
  
  if(is.list(group_names)){
    
    group_names = group_names[[group_type]][group_index]
  }
  
  tmp = data[,pred_var]
  
  if(group_type != "B102"){
    
    tmp[!complete.cases(tmp),] = NA
  }
  
  scores = skalen_scores[[score_name]][[score_type]]
  
  tmp_scores = cbind(scores, tmp)
  
  m = length(group_index)
  
  group = list()
  
  for(i in 1:m){
    
    group[[group_names[i]]] = data.frame(tmp_scores[tmp_scores[,group_type] %in% group_index[[i]], 1])
    
  }
  
  
  t_plot(group = group, n=1, m=length(group_index), main=paste(score_name, "-", group_type, "-", score_type))
  
}


check_skalen = function(data, skalen_scores, variables, pred_var,
                        score_type, score_name, group_names){
  
  for(var_str in variables){
    
    par(mfrow=c(2,2), mar=c(4,4,4,4))
    
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


create_skalen_imp = function(score_type, skalen_scores=skalen_scores, group_index){
  
  tmp_name = c()
  tmp_mean = c()
  
  for(i in 1:length(skalen_scores)){
    
    tmp_mean = c(tmp_mean, mean(skalen_scores[[i]][[score_type]][group_index], na.rm = T))
    tmp_name = c(tmp_name, names(skalen_scores)[i])
  }
  
  most_imp = data.frame("Name" = tmp_name[order(tmp_mean, decreasing = T)], "Wert" = sort(tmp_mean, decreasing = T))
  
  return(most_imp)
  
}


plot_skalen_imp = function(most_imp, mar, ylim, main){
  
  par(mfrow=c(1,1), mar=mar)
  
  plot(most_imp[,2], xaxt="n", xlab="", pch=20, ylim=ylim, main=main)
  axis(side=1, at=1:nrow(most_imp), labels = most_imp[,1], las=2)
  arrows(x0 = 1:nrow(most_imp), y0 = rep(0, nrow(most_imp)),
         y1 = most_imp[,2], col="grey50", length = 0)
  points(1:nrow(most_imp), most_imp[,2], pch = 20)
  
}


create_ums_data = function(data, umsetzung){
  
  data_ums = data
  
  for(i in 1:nrow(umsetzung)){
    
    data_ums[,umsetzung[i,1]] = data_ums[,umsetzung[i,1]] - 1
    
    data_ums[,umsetzung[i,2]] = data_ums[,umsetzung[i,2]] * data_ums[,umsetzung[i,1]]
  }
  
  return(data_ums)
}



plot_combined_imp = function(skalen, data, skalen_scores,
                             umsetzung, ums_data, group_list){
  
  for(i in 1:length(group_list)){
    
    ums_proz = create_ums_proz(data_skalen_ohne, umsetzung, ums_data,
                               group_index=get_index_by_group(data, group_keys = group_list[[i]]))
    
    skalen_imp = create_skalen_imp(score_type = "scores_mean",
                                   skalen_scores = skalen_scores,
                                   group_index=get_index_by_group(data, group_keys = group_list[[i]]))
    
    
    merged_imp = merge(skalen_imp, ums_proz, by.x = "Name", by.y="Skalen")
    
    merged_imp = merged_imp[order(merged_imp[,"Wert"], decreasing = T),]
    
    
    
    # Gemeinsame Darstellung: Skalen_scores und Umsetzung
    
    plot_skalen_imp(merged_imp, mar=c(8,3,3,3), main = names(group_list)[i], ylim=c(1, 6))
    
    offset = 1
    
    arrows(x0 = 1:nrow(merged_imp)+0.05, x1 = 1:nrow(merged_imp)+0.05,
           y0 = rep(offset, nrow(merged_imp)),  y1 = offset + merged_imp[,3]*max(merged_imp[,2]-offset),
           length=0, col="grey20", lwd=3)
    
    axis(4, at = c(offset, (max(merged_imp[,2]) + offset)/2, max(merged_imp[,2])), labels = c(0, 0.5, 1))
    abline(h=offset, lty=2)
  }
}


compare_skalen_ums = function(skalen_scores, ums_scores, score_type){
  
  tmp_mean = c()
  tmp_name = c()
  
  for(i in 1:length(ums_scores)){
    
    tmp_mean = c(tmp_mean, mean(skalen_scores[[i]][[score_type]], na.rm=T) - mean(ums_scores[[i]][[score_type]], na.rm=T))
    tmp_name = c(tmp_name, names(ums_scores)[i])
  }
  
  most_imp = data.frame("Name" = tmp_name[order(tmp_mean, decreasing = T)], "Wert" = sort(tmp_mean, decreasing = T))
  
  most_imp = most_imp[most_imp[,2] > 0,]
  
  return(most_imp)
  
}


create_ums_proz = function(skalen, umsetzung, ums_data, group_index){
  
  tmp_proz = c()
  tmp_name = c()
  
  for(i in 1:length(skalen)){
    
    tmp = ums_data[group_index, umsetzung[umsetzung[, 2] %in% skalen[[i]], 1]]
    
    if(is.null(dim(tmp))){
      tmp_proz = c(tmp_proz, mean(tmp, na.rm = T))
      
    }else{
      
      tmp_proz = c(tmp_proz, mean(apply(tmp, 1, mean, na.rm=T), na.rm=T))
    }
    
    tmp_name = c(tmp_name, names(skalen)[i])
  }
  
  return(data.frame("Skalen"=tmp_name, "Prozente"=tmp_proz))
}



create_model_df = function(ums_scores, score_type){
  
  tmp = list()
  
  for(i in 1:length(ums_scores)){
    
    
    tmp[[names(ums_scores)[i]]] = ums_scores[[i]][[score_type]]
    
  }
  
  
  ums_df = as.data.frame(rlist::list.cbind(tmp))
  
  index = complete.cases(ums_df)
  
  ums_df = ums_df[index,]
  
  return(list(ums_df, index))
}


create_agg_ums = function(skalen_tmp, umsetzung, skala){
  
  tmp = ums_data[,umsetzung[umsetzung[,2] %in% skalen_tmp[[skala]],1]]
  
  if(is.null(dim(tmp))){
    
    tmp_mean = tmp
    
  }else{
    
    tmp_mean = apply(tmp, 1, mean, na.rm=T)
  }
  
  tmp = cbind("Proz" = tmp_mean, "Einrichtung" = ums_data[,"B107"])
  
  tmp_agg = aggregate(tmp[,1], by = list(tmp[,2]), FUN = mean, na.rm = T)
  
  tmp_agg$Names = imp_names[["B107"]][tmp_agg[,1]]
  tmp_agg$Anzahl = table(ums_data[,"B107"])
  
  tmp_agg = tmp_agg[tmp_agg$Anzahl > 2,]  # nur Einrichtungen mit mehr als 2 Abgaben
  
  return(tmp_agg)
}


plot_agg_ums = function(agg_ums, skala){
  
  par(mar=c(14,2,2,2), mfrow=c(1,1))
  
  plot(agg_ums[,2], ylim=c(0,1), xaxt="n", xlab="", cex.axis=0.8, main=skala)
  
  axis(1, at = 1:nrow(agg_ums), labels = agg_ums[,"Names"], las=2, cex.axis=0.7)
  
  arrows(x0=1:nrow(agg_ums), x1=1:nrow(agg_ums),
         y0 = rep(0, nrow(agg_ums)), y1 = agg_ums[,2], length=0, lwd=3)
}




create_agg_df = function(skalen_tmp, umsetzung, variables){
  
  agg_ums = create_agg_ums(skalen_tmp, umsetzung, skala=variables[1])
  
  tmp_value = agg_ums[,2]
  
  tmp_names = agg_ums[,"Names"]
  tmp_id = agg_ums[,1]
  tmp_anzahl = agg_ums[,"Anzahl"]
  
  for(i in 2:length(variables)){
    
    agg_ums = create_agg_ums(skalen_tmp, umsetzung, skala=variables[i])
    
    tmp_value = cbind(tmp_value, agg_ums[,2])
  }
  
  tmp_value = as.data.frame(tmp_value)
  names(tmp_value) = variables
  
  tmp = cbind(tmp_id, tmp_names, tmp_anzahl, tmp_value)
  
  tmp = tmp[,-3]
  names(tmp)[1:3] = c("ID", "Name", "Anzahl")
  
  return(tmp)
  
}



plot_einrichtungen = function(agg_df){
  
  agg_df = agg_df[,complete.cases(t(agg_df))]
  
  par(mfrow=c(2,2), mar=c(8,4,4,1))
  
  for(i in 1:nrow(agg_df)){
    barplot(as.numeric(agg_df[i,4:ncol(agg_df)]),
            ylim=c(0,1),
            las=2,
            cex.main = 0.8,
            main=paste(agg_df[i,"Name"], "-", agg_df[i, "Anzahl"], "-",
                       round(mean(as.numeric(agg_df[i,4:ncol(agg_df)]), na.rm=T),2)*100, "%"),
            names.arg = names(agg_df[,4:ncol(agg_df)]))
  }
}



check_loadings = function(skalen_scores){
  res = list()
  
  for(i in 1:length(skalen_scores)){
    
    a = skalen_scores[[i]][["loadings"]]
    
    if(all(!is.na(a))){
      
      b = attr(a, "dimnames")[[1]]
      
      inspect = b[(a < 0.4 & a >= 0.2)]
      bad = b[(a <  0.2)]
      
      res[[names(skalen_scores)[i]]] = list("inspect"=inspect, "bad"=bad)
      
    }else{
      
      res[[names(skalen_scores)[i]]] = list()
    }
  }
  
  return(res)
}


plot_group_diff = function(skalen, umsetzung, ums_data, group_list){
  
  tmp = list()
  
  for(i in 1:length(group_list)){
    
    ums_proz = create_ums_proz(data_skalen_ohne, umsetzung, ums_data,
                               group_index=get_index_by_group(data, group_keys = group_list[[i]]))
    
    skalen_imp = create_skalen_imp(score_type = "scores_mean",
                                   skalen_scores = skalen_scores,
                                   group_index=get_index_by_group(data, group_keys = group_list[[i]]))
    
    
    merged_imp = merge(skalen_imp, ums_proz, by.x = "Name", by.y="Skalen")
    
    merged_imp = merged_imp[order(merged_imp[,"Wert"], decreasing = T),]
    
    tmp[[names(group_list)[i]]] = merged_imp
  }
  
  res = merge(tmp[[1]], tmp[[2]], by ="Name")
  
  res$Diff.Wert = res$Wert.x - res$Wert.y
  res$Diff.Proz = res$Prozente.x - res$Prozente.y
  
  tmp_Wert = res[order(res[,"Diff.Wert"], decreasing = F),]
  
  plot(tmp_Wert$Diff.Wert, xaxt="n", xlab="", pch=20, main=  paste("Bewertung:", paste(names(group_list), collapse = " - ")))
  axis(side=1, at=1:nrow(tmp_Wert), labels = tmp_Wert$Name, las=2)
  arrows(x0 = 1:nrow(tmp_Wert), y0 = rep(0, nrow(tmp_Wert)),
         y1 = tmp_Wert$Diff.Wert, col="grey50", length = 0)
  points(1:nrow(tmp_Wert), tmp_Wert$Diff.Wert, pch = 20)
  abline(h=0)
  
  
  tmp_Proz = res[order(res[,"Diff.Proz"], decreasing = F),]
  tmp_Proz = tmp_Proz[complete.cases(tmp_Proz),]
  
  plot(tmp_Proz$Diff.Proz, xaxt="n", xlab="", pch=20, main=  paste("Umsetzung:", paste(names(group_list), collapse = " - ")))
  axis(side=1, at=1:nrow(tmp_Proz), labels = tmp_Proz$Name, las=2)
  arrows(x0 = 1:nrow(tmp_Proz), y0 = rep(0, nrow(tmp_Proz)),
         y1 = tmp_Proz$Diff.Proz, col="grey50", length = 0)
  points(1:nrow(tmp_Proz), tmp_Proz$Diff.Proz, pch = 20)
  abline(h=0)
  
}


get_ums_scores = function(ums_data, skalen, skalen_names){
  
  skalen_scores = list()
  
  
  for(i in 1:length(skalen_names)){
    
    tmp_ls = list()
    
    
    tmp_var = umsetzung[umsetzung[,"var.Umgesetzt"] %in% skalen[[skalen_names[i]]], "var.Key"]
    
    tmp = as.data.frame(ums_data[,tmp_var])
    
    if(ncol(tmp) > 0){
      
      
      if(ncol(tmp) == 1){
        
        tmp_ls[["scores_mean"]] = as.numeric(tmp[,1])
        
        tmp_ls[["scores_fact"]] = rep(NA, length(tmp[,1]))
        
        tmp_ls[["loadings"]] = rep(NA, 1)
        
        tmp_ls[["p"]] = NA
        
        skalen_scores[[skalen_names[i]]] = tmp_ls
        
      }else{
        
        na_index = complete.cases(tmp)
        
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
    }else{
      print(paste(skalen_names[i], "besitzt keine Umsetzungsabfrage"))
    }
  }
  
  return(skalen_scores)
}
