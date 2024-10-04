
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

#' Liest Csv Datei mit Umfrageantworten ein und bereitet Daten auf
#'
#' @param Path: Pfad zur csv Datei mit Fragebogenantworten 
#'
import_data_surv <- function(Path){
  
  data <- read.csv(Path,
                   skipNul = T, na.strings = "-9", stringsAsFactors = FALSE)
  
  # Personen rausschmeißen, die nicht zugestimmt haben dass Daten Fragebogen genutzt werden darf
  data_compl <- data[!(is.na(as.numeric(data$A003)) | as.numeric((data$A003) == 2)),]
  
  # Strukturfragebogen aus Datensatz entfernen
  data_compl <- data_compl[,-c(598:680)]
  
  # CASE umbenennen
  names(data_compl)[1] <- "CASE"
  
  # Eva Stuttgart rausnehmen -> Einrichtung hat nur einen einzigen Fragebogen
  # mit gesammelten Antworten der Mitarbeiter abgeben
  data_compl <- data_compl[-which(data_compl$CASE==1164), ]
  
  # Umkodieren Soziale Erwünschtheit
  data_compl$M401_01 <- 5 - as.numeric(data_compl$M401_01)
  
  #beide items mit 4 bewertet
  index <- which(data_compl$M401_01 == 4 & data_compl$M401_02 == 4)
  
  data_compl$sozialErw <- FALSE
  
  data_compl$sozialErw[index] <- TRUE
  
  #Soziale Erwünschtheit rausschmeißen
  data_compl <- data_compl[!data_compl$sozialErw,]
  
  
  # Leute entfernen die nicht mit jM arbeiten:
  data = data[-unique(c(which(data$C207_07 == 2), which(data$B101_04 == 2))), ]
  
  # Johannes Falk Haus zu eva Stuttgart dazuzählen
  data$B107[data$B107 == 14] = 12
  
  # Stiftung Jugendhilfe aktiv zusammenfassen:
  data$B107[data$B107 %in% c(34, 35, 36)] = 50
  
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


beta_est = function(x, yx){
  
  x = x[!is.na(x)]
  
  x = (x - 1)/(length(yx) - 1)
  
  x[x == 0] = 0.0000001
  x[x == 1] = 1 - 0.0000001
  
  m = betareg(x ~ 1)
  
  return (c(predict(m, type="response")[1], predict(m, type="quantile", at = c(0.25, 0.75))[1,]) * (length(yx) - 1) + 1)
}


binom_est = function(x, yx){
  
  x = x[!is.na(x)]
  
  x = (x - 1)/(length(yx) - 1)
  
  m = glm(x ~ 1, family="binomial")
  
  fit = predict(m, type="link", se.fit=T)$fit[1]
  se = predict(m, type="link", se.fit=T)$se.fit[1]
  
  tmp = c(plogis(fit), plogis(fit - 2*se), plogis(fit + 2*se)) * (length(yx) - 1) + 1
  
  if(tmp[3]> length(yx)) tmp[3] = length(yx)
  
  return (tmp)
}


norm_est = function(x, yx){
  
  x = x[!is.na(x)]
  
  m = glm(x ~ 1, family="gaussian")
  
  fit = predict(m, type="response", se.fit=T)$fit[1]
  se = predict(m, type="response", se.fit=T)$se.fit[1]
  
  return (c(fit, fit - se, fit + se))
}



get_ranking = function(plot_data, var_name, type, yx){
  
  if (type == "norm"){
    tmp = apply(plot_data[complete.cases(plot_data[,keys[[var_name]]]),keys[[var_name]]], 2, FUN=norm_est)
  }
  
  if (type == "beta"){
    tmp = apply(plot_data[complete.cases(plot_data[,keys[[var_name]]]),keys[[var_name]]], 2, FUN=beta_est, yx=yx)
    
    # for(i in 1:length(tmp)){
    #   if(tmp[1, i] > tmp[3, i]){
    #     tmp[3, i] = tmp[1, i]
    #   }
    # }
  }
  
  if (type == "binom"){
    
    tmp = apply(plot_data[complete.cases(plot_data[,keys[[var_name]]]), keys[[var_name]]],
                MARGIN = 2,
                FUN=binom_est,
                yx=yx)
    
  }
  
  if (type == "mean"){
    
    tmp_mean = apply(plot_data[complete.cases(plot_data[,keys[[var_name]]]),keys[[var_name]]], 2, mean, na.rm=T) 
    tmp_sd = apply(plot_data[complete.cases(data[,keys[[var_name]]]),keys[[var_name]]], 2, sd, na.rm=T)
    tmp = rbind(tmp_mean,tmp_mean - tmp_sd, tmp_mean + tmp_sd)
  }
  
  df_begr = data.frame("mean"=tmp[1,], "name"=imp_names[[var_name]], "se.1"=tmp[2,], "se.2"=tmp[3,])
  
  return (df_begr[order(df_begr$mean, decreasing = T),])
}



plot_ranked = function(plot_data,
                       var_name,
                       yx,
                       mar,
                       main,
                       type,
                       file,
                       add=0,
                       label="",
                       se="reduced"){
  
  ranked = get_ranking(plot_data, var_name, type=type, yx)
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  # jpeg(filename=file, width=1200, height = 730, units = "px", pointsize = 20)
  
  par(mar=mar)  
  
  plot(ranked$mean, xaxt = "n", xlab = "", pch = 20, type = "n",
       ylab="", yaxt="n", main = main, ylim=c(1, length(yx)))
  
  abline(h=1:length(yx), lwd=0.5, col="grey60")
  axis(1, las=2, at = 1:nrow(ranked), labels = ranked$name)
  axis(2, las=2, at = 1:length(yx), labels=yx)
  
  if(!typeof(add) == "list"){
    
    if(se == T){
      
      arrows(x0 = (1:nrow(ranked)), y0 = ranked[,"mean"],
             y1 = ranked[,"se.1"], col="grey30", length = 0.1, lwd=1, angle=90)
      
      arrows(x0 = (1:nrow(ranked)), y0 = ranked[,"mean"],
             y1 = ranked[,"se.2"], col="grey30", length = 0.1, lwd=1, angle=90)
      
    }
    
    arrows(x0 = (1:nrow(ranked)), y0 = rep(0, nrow(ranked)),
           y1 = ranked[,1], col="grey50", length = 0, lwd=5)
    
    points(ranked$mean,  pch = 20)
    
    
  }else{
    
    if(length(add) == 1){
      
      if(nrow(add[[1]]) == 0){
        dev.off()
        return("nicht genügend Kategorien für Plot")
      }
      
      
      ranked = get_ranking(plot_data,
                           var_name,
                           type=type,
                           yx)
      
      ranked2 = get_ranking(plot_data=add[[1]],
                            var_name = var_name,
                            type=type,
                            yx)
      
      ranked_tmp = merge(ranked, ranked2, by="name", sort = "False")
      
      
      if(se== T){
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.1.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.2.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.1.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.2.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
      }
      
      
      arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.x"], col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.y"], col="#e7298a", length = 0, lwd=5)
      
      
      
      points((1:nrow(ranked_tmp)) - 0.075, ranked_tmp[,"mean.x"], pch=20)#, col="#1b9e77")
      points((1:nrow(ranked_tmp)) + 0.075, ranked_tmp[,"mean.y"], pch=20)#, col="#e7298a")
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a"),
             legend = label, cex = 1.2)
      
      # c("#1b9e77", "#e7298a","#7570b3","#66a61e", "#e6ab02")
    }
    
    if(length(add) == 2){
      
      if(nrow(add[[1]]) == 0 || nrow(add[[2]]) == 0 ){
        dev.off()
        return("nicht genügend Kategorien für Plot")
      }
      
      ranked = get_ranking(plot_data, var_name, type=type, yx)
      ranked2 = get_ranking(add[[1]], var_name = var_name, type=type, yx)
      ranked3 = get_ranking(add[[2]], var_name = var_name, type=type, yx)
      
      ranked_tmp = merge(ranked, ranked2, by="name", sort = "False")
      ranked_tmp2 = merge(ranked, ranked3, by="name", sort = "False")
      
      ranked_tmp$mean.z = ranked_tmp2$mean.y
      ranked_tmp$se.1.z = ranked_tmp2$se.1.y
      ranked_tmp$se.2.z = ranked_tmp2$se.2.y
      
      
      if(se== T){
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.15, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.1.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.15, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.2.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.1.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.2.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
        
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.15, y0 = ranked_tmp[,"mean.z"],
               y1 = ranked_tmp[,"se.1.z"], col="#7570b3", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.15, y0 = ranked_tmp[,"mean.z"],
               y1 = ranked_tmp[,"se.2.z"], col="#7570b3", length = 0.1, lwd=1, angle=90)
      }
      
      
      arrows(x0 = (1:nrow(ranked_tmp)) - 0.15, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.x"], col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) + 0, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.y"], col="#e7298a", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) + 0.15, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.z"], col="#7570b3", length = 0, lwd=5)
      
      points((1:nrow(ranked_tmp)) - 0.15, ranked_tmp[,"mean.x"], pch=20)#, col="#1b9e77")
      points((1:nrow(ranked_tmp)) + 0, ranked_tmp[,"mean.y"], pch=20)#, col="#e7298a")
      points((1:nrow(ranked_tmp)) + 0.15, ranked_tmp[,"mean.z"], pch=20)#, col="#7570b3")
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3"),
             legend = label, cex = 1.2)
      
      # c("#1b9e77", "#e7298a","#7570b3","#66a61e", "#e6ab02")
    }
    
    if(length(add) == 3){
      
      if(nrow(add[[1]]) == 0 || nrow(add[[2]]) == 0 || nrow(add[[3]]) == 0){
        dev.off()
        return("nicht genügend Kategorien für Plot")
      }
      
      ranked = get_ranking(plot_data, var_name, type=type, yx)
      ranked2 = get_ranking(add[[1]], var_name = var_name, type=type, yx)
      ranked3 = get_ranking(add[[2]], var_name = var_name, type=type, yx)
      ranked4 = get_ranking(add[[3]], var_name = var_name, type=type, yx)
      
      ranked_tmp = merge(ranked, ranked2, by="name", sort = "False")
      ranked_tmp2 = merge(ranked, ranked3, by="name", sort = "False")
      ranked_tmp3 = merge(ranked, ranked4, by="name", sort = "False")
      
      
      ranked_tmp$mean.z = ranked_tmp2$mean.y
      ranked_tmp$se.1.z = ranked_tmp2$se.1.y
      ranked_tmp$se.2.z = ranked_tmp2$se.2.y
      
      ranked_tmp$mean.a = ranked_tmp3$mean.y
      ranked_tmp$se.1.a = ranked_tmp3$se.1.y
      ranked_tmp$se.2.a = ranked_tmp3$se.2.y
      
      # ranked_tmp$mean.z = ranked_tmp2$mean.y
      # ranked_tmp$mean.a = ranked_tmp3$mean.y
      
      
      if(se== T){
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.225, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.1.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.225, y0 = ranked_tmp[,"mean.x"],
               y1 = ranked_tmp[,"se.2.x"], col="#1b9e77", length = 0.1, lwd=1, angle=90)
        
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.1.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = ranked_tmp[,"mean.y"],
               y1 = ranked_tmp[,"se.2.y"], col="#e7298a", length = 0.1, lwd=1, angle=90)
        
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = ranked_tmp[,"mean.z"],
               y1 = ranked_tmp[,"se.1.z"], col="#7570b3", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = ranked_tmp[,"mean.z"],
               y1 = ranked_tmp[,"se.2.z"], col="#7570b3", length = 0.1, lwd=1, angle=90)
        
        
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.225, y0 = ranked_tmp[,"mean.a"],
               y1 = ranked_tmp[,"se.1.a"], col="#e6ab02", length = 0.1, lwd=1, angle=90)
        
        arrows(x0 = (1:nrow(ranked_tmp)) + 0.225, y0 = ranked_tmp[,"mean.a"],
               y1 = ranked_tmp[,"se.2.a"], col="#e6ab02", length = 0.1, lwd=1, angle=90)
      }
      
      
      arrows(x0 = (1:nrow(ranked_tmp)) - 0.225, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.x"], col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) - 0.075, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.y"], col="#e7298a", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) + 0.075, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.z"], col="#7570b3", length = 0, lwd=5)
      
      arrows(x0 = (1:nrow(ranked_tmp)) + 0.225, y0 = rep(0, nrow(ranked_tmp)),
             y1 = ranked_tmp[,"mean.a"], col="#e6ab02", length = 0, lwd=5)
      
      points((1:nrow(ranked_tmp)) - 0.225, ranked_tmp[,"mean.x"], pch=20)#, col="#1b9e77")
      points((1:nrow(ranked_tmp)) - 0.075, ranked_tmp[,"mean.y"], pch=20)#, col="#e7298a")
      points((1:nrow(ranked_tmp)) + 0.075, ranked_tmp[,"mean.z"], pch=20)#, col="#7570b3")
      points((1:nrow(ranked_tmp)) + 0.225, ranked_tmp[,"mean.a"], pch=20)#, col="#7570b3")
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3","#e6ab02"),
             legend = label, cex = 1.2)
      
      # c("#1b9e77", "#e7298a","#7570b3","#66a61e", "#e6ab02")
    }
  }
  
  # abline(h=1:length(yx), lwd=0.5, col="grey60")
  # axis(1, las=2, at = 1:nrow(ranked), labels = ranked$name)
  # axis(2, las=2, at = 1:length(yx), labels=yx)
  
  dev.off()
}


plot_ranked_all = function(plot_data,
                           data,
                           var_name,
                           yx,
                           type,
                           mar,
                           main,
                           file,
                           se="reduced"){
  
  # Alle Mitarbeiter
  plot_ranked(plot_data = plot_data,
              var_name = var_name,
              yx= yx,
              type=type,
              mar=mar,
              main=main,
              file=paste0(file,"_gesamt.pdf"),
              se=se)
  
  # Alle Mitarbeiter
  plot_ranked(plot_data = plot_data,
              var_name = var_name,
              yx= yx,
              type=type,
              mar=mar,
              main=main,
              add = list(data[data$B107 != Index_einrichtung,]),  # alle anderen Einrichtungen
              file=paste0(file,"_gesamt_Vergleich.pdf"),
              label = c(Einrichtung, "Andere"),
              se=se)
  
  # # Nach HZE und SBBZ getrennt
  # plot_ranked(plot_data = data[data$B102 > 2,],
  #             var_name = var_name,
  #             yx= yx,
  #             type=type,
  #             mar=mar,
  #             main=paste(main, "HZE/SBBZ"),
  #             file=paste0(file,"_HZE_SBBZ.pdf"),
  #             add = list(data[data$B102 == 2, ]),
  #             label = c("HZE", "SBBZ"),
  #             se=se)
  # 
  # # Nach Geschlecht getrennt
  # plot_ranked(plot_data = data[data$B103 == 1,],
  #             var_name = var_name,
  #             yx= yx,
  #             type=type,
  #             mar=mar,
  #             main=paste(main, "Mitarbeiterinnen/Mitarbeiter"),
  #             file=paste0(file,"_Geschlecht.pdf"),
  #             add = list(data[data$B103 == 2, ]),
  #             label = c("Mitarbeiterinnen", "Mitarbeiter"),
  #             se=se)
  # 
  # # nach Berufserfahrung
  # plot_ranked(plot_data = data[data$B105 == 1,],
  #             var_name = var_name,
  #             yx= yx,
  #             type=type,
  #             mar=mar,
  #             main=paste(main, "nach Berufserfahrung"),
  #             file=paste0(file,"_Berufserfahrung.pdf"),
  #             add = list(data[data$B105 == 2, ], data[data$B105 == 3, ]),
  #             label = imp_names[["B105"]][c(1,2,3)],
  #             se=se)
  # 
  # # nach Alter
  # plot_ranked(plot_data = data[data$B110 == 1,],
  #             var_name = var_name,
  #             yx= yx,
  #             type=type,
  #             mar=mar,
  #             main=paste(main, "nach Alter Mitarbeitetende"),
  #             file=paste0(file,"_AlterMit.pdf"),
  #             add = list(data[data$B110 == 2, ], data[data$B110 == 3, ]),
  #             label = imp_names[["B110"]][c(1,2,3)],
  #             se=se)
  # 
  # # nach Alter der jM
  # plot_ranked(plot_data = data[data$C207_02 == 2,],
  #             var_name = var_name,
  #             yx= yx,
  #             type=type,
  #             mar=mar,
  #             main=paste(main, "nach Alter jM"),
  #             file=paste0(file,"_AlterJM.pdf"),
  #             add = list(data[data$C207_03 == 2, ], data[data$C207_04 == 2, ], data[data$C207_05 == 2, ]),
  #             label = c("6 - 10", "11 - 14", "15 - 17", "18 - 21"),
  #             se=se)
  
}


plot_binary = function(data, var_name, main, mar, ylab, file, ranked=FALSE, add = 0, label){
  
  
  tmp = apply(data[,keys[[var_name]]] - 1, 2, sum, na.rm=T)
  
  # % Ausrechnen
  
  tmp = 100 * tmp / sum(complete.cases(data[,keys[[var_name]]]))
  
  if(ranked == TRUE){
    
    index = order(tmp, decreasing = T)
    
    tmp_labels = imp_names[[var_name]][index]
    
    tmp = sort(tmp, decreasing = T)
    
  }
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  par(mar=mar)
  
  # plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp)))
  
  
  if(typeof(add) == "list"){
    if(length(add) == 1){
      
      tmp2 = apply(add[[1]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      
      tmp2 = 100 * tmp2 / sum(complete.cases(add[[1]][,keys[[var_name]]]))
      
      if(ranked == TRUE){
        
        tmp2 = tmp2[index]
        
      }
      
      plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp, tmp2)))
      
      abline(h=c(0, 50, 100), lwd=0.5, col="grey60")
      
      c("#1b9e77", "#e7298a", "#7570b3")
      
      arrows(x0 = 1:length(tmp) - 0.075, y0 = rep(0, length(tmp)),
             y1 = tmp, col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) + 0.075, y0 = rep(0, length(tmp)),
             y1 = tmp2, col="#e7298a", length = 0, lwd=5)
      
      points(1:length(tmp) - 0.075, tmp, pch = 20)
      
      points(1:length(tmp2) + 0.075, tmp2, pch = 20)
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a"),
             legend = label, cex = 1.2)
      
    }
    if(length(add) == 2){
      
      tmp2 = apply(add[[1]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      tmp3 = apply(add[[2]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      
      
      tmp2 = 100 * tmp2 / sum(complete.cases(add[[1]][,keys[[var_name]]]))
      tmp3 = 100 * tmp3 / sum(complete.cases(add[[2]][,keys[[var_name]]]))
      
      if(ranked == TRUE){
        
        tmp2 = tmp2[index]
        tmp3 = tmp3[index]
        
      }
      
      
      plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp, tmp2, tmp3)))
      
      abline(h=c(0, 50, 100), lwd=0.5, col="grey60")
      
      arrows(x0 = 1:length(tmp) - 0.15, y0 = rep(0, length(tmp)),
             y1 = tmp, col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) + 0.0, y0 = rep(0, length(tmp)),
             y1 = tmp2, col="#e7298a", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) + 0.15, y0 = rep(0, length(tmp)),
             y1 = tmp3, col="#7570b3", length = 0, lwd=5)
      
      points(1:length(tmp) - 0.15, tmp, pch = 20)
      
      points(1:length(tmp2) + 0.0, tmp2, pch = 20)
      
      points(1:length(tmp3) + 0.15, tmp3, pch = 20)
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3"),
             legend = label, cex = 1.2)
      
    }
    if(length(add) == 3){
      
      tmp2 = apply(add[[1]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      tmp3 = apply(add[[2]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      tmp4 = apply(add[[3]][,keys[[var_name]]] - 1, 2, sum, na.rm=T)
      
      
      tmp2 = 100 * tmp2 / sum(complete.cases(add[[1]][,keys[[var_name]]]))
      tmp3 = 100 * tmp3 / sum(complete.cases(add[[2]][,keys[[var_name]]]))
      tmp4 = 100 * tmp4 / sum(complete.cases(add[[3]][,keys[[var_name]]]))
      
      if(ranked == TRUE){
        
        tmp2 = tmp2[index]
        tmp3 = tmp3[index]
        tmp4 = tmp4[index]
        
      }
      
      
      plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp, tmp2, tmp3, tmp4)))
      
      abline(h=c(0, 50, 100), lwd=0.5, col="grey60")
      
      arrows(x0 = 1:length(tmp) - 0.225, y0 = rep(0, length(tmp)),
             y1 = tmp, col="#1b9e77", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) - 0.075, y0 = rep(0, length(tmp)),
             y1 = tmp2, col="#e7298a", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) + 0.075, y0 = rep(0, length(tmp)),
             y1 = tmp3, col="#7570b3", length = 0, lwd=5)
      
      arrows(x0 = 1:length(tmp) + 0.225, y0 = rep(0, length(tmp)),
             y1 = tmp4, col="#e6ab02", length = 0, lwd=5)
      
      
      points(1:length(tmp) - 0.225, tmp, pch = 20)
      
      points(1:length(tmp2) - 0.075, tmp2, pch = 20)
      
      points(1:length(tmp3) + 0.075, tmp3, pch = 20)
      
      points(1:length(tmp4) + 0.225, tmp4, pch = 20)
      
      legend("topright", bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3","#e6ab02"),
             legend = label, cex = 1.2)
    }
  }else{
    
    plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp)))
    
    abline(h=c(0, 50, 100), lwd=0.5, col="grey60")
    
    arrows(x0 = 1:length(tmp), y0 = rep(0, length(tmp)),
           y1 = tmp, col="grey50", length = 0, lwd=5)
    points(1:length(tmp), tmp, pch = 20)
  }
  
  
  
  if(ranked == TRUE){
    
    axis(side=1, at=1:length(tmp), labels = tmp_labels, las=2)
  }else{
    
    axis(side=1, at=1:length(tmp), labels = imp_names[[var_name]], las=2)
  }
  
  dev.off()
}



plot_veto = function(data, var_name, main, mar, ylab, file, ranked=FALSE, labels){
  
  
  tmp = apply(data[,keys[[var_name[1]]]] - 1, 2, sum, na.rm=T)
  
  # % Ausrechnen
  
  tmp = 100 * tmp / sum(complete.cases(data[,keys[[var_name[1]]]]))
  
  if(ranked == TRUE){
    
    index = order(tmp, decreasing = T)
    
    tmp_labels = imp_names[[var_name[1]]][index]
    
    tmp = sort(tmp, decreasing = T)
    
  }
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  par(mar=mar)
  
  # plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp)))
  
  
  tmp2 = apply(data[,keys[[var_name[2]]]] - 1, 2, sum, na.rm=T)
  tmp3 = apply(data[,keys[[var_name[3]]]] - 1, 2, sum, na.rm=T)
  
  
  tmp2 = 100 * tmp2 / sum(complete.cases(data[,keys[[var_name[2]]]]))
  tmp3 = 100 * tmp3 / sum(complete.cases(data[,keys[[var_name[3]]]]))
  
  if(ranked == TRUE){
    
    tmp2 = tmp2[index]
    tmp3 = tmp3[index]
    
  }
  
  
  plot(tmp, xlab="", xaxt="n", ylab=ylab, main = main, type="n", ylim=c(0,max(tmp, tmp2, tmp3)))
  
  abline(h=c(0, 50, 100), lwd=0.5, col="grey60")
  
  arrows(x0 = 1:length(tmp) - 0.15, y0 = rep(0, length(tmp)),
         y1 = tmp, col="#1b9e77", length = 0, lwd=5)
  
  arrows(x0 = 1:length(tmp) + 0.0, y0 = rep(0, length(tmp)),
         y1 = tmp2, col="#e7298a", length = 0, lwd=5)
  
  arrows(x0 = 1:length(tmp) + 0.15, y0 = rep(0, length(tmp)),
         y1 = tmp3, col="#7570b3", length = 0, lwd=5)
  
  points(1:length(tmp) - 0.15, tmp, pch = 20)
  
  points(1:length(tmp2) + 0.0, tmp2, pch = 20)
  
  points(1:length(tmp3) + 0.15, tmp3, pch = 20)
  
  legend("topright", bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3"),
         legend = labels, cex = 1.2)
  
  
  
  if(ranked == TRUE){
    
    axis(side=1, at=1:length(tmp), labels = tmp_labels, las=2)
  }else{
    
    axis(side=1, at=1:length(tmp), labels = imp_names[[var_name[1]]], las=2)
  }
  
  dev.off()
}




plot_binary_all = function(plot_data,
                           data,
                           var_name,
                           main,
                           mar,
                           ylab,
                           file,
                           ranked=FALSE){
  
  
  # bei D303 Auf welche Weise Infos: Unklar was tun da binäre Daten
  plot_binary(plot_data,
              var_name=var_name,
              main=main,
              mar=mar,
              ylab=ylab,
              file=paste0(file,"_gesamt.pdf"),
              ranked = ranked)
  
  
  plot_binary(plot_data,
              var_name=var_name,
              main=paste(main),
              mar=mar,
              ylab=ylab,
              file=paste0(file,"_gesamt_Vergleich.pdf"),
              ranked = ranked,
              add = list(data[data$B107 != Index_einrichtung, ]),
              label = c(Einrichtung, "Andere"))
  
  
  # plot_binary(data= data[data$B103 == 1, ],
  #             var_name=var_name,
  #             main=paste(main, "Mitarbeiterinnen/Mitarbeiter"),
  #             mar=mar,
  #             ylab=ylab,
  #             file=paste0(file,"_Geschlecht.pdf"),
  #             ranked = ranked,
  #             add = list(data[data$B103 == 2, ]),
  #             label = c("Mitarbeiterinnen", "Mitarbeiter"))
  # 
  # 
  # plot_binary(data= data[data$B105 == 1, ],
  #             var_name=var_name,
  #             main=paste(main, "nach Berufserfahrung"),
  #             mar=mar,
  #             ylab=ylab,
  #             file=paste0(file,"_Berufserfahrung.pdf"),
  #             ranked = ranked,
  #             add = list(data[data$B105 == 2, ], data[data$B105 == 3, ]),
  #             label = imp_names[["B105"]][c(1,2,3)])
  # 
  # 
  # plot_binary(data= data[data$B110 == 1, ],
  #             var_name=var_name,
  #             main=paste(main, "nach Alter Mitarbeitetende"),
  #             mar=mar,
  #             ylab=ylab,
  #             file=paste0(file,"_AlterMit.pdf"),
  #             ranked = ranked,
  #             add = list(data[data$B110 == 2, ], data[data$B110 == 3, ]),
  #             label = imp_names[["B110"]][c(1,2,3)])
  # 
  # 
  # plot_binary(data= data[data$C207_03 == 1, ],
  #             var_name=var_name,
  #             main=paste(main, "nach Alter jM"),
  #             mar=mar,
  #             ylab=ylab,
  #             file=paste0(file,"_AlterJM.pdf"),
  #             ranked = ranked,
  #             add = list(data[data$C207_03 == 2, ], data[data$C207_04 == 2, ], data[data$C207_05 == 2, ]),
  #             label = c("6 - 10", "11 - 14", "15 - 17", "18 - 21"))
  
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


get_skalen_scores = function(plot_data, skalen, skalen_names){
  
  skalen_scores = list()
  
  
  for(i in 1:length(skalen_names)){
    
    tmp_ls = list()
    
    
    tmp = plot_data[,skalen[[skalen_names[i]]]]
    
    na_index = complete.cases(tmp)
    
    
    if(is.null(dim(tmp))){
      
      tmp_ls[["scores_mean"]] = as.numeric(tmp)
      
      # tmp_ls[["scores_fact"]] = rep(NA, length(tmp))
      # 
      # tmp_ls[["loadings"]] = rep(NA, 1)
      # 
      # tmp_ls[["p"]] = NA
      
      skalen_scores[[skalen_names[i]]] = tmp_ls
      
    }else{
      
      tmp = tmp[na_index,] # hier gibts nen Fehler weil ein vector ankommt
      
      tmp_na = rep(NA, length(na_index))
      
      tmp_na[na_index] = as.numeric(apply(tmp, 1, mean, na.rm=T))
      
      tmp_ls[["scores_mean"]] = tmp_na
      
      # if(ncol(tmp) < 3){
      #   
      #   tmp_ls[["scores_fact"]] = rep(NA, nrow(tmp))
      #   
      #   tmp_ls[["loadings"]] = rep(NA, ncol(tmp))
      #   
      #   tmp_ls[["p"]] = NA
      #   
      #   skalen_scores[[skalen_names[i]]] = tmp_ls
      #   
      # }else{
      #   
      #   tmp_jittered = apply(tmp, 2, jitter, factor = 0.01)
      #   
      #   fact = factanal(tmp_jittered, 1, scores = "regression")
      #   
      #   tmp_na[na_index] = fact$scores
      #   
      #   tmp_ls[["scores_fact"]] = tmp_na
      #   
      #   tmp_ls[["loadings"]] = fact$loadings
      #   
      #   tmp_ls[["p"]] = fact$PVAL
      #   
      #   
      #   skalen_scores[[skalen_names[i]]] = tmp_ls
      # }
      
      skalen_scores[[skalen_names[i]]] = tmp_ls
    }
  }
  
  return(skalen_scores)
}


get_index_by_group = function(data, var_name_group, group_keys){
  
  return(which(data[,var_name_group] %in% group_keys))
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


create_skalen_imp = function(score_type,
                             skalen_scores=skalen_scores,
                             group_index){
  
  tmp_name = c()
  tmp_mean = c()
  tmp_se.1 = c()
  tmp_se.2 = c()
  
  for(i in 1:length(skalen_scores)){
    
    # tmp_mean = c(tmp_mean, mean(skalen_scores[[i]][[score_type]][group_index], na.rm = T))
    tmp_mean = c(tmp_mean, binom_est(skalen_scores[[i]][[score_type]][group_index], yx = 1:6)[1])
    tmp_name = c(tmp_name, names(skalen_scores)[i])
    tmp_se.1 = c(tmp_se.1, binom_est(skalen_scores[[i]][[score_type]][group_index], yx = 1:6)[2])
    tmp_se.2 = c(tmp_se.2, binom_est(skalen_scores[[i]][[score_type]][group_index], yx = 1:6)[3])
  }
  
  most_imp = data.frame("Name" = tmp_name[order(tmp_mean, decreasing = T)],
                        "se.1"=tmp_se.1[order(tmp_mean, decreasing = T)],
                        "se.2"=tmp_se.2[order(tmp_mean, decreasing = T)], "mean" = sort(tmp_mean, decreasing = T))
  
  return(most_imp)
  
}

# "#1b9e77", "#e7298a",

plot_skalen_imp = function(most_imp,
                           mar,
                           ylim,
                           main,
                           offset,
                           plot=T,
                           file="",
                           se=T,
                           combined = FALSE){
  
  par(mfrow=c(1,1), mar=mar)
  
  if(plot){
    
    pdf(file=file,
        width=14,
        height = 12,
        paper = "a4r")
  }
  
  plot(most_imp[,"mean"],
       xaxt="n",
       xlab="",
       pch=20,
       ylim=ylim,
       main=main,
       type="n",
       yaxt="n",
       ylab="")
  
  abline(h=c(1,2,3,4,5,6), col="grey70")
  
  
  axis(side = 1,
       at = 1:nrow(most_imp),
       labels = most_imp[,1],
       las=2)
  
  axis(side = 2,
       at = 1:6,
       labels = c("gar nicht wichtig", "nicht wichtig", "eher nicht wichtig",
                  "eher wichtig", "wichtig", "vollkommen wichtig"),
       las=2)
  
  if(combined == FALSE){
    
    arrows(x0 = 1:nrow(most_imp) + offset,
           y0 = rep(0, nrow(most_imp)),
           y1 = most_imp[,"mean"],
           col="#1b9e77",
           length = 0,
           lwd=5)
    
    arrows(x0 = 1:nrow(most_imp) - offset,
           y0 = rep(0, nrow(most_imp)),
           y1 = most_imp[,"mean.andere"],
           col="#e7298a",
           length = 0,
           lwd=5)
    
    if(se==TRUE){
      
      arrows(x0 = (1:nrow(most_imp)) + offset,
             y0 = most_imp[,"mean"],
             y1 = most_imp[,"se.1"],
             col="#1b9e77",
             length = 0.1,
             lwd=1,
             angle=90)
      
      arrows(x0 = (1:nrow(most_imp)) + offset,
             y0 = most_imp[,"mean"],
             y1 = most_imp[,"se.2"],
             col="#1b9e77",
             length = 0.1,
             lwd=1,
             angle=90)
      
      arrows(x0 = (1:nrow(most_imp)) - offset,
             y0 = most_imp[,"mean.andere"],
             y1 = most_imp[,"se.1.andere"],
             col="#e7298a",
             length = 0.1,
             lwd=1,
             angle=90)
      
      arrows(x0 = (1:nrow(most_imp)) - offset,
             y0 = most_imp[,"mean.andere"],
             y1 = most_imp[,"se.2.andere"],
             col="#e7298a",
             length = 0.1,
             lwd=1,
             angle=90)
    }
    
    points(1:nrow(most_imp) + offset, most_imp[,"mean"], pch = 20)
    
    points(1:nrow(most_imp) - offset, most_imp[,"mean.andere"], pch = 20)
    
    legend("topright",
           bty="n",
           col=c("#1b9e77", "#e7298a"),
           lty=1,
           lwd=3,
           legend = c(Einrichtung,"Andere"))#, cex=1, text.width =5,
    
    if(plot){
      
      dev.off()
    }
  }else {
    
    arrows(x0 = 1:nrow(most_imp) + offset,
           y0 = rep(0, nrow(most_imp)),
           y1 = most_imp[,"mean"],
           col="grey50",
           length = 0,
           lwd=5)
    
    points(1:nrow(most_imp) + offset, most_imp[,"mean"], pch = 20)
    
    abline(h=c(3.5), col="red3", lty=2, lwd=1)
    
    offset = 1
    
    # axis(4, at = c(offset, (max(merged_imp[,2]) + offset)/2, max(merged_imp[,2])), labels = c("0 %", "50 %", "100 %"), las = 2)
    axis(4, at = c(1, 3.5, 6), labels = c("0 %", "50 %", "100 %"), las = 2)
    
    arrows(x0 = 1:nrow(most_imp) + 0.1, # x1 = 1:nrow(merged_imp),
           y0 = rep(offset, nrow(most_imp)),  y1 = offset + most_imp[,"Prozente"]*max(most_imp[,"mean"]-offset),
           length=0, col="red3", lwd=5)
    
    points(1:nrow(most_imp) + 0.1, offset + most_imp[,"Prozente"]*max(most_imp[,"mean"]-offset), pch=20)
    
    legend("topright", bty="n", col=c("grey50", "red3"), lty=1, lwd=3, legend = c("Wichtigkeit","Umsetzung"))#, cex=1, text.width =5,
    # seg.len=0.5, y.intersp = 0.2)
  }
  return(most_imp)
}


create_ums_data = function(data, umsetzung){
  
  data_ums = data
  
  for(i in 1:nrow(umsetzung)){
    
    data_ums[,umsetzung[i,1]] = data_ums[,umsetzung[i,1]] - 1
    
    data_ums[,umsetzung[i,2]] = data_ums[,umsetzung[i,2]] * data_ums[,umsetzung[i,1]]
  }
  
  return(data_ums)
}



plot_combined_imp = function(skalen,
                             plot_data,
                             mar,
                             skalen_scores,
                             skalen_scores_andere,
                             umsetzung,
                             ums_data,
                             var_name_group,
                             group_list,
                             path,
                             combined=T){
  
  for(i in 1:length(group_list)){
    
    ums_proz = create_ums_proz(data_skalen_ohne,
                               umsetzung,
                               ums_data,
                               group_index=get_index_by_group(plot_data,
                                                              var_name_group = var_name_group,
                                                              group_keys = group_list[[i]]))
    
    skalen_imp = create_skalen_imp(score_type = "scores_mean",
                                   skalen_scores = skalen_scores,
                                   group_index=get_index_by_group(plot_data,
                                                                  var_name_group = var_name_group,
                                                                  group_keys = group_list[[i]]))
    
    skalen_imp_andere = create_skalen_imp(score_type = "scores_mean",
                                          skalen_scores = skalen_scores_andere,
                                          group_index=get_index_by_group(data[data$B107 != Index_einrichtung, ],
                                                                         var_name_group = var_name_group,
                                                                         group_keys = group_list[[i]]))
    
    
    merged_imp = merge(skalen_imp,
                       ums_proz,
                       by.x = "Name",
                       by.y="Skalen")
    
    merged_imp = merge(merged_imp, skalen_imp_andere, by = "Name")
    
    names(merged_imp) = c("Name","se.1", "se.2", "mean", "Prozente", "se.1.andere", "se.2.andere", "mean.andere")
    
    merged_imp = merged_imp[order(merged_imp[,"mean"],
                                  decreasing = T),]
    
    
    # Gemeinsame Darstellung: Skalen_scores und Umsetzung
    
    pdf(file=paste0(path,"_", names(group_list)[i], ".pdf"), width=14, height = 12, paper = "a4r")
    
    
    if(combined == TRUE){
      
      plot_skalen_imp(most_imp = merged_imp,
                      mar=mar,
                      main = names(group_list)[i],
                      ylim=c(1, 6),
                      offset = - 0.1,
                      plot=FALSE,
                      se=FALSE,
                      combined = TRUE)
      
    }else{
      
      plot_skalen_imp(most_imp = merged_imp,
                      mar=mar,
                      main = names(group_list)[i],
                      ylim=c(1, 6),
                      offset = -0.1,
                      plot=FALSE,
                      se=T,
                      combined = FALSE)
    }
    
    dev.off()
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


plot_group_diff = function(plot_data,
                           skalen,
                           umsetzung,
                           ums_data,
                           mar,
                           group_list,
                           path){
  
  for(j in group_list){
    
    if(!any(j %in% plot_data$B102)) { # wenn von der Gruppe kein Antrag gefunden:
      
      return("Gruppen Vergleich nicht möglich in plot_group_diff")
    }
  }
  
  
  tmp = list()
  
  for(i in 1:length(group_list)){
    
    ums_proz = create_ums_proz(data_skalen_ohne,
                               umsetzung,
                               ums_data,
                               group_index=get_index_by_group(plot_data, var_name_group = "B102", group_keys = group_list[[i]]))
    
    skalen_imp = create_skalen_imp(score_type = "scores_mean",
                                   skalen_scores = skalen_scores,
                                   group_index=get_index_by_group(plot_data, var_name_group = "B102", group_keys = group_list[[i]]))
    
    
    merged_imp = merge(skalen_imp, ums_proz, by.x = "Name", by.y="Skalen")
    
    merged_imp = merged_imp[order(merged_imp[,"mean"], decreasing = T),]
    
    tmp[[names(group_list)[i]]] = merged_imp
  }
  
  res = merge(tmp[[1]], tmp[[2]], by ="Name")
  
  res$Diff.Wert = res$mean.x - res$mean.y
  res$Diff.Proz = res$Prozente.x - res$Prozente.y
  
  tmp_Wert = res[order(res[,"Diff.Wert"], decreasing = F),]
  
  
  
  pdf(file=paste0(path, "Vergleich_Bewertung_", paste(names(group_list), collapse = "_"), ".pdf"),
      width=14, height = 12, paper = "a4r")
  
  par(mar=mar)
  
  plot(tmp_Wert$Diff.Wert,
       xaxt="n",
       xlab="",
       type="n",
       main=paste("Bewertung:", paste(names(group_list), collapse = " - ")),
       # ylim=c(-0.5, 0.5), 
       ylab="Unterschiede Bewertung")
  
  
  axis(side=1, at=1:nrow(tmp_Wert), labels = tmp_Wert$Name, las=2)
  
  arrows(x0 = 1:nrow(tmp_Wert), y0 = rep(-1, nrow(tmp_Wert)),
         y1 = 0,
         col="grey80",
         length = 0, lwd=1)
  
  arrows(x0 = 1:nrow(tmp_Wert), y0 = rep(0, nrow(tmp_Wert)),
         y1 = tmp_Wert$Diff.Wert,
         col=ifelse(tmp_Wert$Diff.Wert <= 0, "#1b9e77", "#e7298a"),
         length = 0, lwd=5)
  
  points(1:nrow(tmp_Wert), tmp_Wert$Diff.Wert, pch = 20)
  
  abline(h=0)
  
  legend("topleft",
         lty=1,
         lwd=5,
         col=c("#1b9e77", "#e7298a"),
         # legend = c("Höher bewertet HZE", "Höher bewertet SBBZ"),
         legend = paste("Höher bewertet", rev(names(group_list))),
         bty = "n")
  
  dev.off()
  
  
  
  tmp_Proz = res[order(res[,"Diff.Proz"], decreasing = F),]
  tmp_Proz = tmp_Proz[complete.cases(tmp_Proz),]
  
  pdf(file=paste0(path, "Vergleich_Umsetzung_", paste(names(group_list), collapse = "_"), ".pdf"),
      width=14, height = 12, paper = "a4r")
  
  
  par(mar=mar)
  
  plot(tmp_Proz$Diff.Proz, xaxt="n",
       xlab="",
       pch=20,
       main=  paste("Umsetzung:", paste(names(group_list), collapse = " - ")),
       # ylim=c(-0.2, 0.2),
       ylab = "Unterschied Einschätzung Umsetzung")
  
  arrows(x0 = 1:nrow(tmp_Proz), y0 = rep(-1, nrow(tmp_Proz)),
         y1 = 0,
         col="grey80",
         length = 0, lwd=1)
  
  axis(side=1, at=1:nrow(tmp_Proz), labels = tmp_Proz$Name, las=2)
  arrows(x0 = 1:nrow(tmp_Proz),
         y0 = rep(0, nrow(tmp_Proz)),
         y1 = tmp_Proz$Diff.Proz,
         col=ifelse(tmp_Proz$Diff.Proz <= 0, "#1b9e77", "#e7298a"),
         length = 0,
         lwd=5)
  
  points(1:nrow(tmp_Proz), tmp_Proz$Diff.Proz, pch = 20)
  abline(h=0)
  
  legend("topleft",
         lty=1,
         lwd=5,
         col=c("#1b9e77", "#e7298a"),
         # legend = c("Höher bewertet HZE", "Höher bewertet SBBZ"),
         legend = paste("Höher eingeschätzt", rev(names(group_list))),
         bty = "n")
  
  dev.off()
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
        
        # tmp_ls[["scores_fact"]] = rep(NA, length(tmp[,1]))
        # 
        # tmp_ls[["loadings"]] = rep(NA, 1)
        # 
        # tmp_ls[["p"]] = NA
        
        skalen_scores[[skalen_names[i]]] = tmp_ls
        
      }else{
        
        na_index = complete.cases(tmp)
        
        tmp = tmp[na_index,]
        
        tmp_na = rep(NA, length(na_index))
        
        tmp_na[na_index] = as.numeric(apply(tmp, 1, mean, na.rm=T))
        
        tmp_ls[["scores_mean"]] = tmp_na
        
        # if(ncol(tmp) < 3){
        #   
        #   tmp_ls[["scores_fact"]] = rep(NA, nrow(tmp))
        #   
        #   tmp_ls[["loadings"]] = rep(NA, ncol(tmp))
        #   
        #   tmp_ls[["p"]] = NA
        #   
        #   skalen_scores[[skalen_names[i]]] = tmp_ls
        #   
        # }else{
        #   
        #   tmp_jittered = apply(tmp, 2, jitter, factor = 0.01)
        #   
        #   fact = factanal(tmp_jittered, 1, scores = "regression")
        #   
        #   tmp_na[na_index] = fact$scores
        #   
        #   tmp_ls[["scores_fact"]] = tmp_na
        #   
        #   tmp_ls[["loadings"]] = fact$loadings
        #   
        #   tmp_ls[["p"]] = fact$PVAL
        #   
        #   
        #   skalen_scores[[skalen_names[i]]] = tmp_ls
        # }
        
        skalen_scores[[skalen_names[i]]] = tmp_ls
      }
    }else{
      print(paste(skalen_names[i], "besitzt keine Umsetzungsabfrage"))
    }
  }
  
  return(skalen_scores)
}



#' Erzeugt Plots mit 
#'
#'
#'
#'
descriptive_anal_plots = function(plot_data, data, type, path, se=F){
  
  
  if(!dir.exists(path)) {
    
    dir.create(path, recursive = T)
  }
  
  #  Begriffe:
  
  plot_ranked_all(plot_data,
                  data,
                  var_name = "Begriffe",
                  yx = c("nie", "selten", "häufig", "immer"),
                  type=type,
                  mar=c(18,5,3,3),
                  main="Begriffe",
                  file=paste0(path,"/Begriffe"),
                  se=se)
  
  
  # Verhaltensweisen
  
  plot_ranked_all(plot_data,
                  data,
                  var_name = "Verhaltensweisen",
                  yx = c("gar nicht kennzeichnend", "eher nicht kennzeichnend",
                         "eher kennzeichnend", "vollkommen kennzeichnend"),
                  type=type,
                  mar=c(18,12,3,3),
                  main="Verhaltensweisen",
                  file=paste0(path,"/Verhaltenweisen"),
                  se=se)
  
  
  # Informationen
  
  plot_ranked_all(plot_data,
                  data,
                  var_name = "Informationen", 
                  yx=c("unwichtig","eher unwichtig","eher wichtig","wichtig"),
                  type=type,
                  mar=c(18,7,3,3),
                  main="Informationen",
                  file= paste0(path,"/Informationen"),
                  se=se)
  
  
  # Dokumente
  
  plot_ranked_all(plot_data, 
                  data,
                  yx=c("nie", "selten", "häufig", "immer"),
                  type=type,
                  var_name = "Dokumente",
                  mar=c(18,5,3,3),
                  main="Dokumente",
                  file=paste0(path,"/Dokumente"),
                  se=se)
  
  # interdiszTeam
  
  plot_ranked_all(plot_data,
                  data,
                  yx=c("gar nicht wichtig", "nicht wichtig", "eher nicht wichtig",
                       "eher wichtig", "wichtig", "vollkommen wichtig"),
                  type=type,
                  var_name = "Interdisz",
                  mar=c(14,9,3,3),
                  main="Interdisziplinäres Team",
                  file = paste0(path,"/Interdisz_Team"),
                  se=se)
  
  
  # Kompetenzen
  
  plot_ranked_all(plot_data,
                  data,
                  yx=c("gar nicht wichtig", "nicht wichtig", "eher nicht wichtig",
                       "eher wichtig", "wichtig", "vollkommen wichtig"),
                  type=type,
                  var_name = "Kompetenzen",
                  mar=c(24,9,3,3),
                  main="Kompetenzen",
                  file = paste0(path,"/Kompetenzen"),
                  se=se)
  
  # Fortbildung
  
  plot_ranked_all(plot_data,
                  data = data,
                  yx=c("gar nicht wichtig", "nicht wichtig", "eher nicht wichtig",
                       "eher wichtig", "wichtig", "vollkommen wichtig"),
                  type=type,
                  var_name = "Fortbildung",
                  mar=c(18,9,3,3),
                  main="Fortbildungen",
                  file = paste0(path,"/Fortbildungen"),
                  se=se)
  
  
  
  # Altersgruppen jM
  
  # plot_binary(plot_data,
  #             var_name="AlterGruppen",
  #             main="Altersgruppen",
  #             mar=c(12,5,3,3),
  #             ylab="Prozent der Mitarbeitenden",
  #             file=paste0(path,"/Altersgruppen.pdf"))
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="AlterGruppen",
                  main="Altersgruppen",
                  mar=c(12,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Altersgruppen"),
                  ranked = F)
  
  # plot_binary(plot_data,
  #             var_name=var_name,
  #             main=paste(main),
  #             mar=c(12, 5, 3, 3),
  #             ylab="Prozent der Mitarbeitenden",
  #             file=paste0(path,"/Altersgruppen_Vergleich.pdf"),
  #             ranked = ranked,
  #             add = list(data[data$B107 != i, ]),
  #             label = c(Einrichtung, "Andere"))
  
  
  # Informationsart
  
  # plot_binary(plot_data,
  #             var_name="Informationsart",
  #             main="Art der Information",
  #             mar=c(12,5,3,3),
  #             ylab="Prozent der Mitarbeitenden",
  #             file=paste0(path,"/Informationsart.pdf"),
  #             ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Informationsart",
                  main="Art der Information",
                  mar=c(12,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Informationsart"),
                  ranked = T)
  
  # Informationsbedarf
  
  # plot_binary(plot_data,
  #             var_name="Informationsbedarf",
  #             main="Informationsbedarf",
  #             mar=c(18,5,3,3),
  #             ylab="Prozent der Mitarbeitenden",
  #             file=paste0(path,"/Informationsbedarf.pdf"),
  #             ranked = T)
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Informationsbedarf",
                  main="Informationsbedarf",
                  mar=c(18,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Informationsbedarf"),
                  ranked = T)
  
  # Vetorecht Aufnahme
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Vetor.Auf",
                  main="Vetorecht Aufnahme",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Vetorecht Aufnahme"),
                  ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Vetoem.Auf",
                  main="Vetoempfehlung Aufnahme",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Vetoempfehlung Aufnahme"),
                  ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Entscheidung.Auf",
                  main="Entscheidung Aufnahme",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Entscheidung Aufnahme"),
                  ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Vetor.Ent",
                  main="Vetorecht Entlassung",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Vetorecht Entlassung"),
                  ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Vetoem.Ent",
                  main="Vetoempfehlung Entlassung",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Vetoempfehlung Entlassung"),
                  ranked = T)
  
  
  plot_binary_all(plot_data,
                  data,
                  var_name="Entscheidung.Ent",
                  main="Entscheidung Entlassung",
                  mar=c(15,5,3,3),
                  ylab="Prozent der Mitarbeitenden",
                  file=paste0(path,"/Entscheidung Entlassung"),
                  ranked = T)
  
  
  plot_veto(data=plot_data,
            var_name=c("Entscheidung.Auf","Vetor.Auf","Vetoem.Auf"),
            main="Aufnahme: Entscheidung und Veto",
            mar=c(12,6,3,3),
            ylab="Prozent der Mitarbeitenden",
            file=paste0(path,"/Aufnahme_Einrichtung_gesamt.pdf"),
            ranked = T,
            labels=c("Entscheidung", "Vetorecht", "Vetoempfehlung"))
  
  
  plot_veto(data=data[data$B107 != Index_einrichtung,],
            var_name=c("Entscheidung.Auf","Vetor.Auf","Vetoem.Auf"),
            main="Aufnahme: Entscheidung und Veto",
            mar=c(12,6,3,3),
            ylab="Prozent der Mitarbeitenden",
            file=paste0(path,"/Aufnahme_Andere_gesamt.pdf"),
            ranked = T,
            labels=c("Entscheidung", "Vetorecht", "Vetoempfehlung"))
  
  
  plot_veto(data=plot_data,
            var_name=c("Entscheidung.Ent","Vetor.Ent","Vetoem.Ent"),
            main="Entlassung: Entscheidung und Veto",
            mar=c(12,6,3,3),
            ylab="Prozent der Mitarbeitenden",
            file=paste0(path,"/Entlassung_gesamt.pdf"),
            ranked = T,
            labels=c("Entscheidung", "Vetorecht", "Vetoempfehlung"))
  
  
  plot_veto(data=data[data$B107 != Index_einrichtung,],
            var_name=c("Entscheidung.Ent","Vetor.Ent","Vetoem.Ent"),
            main="Entlassung: Entscheidung und Veto",
            mar=c(12,6,3,3),
            ylab="Prozent der Mitarbeitenden",
            file=paste0(path,"/Entlassung_Andere_gesamt.pdf"),
            ranked = T,
            labels=c("Entscheidung", "Vetorecht", "Vetoempfehlung"))
}



get_item_summary = function(data, file, file.excel, path.plot){
  
  res_tmp = data.frame("Gruppe"=character(0),
                       "Skale"=character(0),
                       "Item"=character(0),
                       "mean"=numeric(0),
                       "se.1"=numeric(0),
                       "se.2"=numeric(0),
                       "umsetzung"=numeric(0),
                       "Frage"=character(0))
  
  res_tmp_names = names(res_tmp)
  
  skalen_df = data.frame("Skale"=character(0), "Item"=character(0))
  
  names_skalen_df = names(skalen_df)
  
  skalen_names = names(skalen)
  
  for(i in 1:length(skalen)){
    
    for(j in 1:length(skalen[[i]])){
      
      tmp = c(skalen_names[i], skalen[[i]][j])
      skalen_df = rbind(skalen_df, tmp)
    }
  }
  
  names(skalen_df) = names_skalen_df
  
  
  
  for(i in names(keys)){
    
    for(j in 1:length(keys[[i]])){
      
      tmp_skale = skalen_df$Skale[skalen_df$Item == keys[[i]][j]]
      
      tmp_fit = binom_est(data[,keys[[i]][j]], yx = 1:n_items$n[n_items$Item == keys[[i]][j]])
      
      # tmp = c(i,
      #         ifelse(length(tmp_skale) == 0, NA, tmp_skale),
      #         keys[[i]][j],
      #         round(mean(data[,keys[[i]][j]], na.rm=T), 2),
      #         median(data[,keys[[i]][j]], na.rm=T),
      #         round(sd(data[,keys[[i]][j]], na.rm=T), 3))
      
      if(length(tmp_skale > 0)){
        
        for(n in 1:length(tmp_skale)){
          
          tmp = c(i,
                  tmp_skale[n],
                  keys[[i]][j],
                  round(tmp_fit[1], 2),
                  round(tmp_fit[2], 3),
                  round(tmp_fit[3], 3))
          
          ums_tmp = data[ ,umsetzung$var.Key[umsetzung$var.Umgesetzt == keys[[i]][j]]]
          
          tmp = c(tmp, round(mean(ums_tmp - 1, na.rm=T)*100, 0))
          
          tmp = c(tmp, codebook$`Variable Label`[codebook$Variable == keys[[i]][j]][1])
          
          res_tmp = rbind(res_tmp, tmp,deparse.level = 2)  
        }
        
        
      }else{  
        
        tmp = c(i,
                NA,
                keys[[i]][j],
                round(tmp_fit[1], 2),
                round(tmp_fit[2], 3),
                round(tmp_fit[3], 3))
        
        ums_tmp = data[ ,umsetzung$var.Key[umsetzung$var.Umgesetzt == keys[[i]][j]]]
        
        tmp = c(tmp, round(mean(ums_tmp - 1, na.rm=T)*100, 0))
        
        tmp = c(tmp, codebook$`Variable Label`[codebook$Variable == keys[[i]][j]][1])
        
        res_tmp = rbind(res_tmp, tmp,deparse.level = 2)
      }
      
    }
  }
  
  
  names(res_tmp) = res_tmp_names
  
  res_tmp = res_tmp[order(res_tmp$Skale),]
  
  write.xlsx(res_tmp,file = file.excel, overwrite = T)
  
  
  unlink(path.plot, recursive = T)
  
  dir.create(path.plot)
  
  for(i in 1:length(skalen)){
    
    fit = as.numeric(res_tmp[res_tmp$Skale == names(skalen)[i],"mean"])
    se1 = as.numeric(res_tmp[res_tmp$Skale == names(skalen)[i],"se.1"])
    se2 = as.numeric(res_tmp[res_tmp$Skale == names(skalen)[i],"se.2"])
    
    
    fit = fit[!is.na(fit)]
    se1 = se1[!is.na(se1)]
    se2 = se2[!is.na(se2)]
    
    if(length(fit) > 0){
      
      jpeg(filename = paste0(path.plot, "/Skala_", i, ".jpg"), width=1000, height=570, pointsize = 20)
      
      par(mar=c(10, 5, 3, 3))
      
      plot(fit, 1:length(fit),
           xlim=c(1,6),
           xaxt="n",
           xlab="",
           yaxt="n",
           ylab="",
           type="n",
           main=names(skalen)[i],
           ylim=c(0.5, length(fit)+0.5))
      
      abline(v=c(1:6), col="grey80")
      
      arrows(x0 = rep(0, length(fit)), x1 = fit, y0 = 1:length(fit), lty=2, length = 0, lwd=1, col="grey50")
      
      arrows(x0 = fit, x1 = se1, y0 = 1:length(fit), length = 0.1, lwd=2, col="grey40", angle = 90)
      arrows(x0 = fit, x1 = se2, y0 = 1:length(fit), length = 0.1, lwd=2, col="grey40", angle = 90)
      
      points(fit, 1:length(fit), pch=20, cex=1.3)
      
      labels = res_tmp$Item[res_tmp$Skale == names(skalen)[i]]
      
      labels = labels[!is.na(labels)]
      
      axis(side=2, at = 1:length(fit), labels = labels, las=2)
      axis(side=1,
           at = 1:6,
           labels = c("stimme gar nicht zu", "stimme nicht zu", "stimme eher nicht zu",
                      "stimme eher zu", "stimme zu", "stimme vollkommen zu"),
           las=2)
      
      dev.off()
    }
  }
}


write_einrichtungen_sbbz_hze = function(data, file){
  
  res = data.frame("Einrichtung"=character(0), "Leitung"=numeric(0), "SBBZ"=numeric(0), "HZE"=numeric(0))
  res_names = names(res)
  
  for(i in unique(data$B107)){
    
    tmp = c(imp_names[["B107"]][i],
            sum(data[data$B107 == i, "B102"] == 1, na.rm=T),
            sum(data[data$B107 == i, "B102"] == 2, na.rm=T),
            sum(data[data$B107 == i, "B102"] > 2, na.rm=T)) 
    res = rbind(res, tmp)
  }
  
  names(res) = res_names
  
  res$Leitung = as.numeric(res$Leitung)
  res$SBBZ = as.numeric(res$SBBZ)
  res$HZE = as.numeric(res$HZE)
  
  
  write.xlsx(res, file = file)
}



fit_randomForest = function(data,
                            ums_data,
                            skalen,
                            skalen_names,
                            main,
                            file){
  
  ums_scores = get_ums_scores(ums_data, skalen = data_skalen, names(data_skalen))
  
  Halte_scores = get_skalen_scores(plot_data = data,
                                   skalen = data_skalen,
                                   skalen_names = "HK.subj.Einrichtung")
  
  
  ums_scores[["HK.subj.Einrichtung"]] = Halte_scores[["HK.subj.Einrichtung"]]
  
  
  ums_tmp = create_model_df(ums_scores = ums_scores, score_type = "scores_mean")
  
  ums_df = ums_tmp[[1]]
  
  
  rfm = randomForest(HK.subj.Einrichtung ~ ., data = ums_df, ntree=5000)
  
  
  m_imp = importance(rfm)
  
  m_names = rownames(m_imp)
  m_names = m_names[order(m_imp, decreasing = T)]
  m_imp = sort(m_imp, decreasing = T)
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  par(mar=c(12,4,3,3))
  
  plot(m_imp,
       type="n",
       xaxt="n",
       xlab="",
       ylab="Wichtigkeit nach Modell",
       main = main)
  
  arrows(x0 = 1:length(m_imp), y0 = 0, y1 = m_imp, length = 0, lwd=5, col="grey50")
  
  points(1:length(m_imp), m_imp, pch=20)
  axis(side = 1, las= 2, at= 1:length(m_imp), labels = m_names)
  
  dev.off()
  
  return (rfm)
}



plot_most_important = function(data,
                               skalen_scores,
                               mar=c(12,4,5,3),
                               main,
                               file, best_var=0){
  
  if(best_var==0){
    
    tmp_mean = numeric(length(skalen_scores))
    tmp_name = names(skalen_scores)
    
    
    for(i in 1:length(skalen_scores)){
      tmp_mean[i] = binom_est(skalen_scores[[i]]$scores_mean, yx=1:6)[1]
      
    }
    
    tmp_imp_names = tmp_name[order(tmp_mean, decreasing = T)][1:13]
    
  }else{
    
    tmp_imp_names = best_var
    
  }
  
  
  res_tmp = matrix(nrow=length(unique(data$B107)), ncol=length(tmp_imp_names)+1)
  
  res_tmp = as.data.frame(res_tmp)
  
  
  names(res_tmp) = c("Einrichtung", tmp_imp_names)
  
  n = 1
  
  for(i in unique(data$B107)){
    
    for(j in tmp_imp_names){
      ums_var = umsetzung$var.Key[umsetzung$var.Umgesetzt %in% skalen[[j]]]
      
      res_tmp[n,j] = mean(unlist(data[data$B107 == i, ums_var])) - 1
      # res_tmp[n, "Einrichtung"] = imp_names[["B107"]][i]
      res_tmp[n, "Einrichtung"] = i
    }
    
    n = n + 1
  }
  
  
  res_tmp[,"Einrichtung"][order(apply(res_tmp[,-1], 1, mean), decreasing = T)]
  
  
  best = tmp_imp_names
  
  
  # Einrichtungen mit weniger als 5 
  
  best_einr = res_tmp[order(apply(res_tmp[,-1], 1, mean), decreasing = T), ]
  
  
  
  tmp_table = table(data$B107)
  
  table_names = as.numeric(dimnames(tmp_table)[[1]])
  
  selected_einrichtung = table_names[table(data$B107) > 3]
  
  
  best_einr = best_einr[best_einr[,"Einrichtung"] %in% selected_einrichtung,]
  
  
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  par(mar=mar, xpd = F)
  
  plot(1:ncol(best_einr[,-1]),
       best_einr[1,-1],
       type="n",
       ylim=c(0,1),
       xlab="",
       xaxt="n",
       ylab="Umsetzung in %",
       yaxt="n")
  
  
  title(main = main, adj=0.2)
  
  abline(h=c(0, 0.25, 0.5, 0.75, 1), col="grey70")
  
  arrows(x0 = (1:ncol(best_einr[,-1]) - 0.15),
         y0=0,
         y1=as.numeric(best_einr[1,-1]), length=0, lwd= 5, col="#1b9e77")
  
  
  arrows(x0 = (1:ncol(best_einr[,-1])),
         y0=0,
         y1=as.numeric(best_einr[2,-1]), length=0, lwd= 5, col= "#e7298a")
  
  
  arrows(x0 = (1:ncol(best_einr[,-1]) + 0.15),
         y0=0,
         y1=as.numeric(best_einr[3,-1]), length=0, lwd= 5, col = "#7570b3")
  
  
  
  axis(side=1, at= 1:ncol(best_einr[,-1]), labels=best, las=2)
  
  
  axis(side=2, at= c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 25, 50, 75, 100), las=2)
  
  
  par(xpd = T)
  
  legend("topright", inset = c(0,-0.25), bty="n", fill=c("#1b9e77", "#e7298a", "#7570b3"),
         legend = imp_names[["B107"]][best_einr[1:3,"Einrichtung"]], cex = 1.2)
  
  dev.off()
  
  
}



plot_rel_density = function(plot_data,
                            ums_scores,
                            mar,
                            main,
                            file,
                            ylim){
  
  ums_tmp = create_model_df(ums_scores = ums_scores,
                            score_type = "scores_mean")
  
  Halte_scores = get_skalen_scores(plot_data = plot_data,
                                   skalen = data_skalen,
                                   skalen_names = "HK.subj.Einrichtung")
  
  
  ums_scores[["HK.subj.Einrichtung"]] = Halte_scores[["HK.subj.Einrichtung"]]
  
  
  ums_tmp = create_model_df(ums_scores = ums_scores,
                            score_type = "scores_mean")
  
  ums_df = ums_tmp[[1]]
  
  
  
  ums_df_agg = ums_df
  
  ums_df_agg$Einrichtung = plot_data$B107
  ums_df_agg$Gruppe = plot_data$B102
  
  
  data_slice = ums_df_agg
  
  
  pdf(file=file, width=14, height = 12, paper = "a4r")
  
  par(mar=mar)
  
  plot(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", c(3:5))])),
       col="#1b9e77", 
       main="subjektive Haltekraft der Einrichtung",
       xlim=c(1, 6),
       ylim=ylim,
       lwd=2,
       xlab="",
       ylab="relative Häufigkeitsdichte", xaxt="n", type="n")
  
  abline(v=c(1:6), col="grey70")
  
  
  if(length(get_index_by_group(data_slice, "Gruppe", c(3:5))) > 2){
    lines(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", c(3:5))]), from=1, to=6),
          col="#1b9e77", lwd=3)
  }
  
  
  if(length(get_index_by_group(data_slice, "Gruppe", 2)) > 2){
    lines(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 2)]), from=1, to=6),
          col="#e6ab02", lwd=3)
  }
  
  
  if(length(get_index_by_group(data_slice, "Gruppe", 1)) > 2){
    lines(density((data_slice$HK.subj.Einrichtung[get_index_by_group(data_slice, "Gruppe", 1)]), from=1, to=6),
          col="#e7298a", lwd=3)
  }
  
  points(jitter(data_slice$HK.subj.Einrichtung, 3),
         rep(0, nrow(data_slice)),
         pch="|",
         col=ifelse(data_slice$Gruppe == 1, "#e7298a",
                    ifelse(data_slice$Gruppe == 2, "#e6ab02", "#1b9e77")),
         cex=2)
  
  axis(1, at=1:6, labels = c("stimme gar nicht zu", "stimme nicht zu", "stimme eher nicht zu",
                             "stimme eher zu", "stimme zu", "stimme vollkommen zu"), las=2)
  
  legend("topleft", fill = c("#1b9e77", "#e6ab02", "#e7298a"),
         legend = c("HZE","SBBZ","Leitung"), bty="n")
  
  dev.off()
  
}


# get Item keys from skalen names 
getItemsFromSkalen = function(target_skalen, skalen_names) {
  
  tmp = character(0)
  
  for (skalen_name in skalen_names) {
    tmp = c(tmp, target_skalen[[skalen_name]])
    
  }
  
  return (tmp)
}

