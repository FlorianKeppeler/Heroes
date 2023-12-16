

# Hier den Pfad aendern zu deinem Verzeichnis mit den Daten

#data <- read.csv(file="C:/Users/Selina/Documents/Arbeit/HerOEs/Projekt HerOEs - DWW/Datenauswertung/heroes_final_data.csv", skipNul = T, na.strings = "-9", stringsAsFactors = FALSE)


# data <- read.csv("C:/Users/Selina/Documents/Arbeit/HerOEs/Projekt HerOEs - DWW/HerOEs/Datenauswertung/heroes_final_data.csv", skipNul = T, na.strings = "-9", stringsAsFactors = FALSE)

import_data <- function(Path, type){
  
  data <- read.csv("C:/Heroes/Downloads soscisurvey/CSV/data_HerOEs_2023-08-27_16-42.csv",
                   skipNul = T, na.strings = "-9", stringsAsFactors = FALSE)
  
  
  head(data[-1,6:25])
  
  names(data)
  
  data_compl <- data[!(is.na(as.numeric(data$A003)) | as.numeric((data$A003) == 2)),]
  
  
  sum(is.na(as.numeric(data$A003)))
  
  as.numeric(data$A003)
  
  
  data_compl$QUESTNNR
  
  # for(i in 1:nrow(data_compl)){
  #   tmp <- sum(is.na(data_compl[i,]))
  #   
  #   if(tmp > 0){
  #     print(paste("Case:", data_compl$ÿþCASE[i], "Na's:", tmp, " Zeile:", i, sep=" "))
  #   }
  # }
  
  # -> bis zu 33 NA's sind OK!!
  
  
  # Zeilen loeschen:
  
  # data_clean <- data_compl[-c(1,2,3,4,5),]
  
  
  # Spalten mit Strukturgutachten Shit rausschloeschen:
  
  # names(data_compl)[598:680]
  
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
}

#------




# Gruppen bilden ueber Indezes

lehr <- data_compl$B102 == 2

nicht_lehr <- data_compl$B102 > 2

mean(as.numeric(data_compl[lehr, "B107"]))


# Mittelwert bilden ueber mehrere Items:

apply(data_compl[lehr, 18:36], 2, FUN = mean, na.rm=T)


names(data_compl)

# mehrere Spalten auswaehlen: ueber Spaltennamen:

apply(data_compl[lehr, c("H802_01", "H805_02")], 2, FUN = mean, na.rm=T)


# mehrere Spalten auswaehlen: ueber Spaltennummer:

apply(data_compl[lehr,c(18, 30, 32)], 2, FUN = mean, na.rm=T)



# Summe

apply(data_compl[lehr, c("H802_01", "H805_02")], 2, FUN = sum, na.rm=T)

# Standardabweichung

apply(data_compl[lehr, c("H802_01", "H805_02")], 2, FUN = sd, na.rm=T)


# Ohne Gruppierung 

apply(data_compl[,c("H802_01", "H805_02")], 2, FUN = mean, na.rm=T)


# Mittelwert ueber mehrere Items

mean(unlist(data_compl[,c("H802_01", "H805_02")]), na.rm=T)



# Wie viel Faelle gab es pro Item: Hier wie viel Frageboegen kamen aus welchen Einrichtungen?

res <- table(data_compl[,"B107"])

res

#Altersgruppen

table(data_compl[,"B110"])

#Berufserfahrung
table(data_compl[,"B105"])

#Geschlecht
table(data_compl[,"B103"])


#Arbeitsbereich
table(data_compl[,"B102"])

barplot(res)

#Wie viele LK pro Einrichtung
table(as.numeric(data_compl[lehr, "B107"]))

#Rating Fortbildungen Mittelwert + Standardabweichungen
mean(as.numeric(data_compl[, "K207_16"]), na.rm=T)
median(as.numeric(data_compl[, "K207_16"]), na.rm=T)
sd(data_compl[,"K207_16"], na.rm=T)

#Wird nicht gefunden! Hä?
#Rating Begrifflichkeit Mittelwert + Standardabweichungen
data_compl[, "C201_02"]

data_compl$C201_02

mean(as.numeric(data_compl[, "C201_02"]), na.rm=T)
sd(data_compl[,"C201_02"], na.rm=T)

data_compl$c201_01

#j.M. nach Altersgruppen
table(data_compl[,"C240_01"])
data_compl$C232_01


########################################################################################
##################
## HALTEKRÄFTE ##
##################
########################################################################################



################################################
# 1. HK Beziehungen zu j.M.
################################################

## 1.a) Beziehungs-gestaltung zu den j.M.


apply(data_compl[, c("D312_02", "D312_03")], 2, FUN = mean, na.rm=T)

#über mehrere Items zusammen
#1.
mean(unlist(data_compl[,c("D312_02", "D312_03")]), na.rm=T)
sd(unlist(data_compl[,c("D312_02", "D312_03")]), na.rm=T)
median(unlist(data_compl[,c("D312_02", "D312_03")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03")]), na.rm=T)
sd(unlist(data_compl[,c("E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03")]), na.rm=T)
median(unlist(data_compl[,c("E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("H811_08", "H812_09", "I902_01", "I911_04")]), na.rm=T)
sd(unlist(data_compl[,c("H811_08", "H812_09", "I902_01", "I911_04")]), na.rm=T)
median(unlist(data_compl[,c("H811_08", "H812_09", "I902_01", "I911_04")]), na.rm=T)

#4.
mean(unlist(data_compl[,c("F524_19", "I912_05", "I915_08")]), na.rm=T)
sd(unlist(data_compl[,c("F524_19", "I912_05", "I915_08")]), na.rm=T)
median(unlist(data_compl[,c("F524_19", "I912_05", "I915_08")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("D312_02", "D312_03",
                          "E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", 
                          "H812_09", "I902_01", "I911_04", "F524_19", 
                          "I912_05", "I915_08")]), na.rm=T)
sd(unlist(data_compl[,c("D312_02", "D312_03", "E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", "H812_09", "I902_01", "I911_04", "F524_19", "I912_05", "I915_08")]), na.rm=T)
median(unlist(data_compl[,c("D312_02", "D312_03",
                            "E414_07", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", 
                            "H812_09", "I902_01", "I911_04", "F524_19", 
                            "I912_05", "I915_08")]), na.rm=T)



#Boxplots für die einzelnen Hypothesen HK BEZIEHUNGEN


par(mar=c(4,10,4,2))

boxplot(unlist(data_compl[,c("I912_05", "I915_08")]),
        unlist(data_compl[,c("H812_09", "I902_01", "I911_04", "F524_19")],),
        unlist(data_compl[,c("E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08")],),
        unlist(data_compl[,c("D312_02", "D312_03")],), 
        unlist(data_compl[,c("D312_02", "D312_03",
                             "E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", 
                             "H812_09", "I902_01", "I911_04", "F524_19", 
                             "I912_05", "I915_08")]),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",outline = F)

axis(side=2, 
     at=1:5, 
     labels = c("4. Hypothese",
                "3. Hypothese",
                "2. Hypothese",
                "1. Hypothese",
                "HK Beziehungen"), 
     las=2)


#Funktioniert noch nicht

res1 <- unlist(lapply(list(unlist(data_compl[,c("D312_02", "D312_03")]),
                           unlist(data_compl[,c("E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08")]),
                           unlist(data_compl[,c("H812_09", "I902_01", "I911_04", "F524_19")]),
                           unlist(data_compl[,c("I912_05", "I915_08")]),
                           unlist(data_compl[,c("D312_02", "D312_03",
                                                "E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08",
                                                "H812_09", "I902_01", "I911_04", "F524_19",
                                                "I912_05", "I915_08")])), FUN = mean, na.rm=T))


points(x = res1, 1:5, pch=20, col="red3")


#points(
#  lapply(list(unlist(data_compl[,c("D312_02", "D312_03")]),
#              unlist(data_compl[,c("E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08")]),
#             unlist(data_compl[,c("H812_09", "I902_01", "I911_04", "F524_19")]),
#              unlist(data_compl[,c("I912_05", "I915_08")]),
#              unlist(data_compl[,c("D312_02", "D312_03",
#                                   "E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08",
#                                   "H812_09", "I902_01", "I911_04", "F524_19",
#                                   "I912_05", "I915_08")])), FUN = mean, na.rm=T),
#
#1:5, pch=20, col="red3")



par(mar=c(3,10,2,2))
boxplot(data_compl[c("D312_02", "D312_03",
                     "E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", 
                     "H812_09", "I902_01", "I911_04", "F524_19", 
                     "I912_05", "I915_08"
)],
horizontal = T,
col = "steelblue3",
yaxt="n",outline = F)

#axis(side=2, 
#    at=1:16, 
#   labels = c("1. Hypothese",
#             "1. Hypothese",
#            "2. Hypothese",
#           "2. Hypothese",
#          "2. Hypothese",
#         "2. Hypothese",
#        "2. Hypothese",
#       "2. Hypothese",
#      "2. Hypothese",
#     "3. Hypothese",
#    "3. Hypothese",
#   "3. Hypothese",
#  "3. Hypothese",
# "4. Hypothese",
#"4. Hypothese",
#" "), 
#las=2)


#points(apply(data_compl[c("D312_02", "D312_03",
#                         "E402_01", "E415_08", "E416_09", "E417_10", "E418_11", "F508_03", "H811_08", 
#                        "H812_09", "I902_01", "I911_04", "F524_19", 
#                       "I912_05", "I915_08"
#)], 2, FUN = mean, na.rm=T),
#       1:15, pch=20, col="red3")










###########################################################
# 2. 2.	Transparenz gegenüber und Beteiligung j.M. und WG
###########################################################

## 2.a) Transparenz gegenüber j.M.
mean(unlist(data_compl[,c("E402_01", "G607_04")]), na.rm=T)
sd(unlist(data_compl[,c("E402_01", "G607_04")]), na.rm=T)
median(unlist(data_compl[,c("E402_01", "G607_04")]), na.rm=T)

## 2.b) Beteiligung von j.M.
mean(unlist(data_compl[,c("E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07")]), na.rm=T)
sd(unlist(data_compl[,c("E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07")]), na.rm=T)
median(unlist(data_compl[,c("E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07")]), na.rm=T)

## 2. c) Transparenz gegenüber WG
mean(unlist(data_compl[,c("G615_12")]), na.rm=T)
sd(unlist(data_compl[,c("G615_12")]), na.rm=T)
median(unlist(data_compl[,c("G615_12")]), na.rm=T)

#4.# 2.d) Beteiligung von WG
mean(unlist(data_compl[,c("E412_05")]), na.rm=T)
sd(unlist(data_compl[,c("E412_05")]), na.rm=T)
median(unlist(data_compl[,c("E412_05")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("E402_01", "G607_04",
                          "E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07", 
                          "G615_12", 
                          "E412_05")]), na.rm=T)
sd(unlist(data_compl[,c("E402_01", "G607_04",
                        "E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07", 
                        "G615_12", 
                        "E412_05")]), na.rm=T)
median(unlist(data_compl[,c("E402_01", "G607_04",
                            "E409_02", "E410_03", "F502_01", "G602_01", "G605_02", "G606_03", "I909_02", "H810_07", 
                            "G615_12", 
                            "E412_05")]), na.rm=T)







########################################################
# 3.	Fallkommunikation mit Kooperationspartner:innen
########################################################

## 3.a) Fallkommunikation mit Jugendämtern 

#1.
mean(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18")]), na.rm=T)
sd(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18")]), na.rm=T)
median(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F504_01")]), na.rm=T)
sd(unlist(data_compl[,c("F504_01")]), na.rm=T)
median(unlist(data_compl[,c("F504_01")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("F545_03")]), na.rm=T)
sd(unlist(data_compl[,c("F545_03")]), na.rm=T)
median(unlist(data_compl[,c("F545_03")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                          "F504_01", 
                          "F545_03")]), na.rm=T)
sd(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                        "F504_01", 
                        "F545_03")]), na.rm=T)
median(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                            "F504_01", 
                            "F545_03")]), na.rm=T)


## 3.b) Fallkommunikation mit KJP

#1.
mean(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12")]), na.rm=T)
sd(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12")]), na.rm=T)
median(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F555_13", "F556_14", "H816_13")]), na.rm=T)
sd(unlist(data_compl[,c("F555_13", "F556_14", "H816_13")]), na.rm=T)
median(unlist(data_compl[,c("F555_13", "F556_14", "H816_13")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                          "F555_13", "F556_14", "H816_13")]), na.rm=T)
sd(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                        "F555_13", "F556_14", "H816_13")]), na.rm=T)
median(unlist(data_compl[,c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                            "F555_13", "F556_14", "H816_13")]), na.rm=T)



## 3.c) Fallkommunikation mit Schulämtern

mean(unlist(data_compl[,c("F547_05", "F513_08")]), na.rm=T)
sd(unlist(data_compl[,c("F547_05", "F513_08")]), na.rm=T)
median(unlist(data_compl[,c("F547_05", "F513_08")]), na.rm=T)




#Evtl. weniger sinnvoll: HK Fallkommunikation mit Kooperationspartner:innen Insgesamt

mean(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                          "F504_01", 
                          "F545_03",
                          "F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                          "F555_13", "F556_14", "H816_13",
                          "F547_05", "F513_08")]), na.rm=T)
sd(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                        "F504_01", 
                        "F545_03",
                        "F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                        "F555_13", "F556_14", "H816_13",
                        "F547_05", "F513_08")]), na.rm=T)
median(unlist(data_compl[,c("F508_03", "F544_02", "F550_08", "F546_04", "I914_07", "G618_15", "G619_16", "F519_14", "G613_10", "F517_12", "G621_18",
                            "F504_01", 
                            "F545_03",
                            "F551_09", "F552_10", "F553_11", "G623_20", "F554_12",
                            "F555_13", "F556_14", "H816_13",
                            "F547_05", "F513_08")]), na.rm=T)





#############################################################################
# 4. Kommunikation mit Eltern und anderen relevanten (Beziehungs-) Personen
#############################################################################

## 4.a) Kommunikation mit Eltern 

#1.
mean(unlist(data_compl[,c("D312_07")]), na.rm=T)
sd(unlist(data_compl[,c("D312_07")]), na.rm=T)
median(unlist(data_compl[,c("D312_07")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F548_06", "G624_21")]), na.rm=T)
sd(unlist(data_compl[,c("F548_06", "G624_21")]), na.rm=T)
median(unlist(data_compl[,c("F548_06", "G624_21")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("F557_15")]), na.rm=T)
sd(unlist(data_compl[,c("F557_15")]), na.rm=T)
median(unlist(data_compl[,c("F557_15")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("D312_07",
                          "F548_06", "G624_21", 
                          "F557_15")]), na.rm=T)
sd(unlist(data_compl[,c("D312_07",
                        "F548_06", "G624_21", 
                        "F557_15")]), na.rm=T)
median(unlist(data_compl[,c("D312_07",
                            "F548_06", "G624_21", 
                            "F557_15")]), na.rm=T)


## 4.b) Kommunikation mit anderen Personen (peers, Freunde etc.) 

#1.
mean(unlist(data_compl[,c("I916_09", "I913_06")]), na.rm=T)
sd(unlist(data_compl[,c("I916_09", "I913_06")]), na.rm=T)
median(unlist(data_compl[,c("I916_09", "I913_06")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F549_07", "G625_22")]), na.rm=T)
sd(unlist(data_compl[,c("F549_07", "G625_22")]), na.rm=T)
median(unlist(data_compl[,c("F549_07", "G625_22")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("I916_09", "I913_06",
                          "F549_07", "G625_22")]), na.rm=T)
sd(unlist(data_compl[,c("I916_09", "I913_06",
                        "F549_07", "G625_22")]), na.rm=T)
median(unlist(data_compl[,c("I916_09", "I913_06",
                            "F549_07", "G625_22")]), na.rm=T)



# Haltekraft Insgesamt
mean(unlist(data_compl[,c("D312_07",
                          "F548_06", "G624_21", 
                          "F557_15",
                          "I916_09", "I913_06",
                          "F549_07", "G625_22")]), na.rm=T)
sd(unlist(data_compl[,c("D312_07",
                        "F548_06", "G624_21", 
                        "F557_15",
                        "I916_09", "I913_06",
                        "F549_07", "G625_22")]), na.rm=T)
median(unlist(data_compl[,c("D312_07",
                            "F548_06", "G624_21", 
                            "F557_15",
                            "I916_09", "I913_06",
                            "F549_07", "G625_22")]), na.rm=T)






######################################
# 5.	Interne Fallkommunikation
######################################

## 5.a) Fallkommunikation intern 

#1.
mean(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18")]), na.rm=T)
sd(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18")]), na.rm=T)
median(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10")]), na.rm=T)
sd(unlist(data_compl[,c("F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10")]), na.rm=T)
median(unlist(data_compl[,c("F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("G609_06", "G616_13", "F513_08")]), na.rm=T)
sd(unlist(data_compl[,c("G609_06", "G616_13", "F513_08")]), na.rm=T)
median(unlist(data_compl[,c("G609_06", "G616_13", "F513_08")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18",
                          "F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10", 
                          "G609_06", "G616_13", "F513_08")]), na.rm=T)
sd(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18",
                        "F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10", 
                        "G609_06", "G616_13", "F513_08")]), na.rm=T)
median(unlist(data_compl[,c("D312_01", "D312_06", "D312_09", "D312_10", "E411_04", "D312_04", "D312_05", "D312_08", "F517_12", "F523_18",
                            "F520_15", "F522_17", "F521_16", "F507_02", "G608_05", "H818_15", "G613_10", 
                            "G609_06", "G616_13", "F513_08")]), na.rm=T)









#################################################################
## 6. HK Krisen ##
#################################################################


# 6.a) Krisenverhinderung

#1.
mean(unlist(data_compl[,c("H703_01", "H707_03")]), na.rm=T)
sd(unlist(data_compl[,c("H703_01", "H707_03")]), na.rm=T)
median(unlist(data_compl[,c("H703_01", "H707_03")]), na.rm=T)

#2.
mean(data_compl[,"H706_02"], na.rm=T)
sd(data_compl[,"H706_02"], na.rm=T)
median(data_compl[,"H706_02"], na.rm=T)

#3. 
mean(data_compl[,"H708_04"], na.rm=T)
sd(data_compl[,"H708_04"], na.rm=T)
median(data_compl[,"H708_04"], na.rm=T)

#4.
mean(unlist(data_compl[,c("H715_11", "H716_12", "H717_13","H718_14" )]), na.rm=T)
sd(unlist(data_compl[,c("H715_11", "H716_12", "H717_13", "H718_14" )]), na.rm=T)
median(unlist(data_compl[,c("H715_11", "H716_12", "H717_13", "H718_14" )]), na.rm=T)

#Insgesamt Krisenverhinderung
mean(unlist(data_compl[,c("H703_01", "H707_03",
                          "H706_02",
                          "H708_04",
                          "H715_11", "H716_12", "H717_13","H718_14")]), na.rm=T)
sd(unlist(data_compl[,c("H703_01", "H707_03",
                        "H706_02",
                        "H708_04",
                        "H715_11", "H716_12", "H717_13","H718_14")]), na.rm=T)
median(unlist(data_compl[,c("H703_01", "H707_03",
                            "H706_02",
                            "H708_04",
                            "H715_11", "H716_12", "H717_13","H718_14")]), na.rm=T)




#Boxplots für die einzelnen Hypothesen HK KRISENVERHINDERUNG


par(mar=c(10,10,10,4))

par(mar=c(4,10,2,2))

plot(1:6, 1:6, xlab="", ylab="",  yaxt="n", type = "n", ylim=c(0.5,5.5))

boxplot(unlist(data_compl[,c("H715_11", "H716_12", "H717_13","H718_14")],),
        unlist(data_compl[,c("H708_04")],),
        unlist(data_compl[,c("H706_02")],),
        unlist(data_compl[,c("H703_01", "H707_03")],),
        unlist(data_compl[,c("H703_01", "H707_03",
                             "H706_02",
                             "H708_04",
                             "H715_11", "H716_12", "H717_13","H718_14" )]),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",
        xaxt= "n",
        outline = F, add=T)

axis(side=2, 
     at=1:5, 
     labels = c("4. Hypothese",
                "3. Hypothese",
                "2. Hypothese",
                "1. Hypothese",
                "HK Krisenverhinderung"), 
     las=2)


res3 <- unlist(lapply(list(unlist(data_compl[,c("H715_11", "H716_12", "H717_13","H718_14")]),
                           unlist(data_compl[,c("H708_04")]),
                           unlist(data_compl[,c("H706_02")]),
                           unlist(data_compl[,c("H703_01", "H707_03")]),
                           unlist(data_compl[,c("H703_01", "H707_03",
                                                "H706_02",
                                                "H708_04",
                                                "H715_11", "H716_12", "H717_13","H718_14" )])), FUN = mean, na.rm=T))


points(x = res3, 1:5, pch=20, col="red3")









#6.b) Krisenbewältigung

#1.
mean(unlist(data_compl[,c("H711_07", "H712_08", "H713_09")]), na.rm=T)
sd(unlist(data_compl[,c("H711_07", "H712_08", "H713_09")]), na.rm=T)
median(unlist(data_compl[,c("H711_07", "H712_08", "H713_09")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("H709_05", "H714_10")]), na.rm=T)
sd(unlist(data_compl[,c("H709_05", "H714_10")]), na.rm=T)
median(unlist(data_compl[,c("H709_05", "H714_10")]), na.rm=T)

#Insgesamt Krisenbewältigung
mean(unlist(data_compl[,c("H711_07", "H712_08", "H713_09",
                          "H709_05", "H714_10")]), na.rm=T)
sd(unlist(data_compl[,c("H711_07", "H712_08", "H713_09",
                        "H709_05", "H714_10")]), na.rm=T)
median(unlist(data_compl[,c("H711_07", "H712_08", "H713_09",
                            "H709_05", "H714_10")]), na.rm=T)



#Boxplots für die einzelnen Hypothesen HK KRISENBEWÄLTIGUNG


par(mar=c(10,10,10,4))

par(mar=c(4,10,2,2))

plot(1:6, 1:6, xlab="", ylab="",  yaxt="n", type = "n", ylim=c(0.5,3.5))

boxplot(unlist(data_compl[,c("H709_05", "H714_10")],),
        unlist(data_compl[,c("H711_07", "H712_08", "H713_09")],),
        unlist(data_compl[,c("H711_07", "H712_08", "H713_09",
                             "H709_05", "H714_10")]),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",
        xaxt= "n",
        outline = F, add=T)

axis(side=2, 
     at=1:3, 
     labels = c("2. Hypothese",
                "1. Hypothese",
                "HK Krisenbewältigung"), 
     las=2)

res4 <- unlist(lapply(list(unlist(data_compl[,c("H709_05", "H714_10")]),
                           unlist(data_compl[,c("H711_07", "H712_08", "H713_09")]),
                           unlist(data_compl[,c("H711_07", "H712_08", "H713_09",
                                                "H709_05", "H714_10")])), FUN = mean, na.rm=T))


points(x = res4, 1:3, pch=20, col="red3")








#################################################################
## 7. HK Krisenaufarbeitung ##
#################################################################


# 7.a) Krisenaufarbeitung

#1.
mean(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04")]), na.rm=T)
sd(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04")]), na.rm=T)
median(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04")]), na.rm=T)

#2.
mean(data_compl[,"H810_07"], na.rm=T)
sd(data_compl[,"H810_07"], na.rm=T)
median(data_compl[,"H810_07"], na.rm=T)

#3. 
mean(unlist(data_compl[,c("H811_08", "H812_09")]), na.rm=T)
sd(unlist(data_compl[,c("H811_08", "H812_09")]), na.rm=T)
median(unlist(data_compl[,c("H811_08", "H812_09")]), na.rm=T)

#4.
mean(unlist(data_compl[,c("H813_10", "H817_14", "H818_15", "I913_06")]), na.rm=T)
sd(unlist(data_compl[,c("H813_10", "H817_14", "H818_15", "I913_06")]), na.rm=T)
median(unlist(data_compl[,c("H813_10", "H817_14", "H818_15", "I913_06")]), na.rm=T)

#5. 
mean(unlist(data_compl[,c("H815_12", "H814_11", "H816_13")]), na.rm=T)
sd(unlist(data_compl[,c("H815_12", "H814_11", "H816_13")]), na.rm=T)
median(unlist(data_compl[,c("H815_12", "H814_11", "H816_13")]), na.rm=T)

#Insgesamt Krisenaufarbeitung
mean(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04",
                          "H810_07",
                          "H811_08", "H812_09",
                          "H813_10", "H817_14", "H818_15", "I913_06",
                          "H815_12", "H814_11", "H816_13")]), na.rm=T)
sd(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04",
                        "H810_07",
                        "H811_08", "H812_09",
                        "H813_10", "H817_14", "H818_15", "I913_06",
                        "H815_12", "H814_11", "H816_13")]), na.rm=T)
median(unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04",
                            "H810_07",
                            "H811_08", "H812_09",
                            "H813_10", "H817_14", "H818_15", "I913_06",
                            "H815_12", "H814_11", "H816_13")]), na.rm=T)



#Boxplots für die einzelnen Hypothesen HK KRISENAUFARBEITUNG


par(mar=c(4,10,2,2))


plot(1:6, 1:6, xlab="", ylab="",  yaxt="n", type = "n", ylim=c(0.5,6.5))

boxplot(unlist(data_compl[,c("H815_12", "H814_11", "H816_13")],),
        unlist(data_compl[,c("H813_10", "H817_14", "H818_15", "I913_06")],),
        unlist(data_compl[,c("H811_08", "H812_09")],),
        unlist(data_compl[,c("H810_07")],),
        unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04")],),
        unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04",
                             "H810_07",
                             "H811_08", "H812_09",
                             "H813_10", "H817_14", "H818_15", "I913_06",
                             "H815_12", "H814_11", "H816_13")]),
        xlim = c(1, 6),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",
        xaxt= "n",
        outline = F, add=T)

#axis(side=1,
#at=c(1,2,3,4,5,6),
#labels=c("vollkommen unwichtig",
#          "unwichtig", 
#           "eher unwichtig",
#            "eher wichtig", 
#             "wichtig", 
#              "vollkommen wichtig"))

axis(side=2, 
     at=1:6, 
     labels = c("5. Hypothese",
                "4. Hypothese",
                "3. Hypothese",
                "2. Hypothese",
                "1. Hypothese",
                "HK Krisenaufarbeitung"), 
     las=2)

res5 <- unlist(lapply(list(unlist(data_compl[,c("H815_12", "H814_11", "H816_13")]),
                           unlist(data_compl[,c("H813_10", "H817_14", "H818_15", "I913_06")]),
                           unlist(data_compl[,c("H811_08", "H812_09")]),
                           unlist(data_compl[,c("H810_07")]),
                           unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04")]),
                           unlist(data_compl[,c("H802_01", "H805_02", "H806_03", "H807_04",
                                                "H810_07",
                                                "H811_08", "H812_09",
                                                "H813_10", "H817_14", "H818_15", "I913_06",
                                                "H815_12", "H814_11", "H816_13" )])), FUN = mean, na.rm=T))


points(x = res5, 1:6, pch=20, col="red3")










#Insgesamt KRISEN
mean(unlist(data_compl[,c("H703_01", "H707_03",
                          "H706_02",
                          "H708_04",
                          "H715_11", "H716_12", "H717_13","H718_14",
                          "H711_07", "H712_08", "H713_09",
                          "H709_05", "H714_10",
                          "H802_01", "H805_02", "H806_03", "H807_04",
                          "H810_07",
                          "H811_08", "H812_09",
                          "H813_10", "H817_14", "H818_15", "I913_06",
                          "H815_12", "H814_11", "H816_13")]), na.rm=T)

sd(unlist(data_compl[,c("H703_01", "H707_03",
                        "H706_02",
                        "H708_04",
                        "H715_11", "H716_12", "H717_13","H718_14",
                        "H711_07", "H712_08", "H713_09",
                        "H709_05", "H714_10",
                        "H802_01", "H805_02", "H806_03", "H807_04",
                        "H810_07",
                        "H811_08", "H812_09",
                        "H813_10", "H817_14", "H818_15", "I913_06",
                        "H815_12", "H814_11", "H816_13")]), na.rm=T)

median(unlist(data_compl[,c("H703_01", "H707_03",
                            "H706_02",
                            "H708_04",
                            "H715_11", "H716_12", "H717_13","H718_14",
                            "H711_07", "H712_08", "H713_09",
                            "H709_05", "H714_10",
                            "H802_01", "H805_02", "H806_03", "H807_04",
                            "H810_07",
                            "H811_08", "H812_09",
                            "H813_10", "H817_14", "H818_15", "I913_06",
                            "H815_12", "H814_11", "H816_13")]), na.rm=T)

par(mar=c(4,10,2,2))


plot(1:6, 1:6, xlab="", ylab="",  yaxt="n", type = "n", ylim=c(0.5,1.5))

boxplot(unlist(data_compl[,c("H703_01", "H707_03",
                             "H706_02",
                             "H708_04",
                             "H715_11", "H716_12", "H717_13","H718_14",
                             "H711_07", "H712_08", "H713_09",
                             "H709_05", "H714_10",
                             "H802_01", "H805_02", "H806_03", "H807_04",
                             "H810_07",
                             "H811_08", "H812_09",
                             "H813_10", "H817_14", "H818_15", "I913_06",
                             "H815_12", "H814_11", "H816_13")]),
        xlim = c(1, 6),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",
        xaxt= "n",
        outline = F, add=T)



axis(side=2, 
     at=1:1, 
     labels = c("HK Krisen"), 
     las=2)

res6 <- unlist(lapply(list(unlist(data_compl[,c("H703_01", "H707_03",
                                                "H706_02",
                                                "H708_04",
                                                "H715_11", "H716_12", "H717_13","H718_14",
                                                "H711_07", "H712_08", "H713_09",
                                                "H709_05", "H714_10",
                                                "H802_01", "H805_02", "H806_03", "H807_04",
                                                "H810_07",
                                                "H811_08", "H812_09",
                                                "H813_10", "H817_14", "H818_15", "I913_06",
                                                "H815_12", "H814_11", "H816_13")])), FUN = mean, na.rm=T))


points(x = res6, 1:1, pch=20, col="red3")





##########################################################
# 8. HK Team
##########################################################

# 8.a) Team-unterstützung/ -rückhalt

apply(data_compl[, c("D312_02", "D312_03")], 2, FUN = mean, na.rm=T)

#über mehrere Items zusammen
#1.
mean(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19")]), na.rm=T)
sd(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19")]), na.rm=T)
median(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08")]), na.rm=T)
sd(unlist(data_compl[,c("F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08")]), na.rm=T)
median(unlist(data_compl[,c("F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("H710_06", "K222_13", "K225_16")]), na.rm=T)
sd(unlist(data_compl[,c("H710_06", "K222_13", "K225_16")]), na.rm=T)
median(unlist(data_compl[,c("H710_06", "K222_13", "K225_16")]), na.rm=T)

#4.
mean(unlist(data_compl[,c("K226_17")]), na.rm=T)
sd(unlist(data_compl[,c("K226_17")]), na.rm=T)
median(unlist(data_compl[,c("K226_17")]), na.rm=T)

#5.
mean(unlist(data_compl[,c("K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08")]), na.rm=T)
sd(unlist(data_compl[,c("K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08")]), na.rm=T)
median(unlist(data_compl[,c("K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08")]), na.rm=T)

#6.
mean(unlist(data_compl[,c("K223_14", "K224_15")]), na.rm=T)
sd(unlist(data_compl[,c("K223_14", "K224_15")]), na.rm=T)
median(unlist(data_compl[,c("K223_14", "K224_15")]), na.rm=T)

#Insgesamt
mean(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19", 
                          "F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08",
                          "H710_06", "K222_13", "K225_16", 
                          "K226_17", 
                          "K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08", 
                          "K223_14", "K224_15"  )]), na.rm=T)
sd(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19", "F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08", "H710_06", "K222_13", "K225_16", "K226_17", "K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08", "K223_14", "K224_15")]), na.rm=T)
median(unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19", "F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08", "H710_06", "K222_13", "K225_16", "K226_17", "K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08", "K223_14", "K224_15")]), na.rm=T)

#




#Boxplots für die einzelnen Hypothesen HK TEAM



par(mar=c(8,8,4,4))

boxplot(unlist(data_compl[,c("K223_14", "K224_15")]),
        unlist(data_compl[,c("K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08")],),
        unlist(data_compl[,c("K226_17")],),
        unlist(data_compl[,c("H710_06", "K222_13", "K225_16")],),
        unlist(data_compl[,c("F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08")],),
        unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19")],),
        unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19", 
                             "F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08",
                             "H710_06", "K222_13", "K225_16", 
                             "K226_17", 
                             "K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08", 
                             "K223_14", "K224_15" )]),
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",outline = F)

axis(side=2, 
     at=1:7, 
     labels = c("6. Hypothese",
                "5. Hypothese",
                "4. Hypothese",
                "3. Hypothese",
                "2. Hypothese",
                "1. Hypothese",
                "HK Team"), 
     las=2)

res2 <- unlist(lapply(list(unlist(data_compl[,c("K223_14", "K224_15")]),
                           unlist(data_compl[,c("K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08")]),
                           unlist(data_compl[,c("K226_17")]),
                           unlist(data_compl[,c("H710_06", "K222_13", "K225_16")]),
                           unlist(data_compl[,c("F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08")]),
                           unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19")]),
                           unlist(data_compl[,c("F509_04", "F510_05", "F522_17", "F523_18", "G617_14", "G620_17", "G621_18", "G622_19", 
                                                "F511_06", "F512_07", "F514_09", "F515_10", "F516_11", "F518_13", "G610_07", "G611_08",
                                                "H710_06", "K222_13", "K225_16", 
                                                "K226_17", 
                                                "K211_02", "K212_03", "K213_04", "K214_05", "K215_06", "K216_07", "K217_08", 
                                                "K223_14", "K224_15" )])), FUN = mean, na.rm=T))


points(x = res2, 1:7, pch=20, col="red3")









#Rekodieren der Variable H707_03 (immer Kontrolle haben)
#data_compl$H707_03_rekodiert <- 7 - data_compl$H707_03
#data_compl$H707_03
#data_compl$H707_03_rekodiert






##########################################################
# 9. Fachliche Begleitung der Teams 
##########################################################

# 9.a) Fachliche Unterstützung/ Kompetenzförderung der MA


#über mehrere Items zusammen
#1.
mean(unlist(data_compl[,c("H808_05", "H809_06")]), na.rm=T)
sd(unlist(data_compl[,c("H808_05", "H809_06")]), na.rm=T)
median(unlist(data_compl[,c("H808_05", "H809_06")]), na.rm=T)

#2.
mean(unlist(data_compl[,c("E418_11", "G612_09")]), na.rm=T)
sd(unlist(data_compl[,c("E418_11", "G612_09")]), na.rm=T)
median(unlist(data_compl[,c("E418_11", "G612_09")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("K219_10", "K220_11", "K218_09", "I910_03")]), na.rm=T)
sd(unlist(data_compl[,c("K219_10", "K220_11", "K218_09", "I910_03")]), na.rm=T)
median(unlist(data_compl[,c("K219_10", "K220_11", "K218_09", "I910_03")]), na.rm=T)

#4.
mean(unlist(data_compl[,c("K202_01", "K211_02")]), na.rm=T)
sd(unlist(data_compl[,c("K202_01", "K211_02")]), na.rm=T)
median(unlist(data_compl[,c("K202_01", "K211_02")]), na.rm=T)

#5.
mean(unlist(data_compl[,c("K221_12")]), na.rm=T)
sd(unlist(data_compl[,c("K221_12")]), na.rm=T)
median(unlist(data_compl[,c("K221_12")]), na.rm=T)



#Insgesamt
mean(unlist(data_compl[,c("H808_05", "H809_06", 
                          "E418_11", "G612_09",
                          "K219_10", "K220_11", "K218_09", "I910_03", 
                          "K202_01", "K211_02",
                          "K221_12")]), na.rm=T)
sd(unlist(data_compl[,c("H808_05", "H809_06", 
                        "E418_11", "G612_09",
                        "K219_10", "K220_11", "K218_09", "I910_03", 
                        "K202_01", "K211_02",
                        "K221_12")]), na.rm=T)
median(unlist(data_compl[,c("H808_05", "H809_06", 
                            "E418_11", "G612_09",
                            "K219_10", "K220_11", "K218_09", "I910_03", 
                            "K202_01", "K211_02",
                            "K221_12")]), na.rm=T)






# 9.b) Pädagogische Grundhaltungen


#über mehrere Items zusammen
#1.1
mean(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04")]), na.rm=T)
sd(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04")]), na.rm=T)
median(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04")]), na.rm=T)

#1.2
mean(unlist(data_compl[,c("J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)
sd(unlist(data_compl[,c("J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)
median(unlist(data_compl[,c("J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)



#Insgesamt
mean(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                          "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)
sd(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                        "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)
median(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                            "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09")]), na.rm=T)




# 9.d) Partizipation des Teams


#über mehrere Items zusammen
#2
mean(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14")]), na.rm=T)
sd(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14")]), na.rm=T)
median(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14")]), na.rm=T)

#3.
mean(unlist(data_compl[,c("F504_01", "F555_13", "F556_14")]), na.rm=T)
sd(unlist(data_compl[,c("F504_01", "F555_13", "F556_14")]), na.rm=T)
median(unlist(data_compl[,c("F504_01", "F555_13", "F556_14")]), na.rm=T)

#4.
mean(unlist(data_compl[,c("H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04")]), na.rm=T)
sd(unlist(data_compl[,c("H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04")]), na.rm=T)
median(unlist(data_compl[,c("H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04")]), na.rm=T)

#5.
mean(unlist(data_compl[,c("H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
sd(unlist(data_compl[,c("H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
median(unlist(data_compl[,c("H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)



#Insgesamt
mean(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                          "F504_01", "F555_13", "F556_14",
                          "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                          "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
sd(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                        "F504_01", "F555_13", "F556_14",
                        "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                        "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
median(unlist(data_compl[,c("G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                            "F504_01", "F555_13", "F556_14",
                            "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                            "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)



# Evtl. weniger sinnvoll: HK Fachkraft Insgesamt
mean(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                          "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09",
                          "G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                          "F504_01", "F555_13", "F556_14",
                          "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                          "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
sd(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                        "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09",
                        "G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                        "F504_01", "F555_13", "F556_14",
                        "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                        "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)
median(unlist(data_compl[,c("J117_12", "J118_13", "J116_11", "J118_13", "J115_10", "J109_04", "J114_09",
                            "J102_01", "J107_02", "J108_03", "J111_06", "J110_05", "J112_07", "J113_08", "J114_09",
                            "G615_12", "D312_06", "D312_09", "D312_10", "E411_04", "D312_08", "F520_15", "G609_06", "G616_13", "H817_14",
                            "F504_01", "F555_13", "F556_14",
                            "H710_06", "K222_13", "K225_16", "K215_06", "K217_08", "H809_06", "K220_11", "K221_12", "H807_04",
                            "H703_01", "H706_02", "H714_10", "H802_01", "H805_02", "H806_03", "H810_07", "H813_10", "H811_08", "H818_15")]), na.rm=T)




##########################################################
# 10. Konkrete Haltekraftabfrage
##########################################################


mean(unlist(data_compl[,c("L301_01", "L302_01", "L303_01", "L304_01", "L305_01")]), na.rm=T)
sd(unlist(data_compl[,c("L301_01", "L302_01", "L303_01", "L304_01", "L305_01")]), na.rm=T)
median(unlist(data_compl[,c("L301_01", "L302_01", "L303_01", "L304_01", "L305_01")]), na.rm=T)









####################################################################
#####################################
#Weiteres
####################################
####################################################################





##Fortbildungen

#an welcher Stelle finde ich diese Variablen?
which(names(data_compl) %in% c("K207_01","K207_16"))

#was ist in diesen Variablen drin an Daten?
head(data_compl[,555:570])

#Ränder einstellen der Grafik: 3 (unten), 16 (links), 2(oben), 2 (rechts)
par(mar=c(3,16,12,1))


#Boxplots ausgeben lassen mit 75% Daten (Blöcke) und 95% Daten (Whiskers)
boxplot(data_compl[,555:570],
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",outline = F)

#Achsen einstellen und beschriften
axis(side=2,
     at=1:16,
     labels = c("Selbstfürsorge/Abgrenzung", 
                "Verhaltenstherapeutische Schulungen",
                "Traumapädagogik",
                "Bindungsthemen",
                "Entwicklungstraumata",
                "Rechtliches Wissen",
                "Umgang mit Betroffenheit",
                "Verhaltensbezogene Subjektlogik",
                "Wissen zu psychischen Störungen",
                "Wissen zu Wirkungen von Stimulanzien",
                "Psychoedukation",
                "Personalentwicklungsgespräche",
                "Konfliktlösung",
                "Deeskalationstechniken",
                "Gruppendynamische Prozesse",
                "Autorität durch Beziehung"),
     las=2)


#Mittelwerte als Punkte angeben lassen
points(apply(data_compl[,555:570], 2, FUN = mean, na.rm=T),
       1:16, pch=20, col="red3")




##Hier das ganze für Begrifflichkeiten
which(names(data_compl) %in% c("C201_01","C201_19"))

head(data_compl[,18:36])


par(mar=c(2,16,2,4))
boxplot(data_compl[,18:36],
        horizontal = T,
        col = "lightsteelblue1",
        yaxt="n",outline = F)

axis(side=2, 
     at=1:19, 
     by = 1,
     labels = c("Herausfordernde junge Menschen",
                "Systemsprenger:in",
                "Die Schwierigsten",
                "Knaller",
                "Brecher",
                "Verhaltenskreative",
                "Problemkinder/-jugendliche",
                "Erziehungsresistente",
                "Verhaltensgestörte",
                "Hilferesistente",
                "Schwierige Kinder/Jugendliche",
                "Jugendhilfe-Aversive",
                "Riskant agierende Kinder/Jugendliche",
                "Heiße Kastanien",
                "Hoch-Risiko-Klientel",
                "Vom System Gesprengte",
                "Grenzgänger:innen",
                "Verweigerer:innen",
                "Systemverlierer:innen"), 
     las=2)



points(apply(data_compl[,18:36], 2, FUN = mean, na.rm=T),
       1:19, pch=20, col="red3")


# hier alle möglichen Farben anzeigen lassen
colors()



for(i in 555:570){
  
  hist(data_compl[,i], main = names(data_compl)[i])
  
}
table(data_compl$C201_05)



# Subset erzeugen nach Einrichtung

data_24 <- data_compl[data_compl$B107 == 24,]


Leitung <- data_24[data_24$B102 == 1,]
Lehr <- data_24[data_24$B102 == 2,]
Hilfe <- data_24[data_24$B102 %in% c(3,4,5),]





#-------------------------------------------------------------
# Bis hier her
#-----------------------------------------------


data_compl[,"C233_01"]


apply(data[-1,], 1, FUN = function(x){sum(is.na(as.numeric(x)))})



sum(is.na(data[3,]))

data[2,]


mean(data[,names(data) == "E402_01"])



interest <- c("J102_")



mean(data[,interest])

# 6 6 4 6 6
# 2 1 2 2 2


names(data)[415:427]


for(i in 1:nrow(data)){
  
  data[i,][data[i,] == -9] <- NA
  
  
}

data[,415:427]


factanal(data[2:5,415:427], factors = 2)






mean(data[,interest])

sum(data[1,] == -9)



head(data_compl)



## Einrichtungsübegreifende Fachkraft

mean(unlist(data_compl[,c("F524_19")]), na.rm=T)
sd(unlist(data_compl[,c("F524_19")]), na.rm=T)
median(unlist(data_compl[,c("F524_19")]), na.rm=T)










