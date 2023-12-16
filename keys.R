source("C:/Heroes/Funktionen.R")

imp_names = list()

imp_names[["B102"]] = c("Leitung","SBBZ", "HZE.station", "HZE.teilst", "HZE.ambul")
imp_names[["B102_edit"]] = c("Leitung","SBBZ", "HZE.gesamt", "HZE.station", "HZE.am.teil")

keys = list()

# nur Items
keys[["Begriffe"]] = create_simple_key("C201_", 1:19)
keys[["Verhaltensweisen"]] = create_simple_key("C205_", 1:13)
keys[["Informationen"]] = create_simple_key("D302_", 1:10)
keys[["Dokumente"]] = create_simple_key("D304_", 1:11)
keys[["Informationsbedarf"]] = create_simple_key("D306_", 1:11)
keys[["Informationswichtigkeit"]] = create_simple_key("D312_", 1:10)
keys[["Aufnahme"]] = c("E402_01",create_complex_key("E4",9:12, 2:5), create_complex_key("E4",14:18, 7:11))
keys[["Vetor.Auf"]] = create_simple_key("E406_", 1:9)
keys[["Vetoem.Auf"]] = create_simple_key("E407_", 1:9)
keys[["Entscheidung.Auf"]] = create_simple_key("E408_", 1:9)
keys[["Settings.Inn"]] = c("F502_01", create_complex_key("F5", 7:24, 2:19))
keys[["Settings.Ex"]] = c("F504_01", create_complex_key("F5",44:57, 2:15))
keys[["Alltag"]] = c("G602_01", create_complex_key("G6", 5:25, 2:22))
keys[["Krisenbew"]] = c("H703_01", create_complex_key("H7", 6:18, 2:14))
keys[["Krisenauf"]] = c("H802_01", create_complex_key("H8", 5:18, 2:15))
keys[["Entlassung"]] = c("I902_01", create_complex_key("I9", 9:16, 2:9))
keys[["Vetor.Ent"]] = create_simple_key("I904_", 1:9)
keys[["Vetoem.Ent"]] = create_simple_key("I905_", 1:9)
keys[["Entscheidung.Ent"]] = create_simple_key("I906_", 1:9)
keys[["Haltungen"]] = c("J102_01", create_complex_key("J1",7:18, 2:13))
keys[["Teamqual"]] = c("K202_01", create_complex_key("K2", 11:26, 2:17))
keys[["Interdisz"]] = create_simple_key("K203_", 1:19)
keys[["Kompetenzen"]] = create_simple_key("K205_", 1:42)
keys[["Fortbildung"]] = create_simple_key("K207_", 1:16)
keys[["Haltekraft"]] = create_simple_key2("L3", 1:5, "_01")
keys[["SEA"]] = create_simple_key("M401_", 1:2)


skalen = list()

# nur Skalen
skalen[["Beziehung.1"]] = create_simple_key("D312_", 2:3)
skalen[["Beziehung.2"]] = c(create_complex_key("E4",14:18,7:11), "F508_03")
skalen[["Beziehung.3"]] = c("I902_01","I911_04", create_complex_key("H8",11:12,8:9))
skalen[["Beziehung.4"]] = c("F524_19", "I912_05","I915_08")
skalen[["Transparenz.jM"]] = c("E402_01","G607_04")
skalen[["Beteiligung.jM"]] = c("E409_02","E410_03","F502_01","G602_01","G605_02",
                               "G606_03", "I909_02","H810_07")
skalen[["Transparenz.WG"]] = c("G615_12")
skalen[["Beteiligung.WG"]] = c("E412_05")
skalen[["Fallkomm.JA.1"]] = c("F508_03","F544_02", "F550_08", "F546_04", "I914_07", "G618_15",
                              "G619_16", "F519_14", "G613_10", "F517_12", "G621_18")
skalen[["Fallkomm.JA.2"]] = c("F504_01")
skalen[["Fallkomm.JA.3"]] = c("F545_03")
skalen[["Fallkomm.KJP.1"]] = c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12")
skalen[["Fallkomm.KJP.2"]] = c("F555_13","F556_14","H816_13")
skalen[["Fallkomm.SA"]] = c("F547_05", "F513_08")
skalen[["Fallkomm.Elt.1"]] = c("D312_07")
skalen[["Fallkomm.Elt.2"]] = c("F548_06","G624_21")
skalen[["Fallkomm.Elt.3"]] = c("F557_15")
skalen[["Komm.andere.1"]] = c("I916_09","I913_06")
skalen[["Komm.andere.2"]] = c("F549_07","G625_22")
skalen[["Fallkomm.Int.1"]] = c("D312_01","D312_06","D312_09","D312_10","E411_04","D312_04",
                               "D312_05","D312_08","F517_12","F523_18")
skalen[["Fallkomm.Int.2"]] = c("F520_15","F522_17","F521_16","F507_02","G608_05",
                               "H818_15","G613_10")
skalen[["Fallkomm.Int.3"]] = c("G609_06","G616_13","F513_08")
skalen[["Krisenver.1"]] = c("H703_01","H707_03")
skalen[["Krisenver.2"]] = c("H706_02")
skalen[["Krisenver.3"]] = c("H708_04")
skalen[["Krisenver.4"]] = c("H715_11", "H716_12","H717_13","H718_14")
skalen[["Krisenbew.1"]] = c("H711_07","H712_08","H713_09")
skalen[["Krisenbew.2"]] = c("H709_05","H714_10")
skalen[["Krisenauf.1"]] = c("H802_01","H805_02","H806_03","H807_04")
skalen[["Krisenauf.2"]] = c("H810_07")
skalen[["Krisenauf.3"]] = c("H811_08","H812_09")
skalen[["Krisenauf.4"]] = c("H813_10","H817_14","H818_15","I913_06")
skalen[["Krisenauf.5"]] = c("H815_12","H814_11","H816_13")
skalen[["Team.1"]] = c("F509_04","F510_05","F522_17","G617_14","G620_17","G622_19",
                       "F523_18","G621_18")
skalen[["Team.2"]] = c("F511_06","F512_07","F514_09","F515_10","F516_11","F518_13", 
                       "G610_07","G611_08")
skalen[["Team.3"]] = c("H710_06","K222_13","K225_16")
skalen[["Team.4"]] = c("K226_17")
skalen[["Team.5"]] = c("K211_02","K212_03","K213_04", "K214_05","K215_06", 
                       "K216_07","K217_08")
skalen[["Team.6"]] = c("K223_14","K224_15")
skalen[["Unterst.1"]] = c("H808_05","H809_06")
skalen[["Unterst.2"]] = c("E418_11","G612_09")
skalen[["Unterst.3"]] = c("K219_10","K220_11","K218_09","I910_03")
skalen[["Unterst.4"]] = c("K202_01","K211_02")
skalen[["Unterst.5"]] = c("K221_12", create_simple_key("K207_",1:16),"K208_01")
skalen[["Unterst.6"]] = c(create_simple_key("K203_",1:19),"K204_01")
skalen[["Paed.Halt.1"]] = c("J117_12","J118_13","J116_11","J118_13",
                            "J115_10","J109_04")
skalen[["Paed.Halt.2"]] = c("J102_01","J107_02","J108_03", "J111_06","J110_05",
                            "J112_07","J113_08","J114_09")
skalen[["Paed.Komp"]] = c(create_simple_key("K205_",1:42), "K206_01")
skalen[["Paed.Part.1"]] = c("Platzhalter für Vetorechte")
skalen[["Paed.Part.2"]] = c("G615_12","D312_06","D312_09","D312_10",
                            "E411_04","D312_08","F520_15","G609_06",
                            "G616_13","H817_14")
skalen[["Paed.Part.3"]] = c("F504_01","F555_13","F556_14")
skalen[["Paed.Part.4"]] = c("H710_06","K222_13","K225_16", "K215_06",
                            "K217_08","H809_06","K220_11","K221_12",
                            "H807_04")
skalen[["Paed.Part.5"]] = c("H703_01","H706_02","H714_10","H802_01",
                            "H805_02","H806_03","H810_07","H813_10",
                            "H811_08","H818_15")
skalen[["Haltekraft"]] = c("L301_01","L302_01","L303_01","L304_01","L305_01")
skalen[["SEA"]] = c("M401_01","M401_02")

 

