source("C:/Heroes/Funktionen.R")
load("C:/Heroes/codebook.RData")


imp_names = list()

imp_names[["B102"]] = c("Leitung","SBBZ", "HZE.station", "HZE.teilst", "HZE.ambul")
imp_names[["B102_edit"]] = c("Leitung","SBBZ", "HZE.gesamt", "HZE.station", "HZE.am.teil")
imp_names[["B103"]] = c("weiblich","männlich","divers")
imp_names[["B105"]] = c("< 3", "4 - 10", "> 11")
imp_names[["B110"]] = c("< 30", "31 - 50", "> 51")
imp_names[["B107"]] = codebook[codebook$Variable == "B107",]$`Response Label`

imp_names[["B107"]][c(10, 14, 21, 28, 37)] = c("ev Jugendhilfe Heilbronn", "Johannes_Falk_Haus Stuttgart", "Kinder und Jugendhilfe Karlshöhe Ludwigsburg",
                                       "Oberlin Jugendhilfe Reutlingen", "Stiftung Tragwerk KirchheimTeck")


imp_names[["Begriffe"]] = c("Herausfordernde junge Menschen", "Systemsprenger:innen", "Die Schwierigsten",
                            "Knaller", "Brecher", "Verhaltenskreative", "Problemkinder/-jugendliche", "Erziehungsresistente",
                            "Verhaltensgestörte", "Hilferesistente", "Schwierige Kinder/Jugendliche", "Jugendhilfe-Aversive",
                            "Riskant agierende Kinder/Jugendliche", "Heiße Kastanien", "Hoch-Risiko-Klientel", "Vom System Gesprengte",
                            "Grenzgänger:innen", "Verweigerer:innen", "Systemverlierer:innen")

imp_names[["AlterGruppen"]] = c("< 6-Jährige", "6 bis 10-Jährige", "11 bis 14-Jährige","15 bis 17-Jährige", "18 bis 21-Jährige", "> 21-Jährige")

imp_names[["Verhaltensweisen"]] = c("Eingeschränkte Gruppenfähigkeit", "Viele kurzfristige Aufenthalte (Einrichtungen,\n KJP, Eltern, Straße, Pflegeeltern)",
                                    "Diagnostizierte psychische Störung", "Psychische Auffälligkeiten","Selbstgefährdung (z.B: Autoaggression,\n Suizidalität, selbstverletzendes Verhalten)",
                                    "Vandalismus, Gewalt gegen Gegenstände", "Fremdgefährdung - sexuelle Gewalt \n (auch Übergriffigkeit)", "Fremdgefährdung - psychische Gewalt",
                                    "Fremdgefährdung - körperliche Gewalt", "Gewalt gerichtet gegen - Fremde", "Gewalt gerichtet gegen - Familienmitglieder",
                                    "Gewalt gerichtet gegen - Gruppenmitglieder", "Gewalt gerichtet gegen - Mitarbeiter:innen")


imp_names[["Informationen"]] = c("Biografie des Falls (Jugendamt)", "Bedarfseinschätzung (Jugendamt)", "Diagnostik der KJP",
                                 "Soziale Kontakte im Herkunftssozialraum", "Aufenthalte in anderen HzE Einrichtungen", "Schulbesuche und -wechsel",
                                 "KJP-Aufenthalte", "Straftaten", "Über Eltern/Familie", "Durch persönliches Kennenlernen")

imp_names[["Informationsart"]] = c("persönlich", "telefonisch", "schriftlich")

imp_names[["Dokumente"]] = c("Schriftliche Biografie (Fließtext)", "Genogramm", "Netzwerkkarte", "Lebensverlaufs-Linie (Grafik/Übersicht)",
                             "Bogen zur Gefährdungseinschätzung", "Gutachten", "Individuelle Lern- und \nEntwicklungsbegleitung (ILEB)", "U-Untersuchungshefte (U1 - U9)",
                             "Zeugnisse", "Schulische Diagnostik", "Ressourcen-Karte")

imp_names[["Informationsbedarf"]] = c("Biografie des Falls", "Diagnostik der KJP", "Soziale Kontakte im Herkunftssozialraum", "Aufenthalte in anderen HzE Einrichtungen",
                                      "Schulbesuche und -wechsel", "Schulische Diagnostik", "KJP-Aufenthalte", "Straftaten", "Informationen über Eltern/Familie",
                                      "Informationen durch \npersönliches Kennenlernen des j.M.",
                                      "Sonstiges")

imp_names[["Vetor.Auf"]] = c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                             "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")


imp_names[["Vetoem.Auf"]] = c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                              "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")

imp_names[["Entscheidung.Auf"]] = c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                                   "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")

imp_names[["Vetor.Ent"]] =  c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                              "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")

imp_names[["Vetoem.Ent"]] =  c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                               "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")

imp_names[["Entscheidung.Ent"]] =  c("Einrichtungsleitung", "Bereichsleitung stationär", "Bereichsleitung ambulant", "Fachdienst", "Lehrkraft",
                                     "WG-Team", "Kinder/Jugendliche in der WG", "Schulleitung SBBZ ESENT", "Sonstiges")

imp_names[["Interdisz"]] = c("Sozialpädagog:in","Erzieher:in", "Psycholog:in", "Naturpädagog:in", "Psychotherapeut:in", "Psychiater:in (Arzt/Ärztin)",
                             "Kinderarzt/ärztin", "SBBZ ESENT Lehrkraft", "Tierpädagog:in", "Schulbegleiter:in", "Schulsozialarbeiter:in", "Mobile Jugendarbeiter:in",
                             "Anti-Aggressions-Trainer:in", "Erlebnispädagog:in", "Fachdienst", "Bereichsleitung", "Schulleitung", "Jurist:in", "Polizei")

imp_names[["Kompetenzen"]] = c("Reflexionsfähigkeit (eigene Anteile der Beziehungsdynamik)", "Gelassenheit", "Aushalten-Können", "Durchsetzungsfähigkeit",
                               "Empathie", "Teamfähigkeit", "Fähigkeit zu eigenständigen Entscheidungen", "Fachliches Wissen", "Regelwissen", "Berufserfahrung", "Intuition",
                               "Kreativität", "Humor", "Positive Beziehungsgestaltung", "Kompetenz zu Reframing (Umdeutung eines Problems)",
                               "Kompetenz beim Umgang mit Widerständen", "Überzeugung schwierige Situationen bewältigen zu können", "Bereitschaft in Konflikte zu gehen",
                               "Flexibilität", "Selbstwirksamkeit", "Pragmatismus", "Kompetenz zur Selbststeuerung", "Fähigkeit zur Abgrenzung", "Authentizität", "Offenheit Neues zu lernen",
                               "Kompetenz zu paradoxem Verhalten", "Ausstrahlung von Sicherheit", "Menschlichkeit", "Bereitschaft zur Transparenz (ggü. j.M.)", "Klare Kommunikation (in non- & verbaler Form)",
                               "Fähigkeit zur Selbstfürsorge", "Freude an der Arbeit", "Konsequentes Handeln", "Lockerheit", "Handlungssicherheit", "Fehlerakzeptanz/-freundlichkeit (ggü. eigenem Scheitern)",
                               "Akzeptanz", "Ressourcenorientierung", "Lösungsorientierung", "Angemessener Umgang Nähe und Distanz zu j.M.", "Eigenmotivation", "Lernbereitschaft")

imp_names[["Fortbildung"]] = c("Selbstfürsorge/Abgrenzung","Verhaltenstherapeutische Schulungen \n(für Gespräche auf Augenhöhe)", "Traumapädagogik", "Bindungsthemen","Entwicklungstraumata",
                               "Rechtliches Wissen", "Umgang mit Betroffenheit", "Verhaltensbezogene Subjektlogik \n(z.B. Annahme des guten Grundes)", "Wissen zu psychischen Störungen",
                               "Wissen zu Wirkungen von Stimulanzien", "Psychoedukation", "Personalentwicklungsgespräche", "Konfliktlösung", "Deeskalationstechniken", "Gruppendynamische Prozesse",
                               "Autorität durch Beziehung")

keys = list()

# nur Items
keys[["Begriffe"]] = create_simple_key("C201_", 1:19)
keys[["Verhaltensweisen"]] = create_simple_key("C205_", 1:13)
keys[["AlterGruppen"]] = create_simple_key("C207_", 1:6)
keys[["Informationen"]] = create_simple_key("D302_", 1:10)
keys[["Informationsart"]] = create_simple_key("D303_",1:3)
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
skalen[["Unterst.5"]] = c("K221_12", create_simple_key("K207_",1:16))
skalen[["Paed.Halt.1"]] = c("J117_12","J118_13","J116_11","J118_13",
                            "J115_10","J109_04")
skalen[["Paed.Halt.2"]] = c("J102_01","J107_02","J108_03", "J111_06","J110_05",
                            "J112_07","J113_08","J114_09")
# skalen[["Paed.Part.1"]] = c("Platzhalter für Vetorechte")
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


skalen_ohne = skalen[-51]



skalen2 = list()

# nur skalen2
skalen2[["Beziehung"]] = c(create_simple_key("D312_", 2:3),
                           create_complex_key("E4",14:18,7:11),
                           "F508_03", "I902_01","I911_04",
                           create_complex_key("H8",11:12,8:9), 
                           "F524_19", "I912_05","I915_08")
skalen2[["Transparenz.jM"]] = c("E402_01","G607_04")
skalen2[["Beteiligung.jM"]] = c("E409_02","E410_03","F502_01","G602_01","G605_02",
                                "G606_03", "I909_02","H810_07")
skalen2[["Transparenz.WG"]] = c("G615_12")
skalen2[["Beteiligung.WG"]] = c("E412_05")
skalen2[["Fallkomm.JA"]] = c("F508_03","F544_02", "F550_08", "F546_04", "I914_07", "G618_15",
                             "G619_16", "F519_14", "G613_10", "F517_12", "G621_18", 
                             "F504_01", "F545_03")
skalen2[["Fallkomm.KJP"]] = c("F551_09", "F552_10", "F553_11", "G623_20", "F554_12", 
                              "F555_13","F556_14","H816_13")
skalen2[["Fallkomm.SA"]] = c("F547_05", "F513_08")
skalen2[["Fallkomm.Elt"]] = c("D312_07", "F548_06","G624_21","F557_15")
skalen2[["Komm.andere"]] = c("I916_09","I913_06", "F549_07","G625_22")
skalen2[["Fallkomm.Int"]] = c("D312_01","D312_06","D312_09","D312_10","E411_04","D312_04",
                              "D312_05","D312_08","F517_12","F523_18", "F520_15","F522_17","F521_16","F507_02","G608_05",
                              "H818_15","G613_10", "G609_06","G616_13","F513_08")
skalen2[["Krisenver"]] = c("H703_01","H707_03","H706_02","H708_04", "H715_11", "H716_12","H717_13","H718_14")
skalen2[["Krisenbew"]] = c("H711_07","H712_08","H713_09","H709_05","H714_10")
skalen2[["Krisenauf"]] = c("H802_01","H805_02","H806_03","H807_04","H810_07", "H811_08","H812_09","H813_10","H817_14",
                           "H818_15","I913_06","H815_12","H814_11","H816_13")
skalen2[["Team"]] = c("F509_04","F510_05","F522_17","G617_14","G620_17","G622_19",
                      "F523_18","G621_18", "F511_06","F512_07","F514_09","F515_10","F516_11","F518_13", 
                      "G610_07","G611_08", "H710_06","K222_13","K225_16", "K226_17",
                      "K211_02","K212_03","K213_04", "K214_05","K215_06", 
                      "K216_07","K217_08", "K223_14","K224_15")
skalen2[["Unterst"]] = c("H808_05","H809_06", "E418_11","G612_09", 
                         "K219_10","K220_11","K218_09","I910_03", "K202_01","K211_02", 
                         "K221_12", create_simple_key("K207_",1:16))
skalen2[["Paed.Halt.1"]] = c("J117_12","J118_13","J116_11","J118_13",
                             "J115_10","J109_04")
skalen2[["Paed.Halt.2"]] = c("J102_01","J107_02","J108_03", "J111_06","J110_05",
                             "J112_07","J113_08","J114_09")
skalen2[["Paed.Part"]] = c("G615_12","D312_06","D312_09","D312_10",
                           "E411_04","D312_08","F520_15","G609_06",
                           "G616_13","H817_14", "F504_01","F555_13","F556_14", "H710_06","K222_13","K225_16", "K215_06",
                           "K217_08","H809_06","K220_11","K221_12",
                           "H807_04","H703_01","H706_02","H714_10","H802_01",
                           "H805_02","H806_03","H810_07","H813_10",
                           "H811_08","H818_15")
skalen2[["Haltekraft"]] = c("L301_01","L302_01","L303_01","L304_01","L305_01")


skalen2_ohne = skalen2[-20]


# Umsetzungsvariablen 

cb_single = codebook[!duplicated(codebook$Variable),]

a = strsplit(cb_single$`Variable Label`, " ")

b = unlist(lapply(a, function(x){return(x[2])}))

c = unlist(lapply(a, function(x){return(x[1])}))

umsetzung = data.frame("var.Key"=cb_single$Variable[b=="Umsetzung"], "var.Umgesetzt"=c[b=="Umsetzung"])
umsetzung = umsetzung[complete.cases(umsetzung),]


# erklärende Variablen

pred_var = c("B102", "B103","B105","B107","B110")
