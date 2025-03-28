#Nun zum tatsächlichem Rechnen und Formatieren der zuvor generierten Daten:
install.packages("rio")
install.packages("apaTables")
install.packages("psych")
install.packages("rstatix")
install.packages("dplyr")
library(psych)
library(rio)
library(apaTables)
library(rstatix)
library(dplyr)
Daten <-import("C:/Users/fritz/Documents/studien literatur foirschungsmethodik/Datensatzsimuliert.xlsx")

R.version.string
citation()

#Zum testen der Hypothese 1
# Betrachten der 3 Gruppenangaben und deren d2 mittelwerte vergleichen



#Grundlegende daten zu den Versuchspersonen
mean(Daten$Alter)
SD(Daten$Alter)
table(Geschlecht)
table(Musikalischeausbildung)
table(Bildungsgrad)


# Modifizieren der daten tabelle fpr eine 3 Stufige annova mit messwiederhohlung
Versuchspersonan <- c(Daten$VPN,Daten$VPN)
Alteran <- c(Daten$Alter, Daten$Alter)
Geschlechtan <- c(Daten$Geschlecht, Daten$Geschlecht)
Bildungsgradan <- c(Daten$Bildungsgrad, Daten$Bildungsgrad)
Aufgabenschwerean <- c(Daten$Aufgabenschwere, Daten$Aufgabenschwere)
Aufgabenschwere_numan <-c(Daten$Aufgabenschwere_num, Daten$Aufgabenschwere_num)
D5_resultan <-c(Daten$D5_low,Daten$D5_int)
D6_resultan <-c(Daten$D6_low,Daten$D6_int)
Intmusican <- c(Daten$Musikwahl_low, Daten$Musikwahl_int)
Durchgang_num <- c(rep(1,300),rep(2,300))
Durchgang <-c(rep("Low",300),rep("int",300))
Datenanno <- data.frame(Versuchspersonan,Alteran, Geschlechtan,Bildungsgradan,Aufgabenschwerean,Aufgabenschwere_numan,D5_resultan,D6_resultan, Intmusican,Durchgang, Durchgang_num)
#testung nach außreißern
#gibt leider kein Schönes packet :c
#zunächst teilung in die Einzelne Bedingungen
 IQR_wert<-IQR(Datenanno$D6_resultan)
q1 <- quantile(Datenanno$D6_resultan, 0.25)
q3 <- quantile(Datenanno$D6_resultan,0.75)
Wisker_down<- q1 - 1.5 * IQR_wert
Wiskers_up<-q3 + 1.5 * IQR_wert
Ausreisser <- Datenanno$D6_resultan[Datenanno$D6_resultan <Wisker_down | Datenanno$D6_resultan > Wiskers_up]
Ausreisser
# 3 Außreißer
# zwar nicht elegant aber egal
Daten <- Daten[!Daten$D6_low == 4 & !Daten$D6_int ==4,]
Versuchspersonan <- c(Daten$VPN,Daten$VPN)
Alteran <- c(Daten$Alter, Daten$Alter)
Geschlechtan <- c(Daten$Geschlecht, Daten$Geschlecht)
Bildungsgradan <- c(Daten$Bildungsgrad, Daten$Bildungsgrad)
Aufgabenschwerean <- c(Daten$Aufgabenschwere, Daten$Aufgabenschwere)
Aufgabenschwere_numan <-c(Daten$Aufgabenschwere_num, Daten$Aufgabenschwere_num)
D5_resultan <-c(Daten$D5_low,Daten$D5_int)
D6_resultan <-c(Daten$D6_low,Daten$D6_int)
Intmusican <- c(Daten$Musikwahl_low, Daten$Musikwahl_int)
#vor entfehrnung der 
Durchgang_num <- c(rep(1,297),rep(2,297))
Durchgang <-c(rep("Low",297),rep("int",297))
Datenanno <- data.frame(Versuchspersonan,Alteran, Geschlechtan,Bildungsgradan,Aufgabenschwerean,Aufgabenschwere_numan,D5_resultan,D6_resultan, Intmusican,Durchgang, Durchgang_num)

#Deskriptive Statistik
describeBy(Datenanno$D6_resultan, list(Datenanno$Aufgabenschwerean, Datenanno$Durchgang))
 DescriptivAn1<-apa.2way.table(iv1 = Durchgang, iv2= Aufgabenschwerean, dv= D6_resultan, data = Datenanno,filename = "C:/Users/fritz/Documents/semester 2/studien literatur foirschungsmethodik/Deskripitvtabelle.doc" )
describe(Datenanno$D6_resultan)

result_AnnoD6 <- aov(Datenanno$D6_resultan ~ Datenanno$Aufgabenschwerean   * Datenanno$Durchgang+ Datenanno$Alteran + Datenanno$Bildungsgradan + Datenanno$Geschlechtan)
summary(result_AnnoD6)
#Resultat: Signifikante unterschiede Bei aufgabenschweren (durch design gewollt), Zwischen den Durchläufen (Wichtig für unsere Hypothese) Und Interaktion zwischen Aufgabenschwere und Musik arousal(Auch wichtig für Hypothese)

anova_test(Datenanno, dv = D6_resultan, wid = Versuchspersonan, between = Aufgabenschwerean, within = Durchgang, effect.size = "pes")
result_Aovsig <-anova_test(Datenanno, dv = D6_resultan, wid = Versuchspersonan, between = Aufgabenschwerean, within = Durchgang, effect.size = "pes")
hoc <- aov(Datenanno$D6_resultan ~ Datenanno$Aufgabenschwerean   * Datenanno$Durchgang)
summary(result_Aovsig)

install.packages("emmeans")
library(emmeans)

post_hoc_results <- emmeans(hoc, ~ Aufgabenschwerean * Durchgang)
summary(post_hoc_results)
pairwise_comparisons <- pairs(post_hoc_results)
summary(pairwise_comparisons)




