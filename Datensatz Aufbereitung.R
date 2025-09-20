#RT Aufbereitung AIT, Gruppe 3

#Pakete installieren####
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ez)) install.packages('ez')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(schoRsch)) install.packages('schoRsch')
if (!require(apaTables)) install.packages('apaTables')
if (!require(pander)) install.packages('pander')
if (!require(papaja)) install.packages('papaja')
if (!require(patchwork)) install.packages('patchwork')
if (!require(viridis)) install.packages('viridis')
if (!require(psych)) install.packages('psych')


#Pakete laden####
library(ggplot2)
library(dplyr)
library(ez)
library(tidyverse)
library(schoRsch)
library(apaTables)
library(pander)
library(papaja)
library(patchwork)
library(viridis)
library(psych)

#Arbeitsverzeichnis anlegen####
setwd("C:/Users/fritz/Documents/Semster 3/Expra/Expra Statistik")

#Daten laden####
data<-read.delim(file="C:/Users/fritz/Documents/Semster 3/Expra/Expra Statistik/Expra_Gr3_rawdata_n94.txt", header = T, stringsAsFactors = T)
#20873 Objekte

names(data)
#41 Spalten/Variablen (columns) 


#Subdatensätze erstellen####
table(data$Running.Trial.)
#18600 Daten wollen wir untersuchen, die anderen 2273 Daten sind Übungsblöcke
#deshalb erstellen wir Subdatensatz, wo nur relevanten Daten drin sind

RTdata<-subset(data, subset = data$Running.Trial.=="lstMain" )
table(RTdata$Subject) 
#angeblich 182 VPN, aber es gibt Bruch, deshalb neue Funktion unten

length(unique(RTdata$Subject))
#93 VPN, hier guckt R, welche Nummern nur einmal vorkommen

SubjectInfo<-RTdata %>%
  group_by(Subject) %>%
  summarize(
    Alter=first(Alter))
summary(SubjectInfo)
SD(data$Alter)
#maximales Alter 52, minimales Alter 18; eigentlich unwichtig, 
#weil wir nur Alter erfasst haben
#mittleres Alter 23,8 Jahre

#Aufbereitung der RT Daten####
RTdata$RT<-RTdata$stimulus2.RT.Trial.
#neue Variable, weil vorherige zu lang

RTdata$ACC<-RTdata$stimulus2.ACC.Trial. 
#Accuracy Variable

RTdata$ERR<-1-RTdata$stimulus2.ACC.Trial.
#Fehlerdatensatz (Gegenteil zu ACC als Kontrolle)

table(RTdata$ACC) 
#930 Fehler bei 93 VPN

table(RTdata$ERR)
#korrespondiert mit Fehlern hier, also stimmt das
#aber wir wollen ja wissen, wie viel Prozent Fehler gemacht werden

round(table(RTdata$ACC)/nrow(RTdata)*100,3)
#5% Fehler bei 93 VPN

Mean_ERR <- RTdata%>%
  group_by(Subject)%>%
  summarize(
    mean_err=mean(ERR))
#VPN, die extrem viele Fehler machen?

boxplot(Mean_ERR$mean_err)
#daran sehen wir, wer Ausreißer ist
#4 extreme Ausreißer

RTdata<-merge(RTdata, Mean_ERR, by="Subject")
RTdata<-subset(RTdata, subset = mean_err<0.25)
length(unique(RTdata$Subject))
#jetzt nur noch 92 VPN, weil 1 VP mehr als 25% Fehler hatte

round(table(RTdata$ACC)/nrow(RTdata)*100,3)
#4,76% Fehler, gerundet auf 3. Nachkommastelle
#nochmalige Ausgabe, weil 1 VP jetzt rausgenommen wurde
#dadurch verringert sich auch die Fehlerrate

RTdata$RT[RTdata$ACC==0]<-NA
#wir setzen RT auf missing, wenn VP Fehler gemacht hat

summary(RTdata$RT)
#877 NA's
#das ist richtig so, weil wir mehr Trials auf missing gesetzt haben
table(RTdata$ACC)
#Durchgänge, die nach einem Fehler passiert sind, wollen wir auch nicht haben

RTdata$prevErr<-lag(RTdata$ERR,1)
RTdata$RT[RTdata$prevErr==1]<-NA
summary(RTdata$RT)
#1687 NA's
1687/18400
#das wären 9,1%, die wege Fehlern rausfallen
#sind zwar mehr missings, aber dafür haben wir 

computeTukeys <- function(x) {
  P25 <- quantile(x$RT, 0.25, na.rm = TRUE, type = 6) # type = 6 -> nimmt SPSS
  P75 <- quantile(x$RT, 0.75, na.rm = TRUE, type = 6)
  outlier <- P75 + 1.5 * (P75 - P25)
  farouts <- P75 + 3.0 * (P75 - P25)
  x <- x %>%
    mutate(Outlier = outlier,
           Farouts = farouts)
  return(x)
}
#langsame RT sind schlecht weil sie MW verzerren
#jeder Wert der über IQR liegt, ist dann Ausreißer, den man im Boxplot sieht
#wir wollen das aber auf jede VP anwenden, deshalb nächste Funktion

RTdata <- RTdata%>%
  group_by(Subject) %>%
  group_modify(~ computeTukeys(.x)) %>%
  ungroup()

table(RTdata$Outlier)
table(RTdata$RT)
RTdata$RT[RTdata$RT<150 | RTdata$RT>RTdata$Outlier]<-NA
summary(RTdata$RT)
#2174 NA's
2174/18400
#11% aller Trials falle raus wegen Fehlern, ACC oder RT
#11% aller Daten verlieren wir also


#Daten aggregieren####
table(RTdata$congruency.Trial.)
#das ist Plausibilitätscheck, den macht man am Anfang eigentlich
#zeigt, dass 9200 jeweils gleich aufgeteilt wurden
RTdata$congruency<-RTdata$congruency.Trial.
table(RTdata$Blocktype)
table(RTdata$Blocktype, RTdata$congruency)
#das zeigt, dass alles richtig aufgeteilt ist

RT.agg<-aggregate(data=RTdata, RT~congruency + Blocktype + Subject, mean)
ERR.agg<-aggregate(data=RTdata, ERR~congruency + Blocktype + Subject, mean)
#wir machen MW-Variable für RT und Err 

#Daten transformieren für JASP####
RT.agg.wider<-RT.agg%>%
  pivot_wider(
    names_from = c("congruency", "Blocktype"),
    values_from = "RT"
  )%>%
  mutate(Cong_Eff_cong20=incong_lstcong20-cong_lstcong20,
         Cong_Eff_cong80=incong_lstcong80-cong_lstcong80)
#das machen wir für JASP, weil das mit wide Format arbeitet
#R arbeitet mit long Format

ERR.agg.wider<-ERR.agg%>%
  pivot_wider(
    names_from = c("congruency", "Blocktype"),
    values_from = "ERR"
  )%>%
  mutate(Cong_Eff_cong20_ERR=incong_lstcong20-cong_lstcong20,
         Cong_Eff_cong80_ERR=incong_lstcong80-cong_lstcong80)
#das kann man dann direkt in einen t-Test machen bei JASP

#Daten speichern####
write.csv2(RT.agg.wider, file="C:/Users/fritz/Documents/semester 3/Expra/RT_agg_wide.csv", row.names = F)
write.csv2(ERR.agg.wider, file="C:/Users/fritz/Documents/semester 3/Expra/ERR_agg_wide.csv", row.names = F)

install.packages("plotflow")


RT_diff<-ezStats(data=RT.agg,
                 dv=RT,
                 wid=Subject,
                 within_full = .(congruency, Blocktype),
                 within = .(Blocktype), 
                 diff=.(congruency))

RT_diff$SE=RT_diff$SD/sqrt(RT_diff$N)

RT_diff$Blocktype<-factor(RT_diff$Blocktype, levels = c("lstcong20", "lstcong80"), labels = c("80% inkongruent", "20% inkongruent"))



plot2<-ggplot(data=RT_diff, mapping = aes(y=Mean, x=Blocktype))+
  geom_bar(stat = "identity", position = "dodge", color="#404040", fill = c("#C5B2F0","#14b8ad"))+
  coord_cartesian(ylim = c(0, 50))+
  labs(x="Blocktyp", y="RT: Inkong-kong (in ms)")+
    theme(text=element_text(size=16))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=.2, color="#404040")

plot2


ggsave(plot=plot2, filename = "plots/ANOVA_EC.bmp")

ERR_diff<-ezStats(data=ERR.agg,
                 dv=ERR,
                 wid=Subject,
                 within_full = .(congruency, Blocktype),
                 within = .(Blocktype), 
                 diff=.(congruency))

#ERR prozente zu Prozent zahlen

ERR_diff$Mean=ERR_diff$Mean*100
ERR_diff$SD=ERR_diff$SD*100
ERR_diff$SE=ERR_diff$SD/sqrt(ERR_diff$N)
ERR_diff$Blocktype<-factor(ERR_diff$Blocktype, levels = c("lstcong20", "lstcong80"), labels = c("80% inkongruent", "20% inkongruent"))

plot3<-ggplot(data=ERR_diff, mapping = aes(y=Mean, x=Blocktype))+
  geom_bar(stat = "identity", position = "dodge", color="#404040", fill = c("#C5B2F0","#14b8ad"))+
  coord_cartesian(ylim = c(0, 10))+
  labs(x="Blocktyp", y="ERR: Inkong-kong (in %)")+
  theme(text=element_text(size=16))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=.2, color="#404040")

plot3

ggsave(plot=plot3, filename = "plots/ANOVA_ERR.bmp")
