library(helpR)
library(lubridate)
library(ggplot2)

tbls <-
  read.all.xlsx.tables( "../../data/" )

t.n <-
  all.xlsx.tables.names( tbls )

t.c.n <- 
  all.xlsx.tables.row.names( tbls )

t.c.n

t.c.n[13]

t.n[13]

t00179 <- get.tbl("T00179", all.tables=tbls)

##Wie viele Kinder machen Sport im Verein?
table(t00179$FB_FV_BP1_SP_VEREIN_K)
summary(t00179$FB_FV_BP1_SP_VEREIN_K)
ggplot(t00179, aes(x=FB_FV_BP1_SP_VEREIN_K))+geom_bar()

##Wie viele Kinder machen mindestens 1x pro Woche Sport im Verein?
t00179[t00179$FB_FV_BP1_SP_VEREIN_K > 1,]
t00179.1 <- t00179[t00179$FB_FV_BP1_SP_VEREIN_K > 1,]
summary(t00179.1$FB_FV_BP1_SP_VEREIN_K)
table(t00179.1$FB_FV_BP1_SP_VEREIN_K)

##Wie viele Kinder machen Sport außerhalb von Verein?
table(t00179$FB_FV_BP1_SPORT_FREIZ_K)
summary(t00179$FB_FV_BP1_SPORT_FREIZ_K)
ggplot(t00179, aes(x=FB_FV_BP1_SPORT_FREIZ_K))+geom_bar()

##Wie viele Kinder machen mindestens 1x pro Woche Sport außerhalb 
#vom Verein?
t00179[t00179$FB_FV_BP1_SPORT_FREIZ_K > 1,]
t00179.1.1 <- t00179[t00179$FB_FV_BP1_SPORT_FREIZ_K > 1,]
summary(t00179.1.1$FB_FV_BP1_SPORT_FREIZ_K)
table(t00179.1.1$FB_FV_BP1_SPORT_FREIZ_K)

##Wie viele Kinder spielen im Freien?
table(t00179$FB_FV_BP1_SPIEL_FREI_K)
summary(t00179$FB_FV_BP1_SPIEL_FREI_K)
ggplot(t00179, aes(x=FB_FV_BP1_SPIEL_FREI_K))+geom_bar()
##Wie viele Kinder spielen mindestens 1x pro Woche im Freien?
t00179[t00179$FB_FV_BP1_SPIEL_FREI_K > 1,]
t00179.1.1.1 <- t00179[t00179$FB_FV_BP1_SPIEL_FREI_K > 1,]
summary(t00179.1.1.1$FB_FV_BP1_SPIEL_FREI_K)
table(t00179.1.1.1$FB_FV_BP1_SPIEL_FREI_K)
# auch noch unklar, ob ich das mit in die Auswertung nehme