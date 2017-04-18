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

t.c.n[12]

t.n[12]

t00159 <- get.tbl("T00159", all.tables=tbls)

##Wie viele Kinder nehmen an freiwilliger Sport-AG teil?
table(t00159$FB_FV_CH_SCHUL_F)
summary(t00159$FB_FV_CH_SCHUL_F)
#auch wieder schwierige Darstellung 

##Wie viele Kinder machen Sport im Verein?
table(t00159$FB_FV_CH_F0010_A)
summary(t00159$FB_FV_CH_F0010_A)
ggplot(t00159, aes(x=FB_FV_CH_F0010_A))+geom_bar()

##Wie viele Kinder machen mindestens 1x pro Woche Sport im Verein?
t00159[t00159$FB_FV_CH_F0010_A > 1,]
t00159.1 <- t00159[t00159$FB_FV_CH_F0010_A > 1,]
summary(t00159.1$FB_FV_CH_F0010_A)
table(t00159.1$FB_FV_CH_F0010_A)

##Wie viele Kinder machen Sport außerhalb von Verein?
table(t00159$FB_FV_CH_F0010_B)
summary(t00159$FB_FV_CH_F0010_B)
ggplot(t00159, aes(x=FB_FV_CH_F0010_B))+geom_bar()

##Wie viele Kinder machen mindestens 1x pro Woche Sport außerhalb 
#vom Verein?
t00159[t00159$FB_FV_CH_F0010_B > 1,]
t00159.1.1 <- t00159[t00159$FB_FV_CH_F0010_B > 1,]
summary(t00159.1.1$FB_FV_CH_F0010_B)
table(t00159.1.1$FB_FV_CH_F0010_B)

##Wie viele Kinder spielen im Freien?
table(t00159$FB_FV_CH_F0009)
summary(t00159$FB_FV_CH_F0009)
ggplot(t00159, aes(x=FB_FV_CH_F0009))+geom_bar()
##Wie viele Kinder spielen mindestens 1x pro Woche im Freien?
t00159[t00159$FB_FV_CH_F0009 > 1,]
t00159.1.1.1 <- t00159[t00159$FB_FV_CH_F0009 > 1,]
summary(t00159.1.1.1$FB_FV_CH_F0009)
table(t00159.1.1.1$FB_FV_CH_F0009)
# auch noch unklar, ob ich das mit in die Auswertung nehme