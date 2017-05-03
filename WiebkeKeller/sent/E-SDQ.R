library(helpR)
library(lubridate)
library(ggplot2)

# tbls <-
#   read.all.xlsx.tables( "../../data/" )
# 
# t.n <-
#   all.xlsx.tables.names( tbls )
# 
# t.c.n <- 
#   all.xlsx.tables.row.names( tbls )
# 
# t.c.n
# 
# t.c.n[8]
# 
# t.n[8]
# 
# d00149 <- get.tbl("D00149", all.tables=tbls)

d00149 <- get.table.by.name( "D00149" )

table( d00149$E_SDQ_GES_SCORE )
summary( d00149$E_SDQ_GES_SCORE )

##Wie viele Kinder haben auffälliges Ergebnis (>17), bzw. grenzwertiges
#Ergebnis(14-16)
d00149$E_SDQ_GES_SCORE[ d00149$E_SDQ_GES_SCORE > 17]
d00149$E_SDQ_GES_SCORE[ d00149$E_SDQ_GES_SCORE > 14 & d00149$E_SDQ_GES_SCORE < 16 ]


d00149.14.16 <- d00149$E_SDQ_GES_SCORE[d00149$E_SDQ_GES_SCORE > 14 & 
                                         d00149$E_SDQ_GES_SCORE < 16]

d00149[d00149$E_SDQ_GES_SCORE > 17,]
d00149.17 <- d00149[d00149$E_SDQ_GES_SCORE > 17,]

##Darstellung der Kinder, die auffälliges Ergebnis haben
ggplot(d00149, aes(x=( E_SDQ_GES_SCORE)))+geom_bar()
ggplot(d00149.17, aes(x = E_SDQ_GES_SCORE))+geom_bar()

summary(d00149.17$E_SDQ_GES_SCORE)
table(d00149.17$E_SDQ_GES_SCORE)

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen prosoziales
#Verhalten? - Kann man eigentlich nicht machen, weil prosoziales Verhalten
#nicht in Gesamtscore einfließt!!!
d00149.17$E_SDQ_PRO_SUM
d00149.17[d00149.17$E_SDQ_PRO_SUM < 4,]
summary(d00149.17$E_SDQ_PRO_SUM < 4,all=F)
table(d00149.17$E_SDQ_PRO_SUM)
ggplot(d00149.17, aes(x = E_SDQ_PRO_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen Hyperaktivität?
d00149.17$E_SDQ_HYP_SUM
d00149.17[d00149.17$E_SDQ_HYP_SUM > 7,]
summary(d00149.17$E_SDQ_HYP_SUM > 7, all=F)
ggplot(d00149.17, aes(x = E_SDQ_PRO_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen emotionale
#Probleme?
d00149.17$E_SDQ_EMO_SUM
d00149.17[d00149.17$E_SDQ_EMO_SUM > 5,]
summary(d00149.17$E_SDQ_EMO_SUM > 5, all=F)
ggplot(d00149.17, aes(x= E_SDQ_EMO_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen 
#Verhaltensauffälligkeiten?
d00149.17$E_SDQ_VER_SUM
d00149.17[d00149.17$E_SDQ_VER_SUM > 4,]
summary(d00149.17$E_SDQ_VER_SUM >4, all=F)
ggplot(d00149.17, aes(x=E_SDQ_VER_SUM))+geom_bar()

##Wie viele Kinder mit auffälligem Gesamt-Score zeigen Probleme mit
#Gleichaltrigen?
d00149.17$E_SDQ_SOP_SUM
d00149.17[d00149$E_SDQ_SOP_SUM >4,]
summary(d00149.17$E_SDQ_SOP_SUM >4, all=F)
ggplot(d00149.17, aes(x=E_SDQ_SOP_SUM))+geom_bar()
