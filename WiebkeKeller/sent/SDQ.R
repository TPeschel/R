library(helpR)
library(lubridate)
library(ggplot2)

tbls <-
  read.all.xlsx.tables( "../../data/" )

t.n <-
  all.xlsx.tables.names( tbls )

t.c.n <- 
  all.xlsx.tables.row.names( tbls )


d00148 <- get.tbl("D00148", all.tables=tbls)

table(d00148$SDQ_GES_SCORE)
summary(d00148$SDQ_GES_SCORE)

##Wie viele Kinder haben auffälligen Gesamt-Score?
d00148[d00148$SDQ_GES_SCORE > 20,]
d00148.20 <- d00148[d00148$SDQ_GES_SCORE > 20,]
ggplot(d00148, aes(x = SDQ_GES_SCORE))+geom_bar()
ggplot(d00148.20, aes(x = SDQ_GES_SCORE))+geom_bar()

table(d00148.20$SDQ_GES_SCORE)
summary(d00148.20$SDQ_GES_SCORE)

##Wie viele der Kinder mit auffälligen Gesamt-Score zeigen Hyperaktivität?
d00148.20$SDQ_HYP_SUM
d00148.20[d00148.20$SDQ_HYP_SUM > 7,]
summary(d00148.20$SDQ_HYP_SUM >7, all=F)
ggplot(d00148.20, aes(x=SDQ_HYP_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligen Gesamt-Score zeigen
#emotionale Probleme?
d00148.20$SDQ_EMO_SUM
d00148.20[d00148.20$SDQ_EMO_SUM > 7,]
summary(d00148.20$SDQ_EMO_SUM >7, all=F)
ggplot(d00148.20, aes(x=SDQ_EMO_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligen Gesamt-Score zeigen Verhaltens-
#auffälligkeiten?
d00148.20$SDQ_VER_SUM
d00148.20[d00148.20$SDQ_VER_SUM > 5,]
summary(d00148.20$SDQ_VER_SUM >5, all=F)
ggplot(d00148.20, aes(x=SDQ_VER_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen Probleme
#mit Gleichaltrigen?
d00148.20$SDQ_SOP_SUM
d00148.20[d00148.20$SDQ_SOP_SUM >6,]
summary(d00148.20$SDQ_SOP_SUM >6, all=F)
ggplot(d00148.20, aes(x=SDQ_VER_SUM))+geom_bar()

