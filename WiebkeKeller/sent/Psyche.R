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

t.c.n[2]

t.n[2]

d00127 <- get.tbl("D00127", all.tables=tbls)

table(d00127$C_DISEASE_TX_ADHS)
summary(d00127$C_DISEASE_TX_ADHS)
##Darstellung? Kann man da vielleicht ein Tortendiagramm machen?
# Oder ist es insgesamt eher sinnlos?

table(d00127$C_DISEASE_TX_DEPRES)
summary(d00127$C_DISEASE_TX_DEPRES)

table(d00127$C_DISEASE_TX_PSY)
summary(d00127$C_DISEASE_TX_PSY)
##Noch fraglich, ob ich die mit in die Auswertung nehme.
