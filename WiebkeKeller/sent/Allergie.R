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
# t.c.n[2]
# 
# t.n[2]

read.all.tables( 
    directory = "~/LIFE/github-tpeschel/R/WiebkeKeller/data/",
    pattern = "*.xlsx" )

my$tables
my$colums
my$names




d00127 <- get.table.by.name( "D00127" )

table( d00127$C_DISEASE_TX_ASTHMA )

summary( d00127$C_DISEASE_TX_ASTHMA )

table( d00127$C_DISEASE_TX_ALLERG )

summary( d00127$C_DISEASE_TX_ALLERG )
##Auch noch fraglich, ob ich die mit in die Auswertung nehme.

table( d00127$C_DISEASE_TX_NEURODER )

summary( d00127$C_DISEASE_TX_NEURODER )

