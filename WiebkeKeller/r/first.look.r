## Loesche alle Variablen
rm( list = ls( ) )

library( helpR )

read.all.tables( 
    directory = "~/LIFE/github-tpeschel/R/WiebkeKeller/data/",
    pattern = "*.xlsx" )

my$tables
my$colums
my$names

focus <- 
    merge( 
        get.table.by.name( "D00127" ),
        get.table.by.name( "T00213" ),
        by.x = c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_SCI_GROUP" ),
        by.y = c( "FB_ALLERGY_BP1_SIC", "FB_ALLERGY_BP1_GRUPPE" ),
        all = F )


table( focus$C_DISEASE_TX_ASTHMA )
table( focus$FB_ALLERGY_BP1_F0043 )
table( focus$C_DISEASE_TX_ASTHMA == 1 & focus$FB_ALLERGY_BP1_F0043 == 2 )

focus[ focus$C_DISEASE_TX_ASTHMA == 1 & focus$FB_ALLERGY_BP1_F0043 == 2, c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_SCI_GROUP", "C_DISEASE_TX_ASTHMA", "FB_ALLERGY_BP1_F0043" ) ]
