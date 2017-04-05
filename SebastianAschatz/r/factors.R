n <- 10

df <- 
    data.frame( 
        Vorsorgeheft = factor( runif( n ) < .5, levels = c( FALSE, TRUE ), labels = c( "Nein", "Ja" ) ),  
        Impfnachweis = factor( runif( n ) < .5, levels = c( FALSE, TRUE ), labels = c( "Nein", "Ja" ) ) )

df

df$Gesundheitsverhalten <- factor( 
    as.numeric( df$Vorsorgeheft ) + as.numeric( df$Impfnachweis ),
    levels = c( 2, 3, 4 ),
    labels = c( "auffaellig", "grenzwertig", "unauffaellig" ) )

df
