setwd( "~/LIFE/myPostGraduates/ThomasBerger/r/TP/" )

#load( "../../data/voice_clean.Rda")

require( xlsx )

require( gamlss )

require( dplyr )

require( lubridate )

require( readxl )

require( lifecuration )

source( "~/connection/connection.r" )

d88 <- get.data( ldb, "D00088" )

## Aequivalent zu Aufklaerungsgespraeche
d171 <- get.data( ldb, "D00171", remove.D.name = T )

## Personaldaten
persdat <- get.persdat( ldb )

##
t865 <- get.data.with.aliases( ldb, "T00865" )

t865 <- add.persdat.age( persdat = persdat, t865 )

t865 <- t865[ 5.5 < t865$age & t865$age < 18, ]

## Familien
d192 <- get.data( ldb, "D00192", remove.D.name = T )
d192 <- unique( d192[, c( "SIC", "FAMILY_ID" ) ] )

u.sics <- unique( t865$SIC[ !t865$SIC %in% d192$SIC ] )
d192.b <- data.frame( SIC = u.sics, FAMILY_ID = 1 : length( u.sics ) )

d192 <- rbind( d192, d192.b )

t865 <- merge( t865, d192, by = c( "SIC" ), all.x = T )

sp.lvl     <- data.frame( sp.lvl   = seq( 1, 4 ) )
sex        <- data.frame( sex      = c( "male", "female" ) )
family     <- data.frame( family   = c( "BCCG", "BCTO", "NO" ) )
mu.df      <- data.frame( mu.df    = c( 1 : 6 ) )
sig.df     <- data.frame( sig.df   = c( 0 : 2 ) )
nu.df      <- data.frame( nu.df    = c( 0 : 2 ) )
mu.link    <- data.frame( mu.link  = c( "identity", "log" ) )
#sig.link   <- data.frame( sig.link = c( "identity", "log" ) )
#nu.link    <- data.frame( nu.link  = c( "identity", "log" ) )
# tau.df     <- data.frame( tau.df  = c( 0 : 2 ) )

lms.params <- merge( merge( merge( merge( merge( merge( sp.lvl, sex ), family ), mu.df ), sig.df ), nu.df ), mu.link )
lms.params$succ <- 0

for( p in c( 1 : nrow( lms.params ) ) ) {

    print( p )
    print( lms.params[ p, ] )

    dat  <- na.omit( t865[ t865$sex == lms.params$sex[ p ], c( paste0( "Dat_Sing.SPL_SPRECH_", lms.params$sp.lvl[ p ] ), "age", "sex", "FAMILY_ID" ) ] )

    names( dat ) <- c( "value", "age", "sex", "FAMILY_ID" )
    
    res <- list( )

    for( i in 1 : 25 ) {
        
        print( paste0( lms.params$succ[ p ], " hits of ", i - 1 ) )
        print( lms.params[ p, ] )
        
        weights <- group_by( dat, FAMILY_ID ) %>%
            summarise(
                n = n( ),
                wgt = n / ( n + 1 )
            )

        weights <- weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]

        tmpdat <- dat[ dat$FAMILY_ID %in% weights$FAMILY_ID, ]

        tmpdat <- tmpdat %>%
            group_by( FAMILY_ID ) %>%
            sample_n( 1 )

        print( "fitting" )

        tr.obj <- try(
            mm <- lms(
                value,
                age,
                data      = tmpdat,
                families  = as.character( lms.params$family[ p ] ),
                method.pb = "ML",
                k         = 2,
                trace     = F,
                mu.df     = lms.params$mu.df[ p ],
                sigma.df  = lms.params$sig.df[ p ],
                nu.df     = lms.params$nu.df[ p ],
                mu.link   = lms.params$mu.link[ p ]#,
 #               sig.link  = lms.params$sig.link[ p ],
 #               nu.link   = lms.params$nu.link[ p ]
            )
        )

        age <- seq( 6, 18, by = 1 / 12 )
        
        if( ( mm$family[ 1 ] == "BCCG" | mm$family[ 1 ] == "BCTO" ) & !( "try-error" %in% class( tr.obj ) ) ) {
            
            lms.params$succ[ p ] <- lms.params$succ[ p ] + 1
        }

        if ( 9 < i & lms.params$succ[ p ] < 5 ) {
            
            lms.params$succ[ p ] <- -1
            
            break;
        }
    }
}

#save( lms.params, file = paste0( "LMS_Params", mg, ".Rda" ) )
write.xlsx( lms.params, file = "LMS_Params_SPL_SPRECH_20170406.xls" )


