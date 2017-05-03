warning = F

source( "~/connection/connection.r" )
library( directlabels )
library( dplyr )
library( gamlss )
library( ggplot2 )
library( lifecuration )
library( lubridate )
library( readxl )
library( reshape2 )
library( svglite )

setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/" )

load( "LMS_F0_SPRECH_1_20170328.Rda" )
res.boys.1  <- res.boys
res.girls.1 <- res.girls

load( "LMS_F0_SPRECH_2_20170328.Rda" )
res.boys.2  <- res.boys
res.girls.2 <- res.girls

load( "LMS_F0_SPRECH_3_20170328.Rda" )
res.boys.3  <- res.boys
res.girls.3 <- res.girls

load( "LMS_F0_SPRECH_4_20170328.Rda" )
res.boys.4  <- res.boys
res.girls.4 <- res.girls

persdat     <- get.persdat( ldb )
data.sprech <- get.data.with.aliases( ldb, "T00865", withTabAlias = F )
data.sprech <- add.persdat.age( persdat, data.sprech )
data.sprech <- filter( data.sprech, age < 18 )

sds.normal <-
    function(
        value, 
        age,
        sex, 
        item, 
        ref, 
        male   = "male",
        female = "female" ) {
        sapply( 
            1 : length( value ),
            function( i ) {
                mu.col    <- paste( item, sex[ i ], "m", sep = "." )
                sigma.col <- paste( item, sex[ i ], "s", sep = "." )
                if( is.na( value[ i ] ) | is.na( age[ i ] ) | is.na( sex[ i ] ) )
                    return( NA )
                m <- approx( ref$age, ref[ ,mu.col ],    xout = age[ i ], rule = 1 )$y
                s <- approx( ref$age, ref[ ,sigma.col ], xout = age[ i ], rule = 1 )$y
                ( value[ i ] - m ) / s
            }
        )
    }

sds.bccg <-
    function( 
        value, 
        age, 
        sex, 
        item, 
        ref, 
        male   = "male", 
        female = "female" ) {
        sapply(
            1 : length( value ),
            function( i ) {
                mu.col    <- paste( item, sex[ i ], "m", sep = "." )
                sigma.col <- paste( item, sex[ i ], "s", sep = "." )
                lamda.col <- paste( item, sex[ i ], "l", sep = "." )
                if( is.na( value[ i ] ) | is.na( age[ i ] ) | is.na( sex[ i ] ) )
                    return( NA )
                m <- approx( ref$age, ref[ , mu.col ],    xout = age[ i ], rule = 1 )$y
                l <- approx( ref$age, ref[ , lamda.col ], xout = age[ i ], rule = 1 )$y
                s <- approx( ref$age, ref[ , sigma.col ], xout = age[ i ], rule = 1 )$y
                ( ( value[ i ] / m ) ** l - 1 ) / ( l * s )
            }
        )
    }

refs <-
    data.frame(
        age = res.girls[[ 1 ]]$age,
    
        sprech1.male.m   = rowMeans( Reduce( bind_cols, lapply( res.boys.1,  function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech1.male.s   = rowMeans( Reduce( bind_cols, lapply( res.boys.1,  function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech1.male.l   = rowMeans( Reduce( bind_cols, lapply( res.boys.1,  function( x ) data.frame( lamda = x$nu ) ) ) ),
        sprech1.female.m = rowMeans( Reduce( bind_cols, lapply( res.girls.1, function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech1.female.s = rowMeans( Reduce( bind_cols, lapply( res.girls.1, function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech1.female.l = rowMeans( Reduce( bind_cols, lapply( res.girls.1, function( x ) data.frame( lamda = x$nu ) ) ) ),
    
        sprech2.male.m   = rowMeans( Reduce( bind_cols, lapply( res.boys.2,  function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech2.male.s   = rowMeans( Reduce( bind_cols, lapply( res.boys.2,  function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech2.male.l   = rowMeans( Reduce( bind_cols, lapply( res.boys.2,  function( x ) data.frame( lamda = x$nu ) ) ) ),
        sprech2.female.m = rowMeans( Reduce( bind_cols, lapply( res.girls.2, function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech2.female.s = rowMeans( Reduce( bind_cols, lapply( res.girls.2, function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech2.female.l = rowMeans( Reduce( bind_cols, lapply( res.girls.2, function( x ) data.frame( lamda = x$nu ) ) ) ),
    
        sprech3.male.m   = rowMeans( Reduce( bind_cols, lapply( res.boys.3,  function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech3.male.s   = rowMeans( Reduce( bind_cols, lapply( res.boys.3,  function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech3.male.l   = rowMeans( Reduce( bind_cols, lapply( res.boys.3,  function( x ) data.frame( lamda = x$nu ) ) ) ),
        sprech3.female.m = rowMeans( Reduce( bind_cols, lapply( res.girls.3, function( x ) data.frame( mu    = x$mu ) ) ) ),
        sprech3.female.s = rowMeans( Reduce( bind_cols, lapply( res.girls.3, function( x ) data.frame( sigma = x$sigma ) ) ) ),
        sprech3.female.l = rowMeans( Reduce( bind_cols, lapply( res.girls.3, function( x ) data.frame( lamda = x$nu ) ) ) ),
        
        sprech4.male.m   = rowMeans( Reduce( bind_cols, lapply( res.boys.4, function( x ) data.frame( mu     = x$mu ) ) ) ),
        sprech4.male.s   = rowMeans( Reduce( bind_cols, lapply( res.boys.4, function( x ) data.frame( sigma  = x$sigma ) ) ) ),
        sprech4.male.l   = 1,
        sprech4.female.m = rowMeans( Reduce( bind_cols, lapply( res.girls.4, function( x ) data.frame( mu     = x$mu ) ) ) ),
        sprech4.female.s = rowMeans( Reduce( bind_cols, lapply( res.girls.4, function( x ) data.frame( sigma  = x$sigma ) ) ) ),
        sprech4.female.l = 1
    )

data.sprech$sprech1_sds <-
    sds.bccg( 
        value  = data.sprech$F0_SPRECH_1,
        age    = data.sprech$age,
        sex    = data.sprech$sex,
        item   = "sprech1",
        male   = "male",  ## unnoetig weil default
        female = "female", ## unnoetig weil default
        ref    = refs[ ,c( "age", "sprech1.male.m", "sprech1.male.s", "sprech1.male.l", "sprech1.female.m", "sprech1.female.l", "sprech1.female.s" ) ] )

data.sprech$sprech2_sds <-
    sds.bccg( 
        value  = data.sprech$F0_SPRECH_2,
        age    = data.sprech$age,
        sex    = data.sprech$sex,
        item   = "sprech2",
        male   = "male",  ## unnoetig weil default
        female = "female", ## unnoetig weil default
        ref    = refs[ ,c( "age", "sprech2.male.m", "sprech2.male.s", "sprech2.male.l", "sprech2.female.m", "sprech2.female.l", "sprech2.female.s" ) ] )

data.sprech$sprech3_sds <-
    sds.bccg( 
        value  = data.sprech$F0_SPRECH_3,
        age    = data.sprech$age,
        sex    = data.sprech$sex,
        item   = "sprech3",
        male   = "male",  ## unnoetig weil default
        female = "female", ## unnoetig weil default
        ref    = refs[ ,c( "age", "sprech3.male.m", "sprech3.male.s", "sprech3.male.l", "sprech3.female.m", "sprech3.female.l", "sprech3.female.s" ) ] )

data.sprech$sprech4_sds <-
    sds.normal( 
        value  = data.sprech$F0_SPRECH_4,
        age    = data.sprech$age,
        sex    = data.sprech$sex,
        item   = "sprech4",
        male   = "male",  ## unnoetig weil default
        female = "female", ## unnoetig weil default
        ref    = refs[ ,c( "age", "sprech4.male.m", "sprech4.male.s", "sprech4.male.l", "sprech4.female.m", "sprech4.female.l", "sprech4.female.s" ) ] )

data.sprech$year <- year( data.sprech$EDAT )

sozdem <- get.data( ldb, "D00177", remove.D.name = T )

nrow( data.sprech )

data.sprech <- 
    merge(
        data.sprech, 
        sozdem,
        by.x = c( "SIC", "year" ),
        by.y = c( "SIC", "JAHR" ),
        all.x = F,
        all.y = F )

## only rows with SCORE_FAM
data.sprech <- data.sprech[ !is.na( data.sprech$SCORE_FAM ), ]

nrow( data.sprech )

ggplot( data.sprech,
    aes( age, sprech1_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) + 
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "sprech1_sds.png" )

ggplot( data.sprech,
    aes( age, sprech2_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) + 
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "sprech2_sds.png" )

ggplot( data.sprech,
    aes( age, sprech3_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) + 
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "sprech3_sds.png" )

ggplot( data.sprech,
    aes( age, sprech4_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) + 
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "sprech4_sds.png" )

save( refs, file = "refs.Rda" )
save( data.sprech, file = "data.sprech.Rda" )
