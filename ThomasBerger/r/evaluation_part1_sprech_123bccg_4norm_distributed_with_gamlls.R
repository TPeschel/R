## current working dir to stack
push( )

## change working dir to data/results
setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/" )

## metadata of all "Aufklärungsgespräche" (A2) 
d88 <- get.data( ldb, "D00088" )

## equivalent to "Aufklaerungsgespraeche"
d171 <- get.data( ldb, "D00171", remove.D.name = T )

## personal data
persdat <- get.persdat( ldb )

## sing and speech voices with aliases
t865 <- get.data.with.aliases( ldb, "T00865" )

## glue persdat to t865
t865 <- add.persdat.age( persdat = persdat, t865 )

## we are interested only in kid's age between 5.5 and 18
t865 <- t865[ 5.5 < t865$age & t865$age < 18, ]

## families
d192 <- get.data( ldb, "D00192", remove.D.name = T )
d192 <- unique( d192[, c( "SIC","FAMILY_ID" ) ] )

## get all sics of t865 which have no family id 
u.sics <- unique( t865$SIC[ !t865$SIC %in% d192$SIC ] )

## create family ids for remaining sics
d192.b <- data.frame( SIC = u.sics, FAMILY_ID = 1 : length( u.sics ) )

## bind them to the table with ordinary family ids
d192 <- rbind( d192, d192.b )

## merge t865 and d192
t865 <- merge( t865, d192, by = c( "SIC" ), all.x = T )

## age in months
age <- seq( 6, 18, by = 1 / 12 )

## parameter = 1,2,3 
params <- c( 1 : 3 )

## for mg in (1,2,3)
for( mg in params ) {

    col.name.stimme.f0.sprech <- paste0( "Stimme.F0_SPRECH_", mg )

    data_boys  <- na.omit( t865[ t865$sex == "male",   c( col.name.stimme.f0.sprech, "age","sex", "FAMILY_ID" ) ] )
    data_girls <- na.omit( t865[ t865$sex == "female", c( col.name.stimme.f0.sprech, "age","sex", "FAMILY_ID" ) ] )
      
    names( data_boys ) <- names( data_girls ) <- c( "value","age","sex","FAMILY_ID" )
    
    res.boys  <- list( )
    res.girls <- list( )
    mod.boys  <- list( )
    mod.girls <- list( )
    
    for( i in 1 : 1500 ) {
 
        ### boys ###################################################################################
        
        print( i )

        weights <- 
        group_by( data_boys, FAMILY_ID ) %>% 
        summarise( 
            n = n( ),
            wgt = n / ( n + 1 ) )  #1-1/(n+1)
    
        weights <-
        weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
        
        tmpdata_boys <-
        data_boys[ data_boys$FAMILY_ID %in% weights$FAMILY_ID, ]
        
        tmpdata_boys <-
        tmpdata_boys %>%
        group_by( FAMILY_ID ) %>%
        sample_n( 1 )
        
                print( "fitting boys" )

        tr.obj1 <-
        try(
            mm_boys <- 
            lms(
                value,
                age,
                data = tmpdata_boys,
                families = "BCCG",
                method.pb = "ML",
                k = 2,
                trace = F,
                sigma.df = 2,
                mu.df = 4,
                nu.df = 0 ) )

        if( mm_boys$family != "NO" & !( "try-error" %in% class( tr.obj1 ) ) ) {
            
            lms.boys <-
            as.data.frame(
                predictAll(
                    mm_boys,
                    newdata = data.frame( age = age ) ) )
            
            lms.boys$age <- age
            
            res.boys[[ length( res.boys ) + 1 ]] <- lms.boys
            mod.boys[[ length( mod.boys ) + 1 ]] <- mm_boys
        }
        
        ### girls ##################################################################################
        
        weights <-
        group_by( data_girls, FAMILY_ID ) %>%
        summarise(
            n = n( ),
            wgt = n / ( n + 1 ) )
        
        weights <-
        weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
        
        tmpdata_girls <-
        data_girls[ data_girls$FAMILY_ID %in% weights$FAMILY_ID, ]
        
        tmpdata_girls <-
        tmpdata_girls %>%
        group_by( FAMILY_ID ) %>%
        sample_n( 1 )
        
        print( "fitting girls" )
        
        tr.obj2 <-
        try(
            mm_girls <-
            lms(
                value, 
                age, 
                data = tmpdata_girls,
                families = "BCCG",
                method.pb = "ML",
                k = 2,
                trace = F,
                sigma.df = 2,
                mu.df = 1, 
                nu.df = 0 ) )
        
        if( mm_girls$family != "NO" & !( "try-error" %in% class( tr.obj2 ) ) ) {
            
            lms.girls <- as.data.frame(
                predictAll(
                    mm_girls,
                    newdata = data.frame( age = age ) ) )
            
            lms.girls$age <- age
            
            res.girls[[ length( res.girls ) + 1 ]] <- lms.girls
            mod.girls[[ length( mod.girls ) + 1 ]] <- mm_girls
        }
    }
    
    save( res.boys, res.girls, file = paste0( "LMS_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )
    save( mod.boys, mod.girls, file = paste0( "MOD_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )
}

mg <- 4

col.name.stimme.f0.sprech <- paste0( "Stimme.F0_SPRECH_", mg )

data_boys  <- na.omit( t865[ t865$sex == "male",   c( col.name.stimme.f0.sprech, "age","sex", "FAMILY_ID" ) ] )
data_girls <- na.omit( t865[ t865$sex == "female", c( col.name.stimme.f0.sprech, "age","sex", "FAMILY_ID" ) ] )
  
names( data_boys ) <- names( data_girls ) <- c( "value","age","sex","FAMILY_ID" )

res.boys <- list( )
res.girls <- list( )
mod.boys <- list( )
mod.girls <- list( )

for( i in 1 : 1500 ) {

    ### boys ###################################################################################
    print( i )
    
    weights <- 
    group_by( data_boys, FAMILY_ID ) %>% 
    summarise(
        n = n( ),
        wgt = n / ( n + 1 ) )  #1-1/(n+1)
    
    weights <-
    weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
    
    tmpdata_boys <-
    data_boys[ data_boys$FAMILY_ID %in% weights$FAMILY_ID, ]
    
    tmpdata_boys <-
    tmpdata_boys %>%
        group_by( FAMILY_ID ) %>%
        sample_n( 1 )
    
    print( "fitting boys" )

    tr.obj1 <- try(
         mm_boys <- gamlss(
             value ~ pb( age, 2 ),
             data = tmpdata_boys,
             sigma.formula = ~poly( age, 2 ),
             family = "NO",
             method.pb = "ML",
             k = 2,
             trace = F ) )
    
    if( !( "try-error" %in% class( tr.obj1 ) ) ) { ##mm_boys$family != "NO" & 
       
         lms.boys <- as.data.frame(
             predictAll(
                 mm_boys,
                 newdata = data.frame( age = age )
             )
         )
         
         lms.boys$age <- age
         res.boys[[ length( res.boys ) + 1 ]] <- lms.boys
         mod.boys[[ length( mod.boys ) + 1 ]] <- mm_boys
    }

    ### girls ##################################################################################
    weights <-
    group_by( data_girls, FAMILY_ID ) %>%
        summarise(
            n = n( ),
            wgt = n / ( n + 1 ) )
    
    weights <- 
    weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
    
    tmpdata_girls <-
    data_girls[ data_girls$FAMILY_ID %in% weights$FAMILY_ID, ]
    
    tmpdata_girls <-
    tmpdata_girls %>%
        group_by( FAMILY_ID ) %>%
        sample_n( 1 )
    
    print( "fitting girls" )
    
    tr.obj2 <- try(
         mm_girls <- gamlss(
             value ~ pb( age, 2 ),
             data = tmpdata_girls,
             sigma.formula = ~poly( age, 2 ),
             family = "NO",
             method.pb = "ML",
             k = 2,
             trace = F ) )

    if( !( "try-error" %in% class( tr.obj2 ) ) ) { #mm_girls$family != "NO" & 
        
        lms.girls <- as.data.frame(
            predictAll(
                mm_girls,
                newdata = data.frame( age = age ) 
            ) 
        )
        
        lms.girls$age <- age
        res.girls[[ length( res.girls ) + 1 ]] <- lms.girls
        mod.girls[[ length( mod.girls ) + 1 ]] <- mm_girls
    }
}

save( res.boys, res.girls, file = paste0( "LMS_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )
save( mod.boys, mod.girls, file = paste0( "MOD_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )

pop( )
