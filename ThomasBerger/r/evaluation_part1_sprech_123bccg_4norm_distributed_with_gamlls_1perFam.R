## current working dir to stack
push( )

#load( "~/LIFE/github-tpeschel/R/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

## change working dir to data/results
setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/" )

## metadata of all "Aufklärungsgespräche" (A2) 
d88 <- get.data( ldb, "D00088" )

## personal data
persdat <- get.persdat( ldb )

## sing and speech voices with aliases
t865 <- get.data.with.aliases( ldb, "T00865" )

## glue persdat to t865
t865 <- add.persdat.age( persdat = persdat, t865 )

## we are interested only in kid's age between 5.5 and 18
t865 <- t865[ 5.5 < t865$age & t865$age < 18, ]

## families
## SozDem
d177 <- get.data( ldb, "D00177", remove.D.name = T )
d177 <- unique( d177[, c( "SIC", "FAM_ID" ) ] )

## merge t865 and d177
t865 <- merge( t865, d177, by = c( "SIC" ), all.x = T )

nrow( t865 )

t865 <- t865[ !( is.na( t865$Stimme.F0_SPRECH_1 ) + is.na( t865$Stimme.F0_SPRECH_2 ) + is.na( t865$Stimme.F0_SPRECH_3 ) + is.na( t865$Stimme.F0_SPRECH_4 ) ), ]

nrow( t865 )

t865.one.per.fam <-  
t865 %>%
    group_by( FAM_ID, sex ) %>%
    summarise( 
        n   = n( ), 
        sel = floor( runif( 1, min = 1, max = n + 1 ) ), 
        sic = SIC[ sel ],
        sgroup = SGROUP[ sel ],
        age = age[ sel ],
        f01 = Stimme.F0_SPRECH_1[ sel ], 
        f02 = Stimme.F0_SPRECH_2[ sel ],
        f03 = Stimme.F0_SPRECH_3[ sel ],
        f04 = Stimme.F0_SPRECH_4[ sel ] ) 


table( floor( t865.one.per.fam$age + .5 ), t865.one.per.fam$sex )

## monthly age in years
age <- seq( 6, 18, by = 1 / 12 )

## parameter = 1,2,3 
params <- c( 1 : 3 )

## for mg in (1,2,3)
for( mg in params ) {

    col.name.stimme.f0.sprech <- paste0( "f0", mg )

    data_boys  <- na.omit( t865.one.per.fam[ t865.one.per.fam$sex == "male",   c( col.name.stimme.f0.sprech, "age","sex", "FAM_ID" ) ] )
    data_girls <- na.omit( t865.one.per.fam[ t865.one.per.fam$sex == "female", c( col.name.stimme.f0.sprech, "age","sex", "FAM_ID" ) ] )
      
    names( data_boys ) <- names( data_girls ) <- c( "value","age","sex","FAM_ID" )
    
    res.boys  <- list( )
    res.girls <- list( )
    mod.boys  <- list( )
    mod.girls <- list( )
    
    for( i in 1 : 1500 ) {
 
        ### boys ###################################################################################
        
        print( i )

        ## Gib jeer Familie Gewicht, entsprechend ihrer Groesse
        weights <- 
            group_by( data_boys, FAM_ID ) %>% 
            summarise( 
                n = n( ),
                wgt = n / ( n + 1 ) )  #1-1/(n+1)
    
        ## Suche zufaellig und abhaengig von der Familiengroesse 450 Gewichte aus
        weights <-
            weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
        
        ## Hole die Daten fuer die ausgewaehlte Familien
        tmpdata_boys <-
            data_boys[ data_boys$FAM_ID %in% weights$FAM_ID, ]
        
        
        tmpdata_boys <-
            tmpdata_boys %>%
            group_by( FAM_ID ) %>%
            sample_n( 1 )
        
        print( "fitting boys" )

        tr.obj1 <-
            try(
                mm_boys <-
                    lms(
                        value,
                        age,
                        data      = tmpdata_boys,
                        families  = "BCCG",
                        method.pb = "ML",
                        k         = 2,
                        trace     = F,
                        sigma.df  = 2,
                        mu.df     = 4,
                        nu.df     = 0 ) )

        if( mm_boys$family != "NO" & !( "try-error" %in% class( tr.obj1 ) ) ) {
            
            lms.boys <-
                as.data.frame(
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
        group_by( data_girls, FAM_ID ) %>%
        summarise(
            n = n( ),
            wgt = n / ( n + 1 ) )
        
        weights <-
        weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
        
        tmpdata_girls <-
        data_girls[ data_girls$FAM_ID %in% weights$FAM_ID, ]
        
        tmpdata_girls <-
        tmpdata_girls %>%
        group_by( FAM_ID ) %>%
        sample_n( 1 )
        
        print( "fitting girls" )
        
        tr.obj2 <-
            try(
                mm_girls <-
                lms(
                    value, 
                    age, 
                    data      = tmpdata_girls,
                    families  = "BCCG",
                    method.pb = "ML",
                    k         = 2,
                    trace     = F,
                    sigma.df  = 2,
                    mu.df     = 1, 
                    nu.df     = 0
                )
            )
        
        if( mm_girls$family != "NO" & !( "try-error" %in% class( tr.obj2 ) ) ) {
            
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
}

mg <- 4

col.name.stimme.f0.sprech <- paste0( "f0", mg )

data_boys  <- na.omit( t865.one.per.fam[ t865.one.per.fam$sex == "male",   c( col.name.stimme.f0.sprech, "age", "sex", "FAM_ID" ) ] )
data_girls <- na.omit( t865.one.per.fam[ t865.one.per.fam$sex == "female", c( col.name.stimme.f0.sprech, "age", "sex", "FAM_ID" ) ] )
  
names( data_boys ) <- names( data_girls ) <- c( "value","age","sex","FAM_ID" )

res.boys <- list( )
res.girls <- list( )
mod.boys <- list( )
mod.girls <- list( )

for( i in 1 : 1500 ) {

    ### boys ###################################################################################
    print( i )
    
    weights <- 
    group_by( data_boys, FAM_ID ) %>% 
    summarise(
        n = n( ),
        wgt = n / ( n + 1 ) )  #1-1/(n+1)
    
    weights <-
    weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
    
    tmpdata_boys <-
    data_boys[ data_boys$FAM_ID %in% weights$FAM_ID, ]
    
    tmpdata_boys <-
    tmpdata_boys %>%
        group_by( FAM_ID ) %>%
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
    group_by( data_girls, FAM_ID ) %>%
        summarise(
            n = n( ),
            wgt = n / ( n + 1 ) )
    
    weights <- 
    weights[ sample( 1 : nrow( weights ), size = 450, prob = weights$wgt ), ]
    
    tmpdata_girls <-
    data_girls[ data_girls$FAM_ID %in% weights$FAM_ID, ]
    
    tmpdata_girls <-
    tmpdata_girls %>%
        group_by( FAM_ID ) %>%
        sample_n( 1 )
    
    print( "fitting girls" )
    
    tr.obj2 <- try(
         mm_girls <- gamlss(
             value ~ pb( age, 2 ),
             data          = tmpdata_girls,
             sigma.formula = ~poly( age, 2 ),
             family        = "NO",
             method.pb     = "ML",
             k             = 2,
             trace         = F
            )
        )

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
