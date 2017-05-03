# Loesche Speicher
rm( list =  ls( ) )

# Lade benoetigte Pakete
library( readxl )
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )


daten <- read_excel("~/LIFE/github-tpeschel/R/JulianeWilz/data/old/FINAL_CT.xlsx")
names(daten) <- make.names(names(daten))


daten <- daten[ !is.na( daten$Alter.Jahre ),]
daten$ag <- factor( floor( daten$Alter.Jahre ) )
daten$CT_VALUE[ !is.na( daten$CT_VALUE ) & daten$CT_VALUE > 100 ] <- NA

daten$EDAT <- as.Date( daten$EDAT_anthroref, origin = "1899-12-30", tz = "GMT" )

# erstmal Altersverteilung anschauen
ggplot( daten ) + theme_bw( ) +
    geom_histogram( aes( daten$AGE_REF_D00040, fill = sex ), position = "dodge", bins = 35 )

d.0.3 <- daten[ daten$AGE_D00040 <= 3, ]
# dann Altersverteilung bis 3 Jahre anschauen
ggplot( d.0.3 ) + theme_bw( ) +
    geom_histogram( aes( AGE_MONTH, fill = sex ), position = "dodge", bins = 36 )

# Hier filterst Du nach allen, die hoechstens 1 Jahr alt sind
focus <- daten[ daten$AGE_D00040 <= 1, ]

# Anzahl der verbliebenen Messungen
nrow( focus )

# Sammle alle Spalten, die interessieren
focus <- ( focus[ , c( "AGE_D00040", "sex", "BMI_SDS_D00040", "HEIGHT_SDS_D00040", "WEIGHT_SDS_D00040", "CALCIUM_VALUE", "CHOLESTERIN_VALUE", "OSTEOCALCIN_VALUE", "IGF1_VALUE" ) ] )
nrow( focus )

# Tabelliere nach Alter ohne Beruecksichtigung des Alters
table( focus$AGE_D00040 )

# Barplotte nach Alter ohne Beruecksichtigung des Alters
plot( factor( focus$AGE_D00040 ) )

# Zerlege das Alter in 12 gleichgrosse Teile und ersetze Jahre durch Tage 
focus$age.cat <- cut( 365.25 * focus$AGE_D00040, breaks = 12 )

# tabeliere und plotte nach Monaten
table( focus$age.cat )

# hiermit siehst Du die Luecke zwischen besser als mit ggplot
plot( focus$age.cat )

# im Alter zwischen 0.574 und 0.857 scheint niemand da gewesen zu sein
ggplot( focus ) +
    geom_histogram( aes( age.cat ), stat="count" )


# %>% ist der pipe-operator von dplyr
# dieser dient nur der besseren Lesbarkeit des folgenden codes
# focus <- mutate( group_by( focus, age.cat, sex ), n = n( ) )
focus %>%
    group_by( age.cat, sex ) %>%
    mutate( n = n( ) ) -> 
focus

# ankucken!
table( focus$age.cat, focus$sex )

# so siehst Du dasselbe nochmal als bar - plot
ggplot( focus ) + theme_bw( ) +
    geom_histogram( aes( age.cat, fill = sex ), position = "dodge", stat = "count" ) +
    geom_hline( yintercept = 30 )

# so siehst Du die einzelnen Messungen, hier mal hoehe-sds
ggplot( focus ) + 
    geom_point( aes( focus$AGE_D00040, focus$WEIGHT_SDS_D00040, col = focus$sex ) )

# left hand side sind alle Variablen die mit der right hand side auf Korrelation getestet werden sollen
lhs <- c( "AGE_D00040", "WEIGHT_SDS_D00040", "HEIGHT_SDS_D00040", "BMI_SDS_D00040", "OSTEOCALCIN_VALUE", "CALCIUM_VALUE", "CHOLESTERIN_VALUE" )
rhs <- c( "WEIGHT_SDS_D00040", "HEIGHT_SDS_D00040", "BMI_SDS_D00040", "OSTEOCALCIN_VALUE", "CALCIUM_VALUE", "CHOLESTERIN_VALUE" )
sex <- c( "male", "female" )

# zeige alle Korrelationen mit Anzahl n und p-Wert
Reduce( 
    bind_rows, 
    lapply( 
        c( "pearson", "spearman" ),
        function( tp ) {
            Reduce( 
                bind_rows, 
                lapply( 
                    sex,
                    function( s ) {
            
                        Reduce( 
                            bind_rows, 
                            lapply( 
                                lhs, 
                                function ( l ) {
                            
                                    Reduce( 
                                        bind_rows, 
                                        lapply(
                                            rhs, 
                                            function ( r ) {
                                    
                                                t <- unlist( rcorr( unlist( focus[ focus$sex == s, l ] ), unlist( focus[ focus$sex == s, r ] ), type = tp ) )
                                                d <- data.frame( sex = s, x = l, y = r, corr = t[ 2 ], n = t[ 6 ], p = t[ 10 ], significance = ifelse( t[ 10 ] <= .05, "yes", "no" ), type = tp )
                                                
                                            }
                                        )
                                    )
                                }
                            )
                        )
                    }
                )
            )
        }
    )
)-> erg

# zeige Ergebnis
View( as.data.frame( erg ) )

# Uebersicht mit fit
my_fn.lm <-
function( data, mapping, ... ) {
    ggplot(
        data = data, 
        mapping = mapping
    ) + 
    geom_point( aes(color = sex ), alpha = .3, ... ) + 
    geom_smooth( method = lm, aes ( fill = sex, color = sex ), alpha = .3, ... ) 
}

g.lm <-
ggpairs(
    focus[ , c( lhs, "sex" ) ],
    lower = list( continuous = my_fn.lm ) 
)

g.lm

# Uebersicht mit gleitendem Mittelwert
my_fn.loess <-
function( data, mapping, ... ) {
    p <- 
    ggplot(
        data = data, 
        mapping = mapping
    ) + 
    geom_point( aes(color = sex ), alpha = .3, ... ) + 
    geom_smooth( method = loess, aes ( fill = sex, color = sex ), alpha = .3, ... ) 
    p
}


g.loess <-
ggpairs(
    focus[ , c( lhs, "sex" ) ],
    lower = list( continuous = my_fn.loess )
)

g.loess

# check for follow ups
# gruppiere nach SIC
# ermittle Gruppengroesse ( eine Gruppe enthaelt alle Besuche zu einer SIC )
# schreibe diese in Spalte "visit.cnt" und in Spalte "visit.num", um den wievielten Besuch es sich handelt
daten %>%
    group_by( SIC ) %>%
    mutate( visit.num = dense_rank( EDAT ) ) ->
follow.ups

table( follow.ups$visit.num, follow.ups$sex )

ggplot( follow.ups ) + geom_histogram( aes( visit.num, fill = sex ), position = "dodge", bins = 7 )

ggplot( follow.ups ) + geom_histogram( aes( AGE_YEARS, fill = factor( visit.num ) ), position = "stack", bins = 18 ) + facet_grid( . ~ sex )

rcorr(  )