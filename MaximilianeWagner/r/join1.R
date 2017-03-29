library( "readxl" )
library( "ggplot2" )
library( "dplyr" )

## Setze hier den Pfad zum Verzeichnis Deiner Dateien
setwd( "../data_neu_20160929" )

## Lade beide Tabellen
t1 <- read_excel( "20161110_Probenliste_AGa (1).xlsx" )
t2 <- read_excel( "PV208_Probauswahl2_20160929.xlsx" )

t1.names <- "Materialnummer" ##names( t1 )
t2.names <- "Materialnummer" ##names( t2 )
t3.names <- t1.names[ t1.names %in% t2.names ]

t1.interests <- c( "Cortisol" )
t2.interests <- c(
    "TEILNEHMER_SIC",       "C_BP_SDS_BP_DIA_3",   "C_BP_SDS_BP_SYS_3",      "C_PUB_STAT_PUB_STATUS",
    "C_ANTHRO_AGE",         "C_AUFKL_GENDER",      "C_SOZ_WI_SOZIO_FAM",     "C_SOZDEM_V_SCHULAB",
    "C_SOZDEM_M_SCHULAB",   "C_SOZDEM_EINZELKIND", "C_SOZDEM_EINKOMMEN",     "C_SOZ_WI_FAS",
    "C_ANTHRO_KH_BMI_ORIG", "C_ANTHRO_KH_BMI_ADJ", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ADJ" )

t1.names <- c( t3.names, t1.interests )
t2.names <- c( t3.names, t2.interests )

## Zusammenfuegen beider Tabellen ueber die Materialnummer
t3 <- merge( t1[ ,t1.names ], t2[ , t2.names ], by = t3.names )

## Zeige alle Namen, die die auf .x enden sind aus t1 und .y aus t2
names( t3 )

t3$sex      <- ifelse( t3$C_AUFKL_GENDER < 2, "male", "female" )
t3$pub.stat <- factor( t3$C_PUB_STAT_PUB_STATUS )

t3$Cortisol <- as.numeric( t3$Cortisol )

t3 <- t3[ !is.na( t3$Cortisol ), ]

t3 <- t3[ t3$Cortisol < 50, ]
t3$log.Cortisol <- log10( t3$Cortisol )
t3$chi2.Cortisol <- sqrt( t3$Cortisol )

t3$pub.cat <- cut( 
    t3$C_PUB_STAT_PUB_STATUS, 
    breaks = c( 0, 1, 4, 6 ), 
    labels = c( "prepubertal", "pubertal", "postpubertal" ) )

ggplot( t3, aes( x=pub.cat, y=Cortisol  ) ) +
    geom_boxplot( ) +
    geom_rug( aes( color = pub.cat ) ) +
    facet_grid(. ~ sex )

ggplot( t3, aes( x=pub.cat, y=log.Cortisol  ) ) +
    geom_boxplot( ) +
    geom_rug( aes( color = pub.cat ) ) +
    facet_grid(. ~ sex )

ggplot( t3, aes( x=pub.cat, y=chi2.Cortisol  ) ) +
    geom_boxplot( ) +
    geom_rug( aes( color = pub.cat ) ) +
    facet_grid(. ~ sex )

table( t3$sex, t3$pub.cat )

mean( t3$log.Cortisol[ t3$sex == "male" & t3$pub.cat == "prepubertal" ], na.rm = T )
mean( t3$log.Cortisol[ t3$sex == "female" & t3$pub.cat == "prepubertal" ], na.rm = T )
mean( t3$log.Cortisol[ t3$sex == "male" & t3$pub.cat == "pubertal" ], na.rm = T )
mean( t3$log.Cortisol[ t3$sex == "female" & t3$pub.cat == "pubertal" ], na.rm = T )
mean( t3$log.Cortisol[ t3$sex == "male" & t3$pub.cat == "postpubertal" ], na.rm = T )
mean( t3$log.Cortisol[ t3$sex == "female" & t3$pub.cat == "postpubertal" ], na.rm = T )

aggregate( t3$log.Cortisol, list( t3$sex, t3$pub.cat ), mean )

t3 %>%
    group_by( sex, pub.cat ) %>%
    mutate( m = mean( log.Cortisol ), s = sd( log.Cortisol ), n = n( ) ) ->
    t3.help

t3.help$g <- .25 * t3.help$n / ( sqrt( 2. * 3.14159 ) * t3.help$s * t3.help$Cortisol ) * exp( -.5 * ( ( t3.help$log.Cortisol - t3.help$m ) / t3.help$s ) ^ 2  )

t3.help$g2 <- .25 * t3.help$n / ( sqrt( 2. * 3.14159 ) * t3.help$s ) * exp( -.5 * ( ( t3.help$log.Cortisol - t3.help$m ) / t3.help$s ) ^ 2  )

ggplot( t3.help, aes( log.Cortisol ) ) +
    geom_histogram( binwidth = .1 ) +
##    geom_text(  ) +
    facet_grid( sex ~ pub.cat ) +
    geom_line( aes(x=log.Cortisol, y=g ), color="red", alpha = "1." ) +
    geom_line( aes(x=log.Cortisol, y=g2 ), color="blue", alpha = "1." )

ggplot( t3, aes( x=C_ANTHRO_AGE, y=log.Cortisol, color = pub.cat ) ) +
    theme_bw( ) +
    scale_x_continuous( breaks = c( 5:20 ) ) +
    geom_boxplot( notch = T, alpha = .2 ) +
    geom_point( ) +
    geom_smooth( aes( group = pub.cat ), method = "lm", alpha = .2 ) +
##    geom_smooth( color = "gray", alpha = .2 ) +
    geom_rug( ) + 
    facet_grid( . ~ sex )

ggplot( t3, aes( y=log.Cortisol, x=t3$C_ANTHRO_KH_BMI_ORIG, color = pub.cat ) ) +
    theme_bw( ) +
##    scale_x_continuous( breaks = c( 5:20 ) ) +
##    geom_boxplot( notch = T, alpha = .2 ) +
    geom_point( ) +
    geom_smooth( aes( group = pub.cat ), alpha = .2 ) +
##    geom_smooth( color = "gray", alpha = .2 ) +
    geom_rug( ) +
    facet_grid( . ~ sex )

ggplot( t3, aes( y=log.Cortisol, x=t3$C_ANTHRO_KH_BMI_ADJ, color = pub.cat ) ) +
    theme_bw( ) +
##    scale_x_continuous( breaks = c( 5:20 ) ) +
##    geom_boxplot( notch = T, alpha = .2 ) +
    geom_point( ) +
    geom_smooth( aes( group = pub.cat ), method = "lm", alpha = .2 ) +
##    geom_smooth( color = "gray", alpha = .2 ) +
    geom_rug( ) +
    facet_grid( . ~ sex )

ggplot( t3, aes( x=log.Cortisol, y=t3$C_ANTHRO_KH_BMI_ADJ, color = pub.cat ) ) +
    theme_bw( ) +
##    scale_x_continuous( breaks = c( 5:20 ) ) +
##    geom_boxplot( notch = T, alpha = .2 ) +
    geom_point( ) +
    geom_smooth( aes( group = pub.cat ), method = "lm", alpha = .2 ) +
##    geom_smooth( color = "gray", alpha = .2 ) +
    geom_rug( ) +
    facet_grid( . ~ sex )

ggplot( t3, aes( x=pub.cat, y=log.Cortisol  ) ) +
    geom_boxplot( notch = T ) +
    geom_rug( aes( color = pub.cat ) ) +
    facet_grid(. ~ sex )

ggplot( t3, aes( x=pub.cat, y=chi2.Cortisol  ) ) +
    geom_boxplot( ) +
    geom_rug( aes( color = pub.cat ) ) +
    facet_grid(. ~ sex )


summary( t3$Cortisol )
summary( t3$log.Cortisol )
summary( t3$chi2.Cortisol )


hist( t3$Cortisol, breaks = 100 )
hist( t3$log.Cortisol, breaks = 100 )
hist( t3$chi2.Cortisol, breaks = 100 )
