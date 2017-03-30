## loesche alle variablen
rm( list = ls( ) )

## wechsle ins aktuelle datenverzeichnis
setwd( "~/LIFE/github-tpeschel/R/WiebkeKeller/data/" )

## lade alle Tabellen
source( "../r/read.all.tables.r" )

## Zeige alle Tabellennamen
table.names

## Zeige alle Spaltennnamen
table.row.names

## Vereinige Tabelle D00078 mit D00148 ueber die SIC und die SCI-GROUP beider Tabellen
d078.d148 <- 
merge(
    get.tbl( "D00078" ),
    get.tbl( "D00148" ),
    by.x = c( "C_SOZDEM_SIC", "C_SOZDEM_SCI_GROUP" ),
    by.y = c( "SDQ_SIC", "SDQ_SCI_GROUP" ) )

## Vereinige neue Tabelle R00001 ueber die SIC beider Tabellen
d078.d148.r001 <- 
merge(
    d078.d148,
    get.tbl( "R00001" ),
    by.x = c( "C_SOZDEM_SIC" ),
    by.y = c( "TEILNEHMER_SIC" ) )

library( dplyr )
library( ggplot2 )
library( lubridate )


## Schmeisse alle raus, die keinen SDQ-Gesamt-Score haben
focus.sdq <- d078.d148.r001[ !is.na( d078.d148.r001$SDQ_GES_SCORE ), ]

## Codiere die Werte 1 und 2 zu male und female
focus.sdq$sex <-
factor( focus.sdq$TEILNEHMER_GESCHLECHT, c( 1, 2 ), c( "male", "female" ) )

## Wandle Geburstag in vernuenftiges Format
focus.sdq$geb.dat <- ymd( 15 + 100 * as.integer( focus.sdq$TEILNEHMER_GEB_JJJJMM ) )

## Ermittle Alter in Tage
focus.sdq$age <- as.numeric( round( difftime( ymd_hms( focus.sdq$C_SOZDEM_EDAT ), focus.sdq$geb.dat, units = "days" ) / 365.25 ) )

## Zeige Min- und Max-Alter
summary( as.numeric( focus.sdq$age ) )


## Gruppiere nach SIC und ermittle die Anzahl der Besuche
focus.sdq.visits <- 
focus.sdq %>%
    group_by( C_SOZDEM_SIC, sex ) %>%
    summarise(
        visits = factor( n( ) ) )

## Plotte die Anzahl an Besuchen pro Geschlecht
ggplot( 
    focus.sdq.visits, 
    aes( visits, fill = sex ) ) +
    geom_bar( position = "dodge" ) +
    ggtitle( "visits per sex with sdq-total-score" ) +
    xlab( "number of visits" ) +
    ylab( "amount of probands" ) +
    scale_fill_manual( values = c("male" = "midnightblue", "female" = "deeppink4" ) ) +
    theme_bw( )

## Tabelliere Besuche 
addmargins( table( focus.sdq.visits$visits, focus.sdq.visits$sex ) )

## Plotte die Anzahl an Besuchen pro Geschlecht
ggplot( 
    focus.sdq, 
    aes( age, fill = sex ) ) +
    geom_bar( position = "dodge", colours = c("green", "blue" ) ) +
    ggtitle( "visits per sex and age with sdq-total-score" ) +
    xlab( "number of visits" ) +
    ylab( "amount of probands" ) +
    scale_fill_manual( values = c("male" = "midnightblue", "female" = "deeppink4" ) ) +
    theme_bw( )
