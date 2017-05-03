rm(list=ls())

setwd("~/Desktop/pv0116_neu/")

library(readxl)
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )

Daten <- read_excel( "PV0116_GesamtJoin.xlsx" )
Daten <- Daten[ !is.na( Daten$CT_S_1_NUM_VALUE ),]

#x und y non-empty numeric vector of data values
##Daten dürfe keine NAs haben
Daten <- Daten[ !is.na( Daten$CA_S_NUM_VALUE ), ]

##nur die Altersgruppe 0-1 betrachten
Daten01 <- Daten[ Daten$AGE_Calcitionin <= 1, ]
#Daten0025 <- Daten[between(Daten$AGE_Calcitionin, 0,0.25),]

quantile( Daten$CT_S_1_NUM_VALUE )

## Calcitonindaten in Gruppen einteilen (Perzentilen)
low  <- Daten[ between( Daten$CT_S_1_NUM_VALUE, 0.5, 1.3 ), ]
high <- Daten[ between( Daten$CT_S_1_NUM_VALUE, 5.2, 48.4 ), ]

## Ausreiser raus!
high <- high[ 2 <= high$CA_S_NUM_VALUE, ]

# Lege jeweils eine neue Spalte an, die die Gruppe festhaelt
low$group    <- "low"
high$group   <- "high"

# Mache wieder einen Datafram daraus
both <- rbind( low, high )

# So kannst Du auf Normalitaet testen.
# Die Nullhypothese ist die Annahme einer Normalverteilung
# Sprich ein kleiner p-Wert spricht dafuer, dass es nicht normal verteilt ist.
# Ein grosser Wert jedoch sagt nicht automatisch, dass es normal verteil ist.
# Verlasse Dich auf Dein Gespuer beim Anschauen eines Barplots!

shapiro.test( low$CT_S_1_NUM_VALUE )
qqnorm( low$CT_S_1_NUM_VALUE )
qqline( low$CT_S_1_NUM_VALUE, col = "red" )

shapiro.test( low$CA_S_NUM_VALUE )
qqnorm( low$CA_S_NUM_VALUE )
qqline( low$CA_S_NUM_VALUE, col = "red" )

shapiro.test( high$CT_S_1_NUM_VALUE )
qqnorm( high$CT_S_1_NUM_VALUE )
qqline( high$CT_S_1_NUM_VALUE, col = "red" )

shapiro.test( high$CA_S_NUM_VALUE )
qqnorm( high$CA_S_NUM_VALUE )
qqline( high$CA_S_NUM_VALUE, col = "red" )

summary( high$CA_S_NUM_VALUE )

ggplot( both,
    aes( CA_S_NUM_VALUE, fill = group ) ) +
    geom_bar( alpha = .5 ) + 
    facet_grid( . ~ group )

# T-Test: Unterschied im Mittelwert von Kalzium in den Gruppen low und high 
t.test( CA_S_NUM_VALUE ~ group,  data = both )

# Der Test mag zwar signifikant, wenn aber der Unterschied relativ klein ist,
# ist die Signifikanz egal. Wenn kaum ein Unterschied festzustellen ist, 
# dann sind die beiden Mittelwerte gleich, egal, wie klein p wird.

ggplot( both,
    aes( group, CA_S_NUM_VALUE ) ) +
    geom_boxplot( notch = T ) 


d <- Daten

summary( d$CT_S_1_NUM_VALUE )

d$age.kat <- cut( as.numeric( d$AGE_Calcitionin ), breaks = 18 )

ggplot( d, aes( age.kat , CT_S_1_NUM_VALUE ) ) +
    geom_boxplot( )

d <- d[ as.numeric( d$AGE_Calcitionin ) < 3, ]

d$age.kat <- cut( as.numeric( d$AGE_Calcitionin ), breaks = 6 )

ggplot( d, aes( age.kat, CT_S_1_NUM_VALUE ) ) +
    geom_boxplot( )
