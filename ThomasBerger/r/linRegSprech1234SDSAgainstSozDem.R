## delete all data
#rm( list = ls( ) )
warning = F

library( directlabels )
library( dplyr )
library( gamlss )
library( ggplot2 )
library( lifecuration )
library( lubridate )
library( readxl )
library( reshape2 )
library( svglite )
library(lme4)
library(effects)
library(multcomp)

load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/refs.Rda" )
load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/data.sprech.Rda" )

mm1 <- lmer( sprech1_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )
summary( glht( mm1 ) )
plot( allEffects( mm1 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech1_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) ) )

ggsave( "linreg.sprech1.f0.sds.against.famscore.png", width = 10, height = 8 )

mm2 <- lmer( sprech2_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )
summary( glht( mm2 ) )
plot( allEffects( mm2 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech2_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) ) )

ggsave( "linreg.sprech2.f0.sds.against.famscore.png", width = 10, height = 8 )

mm3 <- lmer( sprech3_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )
summary( glht( mm3 ) )
plot( allEffects( mm3 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech3_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) ) )

ggsave( "linreg.sprech3.f0.sds.against.famscore.png", width = 10, height = 8 )

mm4 <- lmer( sprech4_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )
summary( glht( mm4 ) )
plot( allEffects( mm4 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech4_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) ) )

ggsave( "linreg.sprech4.f0.sds.against.famscore.png", width = 10, height = 8 )

