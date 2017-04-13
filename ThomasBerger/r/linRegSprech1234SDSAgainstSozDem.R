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

load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/refs.Rda" )
load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/data.sprech.Rda" )

summary( mm1 <- lm( sprech1_sds ~ sex/SCORE_FAM, data = data.sprech ) )
ggplot( data.sprech, aes( SCORE_FAM, sprech1_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) +
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "linreg.f0.sprech1.sds.against.famscore.png" )

summary( mm2 <- lm( sprech2_sds ~ sex/SCORE_FAM, data = data.sprech ) )
ggplot( data.sprech, aes( SCORE_FAM, sprech2_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) +
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "linreg.sprech2.sds.against.famscore.png" )

summary( mm3 <- lm( sprech3_sds ~ sex/SCORE_FAM, data = data.sprech ) )
ggplot( data.sprech, aes( SCORE_FAM, sprech3_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) +
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "linreg.sprech3.sds.against.famscore.png" )

summary( mm4 <- lm( sprech4_sds ~ sex/SCORE_FAM, data = data.sprech ) )
ggplot( data.sprech, aes( SCORE_FAM, sprech4_sds, col = sex ) ) +
    geom_point( alpha = .3, na.rm = T ) +
    geom_smooth( method = "gam", na.rm = T ) +
    facet_grid( sex ~ . ) +
    scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) )

ggsave( "linreg.sprech4.sds.against.famscore.png" )

