## delete all data
rm( list = ls( ) )

library( directlabels )
library( dplyr )
library( gamlss )
library( ggplot2 )
library( lifecuration )
library( lubridate )
library( readxl )
library( reshape2 )
library( svglite )

## connection to data base
source( "~/connection/connection.r" )

## day of computation
#date.today <- gsub( x = Sys.Date( ), pattern = "-1perFam", replacement = "" )
date.today <- "20170328"
#date.today <- 20202020
## push the current working directory on the stack
#push( )

## set the new working directory to ~/LIFE/myPostGraduates/ThomasBerger/r/TP/
setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/r/" )

## evaluation part 1
## using Box-Cox-Cole-Green distribution for
## SPRECH_F0_1..3
## using normal distribution for
## SPRECH_F0_4
source( "evaluation_part1_sprech_123bccg_4norm_distributed_with_gamlls.R" )

## evaluation part 2
## using Box-Cox-Cole-Green distribution for
## SPRECH_F0_1..3
## using normal distribution for
## SPRECH_F0_4
source( "evaluation_part2_sprech_123bccg_4norm_distributed_with_gamlls.R" )

#pop( )
