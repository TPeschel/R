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

## little helper
## just a stack for storing working directory
source( "~/LIFE/myPostGraduates/ThomasBerger/r/TP/wd.stack.R" )

## day of computation
today <- gsub( x = Sys.Date( ), pattern = "-", replacement = "" )

## push the current working directory on the stack
push( )

## set the new working directory to ~/LIFE/myPostGraduates/ThomasBerger/r/TP/
setwd( "~/LIFE/myPostGraduates/ThomasBerger/r/TP/" )

## evaluation part 1
## using Box-Cox-Cole-Green distribution for
## SPRECH_F0_1..3
## using normal distribution for
## SPRECH_F0_4
source( "evaluation_part1_sprech_123_bccg_4_norm_distributed_with_gamlls.R" )


## evaluation part 1
## using Box-Cox-Cole-Green distribution for
## SPRECH_F0_1..3
## using normal distribution for
## SPRECH_F0_4
source( "evaluation_part2_sprech_123_bccg_4_norm_distributed_with_gamlls.R" )

pop( )
