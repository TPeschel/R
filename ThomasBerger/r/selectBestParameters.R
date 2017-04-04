setwd( "~/LIFE/myPostGraduates/ThomasBerger/r/TP/" )

library( "readxl" )

library( "dplyr" )

result <- read_excel( "LMS_Params.xls" )
names( result )[ 1 ] <- "no"

result %>%
    group_by( sex, sp.lvl ) %>%
    summarise( id = as.numeric( no[ which.max( succ ) ] ) ) -> bm

result[ bm$id, ]
