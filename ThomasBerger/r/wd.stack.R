wd.stack <- NULL

push <-
function ( ) {
    
    wd.stack <<- c( wd.stack, getwd( ) )
}

pop <-
function ( ) {
    
    path <- wd.stack[ length( wd.stack ) ]
    
    wd.stack <<- wd.stack[ length( wd.stack ) - 1 ]
    
    setwd( path )
}

