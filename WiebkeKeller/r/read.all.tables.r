## lade paket zum lesen von excelfiles
library( readxl )

## lade alle Tabellen mit ihren Namen
t <- lapply( dir( pattern = "*.xlsx" ), function( d ) list( name = substr( d, 8, 8 + 5 ), table = read_excel( d ) ) )

get.tbl <-
function( name, tbls = t ) {
    
    for( i in 1 : length( tbls ) )
        
        if( tbls[[ i ]]$name == name )
            
            return( tbls[[ i ]]$table )
    
    return( list( NULL ) )
}

table.names <-
unlist( lapply( t, function( tbls ) tbls$name ) )

table.row.names <- 
sapply( table.names, function( n ) names( get.tbl( n ) ) )