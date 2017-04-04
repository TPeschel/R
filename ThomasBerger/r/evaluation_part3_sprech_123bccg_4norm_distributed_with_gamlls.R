push( )

setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/" )

sds <- 
    function( value, age, sex, item, ref.obj, male = "male", female = "female" ) {
        ref <- slot( ref.obj, "ref" )[[ item ]]
        sex <- as.character( sex )
        sapply( 
            1 : length( value ),
            function( i ) {
                if( is.na( value[ i ] | is.na( age[ i ] ) | is.na( sex[ i ] ) ) ) 
                    return( NA )
                m <- approx( ref[[ sex[ i ]]]$age, ref[[ sex[ i ]]]$m, xout = age[ i ], rule = 1 )$y
                l <- approx( ref[[ sex[ i ]]]$age, ref[[ sex[ i ]]]$l, xout = age[ i ], rule = 1 )$y
                s <- approx( ref[[ sex[ i ]]]$age, ref[[ sex[ i ]]]$s, xout = age[ i ], rule = 1 )$y
                ( ( value[ i ] / m ) ** l - 1 ) / ( l * s ) 
            } 
        )
    }

sds( value = )
# 
# 
#   labelsdf <- perc.sum.girls[which(max(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")]
#   labelsdf$xval <- 18.1
#   labelsdf$label <- c(3,10,50,90,97)
#   labelsdf <- labelsdf[!grepl("p[0-9]|se",labelsdf$variable),]
# 
#    annotate("text",x=18.2,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
#                colour = "black",inherit.aes = F, parse = T, vjust = 0.8, hjust = 0.5) +

pop( )