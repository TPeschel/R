push( )

setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/" )

mytheme <- theme( 
    legend.position = "none", 
    axis.text  = element_text( size = 14 ),
    axis.title = element_text( size = 14 )#,
    #panel.grid = element_blank( ) 
    )

## transforms percentiles to real world values relating to Box-Cox-Cole-Green-Distribution
calc.vals.bccg <-
function( perc, mu, sigma, nu ) {
  
    z <- qnorm( perc )
  
    mu * ( z * nu * sigma + 1 ) ** ( 1 / nu )
}

## transforms percentiles to real world values relating to Box-Cox-Cole-Green-Distribution
calc.vals.norm <- function( perc, mu, sigma ) {
  
    z <- qnorm( perc )
  
    z * sigma + mu;
}

## parameter for the first 3 speech levels
params <- c( 1 : 3 ) 

for( mg in params ) {

    load( paste0( "LMS_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )
    
    names( res.girls ) <- 1 : length( res.girls )
    names( res.boys )  <- 1 : length( res.boys )
    
    res.girls <- lapply( 
        res.girls,
        function( x ) {
            x$perc03 <- calc.vals.bccg( perc = 0.03, x$mu, x$sigma, x$nu )
            x$perc10 <- calc.vals.bccg( perc = 0.10, x$mu, x$sigma, x$nu )
            x$perc50 <- calc.vals.bccg( perc = 0.50, x$mu, x$sigma, x$nu )
            x$perc90 <- calc.vals.bccg( perc = 0.90, x$mu, x$sigma, x$nu )
            x$perc97 <- calc.vals.bccg( perc = 0.97, x$mu, x$sigma, x$nu )
            return( x )
        }
    )
    
    res.boys <- lapply( 
        res.boys, 
        function( x ) {
            x$perc03 <- calc.vals.bccg( perc = 0.03, x$mu, x$sigma, x$nu )
            x$perc10 <- calc.vals.bccg( perc = 0.10, x$mu, x$sigma, x$nu )
            x$perc50 <- calc.vals.bccg( perc = 0.50, x$mu, x$sigma, x$nu )
            x$perc90 <- calc.vals.bccg( perc = 0.90, x$mu, x$sigma, x$nu )
            x$perc97 <- calc.vals.bccg( perc = 0.97, x$mu, x$sigma, x$nu )
            return( x )
        }
    )
    
    age.points <- nrow( res.girls[[ 1 ]] )
    
    n.iter <- length( res.girls )
    
    print( n.iter )
    
    res.girls <- Reduce( rbind, res.girls )
    res.girls$which <- rep( 1 : n.iter, each = age.points )
    
    n.iter <- length( res.boys )
    
    age.points <- nrow( res.boys[[ 1 ]] )

    print( n.iter )
    
    res.boys <- Reduce( rbind, res.boys )
    res.boys$which <- rep( 1 : n.iter, each = age.points )
    
    perc.single.girls <- melt( res.girls, id.vars = c( "which", "age", "mu", "sigma", "nu" ) )
    perc.single.boys  <- melt( res.boys,  id.vars = c( "which", "age", "mu", "sigma", "nu" ) )
  
    perc.sum.girls <-
    res.girls %>%
    group_by( age ) %>%
    summarise(
        mean.mu    = mean( mu ),
        sd.mu      = sd( mu ),
        mean.sigma = mean( sigma ),
        sd.sigma   = sd( sigma ),
        mean.nu    = mean( nu ),
        sd.nu      = sd( nu ) )
    
    perc.sum.boys <-
    res.boys %>%
    group_by( age ) %>%
    summarise(
        mean.mu    = mean( mu ),
        sd.mu      = sd( mu ),
        mean.sigma = mean( sigma ),
        sd.sigma   = sd( sigma ),
        mean.nu    = mean( nu ),
        sd.nu      = sd( nu ) )

    perc.sum.girls$mean.perc03 <- calc.vals.bccg( perc = .03, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma, perc.sum.girls$mean.nu )
    perc.sum.girls$mean.perc10 <- calc.vals.bccg( perc = .10, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma, perc.sum.girls$mean.nu )
    perc.sum.girls$mean.perc50 <- calc.vals.bccg( perc = .50, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma, perc.sum.girls$mean.nu )
    perc.sum.girls$mean.perc90 <- calc.vals.bccg( perc = .90, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma, perc.sum.girls$mean.nu )
    perc.sum.girls$mean.perc97 <- calc.vals.bccg( perc = .97, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma, perc.sum.girls$mean.nu )
    
    perc.sum.boys$mean.perc03  <- calc.vals.bccg( perc = .03, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma, perc.sum.boys$mean.nu )
    perc.sum.boys$mean.perc10  <- calc.vals.bccg( perc = .10, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma, perc.sum.boys$mean.nu )
    perc.sum.boys$mean.perc50  <- calc.vals.bccg( perc = .50, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma, perc.sum.boys$mean.nu )
    perc.sum.boys$mean.perc90  <- calc.vals.bccg( perc = .90, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma, perc.sum.boys$mean.nu )
    perc.sum.boys$mean.perc97  <- calc.vals.bccg( perc = .97, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma, perc.sum.boys$mean.nu )
    
    perc.sum.girls <-
    melt(
        as.data.frame( perc.sum.girls ), 
        id.vars = c( "age", "mean.mu", "mean.sigma", "mean.nu", "sd.mu", "sd.sigma", "sd.nu" ) )
    
    perc.sum.boys  <-
    melt(
        as.data.frame( perc.sum.boys ),
        id.vars = c( "age", "mean.mu", "mean.sigma", "mean.nu", "sd.mu", "sd.sigma", "sd.nu" ) )
    
    save( perc.sum.girls, file = paste0( "LMS_4GFX_F0_SPRECH_FEMALE_", mg, "_", date.today, ".Rda" ) )
    save( perc.sum.boys,  file = paste0( "LMS_4GFX_F0_SPRECH_MALE_", mg, "_", date.today, ".Rda" ) )
    
        
    labelsdf.g <- perc.sum.girls[ which( max( perc.sum.girls$age ) == perc.sum.girls$age ), c( "variable", "value" ) ]
    labelsdf.g$xval <- 18.1
    labelsdf.g$label <- c( 3, 10, 50, 90, 97 )
    labelsdf.g <- labelsdf.g[ !grepl( "p[0-9]|se", labelsdf.g$variable ), ]
    
    labelsdf.b <- perc.sum.boys[ which( max( perc.sum.boys$age ) == perc.sum.boys$age ), c( "variable", "value" ) ]
    labelsdf.b$xval <- 18.1
    labelsdf.b$label <- c( 3, 10, 50, 90, 97 )
    labelsdf.b <- labelsdf.b[ !grepl( "p[0-9]|se", labelsdf.b$variable ), ]
    

    spr.descr <- c( "softest speaking", "conversational", "class room", "shouting out" )
    
    ggplot( 
        perc.single.girls,
        aes(
            x = age,
            y = value,
            colour = variable,
            group = paste( variable, which ) 
        ) ) +
#        geom_line( alpha = 0.1 ) +
#        scale_colour_manual( values = c( "firebrick", "orangered", "forestgreen", "orangered", "firebrick" ) ) +
        scale_linetype_manual( values = c( 3, 2, 1, 2, 3 ) ) +
        geom_line( 
            data = perc.sum.girls, 
            inherit.aes = F,
            aes(
                x = age,
                y = value,
                group = paste( variable ),
                linetype = variable
            ),
            colour = "black" ) +
        scale_x_continuous( breaks = c( 5 : 18 ) ) +
        labs( title = paste0( "speaking voice - level ", mg, " (", spr.descr[ mg ], ") - female" ), x = "age [y]", y = "frequency [Hz]" ) +
        annotate(
            "text",
            x = 18.2,
            y = labelsdf.g$value,
            label = paste0("P[",labelsdf.g$label,"]"),
            colour = "black",
            inherit.aes = T, 
            parse = T, 
            vjust = 0.8, 
            hjust = 0.5 ) +
            theme_bw( ) + mytheme

    ggsave( paste0( "PLOT_F0_SPRECH_", mg, "_FEMALE_bw_", date.today, ".png" ) )    

    ggplot( 
        perc.single.boys,
        aes(
            x = age,
            y = value,
            colour = variable,
            group = paste( variable, which ) 
        ) ) +
#        geom_line( alpha = 0.1 ) +
#        scale_colour_manual( values = c( "firebrick", "orangered", "forestgreen", "orangered", "firebrick" ) ) +
        scale_linetype_manual( values = c( 3, 2, 1, 2, 3 ) ) +
        geom_line( 
            data = perc.sum.boys, 
            inherit.aes = F,
            aes(
                x = age,
                y = value,
                group = paste( variable ),
                linetype = variable
            ),
            colour = "black" ) +
        scale_x_continuous( breaks = c( 5 : 18 ) ) +
        labs( title = paste0( "speaking voice - level ", mg, " (", spr.descr[ mg ], ") - male" ), x = "age [y]", y = "frequency [Hz]" ) +
        annotate(
            "text",
            x = 18.2,
            y = labelsdf.b$value,
            label = paste0("P[",labelsdf.b$label,"]"),
            colour = "black",
            inherit.aes = T, 
            parse = T, 
            vjust = 0.8, 
            hjust = 0.5 ) +
            theme_bw( ) + mytheme

        ggsave( paste0( "PLOT_F0_SPRECH_", mg, "_MALE_bw_", date.today, ".png" ) )
}

mg <- 4

load( paste0( "LMS_F0_SPRECH_", mg, "_", date.today, ".Rda" ) )

res.girls <- lapply( 
    res.girls,
    function( x ) {
        x$perc03 <- calc.vals.norm( perc = 0.03, x$mu, x$sigma )
        x$perc10 <- calc.vals.norm( perc = 0.10, x$mu, x$sigma )
        x$perc50 <- calc.vals.norm( perc = 0.50, x$mu, x$sigma )
        x$perc90 <- calc.vals.norm( perc = 0.90, x$mu, x$sigma )
        x$perc97 <- calc.vals.norm( perc = 0.97, x$mu, x$sigma )
        return( x )
    }
)

res.boys <- lapply( 
    res.boys, 
    function( x ) {
        x$perc03 <- calc.vals.norm( perc = 0.03, x$mu, x$sigma )
        x$perc10 <- calc.vals.norm( perc = 0.10, x$mu, x$sigma )
        x$perc50 <- calc.vals.norm( perc = 0.50, x$mu, x$sigma )
        x$perc90 <- calc.vals.norm( perc = 0.90, x$mu, x$sigma )
        x$perc97 <- calc.vals.norm( perc = 0.97, x$mu, x$sigma )
        return( x )
    }
)

age.points <- nrow( res.boys[[ 1 ]] )

n.iter <- length( res.girls )

print( n.iter )

res.girls <- Reduce( rbind, res.girls )
res.girls$which <- rep( 1 : n.iter, each = age.points )

n.iter <- length( res.boys )

print( n.iter )

res.boys <- Reduce( rbind, res.boys )
res.boys$which <- rep( 1 : n.iter, each = age.points )

perc.single.girls <- melt( res.girls, id.vars = c( "which", "age", "mu", "sigma" ) )
perc.single.boys  <- melt( res.boys,  id.vars = c( "which", "age", "mu", "sigma" ) )
# perc.single.girls <- melt( res.girls, id.vars = c( "which", "age", "mu", "sigma", "nu" ) )
# perc.single.boys  <- melt( res.boys,  id.vars = c( "which", "age", "mu", "sigma", "nu" ) )

perc.sum.girls <- res.girls %>%
    group_by( age ) %>%
    summarise(
        mean.mu    = mean( mu ),
        sd.mu      = sd( mu ),
        mean.sigma = mean( sigma ),
        sd.sigma   = sd( sigma )
)
perc.sum.boys <- res.boys %>%
    group_by( age ) %>%
    summarise(
        mean.mu    = mean( mu ),
        sd.mu      = sd( mu ),
        mean.sigma = mean( sigma ),
        sd.sigma   = sd( sigma )
)

perc.sum.girls$mean.perc03 <- calc.vals.norm( perc = .03, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma )
perc.sum.girls$mean.perc10 <- calc.vals.norm( perc = .10, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma )
perc.sum.girls$mean.perc50 <- calc.vals.norm( perc = .50, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma )
perc.sum.girls$mean.perc90 <- calc.vals.norm( perc = .90, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma )
perc.sum.girls$mean.perc97 <- calc.vals.norm( perc = .97, perc.sum.girls$mean.mu, perc.sum.girls$mean.sigma )

perc.sum.boys$mean.perc03  <- calc.vals.norm( perc = .03, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma )
perc.sum.boys$mean.perc10  <- calc.vals.norm( perc = .10, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma )
perc.sum.boys$mean.perc50  <- calc.vals.norm( perc = .50, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma )
perc.sum.boys$mean.perc90  <- calc.vals.norm( perc = .90, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma )
perc.sum.boys$mean.perc97  <- calc.vals.norm( perc = .97, perc.sum.boys$mean.mu, perc.sum.boys$mean.sigma )


perc.sum.girls <- melt( 
  as.data.frame( perc.sum.girls ), 
  id.vars = c( "age", "mean.mu", "mean.sigma", "sd.mu", "sd.sigma" ) )

perc.sum.boys  <- melt(
  as.data.frame( perc.sum.boys ),
  id.vars = c( "age", "mean.mu", "mean.sigma", "sd.mu", "sd.sigma" ) )

save( perc.sum.girls, file = paste0( "LMS_4GFX_F0_SPRECH_FEMALE_", mg, "_", date.today, ".Rda" ) )
save( perc.sum.boys,  file = paste0( "LMS_4GFX_F0_SPRECH_MALE_", mg, "_", date.today, ".Rda" ) )


labelsdf.g <- perc.sum.girls[ which( max( perc.sum.girls$age ) == perc.sum.girls$age ), c( "variable", "value" ) ]
labelsdf.g$xval <- 18.1
labelsdf.g$label <- c( 3, 10, 50, 90, 97 )
labelsdf.g <- labelsdf.g[ !grepl( "p[0-9]|se", labelsdf.g$variable ), ]

labelsdf.b <- perc.sum.boys[ which( max( perc.sum.boys$age ) == perc.sum.boys$age ), c( "variable", "value" ) ]
labelsdf.b$xval <- 18.1
labelsdf.b$label <- c( 3, 10, 50, 90, 97 )
labelsdf.b <- labelsdf.b[ !grepl( "p[0-9]|se", labelsdf.b$variable ), ]

ggplot( 
    perc.single.girls,
    aes(
        x = age,
        y = value,
        colour = variable,
        group = paste( variable, which ) 
    ) ) +
##        geom_line( alpha = 0.1 ) +
##        scale_colour_manual( values = c( "firebrick", "orangered", "forestgreen", "orangered", "firebrick" ) ) +
    scale_linetype_manual( values = c( 3, 2, 1, 2, 3 ) ) +
    geom_line(
        data = perc.sum.girls,
        inherit.aes = F,
        aes(
            x = age,
            y = value,
            group = paste( variable ),
            linetype = variable
        ),
        colour = "black" ) +
    scale_x_continuous( breaks = c( 5 : 18 ) ) +
    labs( title = "speaking voice - level 4 (shouting out) - female", x = "age [y]", y = "frequency [Hz]" ) +
    annotate(
        "text",
        x = 18.2,
        y = labelsdf.g$value,
        label = paste0("P[",labelsdf.g$label,"]"),
        colour = "black",
        inherit.aes = T, 
        parse = T, 
        vjust = 0.8, 
        hjust = 0.5 ) +
    theme_bw( ) + mytheme

ggsave( paste0( "PLOT_F0_SPRECH_", mg, "_FEMALE_bw_", date.today, ".png" ) )

ggplot(
    perc.single.boys,
    aes(
        x = age,
        y = value,
        colour = variable,
        group = paste( variable, which ) ) ) +
##        geom_line( alpha = 0.1 ) +
##        scale_colour_manual( values = c( "firebrick", "orangered", "forestgreen", "orangered", "firebrick" ) ) +
scale_linetype_manual( values = c( 3, 2, 1, 2, 3 ) ) +
geom_line( 
    data = perc.sum.boys, 
    inherit.aes = F,
    aes(
        x = age,
        y = value,
        group = paste( variable ),
        linetype = variable ),
    colour = "black" ) +
    scale_x_continuous( breaks = c( 5 : 18 ) ) +
    labs( title = "speaking voice - level 4 (shouting out) - male", x = "age [y]", y = "frequency [Hz]" ) +
    annotate(
        "text",
        x = 18.2,
        y = labelsdf.b$value,
        label = paste0("P[",labelsdf.b$label,"]"),
        colour = "black",
        inherit.aes = T, 
        parse = T,
        vjust = 0.8,
        hjust = 0.5 ) +
    theme_bw( ) + mytheme

ggsave( paste0( "PLOT_F0_SPRECH_", mg, "_MALE_bw_", date.today, ".png" ) )
        
pop( )
