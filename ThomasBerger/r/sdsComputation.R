source("~/connection/connection.r")
library(dplyr)
library(lifecuration)
library(lubridate)
library(ggplot2)

setwd( "~/LIFE/github-tpeschel/R/ThomasBerger/results/")
(load("LMS_F0_SPRECH_1_20170328.Rda"))

persdat <- get.persdat(ldb)
data <- get.data.with.aliases(ldb, "T00865", withTabAlias = F)
data <- add.persdat.age(persdat, data)
data <- filter(data, age < 18)

# sds.normal <- function(value, age, sex, item, ref, male = "male", female = "female"){
#     sapply(1:length(value),function(i){
#         mu.col <- paste(item, sex[i], "m", sep = ".")
#         sigma.col <- paste(item, sex[i], "s", sep = ".")
#         if(is.na(value[i] | is.na(age[i]) | is.na(sex[i]))) return(NA)
#         m <- approx(ref$age,ref[,mu.col], xout=ref$age[i],rule=1)$y
#         s <- approx(ref$age,ref[,sigma.col],xout=ref$age[i],rule=1)$y
#         (value[i] - m) / s
#     })
# }
# 
# sds.bccg <- function(value, age, sex, item, ref, male = "male", female = "female"){
#     sapply(1:length(value),function(i){
#         mu.col <- paste(item, sex[i], "m", sep = ".")
#         sigma.col <- paste(item, sex[i], "s", sep = ".")
#         lamda.col <- paste(item, sex[i], "l", sep = ".")        
#         if(is.na(value[i] | is.na(age[i]) | is.na(sex[i]))) return(NA)
#         m <- approx(ref$age,ref[,mu.col], xout=ref$age[i],rule=1)$y
#         l <- approx(ref$age,ref[,lamda.col], xout=ref$age[i],rule=1)$y
#         s <- approx(ref$age,ref[,sigma.col],xout=ref$age[i],rule=1)$y
#         ((value[i]/m)**l-1)/(l*s)
#     })
# }
# 

### neu von mandy ###
sds.bccg <- function( value, age, sex, item, ref.obj, male = "male", female = "female" ) {
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

### modifiziert eigentlich neu von mandy ###
sds.norm <- function( value, age, sex, item, ref.obj, male = "male", female = "female" ) {
    ref <- slot( ref.obj, "ref" )[[ item ]]
    sex <- as.character( sex )
    sapply( 
        1 : length( value ),
        function( i ) {
            if( is.na( value[ i ] | is.na( age[ i ] ) | is.na( sex[ i ] ) ) ) 
                return( NA )
            m <- approx( ref[[ sex[ i ]]]$age, ref[[ sex[ i ]]]$m, xout = age[ i ], rule = 1 )$y
            s <- approx( ref[[ sex[ i ]]]$age, ref[[ sex[ i ]]]$s, xout = age[ i ], rule = 1 )$y
            ( value[ i ] - m ) / s
        }
    )
}

# sds.normal <- function(value, age, sex, item, ref, male = "male", female = "female"){
#     sapply(1:length(value),function(i){
#         mu.col <- paste(item, sex[i], "m", sep = ".")
#         sigma.col <- paste(item, sex[i], "s", sep = ".")
#         if(is.na(value[i] | is.na(age[i]) | is.na(sex[i]))) return(NA)
#         m <- approx(ref$age,ref[,mu.col], xout=ref$age[i],rule=1)$y
#         s <- approx(ref$age,ref[,sigma.col],xout=ref$age[i],rule=1)$y
#         (value[i] - m) / s
#     })
# }
# 
# sds.bccg <- function(value, age, sex, item, ref, male = "male", female = "female"){
#     sapply(1:length(value),function(i){
#         mu.col <- paste(item, sex[i], "m", sep = ".")
#         sigma.col <- paste(item, sex[i], "s", sep = ".")
#         lamda.col <- paste(item, sex[i], "l", sep = ".")        
#         if(is.na(value[i] | is.na(age[i]) | is.na(sex[i]))) return(NA)
#         m <- approx(ref$age,ref[,mu.col], xout=ref$age[i],rule=1)$y
#         l <- approx(ref$age,ref[,lamda.col], xout=ref$age[i],rule=1)$y
#         s <- approx(ref$age,ref[,sigma.col],xout=ref$age[i],rule=1)$y
#         ((value[i]/m)**l-1)/(l*s)
#     })
# }


refs <- data.frame(
    age = res.boys[[1]]$age,
    sprech4.male.m =  rowMeans(Reduce(bind_cols,lapply(res.boys, function(x) data.frame(mu = x$mu)))),
    sprech4.male.s =  rowMeans(Reduce(bind_cols,lapply(res.boys, function(x) data.frame(sigma = x$sigma)))),
    sprech4.male.l = 1,
    sprech4.female.m =  rowMeans(Reduce(bind_cols,lapply(res.girls, function(x) data.frame(mu = x$mu)))),
    sprech4.female.s =  rowMeans(Reduce(bind_cols,lapply(res.girls, function(x) data.frame(sigma = x$sigma)))),
    sprech4.female.l = 1
)

data$sprech4_sds <- sds.bccg(value = data$F0_SPRECH_4,
                        age = data$age,
                        sex = data$sex,
                        item = "sprech4",
                        male = "male",  ## unnoetig weil default
                        female = "female", ## unnoetig weil default
                        ref = refs)

data$year <- year(data$EDAT)

sozdem <- get.data(ldb, "D00177", remove.D.name = T)

data <- merge(data, sozdem,
	      by.x = c("SIC","year"),
	      by.y = c("SIC","JAHR"))

summary(mm <- lm(sprech4_sds ~ sex/SCORE_FAM , data = data))
