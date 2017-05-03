library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)

setwd( "~/LIFE/github-tpeschel/R/JulianeWilz/data/old/jwilz_altdaten/")

getwd()

daten <- read_excel("FINAL CT 290416 gelöscht - sex.xlsx")

names(daten) <- make.names(names(daten))


daten <- daten[!is.na(daten$Alter.Jahre),]
daten$ag <- factor(floor(daten$Alter.Jahre))
daten$CT_VALUE[!is.na(daten$CT_VALUE) & daten$CT_VALUE > 100] <- NA
daten$CT_VALUE_Z <- ( daten$CT_VALUE - mean( daten$CT_VALUE, na.rm = T ) ) / sd( daten$CT_VALUE, na.rm = T )
daten$CALCIUM_VALUE_Z <- ( daten$CALCIUM_VALUE - mean( daten$CALCIUM_VALUE, na.rm = T ) ) / sd( daten$CALCIUM_VALUE, na.rm = T )


ggplot( 
    daten,
    aes( CT_VALUE_Z ) ) +
    geom_density( )

ggplot( 
    daten,
    aes( CALCIUM_VALUE_Z, CT_VALUE_Z ) ) +
    geom_point( ) +
    geom_smooth( )

ggplot( 
    daten,
    aes( CALCIUM_VALUE, log( CT_VALUE ), col = sex ) ) +
    geom_point( ) +
    geom_smooth( method = "lm" )

mean( daten$CT_VALUE_Z )

ggplot(daten, aes(x = ag, y = CT_VALUE_Z, fill = sex)) +
  geom_boxplot()

ggplot(daten, aes(x = ag, y = CT_VALUE, fill = sex)) +
  geom_boxplot()

ggpairs(
    daten[,c("CT_VALUE_Z", "CALCIUM_VALUE_Z","sex")],
    mapping = aes(colour = sex) )
        

ggpairs(daten[,c("CT_VALUE", "BETA_CTX_VALUE", "PTH_VALUE", "P1NP_VALUE")])

ggpairs(daten[between(daten$Alter.Jahre,0,2),c("CT_VALUE", "VDBP_VALUE", "IGF1_VALUE", "X125VITD_VALUE", "X25VITD_VALUE","sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5)

daten02 <- daten[between(daten$Alter.Jahre,0,2),]

daten01 <- daten[between(daten$Alter.Jahre, 0,1),]

ggpairs(daten02[,c("CT_VALUE","OSTEOCALCIN_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und Osteocalcin (0-2 Jahre)")

ggpairs(daten02[,c("CT_VALUE","OSTEOCALCIN_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("0- 2-Jährige")

ggpairs(daten02[,c("CT_VALUE","ALK_PHOSP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5)+ ggtitle ("Korrelation von Calcitonin und alkalischer Phosphatase (0-2 Jahre)")

ggpairs(daten02[,c("CT_VALUE","PHOSPHAT_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und Phosphat (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","CALCIUM_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und Calcium (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","BETA_CTX_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und ßCTX (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","PTH_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und Parathormon (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","P1NP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und PINP (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","X25VITD_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und 25OH-Vitamin D (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","VDBP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und VDBP (0-2 Jahre)n")

ggpairs(daten02 [,c("CT_VALUE","IGF1_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und IGF-1 (0-2 Jahre)")

ggpairs(daten02 [,c("CT_VALUE","X125VITD_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und 1,25 Vitamin D (0-2 Jahre)")

ggpairs(daten01[,c("CT_VALUE","P1NP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5) + ggtitle ("Korrelation von Calcitonin und Osteocalcin (0-2 Jahre)")


ggpairs(daten[between(daten$Alter.Jahre,17,18),c("CT_VALUE", "OSTEOCALCIN_VALUE", "IGF1_VALUE", "P1NP_VALUE", "ALK_PHOSP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5)

ggpairs(daten[between(daten$Alter.Jahre,8,9),c("CT_VALUE", "BETA_CTX_VALUE", "PTH_VALUE", "VDBP_VALUE", "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5)

ggpairs(daten[between(daten$Alter.Jahre,8,9),c("CT_VALUE", "CALCIUM_VALUE", "PHOSPHAT_VALUE", "X25VITD_VALUE", "X125VITD_VALUE",  "sex")],
        mapping = aes(colour = sex),
        lower = list(continuous = wrap(c("points","smooth"),alpha = 0.4,size = 0.5)),
        shape = 3, size = 0.5)


cor(daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, use ="complete.obs", method = "pearson")
cor.test (daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, method = "pearson")
cor(daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, use ="complete.obs", method = "spearman")
cor(daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, use ="pairwise.complete.obs", method = "pearson")
rcorr(daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, type = "pearson")
rcorr(daten02$CT_VALUE, daten02$OSTEOCALCIN_VALUE, type = c("pearson", "spearman"))



ggpairs(daten[,c("CT_VALUE", "IGF1_VALUE", "AGE_D00040")])     


ggpairs(daten[,c("CT_VALUE", "AGE_D00040")])



