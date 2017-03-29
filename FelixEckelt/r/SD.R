library( readxl )
library( reshape2 )
require( dplyr )
library( ggplot2 )
library( gamlss )

setwd( "~/LIFE/myPostGraduates/FelixEckelt/")

################################################### Perzentilenberechnung #######################################  
calc.vals.calc <- function( p, mu, sigma, nu, tau ) {
    
    if( sum( tau < 0 ) > 0 ) {
        
        return( NA ) 
    }
    qBCTo( p = p, mu = mu, sigma = sigma, nu = nu, tau = tau )
}

calc.vals.sds <- function( value, mu, sigma, nu, tau ) {
    
    pBCTo( q = value, mu = mu, sigma = sigma, nu = nu, tau = tau )
}

calc.sds <- function( value, age, sex, ref ) {
    
    mu    <- approx( ref$age, ref$mu,    xout = value )$y
    sigma <- approx( ref$age, ref$sigma, xout = value )$y
    nu    <- approx( ref$age, ref$nu,    xout = value )$y
    tau   <- approx( ref$age, ref$tau,   xout = value )$y
    
    calc.vals.sds( value, mu = mu, sigma = sigma, nu = nu, tau = tau )
}

load( "201607datenfuergrafik.rdata" )

################################################# 99.9 ########################################################

estimates       <- unique( perc.single.boys[  ,c( "which", "age", "mu", "sigma", "nu", "tau" ) ] )
estimates.girls <- unique( perc.single.girls[ ,c( "which", "age", "mu", "sigma", "nu", "tau" ) ] )

estimates$variable <- "perc99_9"
estimates$value <- calc.vals.calc( 
    p     = 0.999, 
    mu    = estimates$mu, 
    sigma = estimates$sigma, 
    nu    = estimates$nu, 
    tau   = estimates$tau )

estimates.sum <- estimates %>% group_by( variable, age ) %>% summarise( value = mean( value ) )

perc.sum.boys <- rbind( perc.sum.boys, estimates.sum ) ## 999 wird an die anderen boys Perzentilen gebunden

estimates.girls$variable <- "perc99_9"
estimates.girls$value <- calc.vals.calc(
    p     = 0.999, 
    mu    = estimates.girls$mu, 
    sigma = estimates.girls$sigma, 
    nu    = estimates.girls$nu, 
    tau   = estimates.girls$tau )

estimates.sum.girls <- estimates.girls %>% group_by( variable, age ) %>% summarise( value = mean( value ) )

perc.sum.girls    <- rbind( perc.sum.girls, estimates.sum.girls ) ## 999 wird an die anderen girls Perzentilen gebunden

perc.single.boys  <- rbind( perc.single.boys, estimates )
perc.single.girls <- rbind( perc.single.girls,  estimates.girls )

##################################################### 99. ########################################################

estimates <- unique(perc.single.boys[,c("which","age","mu","sigma","nu","tau")])
estimates.girls <- unique(perc.single.girls[,c("which","age","mu","sigma","nu","tau")])

estimates$variable <- "perc99"
estimates$value <- calc.vals.calc(p = 0.99, 
                                  mu = estimates$mu, 
                                  sigma = estimates$sigma, 
                                  nu = estimates$nu, 
                                  tau = estimates$tau)

estimates.sum <- estimates %>% group_by(variable, age ) %>%
  summarise(value = mean(value))

perc.sum.boys <- rbind(perc.sum.boys,estimates.sum)

estimates.girls$variable <- "perc99"
estimates.girls$value <- calc.vals.calc(p = 0.99, 
                                        mu = estimates.girls$mu, 
                                        sigma = estimates.girls$sigma, 
                                        nu = estimates.girls$nu, 
                                        tau = estimates.girls$tau)

estimates.sum.girls <- estimates.girls %>% group_by(variable, age ) %>%
  summarise(value = mean(value))

perc.sum.girls <- rbind(perc.sum.girls,estimates.sum.girls)

perc.single.boys <- rbind(perc.single.boys, estimates)
perc.single.girls <- rbind(perc.single.girls, estimates.girls)

load("201612datenfuergrafikmit99.rdata")

####################################################### Daten einladen #######################################
calc <- read_excel("../Diss/Promotion/1. PV 116/Datenbearbeitung/FINAL CT 290416 gel?scht - sex.xlsx")

data_boys <- na.omit(calc[calc$sex=="male" & calc$Alter.Jahre < 18,c("SIC","CT_VALUE","Alter.Jahre","sex")])
data_girls <- na.omit(calc[calc$sex=="female" & calc$Alter.Jahre < 18,c("SIC","CT_VALUE","Alter.Jahre","sex")])

names(data_boys) <- names(data_girls) <- c("SIC","value","AGE","SEX")

perc.single.girls <- perc.single.girls[perc.single.girls$which < 201,]
perc.single.boys <- perc.single.boys[perc.single.boys$which < 201,]

#####################################################################################################
tsh <- read_excel("Raue,UKL,LIFE - MEN,TSH,Asthma auf CT1-Duplikate,Erh?ht_051016.xlsx",sheet = "SD")[,1:25]
tsh$sex <- tsh$gender
tsh$sex[tsh$sex == "f"] <- "female"
tsh$sex[tsh$sex == "m"] <- "male"

tsh <- tsh[,c("nr","sex","birth","Messdatum","CT","ageyear","sgtpo","Substitution")]
########################################################################################################

mytheme <- theme(legend.position = "none",
                 axis.text = element_text(size = 18)  ,
                 axis.title = element_text(size = 18),
                 panel.grid = element_blank())

####################### Grafiken ######################  Schilddr?se   ##########################

labelsdf <- as.data.frame(perc.sum.boys[which(min(perc.sum.boys$age) == perc.sum.boys$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2
labelsdf$value[7] <- labelsdf$value[7] + 0.2 ## 99
labelsdf$value[6] <- labelsdf$value[6] + 0.2 ## 99,9 


labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5,99.9,99)

#
#
#
#

####################### Grafiken ######################   SD GIRLS anpassen   ############ 1. Versuch ##################
# Error: Continuous value supplied to discrete scale


drops <- which( tsh$ageyear > 17.99 )

tsh <- tsh[-drops,]

### verschiedenen Perzentilenzeichen ###
p.tsh.girls <- ggplot(perc.single.girls,aes(x=age,
                                            y=value,
                                            colour=variable,
                                            group=paste(variable,which))) +
  geom_line(alpha=0.1) +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey","grey","grey")) +

### Mittlere Perzentile ###  

   geom_line(data = perc.sum.girls, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black",size = 0.5) +

### Plot ### 
  
  geom_point(data = tsh[tsh$sex == "female",], inherit.aes = F,
             aes(x=ageyear,y=CT, shape = sgtpo, fill = factor(Substitution)),
             colour = "grey20",size=3,stroke = 0.5) +
  
##Shape formen/farben### 
  scale_shape_manual(breaks = c(seq(0,18,by = 0.5)), values=c(21, 22, 23, 24), labels=c(seq(0,18,by = 0.5))) +
  scale_fill_manual(breaks = c(seq(0,18,by = 0.5)), values=c("white","black"), labels=c(seq(0,18,by = 0.5))) +
### Beschriftung ###  
  geom_linerange(data = tsh[tsh$CT == 1 & tsh$sex == "female",], inherit.aes = F,
                 aes(x=ageyear,ymax=CT,ymin = -0.1),
                 colour = "white") +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme
p.tsh.girls

ggsave("girlstsh.png",plot=p.tsh.girls, width = 29, height = 21, units = "cm")
############################################################### 2. Versuch #################################################
# Error: ggplot2 doesn't know how to deal with data of class logical

p.tsh.girls <- ggplot(perc.single.girls,aes(x=age,
                                            y=value,
                                            colour=variable,
                                            group=paste(variable,which))) +
  geom_line(alpha=0.1) +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey","grey","grey")) +
  geom_line(data = perc.sum.girls, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black",size = 0.5) +
  geom_point(data = tsh$sex == "female" & tsh$sgtpo==1, inherit.aes = F,
                        aes(x=ageyear,y=CT, fill=as.factor(Substitution)),
                        colour = "grey20", shape=21, size=3, stroke = 0.5) +
  geom_point(data = tsh$sex == "female" & tsh$sgtpo==2, inherit.aes = F,
                        aes(x=ageyear,y=CT, fill=as.factor(Substitutio)),
                        colour = "grey20", shape=22, size=3, stroke = 0.5) +  
  geom_point(data = tsh$sex == "female" & tsh$sgtpo==3, inherit.aes = F,
                        aes(x=ageyear,y=CT,fill=as.factor(Substitutio)),
                        colour = "grey20", shape=23, size=3, stroke = 0.5) +    
  geom_point(data = tsh$sex == "female" & tsh$sgtpo==4, inherit.aes = F,
                        aes(x=ageyear,y=CT, fill=as.factor(Substitutio)),
                        colour = "grey20", shape=24, size=3, stroke = 0.5) +  
  
  geom_linerange(data = tsh[tsh$CT == 1 & tsh$sex == "female",], inherit.aes = F,
                 aes(x=ageyear,ymax=CT,ymin = -0.1),
                 colour = "white") +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  ## scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,75), breaks = c(seq(0,45,by = 5),
  ##                                                                                            seq(50,70,by = 10))) +    
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme

############################################# 3. Versuch #############################################################
# Error: Continuous value supplied to discrete scale

drops <- which( tsh$ageyear > 17.99 )
tsh <- tsh[-drops,]
p.tsh.girls <- ggplot(perc.single.girls,aes(x=age,
                                            y=value,
                                            colour=variable,
                                            group=paste(variable,which))) +
  geom_line(alpha=0.1) +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey","grey","grey")) +
  geom_line(data = perc.sum.girls, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black",size = 0.5) +
  geom_point(data = tsh[tsh$sex == "female",], inherit.aes = F,
             aes(x=ageyear,y=CT, shape = sgtpo, fill = as.factor(Substitution)), colour = "grey20", size=3,stroke = 0.5) +

  scale_shape_manual(values=c(21, 22,23,24)) +
  scale_fill_manual(values=c("black", "white")) +
  
  
  geom_linerange(data = tsh[tsh$CT == 1 & tsh$sex == "female",], inherit.aes = F,
                 aes(x=ageyear,ymax=CT,ymin = -0.1),
                 colour = "white") +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme

ggsave("girlstsh.png",plot=p.tsh.girls, width = 29, height = 21, units = "cm")

#geom_point(data = m, inherit.aes = F,
 #          aes(x = age, y = CT, shape = sex, fill = as.factor(op)), colour = "grey20", size=3,stroke = 0.5) +
 #      scale_shape_manual(values=c(21, 22)) +
 #      scale_fill_manual(values=c("black", "white")) +
  
  
  
Z<-tsh$sgtpo
Z
