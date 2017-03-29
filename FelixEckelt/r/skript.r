library(readxl)
library(reshape2)
require(dplyr)
library(ggplot2)
  
setwd( "~/LIFE/myPostGraduates/FelixEckelt/" )

##  calc <- read_excel( "../Diss/Promotion/1. PV 116/Datenbearbeitung/FINAL CT 290416 gel?scht - sex.xlsx")
calc <- read_excel( "data/FINAL CT 290416 gelscht - sex.xlsx" )

  ##calc <- merge(tmp,x,by.x = "kind",by.y = "SIC",all.x = T)
  ##calc$CT_VALUE.log <- log(calc$CT_VALUE + 1)

calc.cols.old <- c( "SIC", "CT_VALUE", "Alter.Jahre", "sex" )
calc.cols.new <- c ("SIC", "value",    "AGE",         "SEX" )

data_boys  <- na.omit( calc[ calc$sex == "male"   & calc$Alter.Jahre < 18, calc.cols.old ] )
data_girls <- na.omit( calc[ calc$sex == "female" & calc$Alter.Jahre < 18, calc.cols.old ] )

names( data_boys ) <- names( data_girls ) <- calc.cols.new

load( "data/201607datenfuergrafik.rdata" )

perc.single.girls <- perc.single.girls[ perc.single.girls$which < 201, ]
perc.single.boys  <- perc.single.boys[  perc.single.boys$which < 201,  ]
  
  
  
  ###################### MENII Daten von Raue und UKL ###################################
  menII <- read_excel("../Diss/Promotion/3. MEN II/Raue,UKL,LIFE - MEN,TSH,Asthma auf CT1-Duplikate,Erh?ht_051016.xlsx",sheet = "MEN II")
  menII <- menII[,c("nr","sex","gender","age","op","CT")]
  
  menII$sex[menII$gender == "f"] <- "female"
  menII$sex[menII$gender == "m"] <- "male"
  
  #menII$sex[menII$op == "Tx"] <- "ja"
  #menII$sex[menII$op == ""] <- "nein"
  
  ###################### Schilddruesenkranke von Raue und UKL ###################################
  tsh <- read_excel("../Diss/Promotion/3. MEN II/Raue,UKL,LIFE - MEN,TSH,Asthma auf CT1-Duplikate,Erh?ht_051016.xlsx",sheet = "SD")[,1:24]
  tsh$sex <- tsh$gender
  tsh$sex[tsh$sex == "f"] <- "female"
  tsh$sex[tsh$sex == "m"] <- "male"

  #tsh$Messdatum[is.na(tsh$Messdatum)] <- tsh$Abnahmedatum[is.na(tsh$Messdatum)]

  tsh <- tsh[,c("nr","sex","birth","Messdatum","CT","ageyear")]
  ## tsh$CT[!is.na(tsh$CT) & tsh$CT == "<1,0"] <- "0.5"
  ## tsh$CT <- gsub("<","",tsh$CT)
  ## tsh$CT <- gsub(",",".",tsh$CT)
  ## tsh$CT <- as.numeric(tsh$CT)
  #tsh$age <- as.numeric(tsh$Messdatum - tsh$birth)/365.25
   

  ######################## Asthma aus LIFE ##############################################
  asthma <- read_excel("../Diss/Promotion/3. MEN II/Raue,UKL,LIFE - MEN,TSH,Asthma auf CT1-Duplikate,Erh?ht_051016.xlsx",sheet = "CT Asthma")
  asthma$sex <- asthma$gender
  asthma <- asthma[!is.na(asthma$ASTHMA_MED) & asthma$ASTHMA_MED==1,c("nr","sex","CT","age")]
  asthma <- as.data.frame(asthma)

##  
  save(tsh,menII,asthma,file = "201610krankheiten.rdata")
  
  ############################# CT Follow-Up #############################################
  
  calcfu <- read_excel("FINAL 290416 gender SPSS Doppelte.xls",sheet = "double")[,1:201]
  
  data_girlsfu <- na.omit(calcfu[calcfu$sex=="female" & calcfu$Alter.Jahre < 18,c("SIC","CT_VALUE","Alter.Jahre","sex")])
  data_boysfu <- na.omit(calcfu[calcfu$sex=="male" & calcfu$Alter.Jahre < 18,c("SIC","CT_VALUE","Alter.Jahre","sex")])
  
  
  names(data_boysfu) <- names(data_girlsfu) <- c("SIC","value","AGE","SEX")
  
  
  ########################################################################################
  
  
  load("201610krankheiten.rdata")
  
  
  ####################### Glukokortikoide ################################################

  ## fallen weg
  ## library(readxl)
  ## gluco <- read_excel("fremddata/20160714raue_ukl.xlsx","CT Glukos")
  ## gluco$GLUCO_CORT[is.na(gluco$GLUCO_CORT)] <- 0

  
  ###########################################################################################################
  ####################### Grafiken ######################   Basis   #########################################
  mytheme <- theme(legend.position = "none",
                   axis.text = element_text(size = 18)  ,
                   axis.title = element_text(size = 18),
                   panel.grid = element_blank())
  ####################### Grafiken ######################   ASTHMA  BOYS anlegen   ##########################
  labelsdf <- as.data.frame(perc.sum.boys[which(min(perc.sum.boys$age) == perc.sum.boys$age),c("variable","value")])
  labelsdf$value[1] <- labelsdf$value[1] + 0.2
  labelsdf$value[2] <- labelsdf$value[2] + 0.2
  labelsdf$value[3] <- labelsdf$value[3] + 0.2
  labelsdf$value[4] <- labelsdf$value[4] + 0.2
  labelsdf$value[5] <- labelsdf$value[5] + 0.2

  labelsdf$xval <- 0
  labelsdf$label <- c(2.5,10,50,90,97.5)
  ####################### Grafiken ######################   ASTHMA  BOYS anpassen   ##########################

  p.asthma.boys <-     ggplot(perc.single.boys,aes(x=age,
                              y=value,
                              colour=variable,
                              group=paste(variable,which))) +
      geom_line(alpha=0.1) +                                                            ##Dicke der Schattenlinien## 
      scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
      geom_line(data = perc.sum.boys, inherit.aes = F,
                aes(x=age,y=value,group=variable),                                      ##Dicke der mittleren Perzentilenlinie##
                colour = "black",size = 0.5) +
      geom_point(data = asthma[asthma$sex == "male",], inherit.aes = F,                 ##Punkte##
                aes(x=age,y=CT),
                colour = "grey20",fill = "white",size=3, shape = 22,stroke = 0.5) +    
      geom_linerange(data = asthma[asthma$CT == 1 & asthma$sex == "male",], inherit.aes = F, ##Strich unten am Kreis bei CT=1##
                aes(x=age,ymax=CT,ymin = -0.1),
                colour = "white") +
      annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
               colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +    
      scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,75), breaks = c(seq(0,75,by = 5),
                                                                                             seq(50,70,by = 10))) +
      scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                         breaks = c(seq(0,18,by = 0.5)),
                         labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                    15,"",16,"",17,"",18)) +    
      theme_bw() +
      mytheme

  ggsave("boysasthma.pdf",plot=p.asthma.boys, width = 29, height = 21, units = "cm")
 
  
  ############################################################################################################  
  ####################### Grafiken ######################   ASTHMA  GIRLS anlegen   ########################## 
  labelsdf <- as.data.frame(perc.sum.girls[which(min(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")])
  labelsdf$value[1] <- labelsdf$value[1] + 0.2
  labelsdf$value[2] <- labelsdf$value[2] + 0.2
  labelsdf$value[3] <- labelsdf$value[3] + 0.2
  labelsdf$value[4] <- labelsdf$value[4] + 0.2
  labelsdf$value[5] <- labelsdf$value[5] + 0.2
  
  labelsdf$xval <- 0
  labelsdf$label <- c(2.5,10,50,90,97.5)
  ####################### Grafiken ######################   ASTHMA GIRLS anpassen   ##########################
  
  p.asthma.girls <-     ggplot(perc.single.girls,aes(x=age,
                                                     y=value,
                                                     colour=variable,
                                                     group=paste(variable,which))) +
    geom_line(alpha=0.1) +
    scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
    geom_line(data = perc.sum.girls, inherit.aes = F,
              aes(x=age,y=value,group=variable),
              colour = "black",size = 0.5) +
    geom_point(data = asthma[asthma$sex == "female",], inherit.aes = F,
               aes(x=age,y=CT),
               colour = "grey20",fill = "white",size=3, shape = 21,stroke = 0.5) +    
    geom_linerange(data = asthma[asthma$CT == 1 & asthma$sex == "female",], inherit.aes = F,
                   aes(x=age,ymax=CT,ymin = -0.1),
                   colour = "white") +
    annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
             colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +    
    scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +
    ##scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5),
      ##                                                                                        seq(50,70,by = 10))) +
    scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                       breaks = c(seq(0,18,by = 0.5)),
                       labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                  15,"",16,"",17,"",18)) +    
    theme_bw() +
    mytheme
  
  ggsave("girlsasthma.pdf",plot=p.asthma.girls, width = 29, height = 21, units = "cm")
########################################das obere girl funktioniert. hier nur andere optische Einstellungen########################################################  
  #p.asthma.girls <- ggplot(perc.single.girls,aes(x=age,
  #                                               y=value,
  #                                               colour=variable,
  #                                               group=paste(variable,which))) +
  # geom_line(alpha=0.02) +
  #     ##           scale_colour_manual(values = c("firebrick","orangered","forestgreen","orangered","firebrick")) +
  #  scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
  #  geom_line(data = perc.sum.girls, inherit.aes = F,
  #            aes(x=age,y=value,group=variable),
  #            colour = "black", size = 0.5) +
  #  geom_point(data = asthma[asthma$sex == "female",], inherit.aes = F,
  #             aes(x=age,y=CT),
  #             colour = "grey20",fill = "white",size=1, shape = 21,stroke = 0.5) +
  #  geom_linerange(data = asthma[asthma$CT == 0.5 & asthma$sex == "female",], inherit.aes = F,
  #                 aes(x=age,ymax=CT,ymin = -0.1),
  #                 colour = "white") +
  #  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
  #           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  #  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,45), breaks = c(seq(0,45,by = 5))) +        
  #  ##scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,75), breaks = c(seq(0,45,by = 5),
  #     ##                                                                                  seq(50,70,by = 10))) +    
  #  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
  #                     breaks = c(seq(0,18,by = 0.5)),
  #                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
  #                                15,"",16,"",17,"",18)) +    
  #  theme_bw() +
  #  mytheme
  #
  #ggsave("girlsasthma.pdf",plot=p.asthma.girls, width = 29, height = 21, units = "cm")

  
  ############################################################################################################  
  ####################### Grafiken ######################   SD  BOYS anlegen   ########################## 
  labelsdf <- as.data.frame(perc.sum.boys[which(min(perc.sum.boys$age) == perc.sum.boys$age),c("variable","value")])
  labelsdf$value[1] <- labelsdf$value[1] + 0.2
  labelsdf$value[2] <- labelsdf$value[2] + 0.2
  labelsdf$value[3] <- labelsdf$value[3] + 0.2
  labelsdf$value[4] <- labelsdf$value[4] + 0.2
  labelsdf$value[5] <- labelsdf$value[5] + 0.2
  
  labelsdf$xval <- 0
  labelsdf$label <- c(2.5,10,50,90,97.5)
  ####################### Grafiken ######################   SD BOYS anpassen   ##########################
 
   p.tsh.boys <- ggplot(perc.single.boys,aes(x=age,
                              y=value,
                              colour=variable,
                              group=paste(variable,which))) +
      geom_line(alpha=0.1) +
      ##            scale_colour_manual(values = c("firebrick","orangered","forestgreen","orangered","firebrick")) +
      scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
      geom_line(data = perc.sum.boys, inherit.aes = F,
                aes(x=age,y=value,group=variable),
                colour = "black",size = 0.5) +
      geom_point(data = tsh[tsh$sex == "male",], inherit.aes = F,
                aes(x=ageyear,y=CT),
                colour = "grey20",fill = "white",size=3, shape = 22,stroke = 0.5) +
      geom_linerange(data = tsh[tsh$CT == 1 & tsh$sex == "male",], inherit.aes = F,
                aes(x=age,ymax=CT,ymin = -0.1),
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


  ggsave("boystsh.pdf",plot=p.tsh.boys, width = 29, height = 21, units = "cm")

  
  ############################################################################################################  
  ####################### Grafiken ######################   SD  GIRLS anlegen   ############################## 
  labelsdf <- as.data.frame(perc.sum.girls[which(min(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")])
  labelsdf$value[1] <- labelsdf$value[1] + 0.2
  labelsdf$value[2] <- labelsdf$value[2] + 0.2
  labelsdf$value[3] <- labelsdf$value[3] + 0.2
  labelsdf$value[4] <- labelsdf$value[4] + 0.2
  labelsdf$value[5] <- labelsdf$value[5] + 0.2

  labelsdf$xval <- 0
  labelsdf$label <- c(2.5,10,50,90,97.5)
  ####################### Grafiken ######################   SD GIRLS anpassen   ##############################
  
    drops <- which( tsh$ageyear > 17.99 )
    tsh <- tsh[-drops,]
   p.tsh.girls <- ggplot(perc.single.girls,aes(x=age,
                              y=value,
                              colour=variable,
                              group=paste(variable,which))) +
      geom_line(alpha=0.1) +
      ##            scale_colour_manual(values = c("firebrick","orangered","forestgreen","orangered","firebrick")) +
      scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
      geom_line(data = perc.sum.girls, inherit.aes = F,
                aes(x=age,y=value,group=variable),
                colour = "black",size = 0.5) +
      geom_point(data = tsh[tsh$sex == "female",], inherit.aes = F,
                aes(x=ageyear,y=CT),
                colour = "grey20",fill = "white",size=3, shape = 21,stroke = 0.5) +
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

  ggsave("girlstsh.pdf",plot=p.tsh.girls, width = 29, height = 21, units = "cm")

  ##############################################################################################################  
  ####################### Grafiken ######################   MENII  BOYS + GIRLS anlegen   ##############################   
  ####################### Grafiken ######################   MEN II BOYS + GIRLS anpassen   ############################## 
  
  #p.men <-  ggplot(menII,aes(x = age, y = CT, shape = sex, fill = op)) +
   # geom_hline(yintercept = 1) +
   # geom_point(data = menII[menII$sex == "female",], inherit.aes = F,
   #            aes(x=age,y=CT),
  #             colour = "black",fill = "white",size=3, shape = 21,stroke = 0.5) + 
  #  geom_point(data = menII[menII$sex == "male",], inherit.aes = F,
 #              aes(x=age,y=CT),
  #             colour = "black",fill = "white",size=3, shape = 22,stroke = 0.5) +
  #  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,70), breaks = c(seq(0,70,by = 5))) +        
    ## scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,75), breaks = c(seq(0,45,by = 5),
    ##                                                                                            seq(50,70,by = 10))) +    
  #  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
  #                     breaks = c(seq(0,18,by = 0.5)),
  #                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
  #                                15,"",16,"",17,"",18)) +    
  ##  theme_bw() +
  #  mytheme

#p.men
#ggsave("MENII.pdf",plot=p.men, width = 29, height = 21, units = "cm")
  
  menII <- read_excel("../Diss/Promotion/3. MEN II/Raue,UKL,LIFE - MEN,TSH,Asthma auf CT1-Duplikate,Erh?ht_051016.xlsx",sheet = "MEN II")
  menII <- menII[,c("nr","sex","gender","age","op","CT")]
  
  menII$sex[menII$gender == "f"] <- "female"
  menII$sex[menII$gender == "m"] <- "male"
  
  
  
  
  
  m<-menII
  m$CT[ between( m$CT, .95, 1.05 ) ] <- 0
  ##menII$CT[menII$CT < 1 ] <- -0.5
  p.men <-  ggplot(m,aes(x = age, y = CT, shape = sex, fill = as.factor(op))) +
   geom_hline(yintercept = 1) +
  geom_point(size=3) + 
    scale_shape_manual(values=c(21, 22)) +
    scale_fill_manual(values=c("yellow", "black")) +
   
    
   scale_y_continuous(expression(paste("Calcitonin (ng/L)")), c(-5,0,5,10,15,20,25,30,35,40,45,50,60,70,75,80)) +    
    scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                       breaks = c(seq(0,18,by = 0.5)),
                      labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                  15,"",16,"",17,"",18)) +    
    theme_bw()+
     mytheme
    
  
   p.men
   ggsave("MENII.pdf",plot=p.men, width = 29, height = 21, units = "cm")
  


##############################################################################################################  
####################### Grafiken ######################   MENII  BOYS + GIRLS anlegen   ##############################   
####################### Grafiken ######################   MEN II BOYS + GIRLS anpassen   ############################## 
p.men <-  ggplot(menII,aes(x = age, y = CT, shape = sex)) +
  geom_hline(yintercept = 1) +
  geom_point(data = menII[menII$sex == "female",], inherit.aes = F,
             aes(x=age,y=CT),
             colour = "black",fill = "white",size=3, shape = 21,stroke = 0.5) + 
  geom_point(data = menII[menII$sex == "male",], inherit.aes = F,
             aes(x=age,y=CT, shape=1),
             colour = "black",fill = "white",size=3, shape = 22,stroke = 0.5) +
  
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,70), breaks = c(seq(0,70,by = 5))) +        
  ## scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,75), breaks = c(seq(0,45,by = 5),
  ##                                                                                            seq(50,70,by = 10))) +    
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18))    +
    
  theme_bw()+
    mytheme
  
  
p.men

ggsave("MENII.pdf",plot=p.men, width = 29, height = 21, units = "cm")

############################################################################################################  
####################### Grafiken ######################   CT BOYS Cloud anlegen   ################################
labelsdf <- as.data.frame(perc.sum.boys[which(min(perc.sum.boys$age) == perc.sum.boys$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2

labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5)
####################### Grafiken ######################   CT BOYS Cloud anpassen   #######################

  p.cloud.boys <-               ggplot(perc.single.boys,aes(x=age,
                                       y=value,
                                       colour=variable,
                                       group=paste(variable,which))) +
  geom_line(alpha=0.1)  +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +  
  geom_line(data = perc.sum.boys, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black" ,size = 0.5) +
  #geom_point(data = perc.sum.boys, inherit.aes = F,
            # aes(x=age ,y=value,group=variable),
            #colour = "black",size = 0.5) + 
  geom_point(data = data_boys[data_boys$SEX == "male",], inherit.aes = F,
             aes(x=AGE,y=value),
             colour = "grey20",fill = "white",size=3, shape = 22,stroke = 0.5) +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme
ggsave("cloudboys.pdf",plot=p.cloud.boys, width = 29, height = 21, units = "cm")

  
ggplot(data_boys,aes(x=AGE ,y= value, group = SIC)) +
  geom_line()




############################################################################################################  
####################### Grafiken ######################   CT GIRLS Cloud anlegen   ################################
labelsdf <- as.data.frame(perc.sum.girls[which(min(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2

labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5)
####################### Grafiken ######################   CT GIRLS Cloud anpassen   #######################

  p.cloud.girls <-               ggplot(perc.single.girls,aes(x=age,
                                        y=value,
                                        colour=variable,
                                        group=paste(variable,which))) +
  geom_line(alpha=0.1)  +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +  
  geom_line(data = perc.sum.girls, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black" ,size = 0.5) +
  #geom_point(data = perc.sum.girls, inherit.aes = F,
  # aes(x=age ,y=value,group=variable),
  #colour = "black",size = 0.5) + 
  geom_point(data = data_girls[data_girls$SEX == "female",], inherit.aes = F,
             aes(x=AGE,y=value),
             colour = "grey20",fill = "white",size=3, shape = 21,stroke = 0.5) +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme
ggsave("cloudgirls.pdf",plot=p.cloud.girls, width = 29, height = 21, units = "cm")



ggplot(data_girls,aes(x=AGE ,y= value, group = SIC)) +
  geom_line()

########################################################################################################################  
####################### Grafiken ######################   CT BOYS longitudinal anlegen   ################################
labelsdf <- as.data.frame(perc.sum.boys[which(min(perc.sum.boys$age) == perc.sum.boys$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2

labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5)
####################### Grafiken ######################   CT BOYS longitudinal anpassen   #######################

p.fu.boys <-               ggplot(perc.single.boys,aes(x=age,
                                                          y=value,
                                                          colour=variable,
                                                          group=paste(variable,which))) +
  geom_line(alpha=0.1)  +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +  
  geom_line(data = perc.sum.boys, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black" ,size = 0.5) +
  #geom_point(data = perc.sum.boys, inherit.aes = F,
  # aes(x=age ,y=value,group=variable),
  #colour = "black",size = 0.5) + 
  geom_line(data = data_boysfu, inherit.aes = F,
             aes(x=AGE,y=value, group = SIC),
             colour = "grey40") +
  geom_point (data = data_boysfu, inherit.aes = F,
              aes(x=AGE,y=value),
              colour = "grey20",fill = "white",size=3, shape = 22,stroke = 0.5) +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme
ggsave("followupboys.pdf",plot=p.fu.boys, width = 29, height = 21, units = "cm")


  ggplot(data_boys,aes(x=AGE ,y= value, group = SIC)) +
  geom_line()
  
############################################################################################################  
####################### Grafiken ######################   CT GIRLS longitudinal anlegen   ################################
labelsdf <- as.data.frame(perc.sum.girls[which(min(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2

labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5)
####################### Grafiken ######################   CT GIRLS longitudinal anpassen   #######################
p.fu.girls <-               ggplot(perc.single.girls,aes(x=age,
                                                       y=value,
                                                       colour=variable,
                                                       group=paste(variable,which))) +
  geom_line(alpha=0.1)  +
  scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +  
  geom_line(data = perc.sum.girls, inherit.aes = F,
            aes(x=age,y=value,group=variable),
            colour = "black" ,size = 0.5) +
  #geom_point(data = perc.sum.boys, inherit.aes = F,
  # aes(x=age ,y=value,group=variable),
  #colour = "black",size = 0.5) + 
  geom_line(data = data_girlsfu, inherit.aes = F,
            aes(x=AGE,y=value, group = SIC),
            colour = "grey40") +
  geom_point (data = data_girlsfu, inherit.aes = F,
              aes(x=AGE,y=value),
              colour = "grey20",fill = "white",size=3, shape = 21,stroke = 0.5) +
  annotate("text",x=-0.02,y=labelsdf$value,label=paste0("P[",labelsdf$label,"]"),
           colour = "black", parse = T, vjust = 0.8, hjust = 1,size = 5) +
  scale_y_continuous(expression(paste("Calcitonin (ng/L)")),limits = c(-0.1,50), breaks = c(seq(0,50,by = 5))) +        
  scale_x_continuous("Age (years)",limits = c(-0.5,18.2), expand = c(0.02,0),
                     breaks = c(seq(0,18,by = 0.5)),
                     labels = c(0,"",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14,"",
                                15,"",16,"",17,"",18)) +    
  theme_bw() +
  mytheme
ggsave("followupgirls.pdf",plot=p.fu.girls, width = 29, height = 21, units = "cm")






#

##

###

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
####################### Grafiken ######################   SD  GIRLS anlegen   #########################################################################################
labelsdf <- as.data.frame(perc.sum.girls[which(min(perc.sum.girls$age) == perc.sum.girls$age),c("variable","value")])
labelsdf$value[1] <- labelsdf$value[1] + 0.2
labelsdf$value[2] <- labelsdf$value[2] + 0.2
labelsdf$value[3] <- labelsdf$value[3] + 0.2
labelsdf$value[4] <- labelsdf$value[4] + 0.2
labelsdf$value[5] <- labelsdf$value[5] + 0.2

labelsdf$xval <- 0
labelsdf$label <- c(2.5,10,50,90,97.5)  


ggplot(perc.sum.boys,aes(x=age,
                              y=value,
                              group=variable)) +
    geom_line(colour="black")  +
    geom_point(data = data_boys, inherit.aes = F,
               aes(x=AGE ,y= value),
               colour = "grey20",fill = "white",size=1, shape = 21,stroke = 0.5) 
  
  
  ggplot(data_boys,aes(x=AGE ,y= value, group = SIC)) +
    geom_line()
  
  
  ###############################################################################################################  
  ####################### Grafiken ######################   MENII  GIRLS anlegen   ##############################   