

### Ausschluss

##SD_ALLG
sics.raus <- all$SIC[!is.na(all$D00127_SD_ALLG) & all$D00127_SD_ALLG == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
## Anzahl der betroffenen Zeilen
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP","EDAT")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_SD_HYPER
sics.raus <- all$SIC[!is.na(all$D00127_SD_HYPER) & all$D00127_SD_HYPER == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP","EDAT")]

nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP","EDAT")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_SD_HYPO
sics.raus <- all$SIC[!is.na(all$D00127_SD_HYPO) & all$D00127_SD_HYPO == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]

nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_DMI
sics.raus <- all$SIC[!is.na(all$D00127_DM1) & all$D00127_DM1 == 1 ]
##all[all$SIC %in% sics.raus,]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])


nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_DM2
sics.raus <- all$SIC[!is.na(all$D00127_DM2) & all$D00127_DM2 == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP","EDAT")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_NEPHRO
sics.raus <- all$SIC[!is.na(all$D00127_NEPHRO) & all$D00127_NEPHRO == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_ANGIO
sics.raus <- all$SIC[!is.na(all$D00127_ANGIO) & all$D00127_ANGIO == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_DEPRES
sics.raus <- all$SIC[!is.na(all$D00127_DEPRES) & all$D00127_DEPRES == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus),]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_MUSKEL
sics.raus <- all$SIC[!is.na(all$D00127_MUSKEL) & all$D00127_MUSKEL == 1]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_VITD
sics.raus <- all$SIC[!is.na(all$D00127_VITD) & all$D00127_VITD == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])


nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_FRUEGEB
## sics.raus <- all$SIC[!is.na(all$D00127_FRUEHGEB) & all$D00127_FRUEHGEB == 1 ]
## all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##D00127_NGSCREEN
##sics.raus <- all$SIC[!is.na(all$D00127_NGSCREEN) & all$D00127_NGSCREEN == 1 ]
##all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##D00127_BLUT
sics.raus <- all$SIC[!is.na(all$D00127_BLUT) & all$D00127_BLUT == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00127_GERIN
sics.raus <- all$SIC[!is.na(all$D00127_GERIN) & all$D00127_GERIN == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##HNO
##sics.raus <- all$SIC[!is.na(all$HNO) & all$HNO == 1]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##MOE
##sics.raus <- all$SIC[!is.na(all$MOE) & all$MOE == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##INFEKT
##sics.raus <- all$SIC[!is.na(all$INFEKT) & all$INFEKT == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##NEUROL
##sics.raus <- all$SIC[!is.na(all$NEUROL) & all$NEUROL == 1 ]
##all[all$SIC %in% sics.raus,]
##nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##KOPFSCH
##sics.raus <- all$SIC[!is.na(all$KOPFSCH) & all$KOPFSCH == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##D00127_SUCHT
sics.raus <- all$SIC[!is.na(all$D00127_SUCHT) & all$D00127_SUCHT == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##SPRACH
##sics.raus <- all$SIC[!is.na(all$SPRACH) & all$SPRACH == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##HERNIE
##sics.raus <- all$SIC[!is.na(all$HERNIE) & all$HERNIE == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##KH
##sics.raus <- all$SIC[!is.na(all$KH) & all$KH == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##SONST
##sics.raus <- all$SIC[!is.na(all$SONST) & all$SONST == 1 ]
##all[all$SIC %in% sics.raus,]

##nrow(all)
##all <- all[!(all$SIC %in% sics.raus), ]
##nrow(all)

##Ausschluss Medikamente

##D00129_LTHYROX
sics.raus <- all$SIC[!is.na(all$D00129_LTHYROX) & all$D00129_LTHYROX == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))


##D00129_GLUCO_CORT
sics.raus <- all$SIC[!is.na(all$D00129_GLUCO_CORT) & all$D00129_GLUCO_CORT == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus),]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00129_METFORMIN
sics.raus <- all$SIC[!is.na(all$D00129_METFORMIN) & all$D00129_METFORMIN == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))


##Kontrazept
##sics.raus <- all$SIC[!is.na(all$KONTRAZEPT) & all$KONTRAZEPT == 1 ]
##all[all$SIC %in% sics.raus,]

## D00129_WACHSTUM
sics.raus <- all$SIC[!is.na(all$D00129_WACHSTUM) & all$D00129_WACHSTUM == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00129_DESMOPRESS
sics.raus <- all$SIC[!is.na(all$D00129_DESMOPRESS) & all$D00129_DESMOPRESS == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00129_ASTHMA
sics.raus <- all$SIC[!is.na(all$D00129_ASTHMA) & all$D00129_ASTHMA == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])


nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))


##D00129_HORMONE
sics.raus <- all$SIC[!is.na(all$D00129_HORMONE) & all$D00129_HORMONE == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##D00129_INSULIN
sics.raus <- all$SIC[!is.na(all$D00129_INSULIN) & all$D00129_INSULIN == 1 ]
all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")]
nrow(all[all$SIC %in% sics.raus,c("SIC","SCI_GROUP")])

nrow(all)
all <- all[!(all$SIC %in% sics.raus), ]
nrow(all)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##Ausreißer Parameter

##TSH T00491_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00491_F0008))+
  geom_point(alpha = 0.3)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00491_F0008) & all$T00491_F0008 > 14.5, c("SIC","SCI_GROUP","T00491_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00491_F0008))+
  geom_point(alpha = 0.3)


##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##HbA1c T00440_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00440_F0008))+
  geom_point(alpha = 0.3)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00440_F0008) & all$T00440_F0008 > 6.00, c("SIC","SCI_GROUP","T00440_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus


##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))


##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00440_F0008))+
  geom_point(alpha = 0.3)

##PTH T00480_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00480_F0008))+
  geom_point(alpha = 0.3)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00480_F0008) & all$T00480_F0008 > 12.00, c("SIC","SCI_GROUP","T00480_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00480_F0008))+
  geom_point(alpha = 0.3)


##Anti-TSHR T00484_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00484_F0008))+
  geom_point(alpha = 0.3)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00484_F0008) & all$T00484_F0008 > 5.00, c("SIC","SCI_GROUP","T00484_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00484_F0008))+
  geom_point(alpha = 0.3)

##FT4 T00493_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00493_F0008))+
  geom_point(alpha = 0.3)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00493_F0008) & all$T00493_F0008 > 25.00, c("SIC","SCI_GROUP","T00493_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00493_F0008))+
  geom_point(alpha = 0.3)

###einzelner Wert raus: D00077_PH
table(all$D00077_PH)
all$D00077_PH[!is.na(all$D00077_PH) & all$D00077_PH == 96 ]<-NA
all$D00077_PH[!is.na(all$D00077_PH) & all$D00077_PH == 96 ]
table(all$D00077_PH)

###einzelner Wert raus: D00077_G
table(all$D00077_G)
all$D00077_G[!is.na(all$D00077_G) & all$D00077_G == 96 ]<-NA
all$D00077_G[!is.na(all$D00077_G) & all$D00077_G == 96 ]
table(all$D00077_G)

###einzelner Wert raus: D00077_B
table(all$D00077_B)
all$D00077_B[!is.na(all$D00077_B) & all$D00077_B == 96 ]<-NA
all$D00077_B[!is.na(all$D00077_B) & all$D00077_B == 96 ]
table(all$D00077_B)

###einzelner Wert raus: D00077_MENARCHE
table(all$D00077_MENARCHE)
all$D00077_MENARCHE[!is.na(all$D00077_MENARCHE) & all$D00077_MENARCHE == 97 ]<-NA
all$D00077_MENARCHE[!is.na(all$D00077_MENARCHE) & all$D00077_MENARCHE == 97 ]
table(all$D00077_MENARCHE)


## Blutbild

##Basophile T00505_F0008

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0008))+
  geom_point(alpha = 0.3)

table(all$T00505_F0008)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0008) & all$T00505_F0008 > 3.00, c("SIC","SCI_GROUP","T00505_F0008")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##Basophile_abs. T00505_F0016

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0016))+
  geom_point(alpha = 0.3)

table(all$T00505_F0016)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0016) & all$T00505_F0016 > 0.18, c("SIC","SCI_GROUP","T00505_F0016")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0016))+
  geom_point(alpha = 0.3)

##Eosinophile T00505_F0024

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0024))+
  geom_point(alpha = 0.3)

table(all$T00505_F0024)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0024) & all$T00505_F0024 > 26, c("SIC","SCI_GROUP","T00505_F0024")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0024))+
  geom_point(alpha = 0.3)

##Erys T00505_F0040

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0040))+
  geom_point(alpha = 0.3)

table(all$T00505_F0040)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0040) & all$T00505_F0040 > 8, c("SIC","SCI_GROUP","T00505_F0040")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0040))+
  geom_point(alpha = 0.3)


##Hämatokrit T00505_F0048

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0048))+
  geom_point(alpha = 0.3)

table(all$T00505_F0048)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0048) & all$T00505_F0048 > 0.5, c("SIC","SCI_GROUP","T00505_F0048")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0048))+
  geom_point(alpha = 0.3)


##Leucozyten T00505_F0072

##einfache graphische Darstellung
##ggplot(all, aes(x = age, y = T00505_F0072))+
  ##geom_point(alpha = 0.3)

##table(all$T00505_F0072)

###ganze SIC raus
##sics.raus <- all[!is.na(all$T00505_F0072) & all$T00505_F0072 > 18, c("SIC","SCI_GROUP","T00505_F0072")]

##nrow(all)
##all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
##nrow(all)

##sics.raus

##einfache graphische Darstellung
##ggplot(all, aes(x = age, y = T00505_F0072))+
  ##geom_point(alpha = 0.3)



##mittl. Ery Vol.. T00505_F0128

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0128))+
  geom_point(alpha = 0.3)

table(all$T00505_F0128)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0128) & all$T00505_F0128 < 60, c("SIC","SCI_GROUP","T00505_F0128")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0128))+
  geom_point(alpha = 0.3)


## Neutrophile abs.  T00505_F0168

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0168))+
  geom_point(alpha = 0.3)

table(all$T00505_F0168)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0168) & all$T00505_F0168 > 14.0, c("SIC","SCI_GROUP","T00505_F0168")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0168))+
  geom_point(alpha = 0.3)


## Retikulozyten.  T00505_F0176

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0176))+
  geom_point(alpha = 0.3)

table(all$T00505_F0176)

###ganze SIC raus
sics.raus <- all[!is.na(all$T00505_F0176) & all$T00505_F0176 > 40.0, c("SIC","SCI_GROUP","T00505_F0176")]

nrow(all)
all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
nrow(all)

sics.raus

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

##einfache graphische Darstellung
ggplot(all, aes(x = age, y = T00505_F0176))+
  geom_point(alpha = 0.3)


## Thrombozyten  T00505_F0184

##einfache graphische Darstellung
##ggplot(all, aes(x = age, y = T00505_F0184))+
  ##geom_point(alpha = 0.3)

##table(all$T00505_F0184)

###ganze SIC raus
##sics.raus <- all[!is.na(all$T00505_F0184) & all$T00505_F0184 > 700.0, c("SIC","SCI_GROUP","T00505_F0184")]

##nrow(all)
##all <- remove.per.sic.sgroup(all, raus = sics.raus, sic = "SIC", zp = "SCI_GROUP")
##nrow(all)

##sics.raus

##einfache graphische Darstellung
##ggplot(all, aes(x = age, y = T00505_F0184))+
  ##geom_point(alpha = 0.3)

###einzelner Wert raus: FT3 T00492_F0008
table(all$T00492_F0008)
all$T00492_F0008[!is.na(all$T00492_F0008) & all$T00492_F0008 == 16.44 ]<-NA
all$T00492_F0008[!is.na(all$T00492_F0008) & all$T00492_F0008 == 16.44 ]
table(all$T00492_F0008)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

###einzelner Wert raus: Calcium T00477_F0008
table(all$T00477_F0008)
all$T00477_F0008[!is.na(all$T00477_F0008) & all$T00477_F0008 < 1.5 ]<-NA
all$T00477_F0008[!is.na(all$T00477_F0008) & all$T00477_F0008 < 1.5 ]
table(all$T00477_F0008)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

###einzelner Wert raus: Phosphat T00476_F0008
table(all$T00476_F0008)
all$T00476_F0008[!is.na(all$T00476_F0008) & all$T00476_F0008 > 4 ]<-NA
all$T00476_F0008[!is.na(all$T00476_F0008) & all$T00476_F0008 > 4 ]
table(all$T00476_F0008)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

###einzelner Wert raus: alkal. Phosphatase T00475_F0008
table(all$T00475_F0008)
all$T00475_F0008[!is.na(all$T00475_F0008) & all$T00475_F0008 > 40 ]<-NA
all$T00475_F0008[!is.na(all$T00475_F0008) & all$T00475_F0008 > 40 ]
table(all$T00475_F0008)

##Anzahl Probanden (??)
length(unique(all$SIC[!is.na(all$SIC)]))

