# terkepek_masodikszakasz_valtozok_xxxxxx.R után futtatandó
#tmaptools::palette_explorer()
# paletták felfedezéséhez
# http://127.0.0.1:4692/

par(mgp=c(1,.5,0))
# # legend rendberakáshoz majd
# tm_add_legend(type = "fill", 
#               labels = c("0", "1 to 20", "21 to 40", "41 to 60", "61 to 80", "81 to 100"),
#               col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
#               border.lwd = 0.5,
#               title = "Count")
# tm_add_legend(type="symbol",
#               labels=c("9-11,9","12-13,9","14-15,9","16-17,9","18-21"),
#               col = c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404"),
#               border.lwd=0.5,
#               title="")

### lakosságszámhoz viszonyított halálozási térkép
summary(JT@data$meghaltKSH2018_lakos)
hist(JT@data$meghaltKSH2018_lakos,breaks=30)

MHla <- tm_shape(JT) +
  tm_fill("meghaltKSH2018_lakosR", 
          title="", 
          breaks=c(9,12,14,16,18,21),
          palette = c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")) +
  tm_borders("white") +
  tm_text("meghaltKSH2018_lakosRL",size = 0.7) +
  tm_layout(frame = "white",
            legend.show = T,
            legend.position=c("right","bottom"),
            legend.format=list(text.separator=c("-"))) 


MHla

# Ápolt / meghaltKSH: meghaltak számához viszonyítva hány % került be otthoni hospice ellátásb
summary(JT@data$apolt_meghaltKSH2018)
hist(JT@data$apolt_meghaltKSH2018,breaks=30)
table(round(JT@data$apolt_meghaltKSH2018,3),useNA = "always")
# kategorizálva
# 0,1-2,3-5,5-7,7-10,15

APmh <-tm_shape(JT) +
  tm_fill("apolt_meghaltKSH2018R", title="", breaks=c(0,1,3,5,7,10,15),
          palette = "Greens",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("apolt_meghaltKSH2018RL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
APmh

# ezzel derítettem ki a <1 rejtélyt (miért színeződött kétféleképpen)
# JT@data %>% dplyr::filter(jaras_nev=="Kalocsai járás"|jaras_nev=="Kunszentmiklósi járás") %>%
#    dplyr::select(jaras_nev,apolt_meghaltKSH2018,apolt_meghaltKSH2018R,apolt_meghaltKSH2018RL)

  
# Alatt_elhunyt / meghaltKSH: otthoni hospice ellátásban volt
# halálának idején az elhunytak között (%)
summary(JT@data$alatt_elhunyt_meghaltKSH2018)
hist(JT@data$alatt_elhunyt_meghaltKSH2018,breaks=20)
AEmh <-tm_shape(JT) +
  tm_fill("alatt_elhunyt_meghaltKSH2018R", title="", breaks=c(0,0.01,1,2,3,4,7),
          palette = "Greens",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("alatt_elhunyt_meghaltKSH2018RL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
AEmh

#melyek azok, akik
JT@data %>% dplyr::filter(alatt_elhunyt_meghaltKSH2018==0& apolt!=0) %>% 
  dplyr::select(jaras_nev,alatt_elhunyt_meghaltKSH2018,elhunyt2,
                apolt,opiatossz_apolt,misuse,otthonhaltmegarany,apolasinap_apolt)

#otthonukban elhunytak aránya az összes elhunyt közül 
# egyedi színek a 60 alatti 
summary(JT@data$otthonhaltmegarany)
hist(JT@data$otthonhaltmegarany,breaks=20)

OTmh <-tm_shape(JT) +
  tm_fill("otthonhaltmegaranyR", title="", breaks=c(0,60,100),
          palette = c("red","darkgreen"),colorNA="white") +tm_borders("gray") +
  tm_text("otthonhaltmegaranyRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
OTmh


# egy ápoltra jutó ápolási nap
# 14 missing, hiszen ott nincs ápolt
summary(JT@data$apolasinap_apoltra)
hist(JT@data$apolasinap_apoltra,breaks=20)
APna <-tm_shape(JT) +
  tm_fill("apolasinap_apoltraR", title="", breaks=c(1,10,20,30,40,50,60,70,80,107),
          palette = "Purples",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("apolasinap_apoltraRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "")))
APna




# nagy elemszám mellett 


# opiát háziorvos
summary(JT@data$opiathaziorvos_apolt)
hist(JT@data$opiathaziorvos_apolt,breaks=20)

OPho <- tm_shape(JT) +
  tm_fill("opiathaziorvos_apoltR", 
          title="", 
          breaks=c(0,0.1,20,40,60,80,100,150,200),
          palette = "Blues",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("opiathaziorvos_apoltRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
            fun=function(x) paste0(formatC(x, digits=0, format="f"), "")))
OPho


# opiát másorvos
summary(JT@data$opiatmasorvos_apolt)
hist(JT@data$opiatmasorvos_apolt,breaks=20)
OPmo <- tm_shape(JT) +
  tm_fill("opiatmasorvos_apoltR", 
          title="", 
          breaks=c(0,0.1,2,4,6,8,10,16),
          palette = "Blues",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("opiatmasorvos_apoltRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "")))
OPmo


# opiát összesen
summary(JT@data$opiatossz_apolt)
hist(JT@data$opiatossz_apolt,breaks=20)
OPos <- tm_shape(JT) +
  tm_fill("opiatossz_apoltR", 
          title="", 
          breaks=c(0,0.1,20,40,60,80,100,150,205),
          palette = "Blues",colorNA="#fdd49e") +tm_borders("white") +
  tm_text("opiatossz_apoltRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "")))
OPos


# misuse: vagyis azok aránya, akik az ellátás lejárta után 6 hónappal is 
# éltek még az ápoltak közül. 
summary(JT@data$misuse)
hist(JT@data$misuse,breaks=20)

MISu <-tm_shape(JT) +
  tm_fill("misuseR", title="", breaks=c(0,0.1,10,20,30,40,50,60,100),
          palette = c("#18a110","grey90","grey80","grey60","grey50","grey40","grey30","grey20"),colorNA="#fdd49e") +tm_borders("white") +
  tm_text("misuseRL",size = 0.7) +
  tm_layout(frame = "white", 
            legend.position=c("right","bottom"), 
            legend.format=list(text.separator =c("-"),
                               fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
MISu      

# misuse 


# TIPOLÓGIA
TIPo <- tm_shape(JT) +
  tm_fill("TIPOL", title="",breaks=c(0,0.9,1.9,2),
          palette= c("#fdd49e","#ef6548","#41ab5d")) + tm_borders("white")+
  tm_layout(frame = "white",legend.show = F)
TIPo

table(JT@data$TIPOL)
sum(JT@data$lakos[JT@data$TIPOL==0])
sum(JT@data$lakos[JT@data$TIPOL==1])
sum(JT@data$lakos[JT@data$TIPOL==2])

sum(JT@data$apolt[JT@data$TIPOL==0])
sum(JT@data$apolt[JT@data$TIPOL==1])
sum(JT@data$apolt[JT@data$TIPOL==2])


##### KORRELÁCIÓK 
ADAT <- JT@data
round(cor(ADAT[,c("apolt_meghaltKSH2018",
                  "alatt_elhunyt_meghaltKSH2018",
                  "otthonhaltmegarany",
                  "apolasinap_apoltra",
                  "opiatossz_apolt",
                  "opiathaziorvos_apolt",
                  "opiatmasorvos_apolt",
                  "misuse")],use="pairwise.complete.obs"),2)
#### magas ápolási nap mivel jár együtt
cor(JT@data$apolt_meghaltKSH2018,
    JT@data$alatt_elhunyt_meghaltKSH2018,
    JT@data$otthonmeghaltarany,
    JT@data$apolasinap_apoltra,use="pairwise.complete.obs")

###HEATMAP
round(cor(cortab[,names(cortab)],use="pairwise.complete.obs"),3)
# ggplot, utállak
# melted_cor <- melt(round(cor(cortab[,names(cortab)],use="pairwise.complete.obs"),3))
# ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile() +
#   geom_text(aes(Var2, Var1, label = value), size = 5) +
#   scale_fill_gradient2(low = "blue", high = "red",
#                        limit = c(-1,1), name="Correlation") +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.background = element_blank())

# jó guide
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram


col<- colorRampPalette(c("darkblue", "white", "red"))(10)

corrplot(M, method="color", col=col,  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

# hipotetikus korrelátiós tábla
hiptab <- M
hiptab["apolt"       ,"alattelhunyt"] <- 1
hiptab["apolt"       ,"otthonhal"   ] <- 1
hiptab["apolt"       ,"apolasinap"  ] <- 0
hiptab["apolt"       ,"opiatossz"   ] <- 1
hiptab["apolt"       ,"opiathazi"   ] <- 0
hiptab["apolt"       ,"opiatmas"    ] <- 0
hiptab["apolt"       ,"hosszantulel"] <- - 1
hiptab["alattelhunyt","otthonhal"   ] <- 1
hiptab["alattelhunyt","apolasinap"  ] <- 1
hiptab["alattelhunyt","opiatossz"   ] <- 1
hiptab["alattelhunyt","opiathazi"   ] <- 1
hiptab["alattelhunyt","opiatmas"    ] <- 0
hiptab["alattelhunyt","hosszantulel"] <- - 1
hiptab["otthonhal"   ,"apolasinap"  ] <- 1
hiptab["otthonhal"   ,"opiatossz"   ] <- 1
hiptab["otthonhal"   ,"opiathazi"   ] <- 1
hiptab["otthonhal"   ,"opiatmas"    ] <- 0
hiptab["otthonhal"   ,"hosszantulel"] <- - 1
hiptab["apolasinap"  ,"opiatossz"   ] <- 1
hiptab["apolasinap"  ,"opiathazi"   ] <- 1
hiptab["apolasinap"  ,"opiatmas"    ] <- 0
hiptab["apolasinap"  ,"hosszantulel"] <- 0
hiptab["opiatossz"   ,"opiathazi"   ] <- 0
hiptab["opiatossz"   ,"opiatmas"    ] <- 0
hiptab["opiatossz"   ,"hosszantulel"] <-  -1
hiptab["opiathazi"   ,"opiatmas"    ] <- 0
hiptab["opiathazi"   ,"hosszantulel"] <- -1 
hiptab["opiatmas"    ,"hosszantulel"] <-  0     
hiptab["alattelhunyt","apolt"       ] <- 1
hiptab["otthonhal"   ,"apolt"       ] <- 1
hiptab["apolasinap"  ,"apolt"       ] <- 0
hiptab["opiatossz"   ,"apolt"       ] <- 1
hiptab["opiathazi"   ,"apolt"       ] <- 0
hiptab["opiatmas"    ,"apolt"       ] <- 0
hiptab["hosszantulel","apolt"       ] <- 0
hiptab["otthonhal"   ,"alattelhunyt"] <- 1
hiptab["apolasinap"  ,"alattelhunyt"] <- 1
hiptab["opiatossz"   ,"alattelhunyt"] <- 1
hiptab["opiathazi"   ,"alattelhunyt"] <- 1
hiptab["opiatmas"    ,"alattelhunyt"] <- 0
hiptab["hosszantulel","alattelhunyt"] <- -1
hiptab["apolasinap"  ,"otthonhal"   ] <- 1
hiptab["opiatossz"   ,"otthonhal"   ] <- 1
hiptab["opiathazi"   ,"otthonhal"   ] <- 1
hiptab["opiatmas"    ,"otthonhal"   ] <- 0
hiptab["hosszantulel","otthonhal"   ] <- -1
hiptab["opiatossz"   ,"apolasinap"  ] <- 1
hiptab["opiathazi"   ,"apolasinap"  ] <- 1
hiptab["opiatmas"    ,"apolasinap"  ] <- 0
hiptab["hosszantulel","apolasinap"  ] <- 0
hiptab["opiathazi"   ,"opiatossz"   ] <- 0
hiptab["opiatmas"    ,"opiatossz"   ] <- 0
hiptab["hosszantulel","opiatossz"   ] <- -1
hiptab["opiatmas"    ,"opiathazi"   ] <- 0
hiptab["hosszantulel","opiathazi"   ] <- -1 
hiptab["hosszantulel","opiatmas"    ] <-  0     
corrplot(hiptab, method="color", col=col,  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

