rm(list=ls())

library(tidyverse)
library(rgdal) # readOGR
library(rgeos) #gCentroid
library(tmap)
#library(leaflet)
library(sp)
library(sf) # qmt ezt szereti, ne ma spf-et
require(devtools) #tmaptools régi verziójához kell
library(tmaptools) #ez a járások egybeolvasztásához kell 
library(readxl) #excel beolvasáshoz
library(reshape2)
library(shinyjs)
library(corrplot)

setwd("C:/Panni/MasMunka/2022/Hospice/maps")
# betöltöm magát a járási térképet
JT <- readOGR(dsn = "kiindulo_terkepek", layer = "csak_jarasok") 

#járási szintű adatok
h <- read_excel("C:/Panni/MasMunka/2022/Hospice/data/Hospice_jaras_2018_v2.xlsx")
names(h)
# v2: háró sorok kitörlése, 3 változónév

# KSH halálozási adatok
k <- read_excel("C:/Panni/MasMunka/2022/Hospice/data/KSHhalalozas_beolvasasra.xlsx")


# megyenév tisztítása
###### kiírom a file-t, de előtte pár javítás
JT@data$MEGYE[JT@data$MEGYE=="Szabolcs-Szatm\xe1r-Bereg"] <- "Szabolcs-Szatmár Bereg"
JT@data$MEGYE[JT@data$MEGYE=="B\xe1cs-Kiskun"] <- "Bács-Kiskun"
JT@data$MEGYE[JT@data$MEGYE=="Veszpr\xe9m"] <- "Veszprém"
JT@data$MEGYE[JT@data$MEGYE=="Fej\xe9r"] <- "Fejér"
JT@data$MEGYE[JT@data$MEGYE=="Hajd\xfa-Bihar"] <- "Hajdú-Bihar"
JT@data$MEGYE[JT@data$MEGYE=="N\xf3gr\xe1d"] <- "Nógrád"
JT@data$MEGYE[JT@data$MEGYE=="B\xe9k\xe9s"] <- "Békés"
JT@data$MEGYE[JT@data$MEGYE=="J\xe1sz-Nagykun-Szolnok"] <- "Jász-Nagykun-Szolnok"
JT@data$MEGYE[JT@data$MEGYE=="Kom\xe1rom-Esztergom"] <- "Komárom-Esztergom"
JT@data$MEGYE[JT@data$MEGYE=="Gy\xf5r-Moson-Sopron"] <- "Győr-Moson-Sopron"
JT@data$MEGYE[JT@data$MEGYE=="f\xf5v\xe1ros"] <- "Budapest"
JT@data$MEGYE[JT@data$MEGYE=="Csongr\xe1d"] <- "Csongrád-Csanád"
JT@data$MEGYE[JT@data$MEGYE=="Borsod-Aba\xfaj-Zempl\xe9n"] <- "Borsod-Abaúj-Zemplén"


# JT-ben nincs "járás" vég
JT@data$jaras_nev <- str_c(JT@data$JARASNEV," járás")

# rosszul beolvasott járásnevek javítása
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Asz\xf3di járás"                       , "Aszódi járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Balatonalm\xe1di járás"                , "Balatonalmádi járás"    )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Balmaz\xfajv\xe1rosi járás"            , "Balmazújvárosi járás"   )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Bonyh\xe1di járás"                     , "Bonyhádi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xe1tonyterenyei járás"               , "Bátonyterenyei járás"   )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xe9k\xe9si járás"                    , "Békési járás"           )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xf3lyi járás"                        , "Bólyi járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Celld\xf6m\xf6lki járás"               , "Celldömölki járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Csongr\xe1di járás"                    , "Csongrádi járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Domb\xf3v\xe1ri járás"                 , "Dombóvári járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Edel\xe9nyi járás"                     , "Edelényi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Fony\xf3di járás"                      , "Fonyódi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Gyomaendr\xf5di járás"                 , "Gyomaendrődi járás"     )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Gy\xf5ri járás"                        , "Győri járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "G\xe1rdonyi járás"                     , "Gárdonyi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "G\xf6nci járás"                        , "Gönci járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Hajd\xfahadh\xe1zi járás"              , "Hajdúhadházi járás"     )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Hajd\xfaszoboszl\xf3i járás"           , "Hajdúszoboszlói járás"  )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "H\xf3dmez\xf5v\xe1s\xe1rhelyi járás"   , "Hódmezővásárhelyi járás" ) 
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "J\xe1noshalmi járás"                   , "Jánoshalmi járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "J\xe1szber\xe9nyi járás"               , "Jászberényi járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kapuv\xe1ri járás"                     , "Kapuvári járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kisb\xe9ri járás"                      , "Kisbéri járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kisk\xf5r\xf6si járás"                 , "Kiskőrösi járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Koml\xf3i járás"                       , "Komlói járás"           )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kunszentmikl\xf3si járás"              , "Kunszentmiklósi járás"  )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "K\xf5szegi járás"                      , "Kőszegi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mak\xf3i járás"                        , "Makói járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mez\xf5cs\xe1ti járás"                 , "Mezőcsáti járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mez\xf5k\xf6vesdi járás"               , "Mezőkövesdi járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Moh\xe1csi járás"                      , "Mohácsi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "M\xe1t\xe9szalkai járás"               , "Mátészalkai járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "M\xf3ri járás"                         , "Móri járás"             )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Nagyk\xe1ll\xf3i járás"                , "Nagykállói járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Nagyk\xf5r\xf6si járás"                , "Nagykőrösi járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Ny\xedrb\xe1tori járás"                , "Nyírbátori járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Orosh\xe1zi járás"                     , "Orosházi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Pilisv\xf6r\xf6sv\xe1ri járás"         , "Pilisvörösvári járás"   )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xe1szt\xf3i járás"                   , "Pásztói járás"          )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xe9csv\xe1radi járás"                , "Pécsváradi járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xfcsp\xf6klad\xe1nyi járás"          , "Püspökladányi járás"    )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "R\xe9ts\xe1gi járás"                   , "Rétsági járás"          )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Sikl\xf3si járás"                      , "Siklósi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Szeksz\xe1rdi járás"                   , "Szekszárdi járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Szentl\xf5rinci járás"                 , "Szentlőrinci járás"     )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Szigetv\xe1ri járás"                   , "Szigetvári járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Sz\xe9cs\xe9nyi járás"                 , "Szécsényi járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "S\xe1rbog\xe1rdi járás"                , "Sárbogárdi járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "S\xe1rv\xe1ri járás"                   , "Sárvári járás"          )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "S\xfcmegi járás"                       , "Sümegi járás"           )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tatab\xe1nyai járás"                   , "Tatabányai járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tiszak\xe9cskei járás"                 , "Tiszakécskei járás"     )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tisza\xfajv\xe1rosi járás"             , "Tiszaújvárosi járás"    )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "T\xf6r\xf6kszentmikl\xf3si járás"      , "Törökszentmiklósi járás")
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Vecs\xe9si járás"                      , "Vecsési járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "V\xe1ci járás"                         , "Váci járás"             )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "V\xe1s\xe1rosnam\xe9nyi járás"         , "Vásárosnaményi járás"   )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Z\xe1honyi járás"                      , "Záhonyi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "\xd3zdi járás"                         , "Ózdi járás"             )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Baktal\xf3r\xe1nth\xe1zai járás"       , "Baktalórántházai járás" )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Balatonf\xfcredi járás"                , "Balatonfüredi járás"    )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Beretty\xf3\xfajfalui járás"           , "Berettyóújfalui járás"  )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xe1csalm\xe1si járás"                , "Bácsalmási járás"       )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xe9k\xe9scsabai járás"               , "Békéscsabai járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "B\xe9lap\xe1tfalvai járás"             , "Bélapátfalvai járás"    )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Cegl\xe9di járás"                      , "Ceglédi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Cig\xe1ndi járás"                      , "Cigándi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Csurg\xf3i járás"                      , "Csurgói járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Duna\xfajv\xe1rosi járás"              , "Dunaújvárosi járás"     )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Feh\xe9rgyarmati járás"                , "Fehérgyarmati járás"    )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "F\xfczesabonyi járás"                  , "Füzesabonyi járás"      )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Gy\xe1li járás"                        , "Gyáli járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Gy\xf6ngy\xf6si járás"                 , "Gyöngyösi járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "G\xf6d\xf6ll\xf5i járás"               , "Gödöllői járás"         )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Hajd\xfab\xf6sz\xf6rm\xe9nyi járás"    , "Hajdúböszörményi járás" )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Hajd\xfan\xe1n\xe1si járás"            , "Hajdúnánási járás"      )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Hegyh\xe1ti járás"                     , "Hegyháti járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Ibr\xe1nyi járás"                      , "Ibrányi járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "J\xe1szap\xe1ti járás"                 , "Jászapáti járás"        )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kaposv\xe1ri járás"                    , "Kaposvári járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kecskem\xe9ti járás"                   , "Kecskeméti járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kiskunf\xe9legyh\xe1zi járás"          , "Kiskunfélegyházi járás" )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kisv\xe1rdai járás"                    , "Kisvárdai járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kom\xe1romi járás"                     , "Komáromi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Kunszentm\xe1rtoni járás"              , "Kunszentmártoni járás"  )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "K\xf6rmendi járás"                     , "Körmendi járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Martonv\xe1s\xe1ri járás"              , "Martonvásári járás"     )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mez\xf5kov\xe1csh\xe1zai járás"        , "Mezőkovácsházai járás"  )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mez\xf5t\xfari járás"                  , "Mezőtúri járás"         )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Mosonmagyar\xf3v\xe1ri járás"          , "Mosonmagyaróvári járás" )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "M\xf3rahalmi járás"                    , "Mórahalmi járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Nagyat\xe1di járás"                    , "Nagyatádi járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Nagyk\xe1tai járás"                    , "Nagykátai járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Ny\xedradonyi járás"                   , "Nyíradonyi járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Ny\xedregyh\xe1zi járás"               , "Nyíregyházi járás"      )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Oroszl\xe1nyi járás"                   , "Oroszlányi járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xe1pai járás"                        , "Pápai járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xe9csi járás"                        , "Pécsi járás"            )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "P\xe9terv\xe1s\xe1rai járás"           , "Pétervásárai járás"     )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "R\xe1ckevei járás"                     , "Ráckevei járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Salg\xf3tarj\xe1ni járás"              , "Salgótarjáni járás"     )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Si\xf3foki járás"                      , "Siófoki járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Szentgotth\xe1rdi járás"               , "Szentgotthárdi járás"   )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Szigetszentmikl\xf3si járás"           , "Szigetszentmiklósi járás")    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Sziksz\xf3i járás"                     , "Szikszói járás"         )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Sz\xe9kesfeh\xe9rv\xe1ri járás"        , "Székesfehérvári járás"  )
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "S\xe1rospataki járás"                  , "Sárospataki járás"      )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "S\xe1toralja\xfajhelyi járás"          , "Sátoraljaújhelyi járás" )   
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tam\xe1si járás"                       , "Tamási járás"           )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tiszaf\xfcredi járás"                  , "Tiszafüredi járás"      )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Tiszavasv\xe1ri járás"                 , "Tiszavasvári járás"     )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "T\xe9ti járás"                         , "Téti járás"             )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Vasv\xe1ri járás"                      , "Vasvári járás"          )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Veszpr\xe9mi járás"                    , "Veszprémi járás"        )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "V\xe1rpalotai járás"                   , "Várpalotai járás"       )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "Zalaszentgr\xf3ti járás"               , "Zalaszentgróti járás"   )    
JT@data$jaras_nev <- str_replace(JT@data$jaras_nev, "\xc9rdi járás"                         , "Érdi járás"             )    

# egységes nevekért dolgozom (JT@data és h adatbázis)
# mi nem talál párt
setdiff(JT@data$jaras_nev,h$jaras)[order(setdiff(JT@data$jaras_nev,h$jaras))]
setdiff(h$jaras,JT@data$jaras_nev)

# csak Jnoshalmát + a Bp-i kerületeket kell javítani
JT@data$jaras_nev[JT@data$jaras_nev=="Jánoshalmi járás"] <- "Jánoshalmai járás"
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 01. ker. járás"] <- "Budapest 01. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 02. ker. járás"] <- "Budapest 02. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 03. ker. járás"] <- "Budapest 03. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 04. ker. járás"] <- "Budapest 04. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 05. ker. járás"] <- "Budapest 05. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 06. ker. járás"] <- "Budapest 06. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 07. ker. járás"] <- "Budapest 07. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 08. ker. járás"] <- "Budapest 08. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 09. ker. járás"] <- "Budapest 09. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 10. ker. járás"] <- "Budapest 10. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 11. ker. járás"] <- "Budapest 11. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 12. ker. járás"] <- "Budapest 12. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 13. ker. járás"] <- "Budapest 13. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 14. ker. járás"] <- "Budapest 14. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 15. ker. járás"] <- "Budapest 15. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 16. ker. járás"] <- "Budapest 16. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 17. ker. járás"] <- "Budapest 17. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 18. ker. járás"] <- "Budapest 18. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 19. ker. járás"] <- "Budapest 19. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 20. ker. járás"] <- "Budapest 20. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 21. ker. járás"] <- "Budapest 21. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 22. ker. járás"] <- "Budapest 22. ker."
JT@data$jaras_nev[JT@data$jaras_nev=="Budapest 23. ker. járás"] <- "Budapest 23. ker."

# egységes nevekért dolgozom
# mi nem talál párt
setdiff(JT@data$jaras_nev,h$jaras)[order(setdiff(JT@data$jaras_nev,h$jaras))]
setdiff(h$jaras,JT@data$jaras_nev)

# egyesítés JT@data és h
JT@data <- left_join(JT@data,h,by=c("jaras_nev"="jaras"))

# egységes nevekért dolgozom JT@data és k
setdiff(JT@data$jaras_nev,k$KSHjaras)
setdiff(k$KSHjaras,JT@data$jaras_nev)

# a k-t módosítom
k$KSHjaras[k$KSHjaras=="Budapest 01. kerület"] <- "Budapest 01. ker."
k$KSHjaras[k$KSHjaras=="Budapest 03. kerület"] <- "Budapest 03. ker."
k$KSHjaras[k$KSHjaras=="Budapest 05. kerület"] <- "Budapest 05. ker."
k$KSHjaras[k$KSHjaras=="Budapest 07. kerület"] <- "Budapest 07. ker."
k$KSHjaras[k$KSHjaras=="Budapest 09. kerület"] <- "Budapest 09. ker."
k$KSHjaras[k$KSHjaras=="Budapest 11. kerület"] <- "Budapest 11. ker."
k$KSHjaras[k$KSHjaras=="Budapest 13. kerület"] <- "Budapest 13. ker."
k$KSHjaras[k$KSHjaras=="Budapest 15. kerület"] <- "Budapest 15. ker."
k$KSHjaras[k$KSHjaras=="Budapest 17. kerület"] <- "Budapest 17. ker."
k$KSHjaras[k$KSHjaras=="Budapest 19. kerület"] <- "Budapest 19. ker."
k$KSHjaras[k$KSHjaras=="Budapest 21. kerület"] <- "Budapest 21. ker."
k$KSHjaras[k$KSHjaras=="Budapest 23. kerület"] <- "Budapest 23. ker."
k$KSHjaras[k$KSHjaras=="Budapest 02. kerület"] <- "Budapest 02. ker."
k$KSHjaras[k$KSHjaras=="Budapest 04. kerület"] <- "Budapest 04. ker."
k$KSHjaras[k$KSHjaras=="Budapest 06. kerület"] <- "Budapest 06. ker."
k$KSHjaras[k$KSHjaras=="Budapest 08. kerület"] <- "Budapest 08. ker."
k$KSHjaras[k$KSHjaras=="Budapest 10. kerület"] <- "Budapest 10. ker."
k$KSHjaras[k$KSHjaras=="Budapest 12. kerület"] <- "Budapest 12. ker."
k$KSHjaras[k$KSHjaras=="Budapest 14. kerület"] <- "Budapest 14. ker."
k$KSHjaras[k$KSHjaras=="Budapest 16. kerület"] <- "Budapest 16. ker."
k$KSHjaras[k$KSHjaras=="Budapest 18. kerület"] <- "Budapest 18. ker."
k$KSHjaras[k$KSHjaras=="Budapest 20. kerület"] <- "Budapest 20. ker."
k$KSHjaras[k$KSHjaras=="Budapest 22. kerület"] <- "Budapest 22. ker."
k$KSHjaras[k$KSHjaras=="Nyíregyházai járás"]   <- "Nyíregyházi járás"

# egységes nevekért dolgozom JT@data és k
setdiff(JT@data$jaras_nev,k$KSHjaras)
setdiff(k$KSHjaras,JT@data$jaras_nev)

#JT-hez hozzárakom 
JT@data <- left_join(JT@data,k,by=c("jaras_nev"="KSHjaras"))

# Budapest dummy
JT@data$bp <- 0
JT@data$bp[str_sub(JT@data$jaras_nev,1,8)=="Budapest"] <- 1

#0.1 ápoltak száma nominálisan

JT@data$apoltR <- JT@data$apolt
JT@data$apoltR[is.na(JT@data$apoltR)==T] <- 0
JT@data$apoltRL <- round(JT@data$apoltR,0)
JT@data$apoltR[JT@data$apoltRL==0] <- NA
JT@data$apoltRL[JT@data$apoltRL==0] <- NA

JT@data$apoltRL[JT@data$bp==1] <- NA





##### elemzés második szakasza, KSH adatok bevonásával

## halálozások száma lakosságarányosan
JT@data$meghaltKSH2018_lakos <- JT@data$meghaltKSH2018 / JT@data$lakos *1000
# kerekített
JT@data$meghaltKSH2018_lakosR <- round(JT@data$meghaltKSH2018_lakos,0)  
#label
JT@data$meghaltKSH2018_lakosRL <- JT@data$meghaltKSH2018_lakosR
JT@data$meghaltKSH2018_lakosRL[JT@data$bp==1] <- ""

# Ápolt / meghaltKSH: meghaltak számához viszonyítva hány % került be otthoni hospice ellátásba
JT@data$apolt_meghaltKSH2018 <- JT@data$apolt / JT@data$meghaltKSH2018
# kategoriális változó - még meg kell csinálni
# JT@data$apolt_meghaltKSH2018KAT <- -5 # amíg fel nem töltöm
# JT@data$apolt_meghaltKSH2018KAT <- 0 # nincs ápolt
# felszorzott - ábrázolandó
JT@data$apolt_meghaltKSH2018R <- round(JT@data$apolt_meghaltKSH2018*100)
# 0 legyen NA hogy jobban kiugorjon vizuálisan
JT@data$apolt_meghaltKSH2018R[JT@data$apolt_meghaltKSH2018==0] <- NA
# felirat
JT@data$apolt_meghaltKSH2018RL <- round(JT@data$apolt_meghaltKSH2018*100)
JT@data$apolt_meghaltKSH2018RL[JT@data$bp==1] <- NA
#ha <1
JT@data$apolt_meghaltKSH2018RL[JT@data$apolt_meghaltKSH2018<0.01&
                                 JT@data$apolt_meghaltKSH2018>0] <- "<1"
JT@data$apolt_meghaltKSH2018R[JT@data$apolt_meghaltKSH2018<0.01&
                                 JT@data$apolt_meghaltKSH2018>0] <- 0
JT@data$apolt_meghaltKSH2018RL[JT@data$apolt==0] <- ""


#Alatt_elhunyt / meghaltKSH: otthoni hospice ellátásban volt 
# halálának idején az elhunytak között (%) 
JT@data$alatt_elhunyt_meghaltKSH2018 <- JT@data$alatt_elhunyt / JT@data$meghaltKSH2018
# kategoriális - még meg kell csinálni
# felszorzott
JT@data$alatt_elhunyt_meghaltKSH2018R <- round(JT@data$alatt_elhunyt_meghaltKSH2018*100)
JT@data$alatt_elhunyt_meghaltKSH2018R[JT@data$apolt==0] <- NA
# felirat
JT@data$alatt_elhunyt_meghaltKSH2018RL <- round(JT@data$alatt_elhunyt_meghaltKSH2018*100)
JT@data$alatt_elhunyt_meghaltKSH2018R[JT@data$alatt_elhunyt_meghaltKSH2018<0.01&
                                         JT@data$alatt_elhunyt_meghaltKSH2018>0] <- 0.5
JT@data$alatt_elhunyt_meghaltKSH2018RL[JT@data$alatt_elhunyt_meghaltKSH2018<0.01&
                                         JT@data$alatt_elhunyt_meghaltKSH2018>0] <- "<1"
JT@data$alatt_elhunyt_meghaltKSH2018RL[JT@data$bp==1] <- ""
JT@data$alatt_elhunyt_meghaltKSH2018RL[JT@data$apolt==0] <- ""

#otthonukban elhunytak aránya az összes elhunyt közül 
# (adatkikérésig!!)
# van 15 járás, ahol senki sem halt meg 
# a kis elemszámúakat ki kell majd szedni!!!
JT@data$otthonhaltmegarany <- 1 - JT@data$alatt_fekvobeteg_elhunyt/JT@data$elhunyt2
# kategoriális - még meg kell csinálni
# felszorzott
JT@data$otthonhaltmegaranyR <- round(JT@data$otthonhaltmegarany*100)
JT@data$otthonhaltmegaranyR[JT@data$elhunyt2 < 10 ] <- NA
# felirat
JT@data$otthonhaltmegaranyRL <- JT@data$otthonhaltmegaranyR
JT@data$otthonhaltmegaranyRL[JT@data$bp==1] <- NA
JT@data$otthonhaltmegaranyRL[JT@data$otthonhaltmegarany<0.01&
                                         JT@data$otthonhaltmegarany>0] <- "<1"



# egy ápoltra jutó ápolási nap
# 14 missing, hiszen ott nincs ápolt
JT@data$apolasinap_apoltra <- JT@data$apolasinap /JT@data$apolt
# kategoriális - még meg kell csinálni
# kerekített
JT@data$apolasinap_apoltraR <- round(JT@data$apolasinap_apoltra)
# felirat
JT@data$apolasinap_apoltraRL <- round(JT@data$apolasinap_apoltraR)
JT@data$apolasinap_apoltraRL[JT@data$bp==1] <- NA
JT@data$apolasinap_apoltraRL[is.na(JT@data$apolasinap_apoltraR)] <- ""


# opiátok 
JT@data$opiathaziorvos_apolt <- JT@data$kivaltott_opiat_haziorvos / JT@data$apolt
JT@data$opiatmasorvos_apolt  <- JT@data$kivaltott_opiat_masorvos / JT@data$apolt
JT@data$opiatossz <- JT@data$kivaltott_opiat_haziorvos + JT@data$kivaltott_opiat_masorvos
JT@data$opiatossz_apolt <- JT@data$opiatossz / JT@data$apolt

# kategoriális - még meg kell csinálni
# felszorzott
JT@data$opiathaziorvos_apoltR <- round(JT@data$opiathaziorvos_apolt)
JT@data$opiatmasorvos_apoltR <- round(JT@data$opiatmasorvos_apolt)
JT@data$opiatossz_apoltR <- round(JT@data$opiatossz_apolt)
# felirat
JT@data$opiathaziorvos_apoltRL <- JT@data$opiathaziorvos_apoltR
JT@data$opiathaziorvos_apoltRL[JT@data$bp==1] <- NA
JT@data$opiathaziorvos_apoltRL[is.na(JT@data$opiathaziorvos_apoltR)] <- ""

JT@data$opiatmasorvos_apoltRL <- JT@data$opiatmasorvos_apoltR
JT@data$opiatmasorvos_apoltRL[JT@data$bp==1] <- NA
JT@data$opiatmasorvos_apoltRL[is.na(JT@data$opiatmasorvos_apoltR)] <- ""

JT@data$opiatossz_apoltRL <- JT@data$opiatossz_apoltR
JT@data$opiatossz_apoltRL[JT@data$bp==1] <- NA
JT@data$opiatossz_apoltRL[is.na(JT@data$opiatmasorvos_apoltR)] <- ""


# misuse: vagyis azok aránya, akik az ellátás lejárta után 6 hónappal is 
# éltek még az ápoltak közül. 
JT@data$misuse <- 1- (JT@data$alatt_elhunyt + JT@data$hamar_elhunyt + JT@data$kesobb_elhunyt)/JT@data$apolt
# hányan érintettek összesen
sum(JT@data$misuse[JT@data$apolt>0]*JT@data$apolt[JT@data$apolt>0])

# kategoriális - még meg kell csinálni
#JT@data$misuseCAT[JT@data$misuse==0] <- 0

# felszorzott
JT@data$misuseR <- round(JT@data$misuse*100)
# felirat
JT@data$misuseRL <- round(JT@data$misuseR)
JT@data$misuseRL[JT@data$bp==1] <- NA
JT@data$misuseRL[is.na(JT@data$misuseR)] <- ""
JT@data$misuseRL[JT@data$misuseR==0] <- ""



# tipológia
JT@data$TIPOL[JT@data$apolt_meghaltKSH2018 < 0.045] <- 0 # alacsony ellátottságú
JT@data$TIPOL[JT@data$apolt_meghaltKSH2018 >= 0.045 & 
              JT@data$misuse > 0.495] <- 1 #misuse
JT@data$TIPOL[JT@data$apolt_meghaltKSH2018 >= 0.045 & 
                JT@data$misuse <= 0.495] <- 2 #zöldek 
table(JT@data$TIPOL,useNA = "ifany")

# korrelációs heat map-hez
cortab <- JT@data %>% dplyr::select(apolt_meghaltKSH2018,
                                    alatt_elhunyt_meghaltKSH2018,
                                    otthonhaltmegarany,
                                    apolasinap_apoltra,
                                    opiatossz_apolt,
                                    opiathaziorvos_apolt,
                                    opiatmasorvos_apolt,
                                    misuse)
# egyszerűsített feliratok a jobb olvashatóság végett
names(cortab)[1] <- "apolt"
names(cortab)[2] <- "alattelhunyt"
names(cortab)[3] <- "otthonhal"
names(cortab)[4] <- "apolasinap"
names(cortab)[5] <- "opiatossz"
names(cortab)[6] <- "opiathazi"
names(cortab)[7] <- "opiatmas"
names(cortab)[8] <- "hosszantulel"


M <- cor(cortab[,names(cortab)],use="pairwise.complete.obs")


# kiírom a nézegetéshez
# mutatok <- JT@data %>% dplyr::select(jaras_nev,
#                           apolt,
#                           apolt_meghaltKSH2018,
#                           alatt_elhunyt_meghaltKSH2018,
#                           otthonhaltmegarany,
#                           apolasinap_apoltra,
#                           opiathaziorvos_apolt,
#                           opiatmasorvos_apolt,
#                           opiatossz_apolt,
#                           misuse,TIPOL) 
# 
# mutatok_zoldek <- mutatok %>% dplyr::filter(TIPOL==2)
# 
# write_excel_csv2(mutatok,"C:/Panni/MasMunka/2022/Hospice/data/mutatok.csv")
# write_excel_csv2(mutatok_zoldek,"C:/Panni/MasMunka/2022/Hospice/data/mutatok_zoldek.csv")

######## a végén ki kell írni majd az adatokat
#hospice_otthoni_2018_jarasi_feltaro <- JT@data[,c("MEGYE","jaras_nev","lakos","apolt","apolt_tizezerre","el_apolt")]
# write.csv(hospice_otthoni_2018_jarasi_feltaro,"C:/Panni/MasMunka/2022/Hospice/data/hospice_otthoni_2018_jarasi_feltaro.csv")
# write_excel_csv2(hospice_otthoni_2018_jarasi_feltaro,"C:/Panni/MasMunka/2022/Hospice/data/hospice_otthoni_2018_jarasi_feltaro.csv")
