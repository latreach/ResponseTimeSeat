########################################
# Response Time y Response Rate SEAT
# RED SOCIAL TWITTER
##################################

####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
####################################


###Librerías
library(magrittr)
c("dplyr", "tidyr","lubridate","twitteR", "googlesheets") %>% 
  sapply(require,character.only=T)


# Conexión Twitter Cuenta SEAT --------------------------------------------------------
key          = "vDfPjIl7fRMjwHwYfj0rz5Vid"
secret       = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgWB"
access_token = "137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
secret_token = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
setup_twitter_oauth(key, secret, access_token, secret_token)
user         = "SEAT_Mexico"
useriD       = 137735852

# Funciones ---------------------------------------------------------------
source("~/local/seat_analisis_extras/ResponseTime/funcionesResponse.R")

# Objetos globales ----------------------------------------------------------------
meses <-c("Enero","Febrero","Marzo","Abril","Mayo","Junio", "Julio","Agosto",
          "Septiembre","Octubre","Noviembre","Diciembre")


# Directorio --------------------------------------------------------------
setwd("~/local/seat_analisis_extras/ResponseTime/datos/")


# Retweets ----------------------------------------------------------------
RT       = retweetsOfMe(n=800)
TimeLine = RT %>% twListToDF() 

TimeLine %>%  write.csv("BaseRetweet.csv",row.names=F)

TimeLine = TimeLine %>% 
  separate(created, c("Fecha","Hora"),sep=" ") %>% 
  mutate(Fecha = as.Date(Fecha), mes=month(Fecha), dia=day(Fecha)) %>%
  mutate(mesNombre = factor(mes))

levels(TimeLine$mesNombre)<-c("Enero","Febrero","Marzo","Octubre",
                              "Noviembre","Diciembre")

retMes = TimeLine  %>% 
  filter(mes %in% c(2,3)) %>%  filter(isRetweet==F) %>%
  group_by(mesNombre) %>% 
  summarise(cantidad = sum(retweetCount)) %>% 
  rename(No_retweets=cantidad) %>% 
  mutate(tipo="Por Mes") %>%  
  mutate(dia="general") %>% 
  select(mesNombre, dia, No_retweets, tipo) %>%
  data.frame

retDia =TimeLine  %>% 
  filter(mes %in% c(2,3)) %>%  filter(isRetweet==F) %>%
  group_by(mesNombre, dia)%>% 
  summarise(cantidad = sum(retweetCount)) %>% 
  rename(No_retweets=cantidad) %>% 
  mutate(tipo="Por Dia") %>%  data.frame()

TimeLine  %>% 
  filter(mes %in% c(2,3)) %>%  filter(isRetweet==F) %>%
  group_by(mesNombre, dia)%>% 
  summarise(cantidad = sum(retweetCount)) %>% 
  rename(No_retweets=cantidad) %>% 
  mutate(tipo="Por Dia") %>%  data.frame()

rbind(retDia, retMes) %>% write.csv("retweets.csv", row.names=F)

idTimeLine = TimeLine$id


# Response Time -----------------------------------------------------------

##Totales
Menciones = read.csv("Mentions.csv", header = T)
Menciones2 =read.csv("Mentions2.csv",
                     header = T)
Menciones = rbind(Menciones, Menciones2)
Menciones = Menciones %>%  distinct(ID, .keep_all=T)
Menciones %>%  write.csv("Mentions.csv", row.names=F)

MencionesNoseat = Menciones %>%  filter(usuarioID!=useriD)

Mencionesseat = Menciones %>%  filter(usuarioID==useriD)

Preguntas = MencionesNoseat %>% filter(ID %in% Mencionesseat$idReplyTo) %>% 
  filter(grepl("[?]", mensaje))
# Preguntas = MencionesNoseat %>% filter(ID %in% Mencionesseat$idReplyTo) 

#### En proceso
# ids = Menciones$idReplyTo[Menciones$ID %!in% Menciones$idReplyTo]
# ids = ids[ids %!in%  menciones$id]
# ids = ids %>%  na.omit() %>%  as.numeric()
# ids = ids %>% unique 
# 
# faltantes = lapply(ids[1:10], function(x){
#   x = as.character(x)
#   print(x %>% showStatus)
#   X  = tryCatch( 
#     showStatus(x) %>%  twListToDF,
#     error= function(e)NULL)
#   return(X)
# })
# faltantes = faltantes %>%  unlist
# faltantes[1]
# lapply(faltantes[1:2], print)
# 
# test= lapply(faltantes[1:2], function(x){
#   twListToDF(faltantes)
# 
# })
# do.call("rbind",faltantes)
##########EN proceso

merge(Preguntas, Mencionesseat, by="idReplyTo", incomparables = NA) %>%
  mutate(ID.y = as.character(ID.y)) %>%  
  mutate(ID.x = as.character(ID.x)) %>% dim

pruebas = merge(Preguntas,Mencionesseat %>%  rename(IDFromSeat=ID),
                by.x="ID", by.y ="idReplyTo", incomparables = NA) %>%
  mutate(IDFromSeat = as.character(IDFromSeat)) 
names(pruebas)
names(pruebas)<-c("idPregunta","FechaPregunta","IdUserPreguntaReply",
                  "NombreUserPreguntaReply", "idPreguntaReplyTweet", 
                  "MensajePregunta", "UsuarioIdPregunta",
                  "UsuarioNombrePregunta","FechaRespuesta", "IdRespuesta", 
                  "IdRespuestaReply","IdNombreReply","MensajeRespuesta",
                  "UsuarioIdRespuesta", "UsuarioNombreRespuesta")

pruebas  = pruebas %>% 
  mutate(FechaPregunta = as.POSIXct(FechaPregunta), 
         FechaRespuesta = as.POSIXct(FechaRespuesta)) %>% 
  mutate(anioPregunta = year(FechaPregunta), 
         mesPregunta= month(FechaPregunta),
         diaPregunta = day(FechaPregunta),
         anioRespuesta = year(FechaRespuesta),
         mesRespuesta = month(FechaRespuesta),
         diaRespuesta = day(FechaRespuesta),
         diaNombrePregunta = lubridate::wday(FechaPregunta, label=T),
         diaNombreRespuesta = lubridate::wday(FechaRespuesta, label=T),
         diferenciaTiempo = 
           difftime(FechaRespuesta,FechaPregunta, units="hours")) %>% 
  mutate(diferenciaTiempo = as.numeric(diferenciaTiempo)) %>% 
  mutate(horaPregunta = hour(FechaPregunta),
         minutoPregunta = minute(FechaPregunta),
         horaRespuesta = hour(FechaRespuesta),
         minutoRespuesta = minute(FechaRespuesta),
         HoraMinPregunta = paste(horaPregunta, 
                                 minutoPregunta,sep=":"),
         HoraMinRespuesta = paste(horaRespuesta,
                                  minutoRespuesta,sep=":")) %>% 
  mutate(MesNombrePregunta = factor(mesPregunta))

levels(pruebas$MesNombrePregunta)<-meses[2:3]

MencionesNoseat %>% filter(ID %!in% pruebas$idPregunta) %>%  
  filter(grepl("[?]", mensaje)) %>% 
  mutate(ID = as.character(ID)) %>% 
  mutate(link = paste("https://twitter.com/statuses/",ID, sep="")) %>% 
  select(ID, Fecha, mensaje, usuarioID, usuarioNombre, link) %>% 
  write.csv("tweetsNorespondidos.csv", row.names=T)

cantidadg1 = pruebas %>% 
  filter(grepl("[?]",MensajePregunta)) %>% 
  group_by(mesPregunta) %>%  tally %>% select(n) %>%  unlist

general1 =  pruebas  %>%
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(MesNombrePregunta) %>% 
  summarise(tiempo = mean(diferenciaTiempo)) %>% 
  mutate(tiempo= sapply(tiempo, horasHorasMin)) %>% 
  mutate(categoria ="Por Mes") %>% 
  mutate(tipo="ResponseTime General") %>% 
  mutate(No_Tweets = cantidadg1) %>% 
  mutate(dia_Tweet = "General") %>% 
  rename(Response = tiempo) %>% 
  select(MesNombrePregunta, dia_Tweet, Response, 
         categoria, tipo, No_Tweets)%>%  data.frame()


cantidadg2 = pruebas %>% 
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(MesNombrePregunta,diaPregunta) %>%  tally %>% 
  .$n %>%  unlist

general2 = pruebas  %>%
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(MesNombrePregunta,diaPregunta) %>% 
  summarise(tiempo = mean(diferenciaTiempo)) %>% 
  mutate(tiempo= sapply(tiempo,horasHorasMin)) %>% 
  rename(dia_Tweet=diaPregunta) %>% 
  mutate(categoria="Por dia") %>% 
  mutate(tipo="ResponseTime General") %>%
  rename(Response=tiempo) %>% 
  data.frame(., No_Tweets= cantidadg2)

filtrot= pruebas %>%  filter(diaNombrePregunta!="Sun") %>% 
  left_join(pruebas %>% filter(diaNombrePregunta=="Sat" ) %>% 
              filter(horaPregunta<8| horaPregunta>14) %>% 
              mutate(sabadoNoLaboral="NoLaboral")) 

cantidadA1 = filtrot %>% 
  mutate(sabadoNoLaboral=ifelse(is.na(sabadoNoLaboral), "Laboral",
                                "NoLaboral")) %>%
  filter(sabadoNoLaboral=="Laboral") %>% 
  filter(horaPregunta>8) %>%
  filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(mesPregunta, diaPregunta) %>%  tally %>%
  .$n %>%  unlist

ajustado1 = filtrot %>% 
  mutate(sabadoNoLaboral=ifelse(is.na(sabadoNoLaboral), "Laboral",
                                "NoLaboral")) %>%
  filter(sabadoNoLaboral=="Laboral") %>% 
  filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>% 
  group_by(MesNombrePregunta,diaPregunta) %>% 
  summarise(tiempo=mean(diferenciaTiempo)) %>% 
  mutate(tiempo= sapply(tiempo,horasHorasMin)) %>% 
  rename(dia_Tweet=diaPregunta) %>% 
  mutate(categoria="Por dia") %>% 
  mutate(tipo="ResponseTime Ajustado") %>% 
  rename(Response= tiempo) %>% 
  data.frame(., No_Tweets = cantidadA1)

cantidadA2 = filtrot %>% 
  mutate(sabadoNoLaboral=ifelse(is.na(sabadoNoLaboral), "Laboral",
                                "NoLaboral")) %>%
  filter(sabadoNoLaboral=="Laboral") %>% 
  filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(mesPregunta) %>%  tally %>% select(n) %>%  unlist

ajustado2 = filtrot %>% 
  mutate(sabadoNoLaboral=ifelse(is.na(sabadoNoLaboral), "Laboral",
                                "NoLaboral")) %>%
  filter(sabadoNoLaboral=="Laboral") %>% 
  filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>%
  group_by(MesNombrePregunta) %>% 
  summarise(tiempo=mean(diferenciaTiempo)) %>% 
  mutate(tiempo= sapply(tiempo, horasHorasMin)) %>% 
  mutate(categoria ="Por Mes") %>% 
  mutate(tipo="ResponseTime Ajustado") %>% 
  mutate(No_Tweets = cantidadA2) %>% 
  mutate(dia_Tweet = "General") %>% 
  rename(Response = tiempo) %>% 
  select(MesNombrePregunta, dia_Tweet, Response, 
         categoria, tipo, No_Tweets) %>%  data.frame()

rbind(general2,general1, ajustado1, ajustado2) %>%  
  write.csv("ResponseTwitter.csv", row.names=F)


pruebas  %>% 
  filter(grepl("[?]",MensajePregunta)) %>% 
  mutate(idPregunta=as.character(idPregunta)) %>% 
  mutate(IdRespuesta=as.character(IdRespuesta)) %>% 
  mutate(linkPregunta=paste("https://twitter.com/statuses/", 
                            idPregunta,sep="")) %>% 
  mutate(linkRespuesta=paste("https://twitter.com/statuses/",
                             IdRespuesta,sep="")) %>% 
  select(FechaPregunta,MensajePregunta,linkPregunta,FechaRespuesta,
         MensajeRespuesta, linkRespuesta) %>% 
  write.csv("LinksTwitterGeneral.csv",row.names=F)

filtrot %>% 
  mutate(sabadoNoLaboral=ifelse(is.na(sabadoNoLaboral), "Laboral",
                                "NoLaboral")) %>%
  filter(sabadoNoLaboral=="Laboral") %>% 
  filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>% 
  mutate(idPregunta=as.character(idPregunta)) %>% 
  mutate(IdRespuesta=as.character(IdRespuesta)) %>% 
  mutate(linkPregunta=paste("https://twitter.com/statuses/", 
                            idPregunta,sep="")) %>% 
  mutate(linkRespuesta=paste("https://twitter.com/statuses/",
                             IdRespuesta,sep="")) %>% 
  select(FechaPregunta,MensajePregunta,linkPregunta,FechaRespuesta,
         MensajeRespuesta, linkRespuesta) %>% 
  write.csv("LinksTwitterAjustado.csv",
            row.names=F)




# Response Rate -----------------------------------------------------------
MencionesNoseat$ID %in% pruebas$ID %>%  table
Menciones %>%  filter(ID==831658813765910528)
Preguntas %>% dim
MencionesNoseat %>%  head

ReplyTo = Menciones %>%  filter(ID %in% Menciones$idReplyTo) %>% 
  select(Fecha, ID, idReplyTo,usuarioID, usuarioNombre, mensaje) %>% 
  filter(!is.na(idReplyTo))

MencionesFiltro = Menciones %>%  filter(ID %!in% Menciones$idReplyTo) 
mencionado = merge(ReplyTo,MencionesFiltro, by="idReplyTo")
mencionado %>%  head
mencionado %>% dim
ReplyTo %>% head

Menciones %>%  filter(ReplyToUserName!="SEAT México") %>%  
  filter(usuarioNombre=="SEAT México") 




#test = c("Quieres galletas?", "Dame galletas ahora")
#grepl("[?]",test)
pruebas = pruebas %>%  unique 
MencionesNoseat = MencionesNoseat %>%  unique 

mencionesC = pruebas %>% #filter(grepl("[?]",MensajePregunta)) %>% 
  select(FechaPregunta, idPregunta) %>%  
  mutate(contesta = "contestado") %>% rename(Fecha=FechaPregunta) %>% 
  rename(ID=idPregunta)

mencionesNC = MencionesNoseat %>% filter(grepl("[?]", mensaje)) %>% 
  filter(ID %!in% mencionesC$ID) %>% 
  select(Fecha, ID) %>% 
  mutate(contesta="No_contestado") 

mencionesC = rbind(mencionesC, mencionesNC)

mencionesC %>%  separate(Fecha, c("fecha", "hora"), sep=" ") %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(anio=year(fecha), mes=month(fecha), dia=day(fecha)) %>% 
  group_by(mes, dia, contesta) %>% tally %>% 
  spread(contesta, n,fill = 0) %>%  data.frame %>% 
  mutate(totales = rowSums(.[3:4])) %>% 
  mutate(ResponseRate= (contestado/totales)*100) %>% 
  mutate(ResponseRate= round(ResponseRate, digits=2)) %>% 
  write.csv("RRateTW.csv", row.names=F)






