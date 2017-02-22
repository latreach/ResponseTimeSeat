# Librerias ---------------------------------------------------------------
library(magrittr)
c("dplyr", "tidyr","lubridate","twitteR","Rfacebook","ggplot2",
  "twitteR") %>% 
  sapply(require,character.only=T)

####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
####################################


# Conexión facebook API ---------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="35ff99b85e4bf364b35faeb0a850dbd4", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")
idFB_seat = 113144262054871


# Conexión Twitter Cuenta SEAT --------------------------------------------------------
key          = "vDfPjIl7fRMjwHwYfj0rz5Vid"
secret       = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgWB"
access_token = "137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
secret_token = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
setup_twitter_oauth(key, secret, access_token, secret_token)
user         = "SEAT_Mexico"
useriD       = 137735852

# Funciones ---------------------------------------------------------------
"%!in%" <- function(x,y)!("%in%"(x,y))

diasHoras = function(dias){dias*24}
HorasMin  = function(horas){horas*60}
MinHoras  = function(minutos){minutos/60}

horasHorasMin = function(hora){
  x = hora*60
  y = x/60;  y = floor(y)
  z = x - (y*60); z = floor(z)
  if(z< 10){
    z = paste(0 ,z, sep="")
    z = paste(y,z,sep=":")
    return(z)
  }
  z = paste(y,z,sep=":")
  return(z)
}

x= "2017-02-05"
#rm(x)
is.Festivo = function(x){
  festivos = c("-01-01","-02-05", "-03-21","-05-01", "-09-16",
               "-11-20","-12-25")
  x = as.Date(x)
  anioComparativo = lubridate::year(x)
  festivosCompara = paste(anioComparativo, festivos, sep="")
  especiales = festivosCompara[c(2,3,6)]
  extras = lapply(especiales,function(d){
    inicio = paste(year(d), month(d), 1, sep="-")
    final = paste(year(d), month(d)+1, 1, sep="-")
    secuencia  = seq.POSIXt(as.POSIXct(inicio), 
                            as.POSIXct(final), by="day")
    secuencia = secuencia[wday(secuencia)==2]
    if(month(secuencia[1])==2){
      secuencia  = secuencia[1]
    }else{secuencia = secuencia[3]}
  })
  extras= unlist(extras)
  extras = as.Date(as.POSIXct(extras, origin="1970-01-01"))
  
  if(wday(especiales[1])!=2){
    festivosCompara= c(extras[1], as.Date(festivosCompara))
  }
  if(wday(especiales[2])!=2){
  festivosCompara= c(extras[2], as.Date(festivosCompara))
  }
  if(wday(especiales[3])!=2){
     festivosCompara= c(extras[3], as.Date(festivosCompara))
  }
  # if(wday(especiales[1])|wday(especiales[2])|wday(especiales[3])!=2){
  #   festivosCompara= c(extras, as.Date(festivosCompara))
  #   print(festivosCompara)
  # }
  z = festivosCompara[festivosCompara==x]
  print(z)
  
  if(identical(z, character(0))==T){
       "Laboral"
    }else{"Festivoas"}

}

is.Festivo("2016-01-01")


separado = function(x){
  steps=x
  function(y){seq(0, ceiling(max(y)), by=steps)}
}

# Directorio --------------------------------------------------------------

setwd("~/local/seat_analisis_extras/ResponseTime/datos/")

# Globales ----------------------------------------------------------------
meses <-c("Enero","Febrero","Marzo","Abril","Mayo","Junio", "Julio","Agosto",
          "Septiembre","Octubre","Noviembre","Diciembre")

# Facebook ----------------------------------------------------------------
Febrero = getPage(idFB_seat, n=500, since="2017-02-01", until= "2017-02-22",
        feed=T, reactions=F, token = fb_oauth)

Febrero %>%  write.csv("febrero.csv", row.names=F)

#Posteos
foraneos = Febrero %>% filter(from_id!=idFB_seat) 
IDS = foraneos$id

RRate1 = lapply(IDS, function(x){
  X = getPost(x, token = fb_oauth)
  y = X$post %>%  select(id) %>% unlist
  w = X$post %>%  select(created_time) %>%  unlist
  w1 = X$post %>%  select(message) %>%  unlist
  z = X$comments %>%  select(from_id, from_name, message,created_time, id) %>% 
    mutate(idPosteo = y) %>%  mutate(FechaMensaje=w) %>% 
    mutate(MensajePost = w1)
  names(z)<-c("IdCreadorComentario","NombreCreadorComentario", 
              "MensajeComentario", "FechaComentario","IdComentario",
              "IdPosteo","FechaPosteo","MensajePosteo") 
  return(z)
})

RRate1 = do.call("rbind",RRate1) 
RRate1 = RRate1 %>% filter(IdCreadorComentario==idFB_seat) %>% 
          separate(FechaComentario,
                   c("FechaComment", "HoraComment"), sep="T",remove = F) %>% 
          mutate(FechaComentario= gsub("T"," ", FechaComentario)) %>% 
          separate(FechaPosteo,
                   c("FechaPost", "HoraPost"), sep="T",remove = F) %>% 
          mutate(FechaPosteo = gsub("T"," ", FechaPosteo))

RRateG = RRate1 %>%  group_by(IdPosteo) %>% filter(row_number()==1) %>%
          data.frame

RRateG = RRateG %>% mutate(FechaComentario= as.POSIXct(FechaComentario)) %>% 
          mutate(FechaPosteo = as.POSIXct(FechaPosteo)) %>%  
          mutate(diferenciaTiempo = 
                   difftime(FechaComentario, FechaPosteo, units="hours")) %>% 
          mutate(diferenciaTiempo = as.numeric(diferenciaTiempo)) %>% 
          mutate(mesPost = month(FechaPost), diaPost = day(FechaPost)) %>% 
          mutate(mesComment = month(FechaComment),
                 diaComment = day(FechaComment)) %>%
          mutate(anioComment =year(FechaComment), anioPost = year(FechaPost)) %>% 
          mutate(diaNombrePost= wday(FechaPost, label = T), 
                 diaNombreComment=wday(FechaPost, label=T)) %>% 
          mutate(horaPost = hour(FechaPosteo),
                 horaComment = hour(FechaComentario)) %>% 
          mutate(minutoPost = minute(FechaPosteo),
                 minutoComment = minute(FechaComentario),
                 minutoHoraPost = paste(horaPost, minutoPost, sep=":"), 
                 minutoHoraComment = paste(horaComment,
                                           minutoComment, sep=":")) %>% 
          mutate(mesNombrePost = factor(mesPost))

levels(RRateG$mesNombrePost)<-meses[2]
RRate1$IdPosteo %in% RRateG$IdPosteo %>%  table

paste("tu response Rate es ", 100,"%", sep=" " )
RRateG %>%  select(mesPost, diaPost, diferenciaTiempo) %>% 
  group_by(mesPost, diaPost) %>%  summarise(RRateG = mean(diferenciaTiempo))


cuantosDia =RRateG %>%  select(mesPost, diaPost, diferenciaTiempo) %>% 
  group_by(diaPost) %>%  tally %>%  select(n) %>%  unlist

cuantosMes =RRateG %>%  select(mesPost, diaPost, diferenciaTiempo) %>% 
  group_by(mesPost) %>%  tally %>%  select(n) %>% unlist

RRateDia = RRateG %>%  select(mesNombrePost, diaPost, diferenciaTiempo) %>% 
            group_by(mesNombrePost, diaPost) %>%  
            summarise(RRateG = mean(diferenciaTiempo)) %>% 
            mutate(RRateG= sapply(RRateG, horasHorasMin)) %>%  
            mutate(categoria ="Por día") %>%  rename(diaPosteo=diaPost) %>% 
            mutate(tipo="ResponseTime General") %>%  rename(Response=RRateG) %>% 
            mutate(No_Posteos = cuantosDia) %>% data.frame


RRateMensual = RRateG %>%  select(mesNombrePost, diaPost, diferenciaTiempo) %>% 
                group_by(mesNombrePost) %>% 
                summarise(RRateG = mean(diferenciaTiempo)) %>% 
                mutate(diaPosteo="General") %>% 
                mutate(RRateG =sapply(RRateG, horasHorasMin)) %>% 
                mutate(categoria ="Por Mes") %>%
                mutate(tipo="ResponseTime General") %>% 
                rename(Response=RRateG) %>% 
                mutate(No_Posteos= cuantosMes) %>% 
                select(mesNombrePost, diaPosteo, Response, categoria, tipo,
                       No_Posteos) %>% data.frame

cuantosDiaT = RRateG %>%  
            left_join(RRateG %>% filter(diaNombrePost %in% "Sat") %>% 
                        filter(horaPost<8) %>%  filter(horaPost>14) %>% 
                        mutate(sabadoNoLaboral="No Labora") %>%
                        mutate(sabadoNoLaboral =
                                 ifelse(sabadoNoLaboral=="No Labora",
                                        "No Labora","Labora"))) %>% 
            filter(diaNombrePost %!in% c("Sun", "Sat")) %>% 
            filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>%
            filter(diaPost!=6) %>% 
            filter(grepl("[?]", MensajePosteo)) %>% group_by(diaPost) %>% 
            tally %>% select(n) %>%  unlist

cuantosMesT = RRateG %>%  filter(diaNombrePost %!in% c("Sun", "Sat")) %>% 
                filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
                filter(diaPost!=6) %>% filter(grepl("[?]", MensajePosteo)) %>%  
                group_by(mesPost) %>% tally %>% select(n) %>%  unlist


RRateAjustadoMes = RRateG %>%  filter(diaNombrePost %!in% c("Sun", "Sat")) %>% 
                    filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
                    filter(grepl("[?]", MensajePosteo)) %>% 
                    mutate(diferenciaTruncada =
                             difftime(FechaComentario, 
                                      FechaPosteo, units="hours")) %>% 
                    mutate(diferenciaTruncada =
                             as.numeric(diferenciaTruncada)) %>% 
                    group_by(mesNombrePost) %>% 
                    summarise(RRateAdjust = mean(diferenciaTruncada)) %>% 
                    mutate(diaPosteo="General") %>% 
                    mutate(RRateAdjust =sapply(RRateAdjust, horasHorasMin)) %>% 
                    mutate(categoria ="Por Mes") %>%
                    mutate(tipo="ResponseTime Ajustado") %>% 
                    rename(Response=RRateAdjust) %>%  
                    mutate(No_Posteos= cuantosMesT) %>% 
                    select(mesNombrePost, diaPosteo,
                           Response, categoria, tipo, No_Posteos) %>% data.frame
  
RRateAjustadoDia = RRateG %>% filter(diaNombrePost %!in% c("Sun", "Sat")) %>% 
                    filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
                    filter(grepl("[?]", MensajePosteo)) %>% 
                    mutate(diferenciaTruncada =
                             difftime(FechaComentario, FechaPosteo,
                                      units="hours")) %>% 
                    mutate(diferenciaTruncada =
                             as.numeric(diferenciaTruncada)) %>% 
                    group_by(mesNombrePost, diaPost) %>% 
                    summarise(RRateAdjust =
                                mean(diferenciaTruncada)) %>% 
                    mutate(RRateAdjust= sapply(RRateAdjust, horasHorasMin)) %>% 
                    mutate(categoria ="Por día") %>%
                    rename(diaPosteo=diaPost) %>% 
                    mutate(tipo="ResponseTime Ajustado") %>%  
                    rename(Response=RRateAdjust) %>% 
                    mutate(No_Posteos = cuantosDiaT) %>% data.frame
  
rbind(RRateDia, RRateMensual, RRateAjustadoDia, RRateAjustadoMes) %>%  
  write.csv("ResponseTimeFbPosteos.csv",
            row.names=F)

RRateG %>%  mutate(Link = paste("https://www.facebook.com/", IdPosteo, sep= "")) %>%
            select(MensajePosteo,IdPosteo, Link, FechaPost, HoraPost, anioPost,
                   mesPost,mesNombrePost, diaPost,diaNombrePost ) %>% 
            write.csv("linksGeneral.csv")

RRateG %>% mutate(Link = paste("https://www.facebook.com/", IdPosteo,
                               sep= "")) %>% 
            filter(diaNombrePost %!in% c("Sun", "Sat")) %>% 
            filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
            filter(grepl("[?]", MensajePosteo)) %>% 
            select(MensajePosteo,IdPosteo, Link, FechaPost, HoraPost,
                   anioPost, mesPost,mesNombrePost, diaPost,diaNombrePost) %>% 
            write.csv("linksAjustado.csv")

# Comentarios -------------------------------------------------------------
idsComments  = RRateG$IdComentario
IDsPost      = Febrero$id
contador     = 0

#A prueba de errores
Comments =  lapply(IDsPost, function(x){
  contador<<- contador+1
  print(c(x,contador))
  X = tryCatch(
    getPost(x, token = fb_oauth)$comments %>%  
      mutate(idPost = x),
    error =  function(e){NULL})
  return(X)
})

Comments = do.call("rbind", Comments)
#Comments = Comments %>% filter(id %!in% idsComments) 
comentariosIDS = Comments$id
writeLines(comentariosIDS,
           "~/local/seat_analisis_extras/ResponseTime/idsComments.txt")

### Las replicas a comentarios se obtendrán con el archivo
### facebookRepliesComments.py

idsSinresponder = 
  readLines("SinResponder.txt") %>% 
  gsub("(/).*","", .)

Comments %>%  filter(id %in% idsSinresponder) %>% 
              filter(from_id!=idFB_seat) %>% 
              filter(grepl("[?]",message)) %>% head

Comments %>%  filter(id %!in% idsSinresponder) %>% 
  filter(from_id==idFB_seat) %>% tail

replies = read.csv( "CommentsReply.csv", header = T)

replies %>% filter(IDUsuarioCommentParent!=idFB_seat) %>% 
  filter(IDUsuarioReply==idFB_seat) %>% head
  
# Datos -------------------------------------------------------------------
# menciones  = mentions(n=1000)
# menciones  = menciones %>% twListToDF()
# 
# menciones %>%  filter(id==831622235060199424)
# menciones %>%  separate(created,c("Fecha","Hora"), sep=" ",remove=F) %>%
#   mutate(Fecha = as.Date(Fecha), anio = year(Fecha),mes = month(Fecha),
#          dia=day(Fecha)) %>%
#   filter(Fecha>"2017-02-01") %>% filter(dia==8)
# TimeLine = userTimeline(user="SEAT_Mexico",
#                         includeRts = T, n=3200)

RT       = retweetsOfMe(n=800)
TimeLine = RT %>% twListToDF() 

TimeLine %>%  write.csv("BaseRetweet.csv",row.names=F)

TimeLine = TimeLine %>% 
            separate(created, c("Fecha","Hora"),sep=" ") %>% 
            mutate(Fecha = as.Date(Fecha), mes=month(Fecha), dia=day(Fecha)) %>%
            mutate(mesNombre = factor(mes))

levels(TimeLine$mesNombre)<-c("Enero","Febrero","Septiembre","Octubre",
                              "Noviembre","Diciembre")

retMes = TimeLine  %>% 
          filter(mes==2) %>%  filter(isRetweet==F) %>%
          group_by(mesNombre) %>% 
          summarise(cantidad = sum(retweetCount)) %>% 
          rename(No_retweets=cantidad) %>% 
          mutate(tipo="Por Mes") %>%  
          mutate(dia="general") %>% 
          select(mesNombre, dia, No_retweets, tipo) %>%
          data.frame

retDia =TimeLine  %>% 
        filter(mes==2) %>%  filter(isRetweet==F) %>%
        group_by(mesNombre, dia)%>% 
        summarise(cantidad = sum(retweetCount)) %>% 
        rename(No_retweets=cantidad) %>% 
        mutate(tipo="Por Día") %>%  data.frame()

rbind(retDia, retMes) %>% write.csv("retweets.csv", row.names=F)

idTimeLine = TimeLine$id

Menciones = read.csv("Mentions.csv", header = T)
Menciones2 =read.csv("Mentions2.csv",
                     header = T)
Menciones = rbind(Menciones, Menciones2)
Menciones = Menciones %>%  distinct(ID, .keep_all=T)
Menciones %>%  write.csv("Mentions.csv", row.names=F)

MencionesNoseat = Menciones %>%  filter(usuarioID!=useriD)

Mencionesseat = Menciones %>%  filter(usuarioID==useriD)

Preguntas = MencionesNoseat %>% filter(ID %in% Mencionesseat$idReplyTo) 

#### En proceso
ids = Menciones$idReplyTo[Menciones$ID %!in% Menciones$idReplyTo]
ids = ids[ids %!in%  menciones$id]
ids = ids %>%  na.omit() %>%  as.numeric()
ids = ids %>% unique 

faltantes = lapply(ids[1:10], function(x){
  x = as.character(x)
  print(x %>% showStatus)
  X  = tryCatch( 
    showStatus(x) %>%  twListToDF,
    error= function(e)NULL)
  return(X)
})
faltantes = faltantes %>%  unlist
faltantes[1]
lapply(faltantes[1:2], print)

test= lapply(faltantes[1:2], function(x){
  twListToDF(faltantes)

})
do.call("rbind",faltantes)
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
                    diaNombrePregunta = wday(FechaPregunta, label=T),
                    diaNombreRespuesta = wday(FechaRespuesta, label=T),
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

levels(pruebas$MesNombrePregunta)<-meses[2]

MencionesNoseat %>% filter(ID %!in% pruebas$idPregunta) %>%  
  mutate(ID = as.character(ID)) %>% 
  mutate(link = paste("https://twitter.com/statuses/",ID, sep="")) %>% 
  select(ID, Fecha, mensaje, usuarioID, usuarioNombre, link) %>% 
  write.csv("tweetsNorespondidos.csv", row.names=T)

cantidadg1 = pruebas %>% 
              filter(mesPregunta==2) %>%
              group_by(mesPregunta) %>%  tally %>% select(n) %>%  unlist

general1 =  pruebas %>%  filter(mesPregunta==2) %>%
              group_by(MesNombrePregunta) %>% 
              summarise(tiempo = mean(diferenciaTiempo)) %>% 
              mutate(tiempo= horasHorasMin(tiempo)) %>% 
              mutate(categoria ="Por Mes") %>% 
              mutate(tipo="ResponseTime General") %>% 
              mutate(No_Tweets = cantidadg1) %>% 
              mutate(dia_Tweet = "General") %>% 
              rename(Response = tiempo) %>% 
              select(MesNombrePregunta, dia_Tweet, Response, 
                     categoria, tipo, No_Tweets)%>%  data.frame()


cantidadg2 = pruebas %>%
  filter(mesPregunta==2) %>% group_by(diaPregunta) %>%  tally %>% 
  select(n) %>%  unlist

general2 = pruebas %>%  filter(mesPregunta==2) %>%
            group_by(MesNombrePregunta,diaPregunta) %>% 
            summarise(tiempo = mean(diferenciaTiempo)) %>% 
            mutate(tiempo= sapply(tiempo,horasHorasMin)) %>% 
            rename(dia_Tweet=diaPregunta) %>% 
            mutate(categoria="Por dia") %>% 
            mutate(tipo="ResponseTime General") %>% 
            mutate(No_Tweets=cantidadg2)%>%
            rename(Response=tiempo) %>% data.frame()

cantidadA1 = pruebas %>% filter(mesPregunta==2) %>% 
              filter(diaPregunta %!in% c("Sun","Sat")) %>% 
              filter(horaPregunta>8) %>%
              filter(HoraMinPregunta<"18:30") %>% 
              filter(grepl("[?]",MensajePregunta)) %>%
              group_by(diaPregunta) %>%  tally %>%
              select(n) %>%  unlist

ajustado1 = pruebas %>%  filter(mesPregunta==2) %>% 
              filter(diaPregunta %!in% c("Sun","Sat")) %>% 
              filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
              filter(grepl("[?]",MensajePregunta)) %>% 
              group_by(MesNombrePregunta,diaPregunta) %>% 
              summarise(tiempo=mean(diferenciaTiempo)) %>% 
              mutate(tiempo= sapply(tiempo,horasHorasMin)) %>% 
              rename(dia_Tweet=diaPregunta) %>% 
              mutate(categoria="Por dia") %>% 
              mutate(tipo="ResponseTime Ajustado") %>% 
              mutate(No_Tweets=cantidadA1) %>%
              rename(Response= tiempo) %>% data.frame()


cantidadA2 = pruebas %>%  filter(mesPregunta==2) %>% 
              filter(diaPregunta %!in% c("Sun","Sat")) %>% 
              filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
              filter(grepl("[?]",MensajePregunta)) %>%
              group_by(mesPregunta) %>%  tally %>% select(n) %>%  unlist

ajustado2 = pruebas %>%  filter(mesPregunta==2) %>% 
              filter(diaPregunta %!in% c("Sun","Sat")) %>% 
              filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
              filter(grepl("[?]",MensajePregunta)) %>%
              group_by(MesNombrePregunta) %>% 
              summarise(tiempo=mean(diferenciaTiempo)) %>% 
              mutate(tiempo= horasHorasMin(tiempo)) %>% 
              mutate(categoria ="Por Mes") %>% 
              mutate(tipo="ResponseTime Ajustado") %>% 
              mutate(No_Tweets = cantidadA2) %>% 
              mutate(dia_Tweet = "General") %>% 
              rename(Response = tiempo) %>% 
              select(MesNombrePregunta, dia_Tweet, Response, 
                     categoria, tipo, No_Tweets) %>%  data.frame()

rbind(general2,general1, ajustado1, ajustado2) %>%  
  write.csv("ResponseTwitter.csv")


pruebas %>%  filter(mesPregunta==2) %>% 
  mutate(idPregunta=as.character(idPregunta)) %>% 
  mutate(IdRespuesta=as.character(IdRespuesta)) %>% 
  mutate(linkPregunta=paste("https://twitter.com/statuses/", 
                            idPregunta,sep="")) %>% 
  mutate(linkRespuesta=paste("https://twitter.com/statuses/",
                             IdRespuesta,sep="")) %>% 
  select(FechaPregunta,MensajePregunta,linkPregunta,FechaRespuesta,
         MensajeRespuesta, linkRespuesta) %>% 
  write.csv("LinksTwitterGeneral.csv",row.names=F)

pruebas %>% 
  filter(mesPregunta==2) %>% 
  filter(diaPregunta %!in% c("Sun","Sat")) %>% 
  filter(horaPregunta>8) %>% filter(HoraMinPregunta<"18:30") %>% 
  filter(grepl("[?]",MensajePregunta)) %>% 
  filter(mesPregunta==2) %>% 
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

# Gráficas ----------------------------------------------------------------
testeo = read.csv("testeo.csv", header = T)
trimestre = c("1er Trimestre", "2do Trimestre", "3er Trimestre",
              "4to Trimestre") %>% 
              sapply(rep, 3) %>%
              unlist %>%  as.character() %>% c("1er Trimestre")

testeo$trimestre = trimestre

testeo = testeo %>%  gather(tipo, valor, -mes, -anio, -trimestre) 

niveles =  levels(factor(testeo$mes))  
niveles[c(4,5,8,1,9,7,6,2,12,11,10,3)]

testeo$mesReorder = factor(testeo$mes, 
                           levels = niveles[c(4,5,8,1,9,7,6,2,12,11,10,3)])

testeo$mesReorder2 = factor(testeo$mes, 
                           levels = niveles[c(3,10,11,12,2,6,7,9,1,8,5,4)])

testeo %>%  mutate(valor = ifelse(valor==0, NA, valor)) %>% 
ggplot(aes(x = mesReorder, y = valor, fill=factor(anio)))+
  geom_bar(stat="identity")+
  facet_wrap(~trimestre,scales="free")
  
ggplot(testeo,aes(y = mesReorder2, x = valor, color=factor(anio),
                  shape=factor(tipo)))+
  geom_point(size=8)+ scale_shape_manual(values=c( "*", "♥"), name="")+
  scale_color_manual(values=c("steelblue","darkgreen"), name="")+
  xlab("Cantidad")+ ylab("Mes") + theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~trimestre,scales="free_y")+
  scale_x_continuous(breaks = separado(1000))+
  geom_text(aes(label= ifelse(valor==0,"", valor)), 
            position=position_dodge(width=0.9),
            vjust=-1.1,size=2.5,family="bold", col="black",
            check_overlap = F)+
  theme(panel.grid.major.x   = element_blank())+
  theme(panel.grid.minor.x   = element_blank())



ggplot(testeo,aes(y = mesReorder2, x = valor, color=factor(anio),
                  shape=factor(tipo)))+
  geom_point(size=5)+ scale_shape_manual(values=c(20,18 ), name="")+
  scale_color_manual(values=c("steelblue","darkgreen"), name="")+
  xlab("Cantidad")+ ylab("Mes") + theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~trimestre,scales="free_y")+
  scale_x_continuous(breaks = separado(1000))+
  geom_text(aes(label= ifelse(valor==0,"", valor)), 
            position=position_dodge(width=0.9),
            vjust=-1.1,size=2.5,family="bold", col="black",
            check_overlap = F)+
  theme(panel.grid.major.x   = element_blank())+
  theme(panel.grid.minor.x   = element_blank())


