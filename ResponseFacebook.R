####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
####################################

######
#Cargando las librerías
######

# Librerias ---------------------------------------------------------------
library(magrittr)
c("dplyr", "tidyr","lubridate","Rfacebook","ggplot2","googlesheets") %>% 
  sapply(require,character.only=T)


# Realizando la conexión APIS REDES SOCIALES

# Conexión facebook API ---------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="35ff99b85e4bf364b35faeb0a850dbd4", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")
idFB_seat = 113144262054871


# Funciones ---------------------------------------------------------------
source("~/local/seat_analisis_extras/ResponseTime/funcionesResponse.R")

# Directorio --------------------------------------------------------------
setwd("~/local/seat_analisis_extras/ResponseTime/datos/")

# Objetos globales ----------------------------------------------------------------
meses <-c("Enero","Febrero","Marzo","Abril","Mayo","Junio", "Julio","Agosto",
          "Septiembre","Octubre","Noviembre","Diciembre")

# Facebook ----------------------------------------------------------------
Febrero =  read.csv("febreroMarzo.csv", header = T)

Febrero1 = getPage(idFB_seat, n=1000, since="2017-02-01", until= "2017-03-27",
        feed=T, reactions=F, token = fb_oauth)

Febrero = rbind(Febrero1, Febrero) %>% 
  distinct(id, .keep_all=T)
Febrero %>%  write.csv("febreroMarzo.csv", row.names=F)

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
RRate1 %>%  write.csv("foraneosSeat.csv", row.names=F)

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
          mutate(diaNombrePost= lubridate::wday(FechaPost, label = T), 
                 diaNombreComment=lubridate::wday(FechaPost, label=T)) %>% 
          mutate(horaPost = hour(FechaPosteo),
                 horaComment = hour(FechaComentario)) %>% 
          mutate(minutoPost = minute(FechaPosteo),
                 minutoComment = minute(FechaComentario),
                 minutoHoraPost = paste(horaPost, minutoPost, sep=":"), 
                 minutoHoraComment = paste(horaComment,
                                           minutoComment, sep=":")) %>% 
          mutate(mesNombrePost = factor(mesPost))

levels(RRateG$mesNombrePost)<-meses[2:3]
RRate1$IdPosteo %in% RRateG$IdPosteo %>%  table

paste("tu response Rate es ", 100, "%", sep=" " )
RRateG %>%  select(mesPost, diaPost, diferenciaTiempo) %>% 
  group_by(mesPost, diaPost) %>%  summarise(RRateG = mean(diferenciaTiempo))

###Totales----------------------------------
cuantosDiaT = RRateG %>%  select(mesNombrePost, diaPost, diferenciaTiempo) %>%
  group_by(mesNombrePost, diaPost) %>%  tally %>% .$n %>%  unlist

cuantosMesT =RRateG %>%  select(mesPost, diaPost, diferenciaTiempo) %>%
  group_by(mesPost) %>%  tally %>%  select(n) %>% unlist


RRateDiaT = RRateG %>%  select(mesNombrePost, diaPost, diferenciaTiempo) %>%
            group_by(mesNombrePost, diaPost) %>%
            summarise(RRateG = mean(diferenciaTiempo)) %>%
            mutate(RRateG= sapply(RRateG, horasHorasMin)) %>%
            mutate(categoria ="Por día") %>%  rename(diaPosteo=diaPost) %>%
            mutate(tipo="ResponseTime Total") %>%  rename(Response=RRateG) %>%
            data.frame(., No_Posteos=cuantosDiaT) %>% data.frame

RRateMensualT = RRateG %>%  select(mesNombrePost, diaPost, diferenciaTiempo) %>%
                group_by(mesNombrePost) %>%
                summarise(RRateG = mean(diferenciaTiempo)) %>%
                mutate(diaPosteo="General") %>%
                mutate(RRateG =sapply(RRateG, horasHorasMin)) %>%
                mutate(categoria ="Por Mes") %>%
                mutate(tipo="ResponseTime Total") %>%
                rename(Response=RRateG) %>%
                mutate(No_Posteos= cuantosMesT) %>%
                select(mesNombrePost, diaPosteo, Response, categoria, tipo,
                       No_Posteos) %>% data.frame

rbind(RRateDiaT, RRateMensualT) %>%  
  write.csv("ResponseTimeFBTotal.csv", row.names=T)

# General y Ajustado ------------------------------------------------------

cuantosDia = RRateG %>% filter(grepl("[?]", MensajePosteo)) %>% 
  select(mesNombrePost, diaPost, diferenciaTiempo) %>% 
  group_by(mesNombrePost, diaPost) %>%  tally %>% .$n %>%  unlist 


cuantosMes = RRateG %>%  filter(grepl("[?]", MensajePosteo)) %>% 
  select(mesPost, diaPost, diferenciaTiempo) %>% 
  group_by(mesPost) %>%  tally %>%  select(n) %>% unlist

RRateDia = RRateG %>%  filter(grepl("[?]", MensajePosteo)) %>% 
            select(mesNombrePost, diaPost, diferenciaTiempo) %>% 
            group_by(mesNombrePost, diaPost) %>%  
            summarise(RRateG = mean(diferenciaTiempo)) %>% 
            mutate(RRateG= sapply(RRateG, horasHorasMin)) %>%  
            mutate(categoria ="Por día") %>%  rename(diaPosteo=diaPost) %>% 
            mutate(tipo="ResponseTime General") %>%  rename(Response=RRateG) %>% 
            data.frame(., No_Posteos=cuantosDia) %>% data.frame


RRateMensual = RRateG %>%filter(grepl("[?]", MensajePosteo)) %>% 
                select(mesNombrePost, diaPost, diferenciaTiempo) %>% 
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

filtro1 = RRateG %>%  filter(diaNombrePost!="Sun") %>% 
            left_join(RRateG %>% filter(diaNombrePost=="Sat" & horaPost>14| 
                                          horaPost<8) %>% 
                        mutate(sabadoNoLaboral="NoLaboral")) %>% 
          mutate(festivo =sapply(FechaPost, is.Festivo)) %>% 
          filter(festivo==F)
                      

cuantosDiaT = filtro1 %>% 
  mutate(sabadoNoLaboral= ifelse(is.na(sabadoNoLaboral),"Laboral",
                  "NoLaboral")) %>% filter(sabadoNoLaboral=="Laboral") %>% 
    filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>%
            filter(grepl("[?]", MensajePosteo)) %>% group_by(mesNombrePost,diaPost) %>% 
            tally %>% .$n %>%  unlist

cuantosMesT = filtro1  %>% 
                mutate(sabadoNoLaboral= ifelse(is.na(sabadoNoLaboral),
                                               "Laboral","NoLaboral")) %>%
                filter(sabadoNoLaboral=="Laboral") %>% 
                filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
                filter(grepl("[?]", MensajePosteo)) %>%  
                group_by(mesPost) %>% tally %>% select(n) %>%  unlist


RRateAjustadoMes = filtro1 %>%  
                    mutate(sabadoNoLaboral= ifelse(is.na(sabadoNoLaboral),"Laboral",
                                 "NoLaboral")) %>% 
                    filter(sabadoNoLaboral=="Laboral") %>% 
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
  
RRateAjustadoDia = filtro1 %>% 
                    mutate(sabadoNoLaboral= ifelse(is.na(sabadoNoLaboral),"Laboral",
                                 "NoLaboral")) %>% 
                    filter(sabadoNoLaboral=="Laboral") %>% 
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
                    data.frame(., No_Posteos=cuantosDiaT)
  
rbind(RRateDia, RRateMensual, RRateAjustadoDia, RRateAjustadoMes) %>%  
  write.csv("ResponseTimeFbPosteos.csv",
            row.names=F, fileEncoding="UTF-8")

RRateG %>% filter(grepl("[?]", MensajePosteo)) %>% 
            mutate(Link = paste("https://www.facebook.com/", IdPosteo, sep= "")) %>%
            select(MensajePosteo,IdPosteo, Link, FechaPost, HoraPost, anioPost,
                   mesPost,mesNombrePost, diaPost,diaNombrePost, 
                   FechaComment, HoraComment) %>% 
            write.csv("linksGeneral.csv")

filtro1 %>% mutate(Link = paste("https://www.facebook.com/", IdPosteo,
                               sep= "")) %>% 
            mutate(sabadoNoLaboral= ifelse(is.na(sabadoNoLaboral),"Laboral",
                                 "NoLaboral")) %>% 
            filter(sabadoNoLaboral=="Laboral") %>% 
            filter(horaPost>8) %>% filter(minutoHoraPost<"18:30") %>% 
            filter(grepl("[?]", MensajePosteo)) %>% 
            select(MensajePosteo,IdPosteo, Link, FechaPost, HoraPost,
                   anioPost, mesPost,mesNombrePost, diaPost,diaNombrePost,
                   FechaComment, HoraComment) %>% 
            write.csv("linksAjustado.csv")


##Response rate
foraneos = foraneos %>%  
  select(from_id, from_name, message, created_time, id,link, comments_count) %>% 
  separate(created_time, c("fecha", "hora"), sep="T") %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(anio=year(fecha), mes=month(fecha), dia=day(fecha),
         mesNombre = factor(mes))

No_contestadosfb  = foraneos %>%  filter(comments_count==0) %>% 
  mutate(link = paste("https://www.facebook.com/", id,
                      sep= "")) %>%  select(-comments_count) %>% 
  mutate(MensajeComentario=NA)

contestadofb = foraneos %>%  filter(comments_count!=0) %>% 
  mutate(link = paste("https://www.facebook.com/", id,
                      sep= "")) %>%  select(-comments_count)

contestacion = RRate1 %>% select(MensajeComentario, IdPosteo) 
contestadoUnion = merge(contestadofb,contestacion, by.x="id",
                        by.y="IdPosteo") 

contestadoUnion  = contestadoUnion[order(colnames(contestadoUnion))]
No_contestadosfb = No_contestadosfb[order(colnames(No_contestadosfb))]

contestadosFB = rbind(contestadoUnion,No_contestadosfb) %>% 
  mutate(contesta =
           ifelse(is.na(MensajeComentario),"No contesto","Contesto")) 

contestadosFB %>%
  filter(grepl("[?]", message)) %>% 
  filter(!is.na(MensajeComentario)) %>% 
  group_by(mes, dia) %>%  tally %>% 
  left_join(contestadosFB %>%
              filter(is.na(MensajeComentario)) %>% 
              group_by(mes, dia) %>%
              tally %>% rename(nocontestado=n)) %>% 
  mutate(nocontestado=ifelse(is.na(nocontestado),0,
                             nocontestado)) %>% data.frame %>%  
  mutate(totales = rowSums(.[,3:4])) %>% 
  mutate(RRate = (n/totales)*100) %>% 
  write.csv("RRateFB.csv", row.names=F)



# contestadosFB %>% filter(!is.na(MensajeComentario)) %>%
#   group_by(mes, dia) %>%  tally %>%
#   left_join(contestadosFB %>%
#               filter(is.na(MensajeComentario)) %>%
#               group_by(mes, dia) %>%
#               tally %>% rename(nocontestado=n)) %>%
#   mutate(nocontestado=ifelse(is.na(nocontestado),0,
#                              nocontestado)) %>% data.frame %>%
#   mutate(totales = rowSums(.[,3:4])) %>%
#   mutate(RRate = (n/totales)*100) %>%
#   write.csv("RRateFB.csv", row.names=F)


RRateMensualT
RRateMensual



  

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


