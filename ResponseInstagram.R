library(magrittr)
c("httr","XML","dplyr","data.table") %>% 
  sapply(require,character.only=T)

##############################################################
#El siguiente script está diseñado para obtener información básica 
# de Instagram en modo SandBox
# Para obtener información completamente pública 
##############################################################



###INSTAGRAM ACCESS TOKEN
basein     = "https://api.instagram.com/v1/"
self       = "users/self/"
media1     = "media/recent?access_token="
media2     = "media/"
comments   = "/comments?access_token="
accessT    = "?access_token="
in_token   = "24665227.e192121.5e0c22f680154a628cf119f6dec5be76"
idinstSeat = 24665227
idPostIn   = "1464918374701094295_24665227"

infoinst = paste(basein, self,media1, in_token, sep="" )

t = infoinst %>%  readLines() %>%  fromJSON() 
t = t$data
idsPost = t %>%  lapply(function(x){x$id}) %>%  unique %>%  unlist

mediaInfo    = paste(basein,media2, idsPost,accessT, in_token, sep="" )
commentsinst = paste(basein,media2, idsPost, comments, in_token, sep="" )

x = commentsinst[5] %>%  readLines %>%  fromJSON()
z = commentsinst[1] %>%  readLines %>%  fromJSON()

paste(basein,media2, idPostIn, comments, in_token, sep="" ) %>% 
  readLines() %>%  fromJSON()


x = mediaInfo[5] %>%  readLines %>%  fromJSON
z = mediaInfo[8] %>%  readLines %>%  fromJSON

z = z$data
z %>% list %>%   lapply(function(x){x$likes$count})
z %>% list %>%   lapply(function(x){x$comments$count})
z %>% list %>%   lapply(function(x){x$created_time})
z %>% list %>%   lapply(function(x){x$caption$text})
z %>% list %>%   lapply(function(x){x$link})
z %>% list %>%   lapply(function(x){x$tags})
z %>% list %>%   lapply(function(x){x$type})
z %>% list %>%   lapply(function(x){x$id})

mediaInfo 

infoInsta = lapply(mediaInfo,function(x){
  y = readLines(x) %>%  fromJSON
  y = y$data
  y = y %>%  list
  likes    = y %>%  
    lapply(function(d){d$likes$count}) %>%  as.numeric
  
  comments = y  %>% 
    lapply(function(d){d$comments$count}) %>%  as.numeric
  created_time = y %>%  
    lapply(function(d){d$created_time}) %>%  as.numeric
  created_time = as.POSIXct(created_time, origin="1970-01-01")
  
  texto = y  %>%  
    lapply(function(d){d$caption$text})
  link = y %>%   
    lapply(function(d){d$link})
  tags = y  %>%   
    lapply(function(d){d$tags})
  type = y  %>% 
    lapply(function(d){d$type})
  link = y  %>% 
    lapply(function(d){d$link})
  id = y  %>% 
    lapply(function(d){d$id})
  
  X = data.frame(id,link, likes, comments, created_time, texto, 
                 tags, type)
  names(X)<-c("id","link", "likes", "comments", "created_time", "texto", 
              "tags", "type")
  Sys.sleep(1.5)
  return(X)
}) %>%  do.call("rbind",.)

infoInsta = infoInsta %>%  distinct(id, .keep_all=T)
infoInsta = infoInsta %>% separate(created_time, c("fecha","hora"), sep=" ")
  


infoInsta %>%  
  mutate(fecha= as.Date(fecha)) %>% 
  ggplot(aes(x =fecha, y = likes)) +
  geom_point(col="darkred")+ geom_line(col="steelblue") + 
  theme_bw()

infoInsta %>%  
  mutate(fecha= as.Date(fecha)) %>% 
  ggplot(aes(x =fecha, y = comments)) +
  geom_point(col="darkred")+ geom_line(col="steelblue") + 
  theme_bw()

infoInsta %>%  
  write.csv("~/local/seat_analisis_extras/ResponseTime/datos/infoInsta.csv",
            row.names=F)

