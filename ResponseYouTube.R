# Librerías ---------------------------------------------------------------
library(magrittr)
c("httr", "XML","rvest","rjson","dplyr","tidyr","lubridate") %>% 
  sapply(require, character.only=T)

####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################



accesstokenyt = "AIzaSyD-oZ8AqsVMJ7SzXd0yfpRTAzJaplegaB0"
idcanalSeat = "UC-zZEHZcc6AzBZ3iu55Eegg"
base="https://www.googleapis.com/youtube/v3/"
##Playlist de los canales
idsP = paste0(base,"playlists?part=snippet&channelId=",idcanalSeat,
       "&key=",accesstokenyt,"&fields=items(id)") %>% 
  readLines() %>% .[grep("id", .)] %>%
  gsub('\"',"",., fixed=T) %>% gsub("id:","", .) %>% 
  gsub("[[:blank:]]","",.)


##Videos de playlist
videoIDs = sapply(idsP, function(d){
  x = paste0(base,"playlistItems?part=snippet,contentDetails&playlistId=",d,
         "&key=",accesstokenyt,"&maxResults=50") %>%  readLines
  
test = grep("nextPageToken",x)

 if(!identical(test, integer(0))){
    z = x[grep("nextPageToken",x)] %>%  
      gsub('\"',"",., fixed=T) %>% 
      gsub("nextPageToken:|[[:blank:]]|,","",.)
    
    y = paste0(base,
               "playlistItems?part=snippet,contentDetails&playlistId=",d,
               "&key=",accesstokenyt,"&maxResults=50&pageToken=",z) %>%
        readLines()
 }

  if(!identical(test, integer(0))){
    x = x %>% .[grep("videoId",.)] %>% 
      gsub('\"',"",.,fixed=T) %>% 
      gsub("videoId:|,","",.) %>% 
      gsub("[[:blank:]]","",.) %>%  unique
    
    a = y %>% .[grep("videoId",.)] %>% 
      gsub('\"',"",.,fixed=T) %>% 
      gsub("videoId:|,","",.) %>% 
      gsub("[[:blank:]]","",.) %>%  unique
    
    return(c(x,a))
  
  }else{
    x = x %>% .[grep("videoId",.)] %>% 
      gsub('\"',"",.,fixed=T) %>% gsub("videoId:|,","",.) %>% 
      gsub("[[:blank:]]","",.) %>%  unique
    
      return(x)}

})
videoIDs=videoIDs %>%  unlist %>%  unname


##Información de videos
parametros=paste0("&fields=items(replies(comments)",
                  "snippet(topLevelCommment(snippet(textDisplay,publishedAt,authorDisplayName)))")

paste0(base,"commentThreads?part=snippet,replies&videoId=",
       videoIDs[1],"&key=",accesstokenyt,
       "&fields=items(replies(comments))",
       "&fields=items(snippet(topLevelComment(snippet(textDisplay,publishedAt,authorDisplayName))))")



###pruebas  
x  = paste0(base,"playlistItems?part=snippet,contentDetails&playlistId=",idsP[4],
       "&key=",accesstokenyt,"&maxResults=50") %>%  readLines() 


x[grep("nextPageToken",x)] %>%  gsub('\"', "",.,fixed=T) %>% 
  gsub("nextPageToken:|[[:blank:]]|,","",.)

y = paste0(base,"playlistItems?part=snippet,contentDetails&playlistId=",idsP[1],
       "&key=",accesstokenyt) %>%  readLines()
grep("nextPageToken",x)
grep("nextPageToken",y)
