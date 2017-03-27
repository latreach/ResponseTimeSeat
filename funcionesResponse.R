####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
####################################

"%!in%" <- function(x,y)!("%in%"(x,y))

diasHoras <- function(dias){
  dias*24
}

HorasMin  <- function(horas){
  horas*60
}

MinHoras  <- function(minutos){
  minutos/60
}


horasHorasMin <- function(hora){
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

is.Festivo = function(x) {
  ####dias oficiales tomados de la ley federal del trabajo:
  ##http://www.diputados.gob.mx/LeyesBiblio/pdf/125_120615.pdf
  
  festivos = c("-01-01","-02-05", "-03-21","-05-01", "-09-16",
               "-11-20","-12-25")
  x = as.Date(x)
  anioComparativo = lubridate::year(x)
  festivosCompara = paste(anioComparativo,festivos, sep="")
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
    festivosCompara= c(extras[1], as.Date(festivosCompara[-2]))
  }
  if(wday(especiales[2])!=2){
    festivosCompara= c(extras[2], as.Date(festivosCompara[-3]))
  }
  if(wday(especiales[3])!=2){
    festivosCompara= c(extras[3], as.Date(festivosCompara[-6]))
  }
  z = festivosCompara[festivosCompara==x]
  z = as.character(z)
  z = ifelse(identical(z, character(0)),
             FALSE,TRUE)
  return(z)
}
is.Festivo("2019-12-25")


separado = function(x){
  steps=x
  function(y){seq(0, ceiling(max(y)), by=steps)}
}
