"""
Este script está diseñado para obtener todos los comentarios hijos 
de los comentarios (valga la redundancia) de los posteos de facebook (comments replies).
Se necesita el access token de la cuenta comercial del último nivel
Este script solo servirá para el dueño de la cuenta en cuestión
"""

"""
## Autor 
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
"""

## Librerías----------------------
import pandas as pd 
import numpy as np
import facebook
import sys
import datetime
import itertools
import time
from itertools import chain
from datetime import timedelta, date


##Comenzando conexión---------------------
token ="tuToken"
api= facebook.GrapApi(token)

##Leyendo los ids ------------------------
##Los ids fueron obtenidos con R
ids = open("idsComments.txt", "r").read()
ids = ids.split()

strings = []
for i in ids:
    objeto = "".join([i,"/comments"])
    strings.append(objeto)

##Obteniendo los comentarios hijos
args = {"fields": "parent,message,id, created_time,from"}

RepliesComments =[]
SinResponder = []
contador = 0
for i in strings:
    try:
        replies = api.get_object(i **args)
        replies = replies["data"]
        contador = contador + 1 
        print [i, contador]
        if replies !=[]:
            RepliesComments.append(replies)
        else:
            SinResponder.append(i)
    except facebook.GraphAPIError:
        next
        print [i, "No se puede leer este ID"]

Replies = list(chain.from_iterable(RepliesComments))
RepliesExtract =[]
for i in Replies:
    temp = dict(IDReply =i["id"])
    temp.update(FechaReply = i["created_time"])
    temp.uptate(IDUsuarioReply = i["from"]["id"])
    temp.update(NombreUsuarioReply = i["from"]["name"])
    temp.update(MensajeReply=i["message"])
    temp.update(FechaCommentParent = i["parent"]["created_time"])
    temp.update(NombreUsuarioCommentParent = i["parent"]["from"]["name"])
    temp.update(IDUsuarioCommentParent = i["parent"]["from"]["id"])
    temp.update(IDParent=i["parent"]["id"])
    temp.update(MensajeParent=i["parent"]["message"])
    RepliesExtract.append(temp)

RepliesDataFrame = pd.DataFrame.from_dict(RepliesExtract)

RepliesDataFrame.to_csv("CommentsReply.csv", sep=",", header=True, index=False,
                        encoding="utf-8)


Norespondidos = open("SinResponder.txt", "w")

for i in SinResponder:
    print >> Norespondidos, i

Norespondidos.close()







