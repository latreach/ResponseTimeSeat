#encoding: utf-8
"""
Comenzando con la obtención de Tweets 
mensuales de la cuenta de SEAT 
Se usará la ayuda de un repositorio para la obtención de los IDS
Repositorio: https://github.com/Jefferson-Henrique/GetOldTweets-python
El repositorio deberá estar en la misma dirección que este script
O pueden llamarlo
Lo demás será via API
"""
## Librerías------------------------------------------------------------
import got  ## Funciones del repositorio
import tweepy
import pandas as pd
import time
import json
from itertools import chain


## Ids con ayuda del respositorio-------------------------------------
tweetCriteria =got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince('2017-02-21').setUntil('2017-02-25')

mentions = got.manager.TweetManager.getTweets(tweetCriteria)[1:5000]

ids = []
for i in mentions:
    ids.extend([i.id])

###Con la API de Twitter------------------------------------

##Creado por Fernando Dorantes Nieto

### Conexión de la API --------------------------------------
consumer_key    = "vDfPjIl7fRMjwHwYfj0rz5Vid"
consumer_secret = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgW"
access_token    ="137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
access_token_secret = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
auth =  tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)


##Obteniendo los IDS --------------------------------------
tweets = []
contador = 0
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador +1
        print [x.text,contador]
        temp = dict(ID= x.id, usuarioNombre = x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje = x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next
        print [i, "problemas con este ID"]
    time.sleep(3)

Mentions = pd.DataFrame.from_dict(tweets)
Mentions.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions.csv",
                header=True, index=False, encoding="utf-8")

