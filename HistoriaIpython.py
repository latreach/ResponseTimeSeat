import got
import tweepy
import pandas as pd
import time
import json
from itertools import chain
tweetCriteria = got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince("2017-02-01").setUntil("2017-02-14")
mentions = got.manager.TweetManager.getTweets(tweetCriteria)[1:5000]
ids =[]
for i in mentions:
    ids.extend([i.id])
ids[1]
len(ids)
##API  conexión
consumer_key ="vDfPjIl7fRMjwHwYfj0rz5Vid"
consumer_secret = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgWB"
access_token ="137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
access_token_secret = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)
api.get_status(ids[1])
type(api.get_status(ids[1]))
type(api.get_status(ids[1])).geo_enable
api.get_status(ids[1]).geo_enable
api.get_status(ids[1]).Statis
api.get_status(ids[1]).Status
test=[]
for i in ids:
    x= api.get_status(i)
    test.append(i)
test[1]
for i in ids:
    x= api.get_status(i)
    test.append(x)
test =[]
for i in ids:
    x= api.get_status(i)
    test.append(x)
test[1]
type(test[1])
json.dumps(test[1])
json.dumps(test[1]._json)
json.dumps(test[1]_json)
json.dumps(test[1]._json)
json.dumps(api.get_status("831303598332137472")._json)
tweets =[]
contador= 0
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
tweets =[]
contador= 0
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
tweets =[]
contador= 0
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
len(ids)
ids[1:10]
len(ids)
1977*2
3954/60
3954/3600
1977*1
contador= 0
tweets =[]
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(1)
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(1)
tweets =[]
contador = 0
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(1)
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(1)
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(3)
for i in ids:
    x = api.get_status(i)
    contador = contador +1
    print[x.text,contador]
    temp = dict(ID = x.id, usuarioNombre = x.author.name)
    temp.update(usuarioID = x.author.id)
    temp.update(ReplyToUserName = x.in_reply_to_screen_name)
    temp.update(ReplyToUserID = x.in_reply_to_user_id)
    temp.update(idReplyTo = x.in_reply_to_status_id_str)
    temp.update(Fecha = x.created_at)
    temp.update(mensaje= x.text)
    tweets.append(temp)
    time.sleep(3)
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador + 1
        print[x.text,contador]
        temp = dict(ID = x.id, usuarioNombre = x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje= x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next
        print [i, "Problemas con este ID"]
    time.sleep(1)
len(ids)
len(temp
)
temp]
temp[1]
len(tweets)
tweets = []
1977*2
3600/3984
tweets = []
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador + 1
        print[x.text,contador]
        temp = dict(ID = x.id, usuarioNombre = x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje= x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next
        print [i, "Problemas con este ID o lo borraron o está bloqueado"]
    time.sleep(1.5)
Mentions = pd.DataFrame.from_dict(tweets)
Mentions.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions.csv", header=True, index=False, encoding="utf-8")
tweetCriteria2 = got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince("2017-02-14").setUntil("2017-02-15")
mentions2 = got.manager.TweetManager.getTweets(tweetCriteria2)[0:1000]
ids2 = []
for i in mentions2:
    ids2.extend([i.id])
len(ids2)
tweets2 = []
contador= 0
for i in ids2:
    try:
        x = api.get_status(i)
        contador = contador + 1
        print[x.text,contador]
        temp = dict(ID = x.id, usuarioNombre = x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje= x.text)
        tweets2.append(temp)
    except tweepy.TweepError:
        next
        print [i, "Problemas con este ID o lo borraron o está bloqueado"]
    time.sleep(1)
Mentions2 = pd.DataFrame.from_dict(tweets2)
Mentions2.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions2.csv", header=True, index=False, encoding="utf-8")
%history -f /home/datascience/local/seat_analisis_extras/ResponseTime/HistoriaIpython.py
