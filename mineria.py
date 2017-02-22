import got
import tweepy
import pandas as pd
import time
import json
from itertools import chain
tweetCriteria = got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince("2017-02-15").setUntil("2017-02-16")
mentions = got.manager.TweetManager.getTweets(tweetCriteria)[1:1000]
ids =[]
for i in mentions:
    ids.extend([i.id])
len(ids)
consumer_key    = "vDfPjIl7fRMjwHwYfj0rz5Vid"
consumer_secret = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgW"
access_token    ="137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
access_token_secret = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
auth =  tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)
tweets = []
contador = 0
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador +1
        print [x.text, contador]
        temp = dict(ID = x.id, usuarioNombre =x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje = x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next 
        print [i, "Problemas con este ID"]
auth
ids[1]
api.get_status(ids[1])
consumer_key = "vDfPjIl7fRMjwHwYfj0rz5Vid"
consumer_secret = "W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgWB"
access_token = "137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK"
access_token_secret = "gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt"
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)
tweets =[]
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador +1
        print [x.text, contador]
        temp = dict(ID = x.id, usuarioNombre =x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje = x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next 
        print [i, "Problemas con este ID"]
Mentions = pd.DataFrame.from_dict(tweets)
Mentions.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions3.csv", header=True, index=False, encoding="utf-8")
tweetCriteria = got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince("2017-02-16").setUntil("2017-02-17")
mentions = got.manager.TweetManager.getTweets(tweetCriteria)[1:1000]
ids = []
for i in mentions:
    ids.extend([i.id])
len(ids)
tweets =[]
contador = 0
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador +1
        print [x.text, contador]
        temp = dict(ID = x.id, usuarioNombre =x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje = x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next 
        print [i, "Problemas con este ID"]
    time.sleep(1)
Mentions  = pd.DataFrame.from_dict(tweets)
len(tweets)
Mentions.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions3.csv", header=True, index=False, encoding="utf-8")
tweetCriteria = got.manager.TweetCriteria().setQuerySearch("SEAT_Mexico").setSince("2017-02-16").setUntil("2017-02-17")
mentions
mentions = got.manager.TweetManager.getTweets(tweetCriteria)[1:50000]
for i in mentions:
    print i.created_at
for i in mentions:
    print i.created_time
for i in mentions:
    print i.date
ids =[]
for i in mentions:
    ids.extend([i.id])
tweets = []
contador =
contador = 0
for i in ids:
    try:
        x = api.get_status(i)
        contador = contador +1
        print [x.text, contador]
        temp = dict(ID = x.id, usuarioNombre =x.author.name)
        temp.update(usuarioID = x.author.id)
        temp.update(ReplyToUserName = x.in_reply_to_screen_name)
        temp.update(ReplyToUserID = x.in_reply_to_user_id)
        temp.update(idReplyTo = x.in_reply_to_status_id_str)
        temp.update(Fecha = x.created_at)
        temp.update(mensaje = x.text)
        tweets.append(temp)
    except tweepy.TweepError:
        next 
        print [i, "Problemas con este ID"]
    time.sleep(1)
Mentions = pd.DataFrame.from_dict(tweets)
Mentions.to_csv("/home/datascience/local/seat_analisis_extras/ResponseTime/Mentions3.csv", header=True, index=False, encoding="utf-8")
%history -f /home/datascience/local/seat_analisis_extras/ResponseTime/mineria.py
