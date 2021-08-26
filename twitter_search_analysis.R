
###########################
# TWITTER SEARCH ANALYSI
###########################

# CAN ACCESS MORE TWEETS, BUT NEED ALOT OF WORK TO PROPERLY DISTINGUISH THE SENTIMENT AND REMOVE NOISE AND INCRORECT NAMING

library(rtweet)
library(tidytext)


# whatever name you assigned to your created app
appname <- "2021 Brownlow Medal"

## api key (example below is not a real key)
key <- "26mHI7mqatQpd8rUWd4xgXNZR"

## api secret (example below is not a real key)
secret <- "56lPdRSrqWcN5fvb9L2vPZ4Fj8n2vuW3FMCr1VBPRuHaPIzF9Q"


access_token <- "1131167413200801792-bkPj8UEizEyArrwcSBniTnnWgGZTO5"

access_secret <- "JEwtzU7IVH8gY3Twd1tTF4IJyU6J51hLMpcCIw0mAMKAQ"



# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)



## search for 500 tweets using the #rstats hashtag
rstats_tweets <- search_tweets(q = "'Brownlow Medal'",
                               n = 250000,
                               
                               #geocode = lookup_coords("aus"),
                               # max_id = TRUE,
                               retryonratelimit = TRUE) 
#?search_tweets

glimpse(rstats_tweets)
