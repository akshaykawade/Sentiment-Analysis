######Loading required librabries######

library('twitteR')
library('ROAuth')
library('RCurl')
library('httr')
library('stringr')
library('plyr')
library('dplyr')
library('tm')
library('ggmap')
library('wordcloud')

######Establising an API connection######

# Collect tokens from your Twitter Developer Account
# Feed the Tokens into R

key="<API key>"      #API key
secret="<API Secret Key>"   #API secret key
atoken="<Access Token>"   #Access Token
asecret="Access Token Secret"  #Access token secret

# Set up the connection to be able to harvest Tweets

twitteR:::setup_twitter_oauth(key,secret,atoken,asecret)
scraping_date_since=as.character( Sys.Date()-10)
typeof(scraping_date_since)


###### Harvesting the Tweets ######

First_Search_Term=readline(prompt = "Enter first search term: ")
Second_Search_Term=readline(prompt = "Enter second search term: ")

First_latlonmil = readline(prompt = "For 1st search term, enter in comma seperated format: 'lat,lon,miles' (e.g. "18.52,73.85,200mi")")
Second_latlonmil = readline(prompt = "For 2nd search term, enter in comma seperated format: 'lat,lon,miles' (e.g. "18.52,73.85,200mi")")

# Refine the scraping process via parameters like 
# Location, Time, Numbers, Language
# Locations are defined by the lat-long coordinates
# You can get the coordinates from latlong.net

First_Search_Term_Object=twitteR:::searchTwitter(First_Search_Term,n=200,lang='en',since = scraping_date_since, geocode = First_latlonmil)
Second_Search_Term_Object=twitteR:::searchTwitter(Second_Search_Term,n=200,lang='en',since = scraping_date_since, geocode = Second_latlonmil)

# Extracting tweets' text

first_tweettext = sapply(First_Search_Term_Object, function(x) x$getText())
second_tweettext = sapply(Second_Search_Term_Object, function(x) x$getText())


######Data Cleaning######

# Convert the text to ASCII format
# R handles ASCII well

first_tweettext=lapply(
  first_tweettext,
  function(x) iconv(x, "latin1",
  "ASCII", sub="")
)

second_tweettext=lapply(
  second_tweettext,
  function(x) iconv(x, "latin1",
                    "ASCII", sub="")
)


# Remove URLs
first_tweettext=lapply(first_tweettext, function(x) gsub("htt.*",'',x))
second_tweettext=lapply(second_tweettext, function(x) gsub("htt.*",'',x))

# Remove Hashes
first_tweettext=lapply(first_tweettext, function(x) gsub("#",'',x))
second_tweettext=lapply(second_tweettext, function(x) gsub("#",'',x))


######Getting opinion lexicons######
# Download positive and negative words file from
# "http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar"
# Extract the two files from the downloaded zip file
# Please save both the files in the working directory

pos=readLines("positive_words.txt")
neg=readLines("negative_words.txt")


######Sentiment Analysis Logic######

sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  # .progress: passed to laply() for control of progress bar
  
  # Some more cleaning
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   # remove punctuation - using global substitute
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   # remove control characters
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   # remove digits
                   singletweet = gsub("\\d+", "", singletweet)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   
                   singletweet = sapply(singletweet, tryTolower)
                   
                   # split sentence by space
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words with dictionaries of positive negative terms
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )
  
  # data frame with scores for each sentence
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

first_scores= sentimentfun(first_tweettext,pos,neg, .progress = 'text')
second_scores= sentimentfun(second_tweettext,pos,neg, .progress = 'text')

First_sentiment_score=mean(first_scores$score)
Second_sentiment_score=mean(second_scores$score)

if(First_sentiment_score>Second_sentiment_score){
print(First_Search_Term,"is more optimistically popular than",Second_Search_Term)
} else{
print(Second_Search_Term,"is more optimistically popular than",First_Search_Term)
}
