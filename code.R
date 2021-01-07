#To plot data
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

#For all data handling and analysis
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#Keyword analysis
if (!require("textrank")) install.packages("textrank", quiet=TRUE) ; require("textrank")

##package if required###
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")

# For annotations
if(!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")


#For colors
palette()

#For WORDCLOUD
install.packages('wordcloud')
library(wordcloud)

#For DATES
install.packages('lubridate')
library(lubridate)



#Algorithm  
install.packages("randomForest")
library(randomForest)

#Read the data
install.packages('readr')
library(readr)

# color palettes
install.packages("RColorBrewer") 
library("RColorBrewer")

#For Cool Net
install.packages('radarchart')
library(radarchart)

library(dplyr)
library(tidyr)
library(ggplot2)
#--------------------------------------------------------------------------------------------------------
rm(list = ls())


#-----------------------------------------------------------------------------------------------------
#Get TWEETS!

###Search the last month, multiple calls combine into one final table ZapposComplete30days###
ZapposTweets <-search_fullarchive(q = "Zappos OR \"-filter:retweets\"",
                                  n =2000, 
                                  fromDate ="201901012345",
                                  toDate ="201901312345",
                                  env_name = "FullArch",
                                  token = twitter_token)  

ZapposTweets <-search_fullarchive(q = "Zappos OR \"-filter:retweets\"",
                                  n =2000, 
                                  fromDate ="201902012345",
                                  toDate ="201902282345",
                                  env_name = "FullArch",
                                  token = twitter_token)

Zappos30dayTweets <- search_30day(q = "Zappos OR \"-filter:retwteets\"",
                                  n = 100 ,
                                  env_name = "First",
                                  token = twitter_token)

Zappos30dayTweets2ndCollect<- search_30day(q ="Zappos OR \"-filter:retwteets\"",
                                           n = 24900,
                                           fromDate ="201912301220",
                                           toDate = "202001272002",
                                           env_name = "First",
                                           token = twitter_token)

Zappos30dayTweets3rdCollect <- search_30day(q ="Zappos OR \"-filter:retwteets\"",
                                            n = 20116,
                                            fromDate ="201912301220",
                                            toDate ="202001162144",
                                            env_name = "First",
                                            token = twitter_token)

Zappos30dayTweets4thCollect <- search_30day(q ="Zappos OR \"-filter:retwteets\"",
                                            n = 20116,
                                            fromDate ="201912291300",
                                            toDate ="201912301345",
                                            env_name = "First",
                                            token = twitter_token)

#--------------------------------------------------------------------------------------------------------------
####################
#### Data Prep #####
####################

###combine all tables into one table###
ZapposComplete30day <- bind_rows(Zappos30dayTweets,Zappos30dayTweets2ndCollect,Zappos30dayTweets3rdCollect,Zappos30dayTweets4thCollect)


###Create a dataframe from desired variables###
ZapposBase <- select(ZapposComplete30day, user_id, status_id, created_at, screen_name, text, source, reply_to_status_id, reply_to_user_id, lang, is_retweet)

###Write the datafrme to a csv###
write.csv(ZapposBase,file = "C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/ZappposBaseComplete.csv", row.names = TRUE)

#Starting Table 
basetable<-read_delim("C:/Users/aolivera/Documents/GitHub/ZappposBaseComplete.csv", delim = ',')

###Table preporcessing ###
###remove punctuation from tweets###
ZapposBase_clean <- mutate(ZapposBase, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


##Tokenization (+ going to lowercase)##
Zappos_Tokenized <- ZapposBase_clean %>% unnest_tokens(output = "word", # how should the new column be named?
                                                       input = text, # where can we find the text? 
                                                       token = "words", # which tokenization scheme should we follow?
                                                       drop=FALSE,to_lower=TRUE)
###To keep pnly English type of lang###
Zappos_Tokenized <- filter(Zappos_Tokenized, lang =="en")
unique(Zappos_Tokenized$lang)

#Remove some other elements such as # and @ signs if they might occur
ZapposBase_Tokenized <- filter(Zappos_Tokenized, substr(word, 1, 1) != '#', 
                               substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags


###Correct the spelling in the tweets###
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

###Apply spelling correct function to the Data###
Zappos_Tokenized <- Zappos_Tokenized %>%mutate(suggestion = correct_spelling(word))

#Table after spelling
afterspell<-read_delim("C:/Users/aolivera/Documents/GitHub/Zappos_Tokenized_spell.csv", delim = ',')

#Get Days of Month
afterspell$newday <- mday(afterspell$created_at)

#Delete Zappos rows
Zappos_Tokenized_wd <- filter(afterspell, word != "zappos")

# Delete Stopwords
Zappos_Tokenized_wd <- Zappos_Tokenized_wd %>% 
                      anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here
# this strongly reduces the number of words

###Stem words for colud###
Zappos_Tokenized_stem <- Zappos_Tokenized_wd %>% 
                          mutate(word_stem = wordStem(word)) 


##################################################################
######### DATA ANALYSIS ########################################
###################################################################

# Compute complaint word counts and assign to word_counts
word_counts_stem <- Zappos_Tokenized_stem %>% 
                          group_by(word_stem)%>%  
                          count(word_stem) %>%
                          arrange(desc(n)) 

word_counts_suggestion <- Zappos_Tokenized_stem %>% 
                              group_by(suggestion)%>%  
                              count(suggestion) %>%
                              arrange(desc(n)) 
      
#WordCloud with Stemmed words 
set.seed(5)
wordcloud(words = word_counts_stem$word_stem, 
          freq = word_counts_stem$n, 
          random.order=FALSE,
          min.freq = 1,
          rot.per=0.35,
          colors=brewer.pal(6, "Dark2"),
          scale=c(3,1),
          )
            
#WordCloud with Words from dictionary                
set.seed(7)
wordcloud(words = word_counts_suggestion$suggestion, 
          freq  = word_counts_suggestion$n, 
          random.order=FALSE,
          min.freq = 1,
          rot.per=0.35,
          colors=brewer.pal(6, "Dark2"),
          scale=c(2,0.5),
          max.words = 200
          )

##""##

#PREP PLOT CHANNELS
chanel_table <- Zappos_Tokenized_wd %>%
                  group_by(source)%>%
                  count(n())%>%
                  arrange(desc(n))%>%
                  filter(n>888) 

save(chanel_table,file='C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/chanel_table.RData')

# PLOT TOP 10 CHANNELS 
ggplot(data = chanel_table, aes(x = reorder(source, n), y = n)) +
  geom_bar(stat="identity", fill=c("steelblue1")) +
  labs( y = 'Number of Tweets',
        x = 'Source',
        title = "Top 4 Sources") +
  coord_flip()


##""##

#Prep for Daily Tweets 
#Get only the month day
basetable$newday <- mday(basetable$created_at)

#Prep for Daily Tweets 
#Group by day
day_table <- basetable %>%
                group_by(newday)%>%
                count(newday) %>%
                arrange(desc(n)) 

save(day_table,file='C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/day_table.RData')

#PLOT 
#Daily Tweets
ggplot(data = day_table, aes(x = newday, y = n))+
    geom_line(colour="steelblue1", size=1.5, stat="identity") +
  scale_x_continuous(breaks = seq(1,31)) +
  ggtitle("Number of Tweets per Day") + 
  labs(x="Days", y="Tweets")
  

## "" ##
#Make bigrams and group them by unique tweet
bigrams <- basetable %>% unnest_tokens(output = "bigram",
                                       input = text,
                                       token = "ngrams",n=2, drop=FALSE) %>%
                                       count(status_id,bigram)

# Get frequency of biagrams
bigrams_count <- bigrams %>% group_by(bigram) %>%
  summarize(freq = n())%>%
  arrange(desc(freq))

#PLOT
# WordCloud Bigram -With stopwords
set.seed(1234)
wordcloud(words = bigrams_count$bigram, 
          freq = bigrams_count$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(0.9,0.5),
          random.order=FALSE,
          )

## "" ##

#Delete stopwords to make biagrams
bigrams_nosw <- afterspell %>% unnest_tokens(output = "bigram",
                                             input = text,
                                             token = "ngrams",n=2, drop=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
  filter(!word1 %in% get_stopwords()$word) %>% # Check for each of the words whether it is a stopword
  filter(!word2 %in% get_stopwords()$word) %>% # if they are, delete
  unite(bigram, word1, word2, sep = " ")  # unite the words again

#Get frequency of biagram with no sotpwords
bigramSTOP_count <- bigrams_nosw %>% group_by(bigram) %>%
  summarize(freq = n())%>%
  arrange(desc(freq))

#PLOT 
#Wordcloud BIGRAM with NO stopwords
set.seed(1234)
wordcloud(words = bigramSTOP_count$bigram, 
          freq = bigramSTOP_count$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(0.9,0.5),
          random.order=FALSE,
)

## '' ##

#PREP FOR ANNOTATION
###remove punctuation from tweets###
ZapposBase_clean <- filter(basetable, lang =="en")
ZapposBase_clean$text  <-  gsub(pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "",ZapposBase_clean$text )
ZapposBase_clean$text <- gsub("@","",ZapposBase_clean$text)
ZapposBase_clean$text <- gsub("#","",ZapposBase_clean$text)


#Getting dictionary and prepring package
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

#Annotations
x <- udpipe_annotate(ud_model, x =ZapposBase_clean$text)
#Convert it to DF
annotate <- as.data.frame(x)

#Delete Zappos rows
annotate_2 <- filter(annotate, lemma != "zappos")
#Filter by NOUN and ADJ
annotate_2 <- filter(annotate, upos == "ADJ" | upos == "NOUN")

#Get frequency of lemma and filtered
anno_count <- annotate_2 %>% group_by(lemma) %>%
                                summarize(freq = n())%>%
                                arrange(desc(freq))

#PLOT
#Wordcloud of lemma
set.seed(1234)
wordcloud(words = anno_count$lemma, 
          freq = anno_count$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(0.9,0.5),
          random.order=FALSE,
          max.words = 200
          )


#Creating keywords to ngrams=8 
keyword1 <- textrank_keywords(annotate_2$lemma, ngram_max = 8, sep = " ")

#Extract Unigram
keyw1 <- subset(keyword1$keywords, ngram == 1 & freq >= 2) 

#Extract Bigram
keyw2 <- subset(keyword1$keywords, ngram == 2 & freq >= 2) 

save(keyw2,file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/keyw2.RData')

#Extract Trigram
keyw3 <- subset(keyword1$keywords, ngram == 3 & freq >= 2) 

save(keyw1,file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/keyw1.RData')


keyw1 <- keyw1 %>%
  filter(keyword != "zappo")


#PLOT
#WordCloud KEYWORDS LEMMA UNIGRAM
set.seed(1234)
wordcloud(words = keyw1$keyword, 
          freq = keyw1$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(0.9,0.5),
          random.order=FALSE,
          max.words = 50
)

#PLOT
#WordCloud KEYWORDS LEMMA BIGRAM
set.seed(1234)
wordcloud(words = keyw2$keyword, 
          freq = keyw2$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(0.8,0.5),
          random.order=FALSE,
          max.words = 30
)

#PLOT
#WordCloud KEYWORDS LEMMA TRIGRAM
set.seed(1234)
wordcloud(words = keyw3$keyword, 
          freq = keyw3$freq, 
          min.freq = 1,
          rot.per=0.30,
          colors=brewer.pal(6, "Dark2"),
          scale=c(1,0.5),
          random.order=FALSE,
          max.words = 60
          )


#______________________________________________________________________________________________________
###############################
# SENTIMENT ANALYSIS ##########
###############################

##########
## BING ##
##########

# Get bing dictonary and count words
bing <- get_sentiments("bing") 

# join dictionary and words
z_bing_sent <- inner_join(Zappos_Tokenized_wd,get_sentiments("bing"))

#PREP TIME Analysis
day_table_BING <- z_bing_sent %>%
                          count(newday, sentiment) %>%                # count the positives and negatives per id (status)
                          spread(sentiment, n,fill = 0) 

save(day_table_BING, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/day_table_BING.RData')


#PLOT TWEETS per DAY
ggplot(data = day_table_BING, aes(x = newday)) +
  geom_line( aes(y = negative), colour="indianred3", size=1, stat="identity")+
  geom_line( aes(y = positive), colour="steelblue1", size=1, stat="identity")+
  scale_x_continuous(breaks = seq(1,31)) +
  ggtitle("Positive and Negative sentiments") + 
  labs(x="Days", 
       y="Tweets") 
  
#Prep for PLOT
#Get top 10 words  
summary_bing <- z_bing_sent %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

#PLOT 1
# Plot 10 most relevant negative and positive words (BING)
summary_bing %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Number of times word mentioned",
       x = 'Words',
       title = '10 most mentioned words - Bing Dict.') +
  coord_flip()

save(summary_bing, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/summary_bing.RData')

# Frequency per sentiment BING and to total
statusSentiment_bing <- z_bing_sent %>%
                          count(status_id, sentiment) %>%                # count the positives and negatives per id (status)
                          spread(sentiment, n,fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
                          mutate(sentiment = positive - negative) 

# Get totals 
TotalScore_Bing <- statusSentiment_bing %>%
                    summarize(Positive = sum(positive), Negative = sum(negative)) 
                    
#TotalScore_Bing <- mutate(TotalScore_Bing, difference = Positive - Negative)

#Prep for PLOT
#Make it DF
TotalScore_Bing <- as.data.frame(t(TotalScore_Bing))
#Put name for col
colnames(TotalScore_Bing) <- 'Freq'
#add rows with names
TotalScore_Bing<- TotalScore_Bing%>%
  mutate(sentiment = c('Positive','Negative')) 
  
save(TotalScore_Bing, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/TotalScore_Bing.RData')

# PLOT 2
# Total negative and positive of BING

ggplot(data = TotalScore_Bing, aes(x= sentiment, y = Freq)) +
  geom_bar(stat="identity", fill=c("indianred3",'royalblue')) +
  labs( y = 'Number of words', 
        title = "Total Words in Sentiments - Bing Dict.")

#######



################
## AFFIN #######
################

#Check dictionary
afinn <- get_sentiments("afinn")

## Score per Post AFFIN ##
statusSentiment_AFFIN <- inner_join(Zappos_Tokenized_wd,get_sentiments("afinn")) 

# Adding 'negative' and 'positive' if is <0 or >0
for (x in  1:length(statusSentiment_AFFIN$value)) {
  
if (statusSentiment_AFFIN$value[x] < 0){
    statusSentiment_AFFIN$Sentiments[x] <- 'negative'
} else {
    statusSentiment_AFFIN$Sentiments[x] <- 'positive'
}
}       


#PREP PLOT 1.1
#Lowest 10 values (negatives)
summary_affin_negative <- statusSentiment_AFFIN %>% 
  group_by(suggestion) %>%
  summarize(score = sum(value)) %>%
  top_n(-10) %>%
  arrange(score) %>%
  as.data.frame(stringsAsFactors=FALSE)

#PREP PLOT 1.2
#Top 10 values
summary_affin_positive <- statusSentiment_AFFIN %>% 
  group_by(suggestion) %>%
  summarize(score = sum(value)) %>%
  top_n(10) %>%
  arrange(desc(score)) %>%
  as.data.frame(stringsAsFactors=FALSE)
  
#PLOT 1.1
#NEGATIVE score of words AFFIN
summary_affin_negative %>%
  mutate(suggestion = reorder(suggestion, score)) %>%
  ggplot(aes(x = suggestion, y= score, fill=score)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Score per word",
       x = 'Words', 
       title = 'Score Negative Words') +
  coord_flip()

save(summary_affin_negative, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/summary_affin_negative.RData')


#PLOT 1.2
#POSITVE Score AFFIN
summary_affin_positive %>%
  mutate(suggestion = reorder(suggestion, score)) %>%
  ggplot(aes(x = suggestion, y= score, fill=score)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Total score per word",
       x = 'Words', 
       title = 'Score Positive Words - Affin Dict.') +
  coord_flip()

save(summary_affin_positive, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/summary_affin_positive.RData')


#Prep PLOT 2
# Get totals
Affin_tot_sent <- statusSentiment_AFFIN %>%
  group_by(Sentiments) %>%
  summarize(total_sent = sum(value))

save(Affin_tot_sent, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/Affin_tot_sent.RData')


# PLOT 2
# Total negative and positive of AFFIN
palette()
ggplot(data = Affin_tot_sent, aes(x= Sentiments, y = total_sent)) +
  geom_bar(stat="identity", fill=c("indianred3",'royalblue')) +
  labs(y = "Score",
       x = 'Sentiment', 
       title = 'Total Score per Sentiment - Affin Dict.')
#######



#PREP Day table 
day_table_AFFIN_2<- statusSentiment_AFFIN %>%
                      group_by(newday, Sentiments)%>%
                      summarize(score = sum(value))

#Change negative to positive, absolute value
for (x in  1:length(day_table_AFFIN_2$score)) {
  
  if (day_table_AFFIN_2$Sentiments[x] == 'negative'){
    day_table_AFFIN_2$score[x] <- day_table_AFFIN_2$score[x]*(-1)
  } 
}  

save(day_table_AFFIN_2, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/day_table_AFFIN_2.RData')


#PLOT SCORE per DAY
ggplot(data = day_table_AFFIN_2, aes(x = newday, y = score )) +
  geom_line( aes(color = Sentiments, linetype = Sentiments))+
  ggtitle("Positive and Negative sentiments") + 
  labs(x="Days", 
       y="Tweets") 



###################
## NRC  ###########
###################

#Check dictionary
nrc_sent <- get_sentiments("nrc")

#Match words and sentiment
statusSentiment_nrc2 <- inner_join(Zappos_Tokenized_wd,get_sentiments("nrc")) 

#Prep tweets per tweet
day_table_NRC<- statusSentiment_nrc2 %>%
                  count(newday, sentiment)


#Getting frequency of words, spreding it and getting the total
statusSentiment_nrc <- inner_join(Zappos_Tokenized_wd,get_sentiments("nrc"))%>%
                        count(status_id, sentiment) %>%                # count the positives and negatives per id (status)
                        spread(sentiment, n,fill = 0) 

#Prep PLOT 1
#Get top 10 words  
summary_nrc <- statusSentiment_nrc2 %>%  
  count(suggestion,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

save(summary_nrc,file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/summary_nrc.RData')


# PLOT 1 (use zoom to see it)
# Plot 10 most relevant negative and positive words (NRC)
summary_nrc %>%
  ungroup() %>%
  mutate(suggestion = reorder(suggestion, n)) %>%
  ggplot(aes(suggestion, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Number of Words",
       x = 'Words', 
       title = 'Most relevant words - NRC Dict.') +
  coord_flip()

#PREP PLOT 2
# Get totals
TotalScore_NRC <- as.data.frame(colSums(statusSentiment_nrc))
#Add name to column
names(TotalScore_NRC) <- c('Freq')
a <- c("status_id","anger","anticipation","disgust","fear","joy","negative","positive","sadness","surprise","trust","sentiment")

#adding sentiment label
for (x in 1:11) {
  TotalScore_NRC$sentiment[x] <- a[x]
}

#deleting status id. No need for plotting
TotalScore_NRC <- TotalScore_NRC %>%
                    filter(sentiment != 'status_id')

save(TotalScore_NRC,file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/TotalScore_NRC.RData')


#PLOT 2
#PLOT of Total FREQ in NRC
TotalScore_NRC %>%
  mutate(sentiment = reorder(sentiment, Freq)) %>%
  ggplot(aes(x = sentiment, y= Freq, fill=Freq)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Number of words ",
       x = 'Sentiment', 
       title = 'Number of words per sentiment - NRC dict.') +
  coord_flip()

#PREP PLOT Multiple Sentiments
#Get number of words per sentiment
polarity <- inner_join(Zappos_Tokenized_wd,get_sentiments("nrc"))%>%
                         count(suggestion, sentiment) %>%                # count the positives and negatives per id (status)
                         spread(sentiment, n,fill = 0)%>%
                          data.frame(row.names = "suggestion")                             # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
                                  
save(polarity, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/polarity.RData')

#PLOT WORDCLOUD
set.seed(1)
comparison.cloud(polarity, 
                 scale = c(0.9,0.5), 
                 match.colors = TRUE,
                 title.size = 1, 
                 max.words = 90)



#PREP PLOT NET
# Get top 10
summary_nrc6 <- statusSentiment_nrc2 %>%  
  count(sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)%>%
  rename('Number of Words' = n)

save(summary_nrc6, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/summary_nrc6.RData')

#PLOT NET MULTIPLE SENTIMENTS
chartJSRadar(summary_nrc6,main = 'Sentiment Analysis' )


#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------

###############################################
############## TWEETS BY ZAPPOS ###############
###############################################


##################################
###### SENTIMENT ANALYSIS ########
##################################



load('C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Mktg Research/TweetsbyZappos.RData')

###Table preporcessing ###
###remove punctuation from tweets###
TweetsbyZappos_clean <- mutate(TweetsbyZappos, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


##Tokenization (+ going to lowercase)##
tweets_token <- TweetsbyZappos_clean %>% unnest_tokens(output = "word", # how should the new column be named?
                                                       input = text, # where can we find the text? 
                                                       token = "words", # which tokenization scheme should we follow?
                                                       drop=FALSE,to_lower=TRUE)


###To keep pnly English type of lang###
tweets_token <- filter(tweets_token, lang =="en")
                  unique(tweets_token$lang)

#Remove some other elements such as # and @ signs if they might occur
tweets_token <- filter(tweets_token, substr(word, 1, 1) != '#', 
                       substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags





#Get Days of Month
tweets_token$newday <- mday(tweets_token$created_at)

#Delete Zappos rows
tweets_token_en <- filter(tweets_token, word != "zappos")

# Delete Stopwords
tweets_token_en <- tweets_token_en %>% 
  anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here
# this strongly reduces the number of words

###Stem words for colud###
Zappos_Tokenized_stem <- tweets_token_en %>% 
  mutate(word_stem = wordStem(word)) 

###############################
# SENTIMENT ANALYSIS ##########
###############################

##########
## BING ##
##########

# Get bing dictonary and count words
bing <- get_sentiments("bing") 

# join dictionary and words
t_bing_sent <- inner_join(tweets_token_en,get_sentiments("bing"))

library(dplyr)
library(tidyr)
#PREP TIME Analysis
t_day_table_BING <- t_bing_sent %>%
  count(newday, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n,fill = 0) 

save(t_day_table_BING, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_day_table_BING.RData')

#PLOT TWEETS per DAY
ggplot(data = t_day_table_BING, aes(x = newday)) +
  geom_line( aes(y = negative), colour="indianred3", size=1, stat="identity")+
  geom_line( aes(y = positive), colour="steelblue1", size=1, stat="identity")+
  ggtitle("Positive and Negative sentiments") + 
  labs(x="Days", 
       y="Tweets") 

#Prep for PLOT
#Get top 10 words  
t_summary_bing <- t_bing_sent %>%  count(word,sentiment,sort=TRUE) %>%
                                      group_by(sentiment) %>%
                                      top_n(10) %>%  
                                      arrange(n) %>%
                                      as.data.frame(stringsAsFactors=FALSE)

#PLOT 1
# Plot 10 most relevant negative and positive words (BING)
t_summary_bing %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Number of times word mentioned",
       x = 'Words',
       title = '10 most mentioned words - Bing Dict.') +
  coord_flip()

save(t_summary_bing, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_summary_bing.RData')

# Frequency per sentiment BING and to total
t_statusSentiment_bing <- t_bing_sent %>%
  count(status_id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n,fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative) 

# Get totals 
t_TotalScore_Bing <- t_statusSentiment_bing %>%
  summarize(Positive = sum(positive), Negative = sum(negative)) 

#TotalScore_Bing <- mutate(TotalScore_Bing, difference = Positive - Negative)

#Prep for PLOT
#Make it DF
t_TotalScore_Bing <- as.data.frame(t(t_TotalScore_Bing))
#Put name for col
colnames(t_TotalScore_Bing) <- 'Freq'
#add rows with names
t_TotalScore_Bing<- t_TotalScore_Bing%>%
  mutate(sentiment = c('Positive','Negative')) 

# PLOT 2
# Total negative and positive of BING

ggplot(data = t_TotalScore_Bing, aes(x= sentiment, y = Freq)) +
  geom_bar(stat="identity", fill=c("indianred3",'royalblue')) +
  labs( y = 'Number of words', 
        title = "Total Words in Sentiments - Bing Dict.")

save(t_TotalScore_Bing, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_TotalScore_Bing.RData')

#######



########################################
## AFFIN ############################
######################################

#Check dictionary
afinn <- get_sentiments("afinn")

## Score per Post AFFIN ##
t_statusSentiment_AFFIN <- inner_join(tweets_token_en,get_sentiments("afinn")) 

# Adding 'negative' and 'positive' if is <0 or >0
for (x in  1:length(t_statusSentiment_AFFIN$value)) {
  
  if (t_statusSentiment_AFFIN$value[x] < 0){
    t_statusSentiment_AFFIN$Sentiments[x] <- 'negative'
  } else {
    t_statusSentiment_AFFIN$Sentiments[x] <- 'positive'
  }
}       


#PREP PLOT 1.1
#Lowest 10 values (negatives)
t_summary_affin_negative <- t_statusSentiment_AFFIN %>% 
  group_by(word) %>%
  summarize(score = sum(value)) %>%
  top_n(-10) %>%
  arrange(score) %>%
  as.data.frame(stringsAsFactors=FALSE)

#PREP PLOT 1.2
#Top 10 values
t_summary_affin_positive <- t_statusSentiment_AFFIN %>% 
  group_by(word) %>%
  summarize(score = sum(value)) %>%
  top_n(10) %>%
  arrange(desc(score)) %>%
  as.data.frame(stringsAsFactors=FALSE)

#PLOT 1.1
#NEGATIVE score of words AFFIN
t_summary_affin_negative %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(x = word, y= score, fill=score)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Score per word",
       x = 'Words', 
       title = 'Score Negative Words') +
  coord_flip()

save(t_summary_affin_negative, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_summary_affin_negative.RData')

#PLOT 1.2
#POSITVE Score AFFIN
t_summary_affin_positive %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(x = word, y= score, fill=score)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Total score per word",
       x = 'Words', 
       title = 'Score Positive Words - Affin Dict.') +
  coord_flip()

save(t_summary_affin_positive, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_summary_affin_positive.RData')


#Prep PLOT 2
# Get totals
t_Affin_tot_sent <- t_statusSentiment_AFFIN %>%
  group_by(Sentiments) %>%
  summarize(total_sent = sum(value))

# PLOT 2
# Total negative and positive of AFFIN
palette()
ggplot(data = t_Affin_tot_sent, aes(x= Sentiments, y = total_sent)) +
  geom_bar(stat="identity", fill=c("indianred3",'royalblue')) +
  labs(y = "Score",
       x = 'Sentiment', 
       title = 'Total Score per Sentiment - Affin Dict.')

save(t_Affin_tot_sent, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_Affin_tot_sent.RData')


#######



#PREP Day table 
t_day_table_AFFIN_2<- t_statusSentiment_AFFIN %>%
  group_by(newday, Sentiments)%>%
  summarize(score = sum(value))

#Change negative to positive, absolute value
for (x in  1:length(t_day_table_AFFIN_2$score)) {
  
  if (t_day_table_AFFIN_2$Sentiments[x] == 'negative'){
    t_day_table_AFFIN_2$score[x] <- t_day_table_AFFIN_2$score[x]*(-1)
  } 
}  

#PLOT SCORE per DAY
ggplot(data = t_day_table_AFFIN_2, aes(x = newday, y = score )) +
  geom_line( aes(color = Sentiments, linetype = Sentiments))+
  ggtitle("Positive and Negative sentiments") + 
  labs(x="Days", 
       y="Tweets") 

save(t_day_table_AFFIN_2, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_day_table_AFFIN_2.RData')

################################################
######################         NRC          #######################################
###############################################

#Check dictionary
nrc_sent <- get_sentiments("nrc")

#Match words and sentiment
T_statusSentiment_nrc2 <- inner_join(tweets_token_en,get_sentiments("nrc")) 

#Getting frequency of words, spreding it and getting the total
t_statusSentiment_nrc <- inner_join(tweets_token_en,get_sentiments("nrc"))%>%
  count(status_id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n,fill = 0)%>%
  select(-status_id)

#Prep PLOT 1
#Get top 10 words  
t_summary_nrc <- T_statusSentiment_nrc2 %>%  
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

# PLOT 1 (use zoom to see it)
# Plot 10 most relevant negative and positive words (NRC)
t_summary_nrc %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Number of Words",
       x = 'Words', 
       title = 'Most relevant words - NRC Dict.') +
  coord_flip()

save(t_summary_nrc, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_summary_nrc.RData')

#PREP PLOT 2
# Get totals
t_TotalScore_NRC <- as.data.frame(colSums(t_statusSentiment_nrc))
#Add name to column
names(t_TotalScore_NRC) <- c('Freq')
a <- c("anger","anticipation","disgust","fear","joy","negative","positive","sadness","surprise","trust","sentiment")

#adding sentiment label
for (x in 1:12) {
  t_TotalScore_NRC$sentiment[x] <- a[x]
}

#PLOT 2
#PLOT of Total FREQ in NRC
t_TotalScore_NRC %>%
  mutate(sentiment = reorder(sentiment, Freq)) %>%
  ggplot(aes(x = sentiment, y= Freq, fill=Freq)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Number of words ",
       x = 'Sentiment', 
       title = 'Number of words per sentiment - NRC dict.') +
  coord_flip()

save(t_TotalScore_NRC, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_TotalScore_NRC.RData')

#PREP PLOT Multiple Sentiments
#Get number of words per sentiment
t_polarity <- inner_join(tweets_token_en,get_sentiments("nrc"))%>%
  count(word, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n,fill = 0)%>%
  data.frame(row.names = "word")                             # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 

save(t_polarity, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_polarity.RData')


#PLOT WORDCLOUD
set.seed(1)
comparison.cloud(t_polarity, 
                 scale = c(0.5,0.3), 
                 match.colors = TRUE,
                 title.size = 0.3, 
                 max.words = 50)



#PREP PLOT NET
# Get top 10
t_summary_nrc6 <- T_statusSentiment_nrc2 %>%  
  count(sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)%>%
  rename('Number of Words' = n)

#PREP PLOT NET
# Get top 10
t_summary_nrc8 <- T_statusSentiment_nrc2 %>%  
  count(sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)%>%
  rename('Number of Words' = n) %>%
  filter(sentiment == 'positive' | sentiment == 'negative')


#PLOT NET MULTIPLE SENTIMENTS
chartJSRadar(t_summary_nrc6,main = 'Sentiment Analysis' )

save(t_summary_nrc6, file = 'C:/Users/aolivera/OneDrive - IESEG/IESEG/Courses/Second Semester/Social Media Anal/Group Project/Variables_rmarkdown/t_summary_nrc6.RData')



for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#package if required
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
library(readr)
library(dplyr)
library(ggplot2)
install.packages("textstem")
library(textstem)
install.packages("tidyquant")
install.packages("tidyverse")
library(tidyverse)
library(tidyquant)
install.packages("ggthemes")
library(ggthemes)
install.packages("tidyverse")
install.packages("usmap")
install.packages("corrplot")