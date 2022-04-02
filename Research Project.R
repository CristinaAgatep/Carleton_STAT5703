library(tidyr)
library(dplyr)
library(ggplot2) # plots
library(SnowballC) # stemming words
library(tm) # Includes stop word information
library(gutenbergr) # Source of books
library(reshape2) # Create the comparison word clouds (positive vs. negative)
library(tidytext) # get_sentiments() function and tokenizing
library(data.table) # To group text by chapters
library(zoo)
library(wordcloud)
library(gridExtra)
library(tidygraph)
library(ggraph)
library(sampling)
library(stopwords)

my_mirror <- "http://mirrors.xmission.com/gutenberg/"



###### FUNCTIONS ########
# Removing punctuation function
replacePunctuation = function(x) {
  gsub("[[:punct:]]+", "", x) 
}


### Cleans and tokenizes the text into unigrams
# Input: Dataset with column "chapter" and column "text". Tokenization on the text column
tokenize = function(text_ds){
  # Convert to lower case
  text_ds$text<-tolower(text_ds$text)
  
  # Replacing punctuation with blanks
  text_ds$text = replacePunctuation(text_ds$text)
  
  # Tokenizing by word, remove certain stopwords, flagging negative words
  unigrams = text_ds |>
    unnest_tokens(output=word, input=text) |>
    anti_join(stops, by="word") |>
    mutate(negative.flag = ifelse(word %in% negatives, 1, 0))
  
  #### Deal with affects of negative words on sentiments
  # If a word has a negative before its occurrence, change the
  # sentiment score onto the opposite side of neural
  
  # Create a separate vector for negative flag
  negative.flag = unigrams$negative.flag
  
  negative.flag = negative.flag |>
    as.data.frame()
  
  # Shift the negative flags by one
  negative.flag = rbind(0, negative.flag)
  
  negative.flag = negative.flag[1:nrow(negative.flag)-1, ]
  
  # Bind the negative flag vector back into the unigram dataset, 
  # now flagging the words with a negative word before it
  unigrams = cbind(unigrams[,1:2], negative.flag)
  
  # Remove all negative stop words, stemming words
  unigrams = unigrams |>
    filter(word %in% negatives == FALSE) 
  
    # mutate(stemmed.word = wordStem(word))
  
  return(unigrams)
}

rolling_means = function(unigrams, window, color){
  means = as.data.frame(rollmean(unigrams$dodds.score, window))
  avg.ID = (1:nrow(means) / nrow(means) ) * 100
  
  means = cbind(avg.ID, means) 
  colnames(means) = c('avg.ID', 'means')
  
  return(means |>
           ggplot(aes(x=avg.ID, y=means)) + 
           geom_line(color = color) + 
           labs(x="Percentage of book", y="Rolling average of sentiment scores") +
           scale_y_continuous(breaks=seq(5.30, 5.60, 0.05))) 
           
}



############## PREPARE THE SENTIMENT WORDBANKS #################

####### STOP WORDS DATASET
stops = data.frame(word=c(stopwords(),letters))

# remove punctuation from stopwords for consistency
stops$word = replacePunctuation(stops$word)

# To play with negation - removing some negation words from the "stops" dataset. Can add more or 
# remove some later if needed
negatives = c("isnt", "arent", "wasnt", "werent", "hasnt", "havent", "hadnt", "doesnt", 
              "didnt", "dont", "wont", "wouldnt", "shant", "shouldnt", "cant", "cannot",
              "couldnt", "mustnt", "not", "nor")

stops = stops |>
  filter(! word %in% negatives)

########### SENTIMENT SCORES FROM DODDS #######
dodds = read.csv("http://www.uvm.edu/storylab/share/papers/dodds2014a/data/english-gbook-words.txt", 
                 header=FALSE)
scores.temp = read.csv("http://www.uvm.edu/storylab/share/papers/dodds2014a/data/english-gbook-scores.txt",
                       header=FALSE)

dodds = cbind(dodds, scores.temp)
colnames(dodds) = c("original.word", "score")


dodds.french = read.csv("https://www.uvm.edu/storylab/share/papers/dodds2014a/data/french-gbook-words.txt",
                        header=FALSE)
scores.french.temp = read.csv("https://www.uvm.edu/storylab/share/papers/dodds2014a/data/french-gbook-scores.txt", 
                              header=FALSE)

dodds.french = cbind(dodds.french, scores.french.temp)
colnames(dodds.french) = c("original.word", "score")


######### NRC SENTIMENTS #########
# Sentiments are negative or positive, and are also labelled by more specific types 

# Distribution of sentiment values
# get_sentiments("nrc") |> 
#   filter(sentiment != "positive" & sentiment != "negative") |>
#   ggplot(aes(y=sentiment)) + 
#   geom_bar(fill = "lavender", color="purple")
# 
# # Distribution of positive and negative values
# get_sentiments("nrc") |> 
#   filter(sentiment == "positive" | sentiment == "negative") |>
#   ggplot(aes(y=sentiment)) + 
#   geom_bar(fill = "orange", color="yellow")

nrc.emotion = get_sentiments("nrc")  |>
  filter(sentiment != "positive" & sentiment != "negative") 

emotions <- list('trust', 'surprise', 'sadness', 'joy', 
                 'fear', 'disgust', 'anticipation', 
                 'anger')
colours = list("lightblue", "pink", "darkblue", "yellow", 
               "purple", "lightgreen", "orange", "red")

nrc.bin = get_sentiments("nrc")  |>
  filter(sentiment == "positive" | sentiment == "negative")




################# 1. LITTLE WOMEN - DATASET MANIPULATION #################

Little.Women = gutenberg_download(514, mirror=my_mirror)

# Extracting the chapters into a new column

Little.Women = Little.Women |>
  filter(text != "") |>
  mutate(chapter.empty = ifelse(grepl("CHAPTER [A-Z]+", text), text, ""))

## Currently: chapters are only flagged in the row they were found. Below we will fill the empty rows
## with the chapter value

Little.Women <- Little.Women[-c(1:53), ] # Removing the table of contents

Little.Women = data.table(Little.Women)
Little.Women[!nzchar(chapter.empty),chapter.empty:=NA][,chapter.word:=na.locf(chapter.empty)]

# Turning the chapter variable into numeric
Little.Women[, chapter:=rleid(chapter.word)]

# Removing the rows with CHAPTER __ in text. Also removing flagged chapter column
Little.Women = Little.Women |>
  filter(grepl("CHAPTER [A-Z]+", text) == FALSE) |>
  select(chapter, text)

Little.Women.unigram = tokenize(Little.Women)

## Joining the sentiment scores from Dodd's study. If there is no score, set it as a neutral score
Little.Women.unigram = Little.Women.unigram |>
  left_join(dodds, by = c("word" = "original.word")) |>
  rename(dodds.score = score) |>
  mutate(dodds.score = ifelse(is.na(dodds.score) | word == 'march', 5, dodds.score)) 

# If the word had a negative instance - the score is adjusted
Little.Women.unigram = Little.Women.unigram |>
  mutate(dodds.score = ifelse(negative.flag == 1, 5 - (dodds.score-5), dodds.score))

# Dividing the dataset into four parts
Little.Women.unigram = Little.Women.unigram |>
  mutate(part = ifelse(chapter <= 13, 1, 
                       ifelse(chapter >= 14 & chapter <= 23, 2, 
                              ifelse(chapter >= 24 & chapter <= 36, 3,
                                     ifelse(chapter >= 37 & chapter <= 47, 4, 0)
                              )
                       )
  )
  )








########## 2. MONTE CRISTO - DATASET MANIPULATION #######

Monte.Cristo = gutenberg_download(1184, mirror=my_mirror)

# Extracting the chapters into a new column
Monte.Cristo = Monte.Cristo |>
  filter(text != "") |>
  mutate(chapter.empty = ifelse(grepl("Chapter [0-9]+", text), text, ""))

# Removing the table of contents
Monte.Cristo <- Monte.Cristo[-c(1:129), ] # Removing the table of contents

# Creating a character variable chapter column
Monte.Cristo = data.table(Monte.Cristo)
Monte.Cristo[!nzchar(chapter.empty),chapter.empty:=NA][,chapter.word:=na.locf(chapter.empty)]

# Turning the chapter variable into numeric
Monte.Cristo[, chapter:=rleid(chapter.word)]

# Removing the rows with CHAPTER __ in text. Also removing flagged chapter column
Monte.Cristo = Monte.Cristo |>
  filter(grepl("Chapter [0-9]+", text) == FALSE) |>
  select(chapter, text)

Monte.Cristo.unigram = tokenize(Monte.Cristo)

## Joining the sentiment scores from Dodd's study. If there is no score, set it as a neutral score
# Also setting the word 'de' as neutral - some names have the word 'de' in them (e.g. Madame de Villefort)
Monte.Cristo.unigram = Monte.Cristo.unigram |>
  left_join(dodds, by = c("word" = "original.word")) |>
  rename(dodds.score = score) |>
  mutate(dodds.score = ifelse(is.na(dodds.score) | word == 'de', 5, dodds.score))

# If the word had a negative instance - the score is adjusted
Monte.Cristo.unigram = Monte.Cristo.unigram |>
  mutate(dodds.score = ifelse(negative.flag == 1, 5 - (dodds.score-5), dodds.score))


########## 3. FRANKENSTEIN - DATASET MANIPULATION #############

Frank = gutenberg_download(84, mirror = my_mirror)

# Extracting the chapters into a new column
contents = c("Chapter [0-9]+", "Letter [0-9]+")

Frank = Frank |>
  filter(text != "") |>
  mutate(chapter.empty = ifelse(grepl(paste(contents, collapse="|"), text), text, ""))

# Create chapter column
Frank <- Frank[-c(1:32), ] # Removing the table of contents

Frank = data.table(Frank)
Frank[!nzchar(chapter.empty),chapter.empty:=NA][,chapter.word:=na.locf(chapter.empty)]

# Turning the chapter variable into numeric
Frank[, chapter:=rleid(chapter.word)]

# Removing the rows with CHAPTER __ in text. Also removing flagged chapter column
Frank = Frank |>
  filter(grepl(paste(contents, collapse="|"), text) == FALSE) |>
  select(chapter, text)

Frank.unigram = tokenize(Frank)

## Joining the sentiment scores from Dodd's study. If there is no score, set it as a neutral score
Frank.unigram = Frank.unigram |>
  left_join(dodds, by = c("word" = "original.word")) |>
  rename(dodds.score = score) |>
  mutate(dodds.score = ifelse(is.na(dodds.score), 5, dodds.score))

# If the word had a negative instance - the score is adjusted
Frank.unigram = Frank.unigram |>
  mutate(dodds.score = ifelse(negative.flag == 1, 5 - (dodds.score-5), dodds.score))




############# 4. GENERAL EXPLORATION #########

######### 4.1. 25 Most Common Words ###########

# Little Women
Little.Women.counts = Little.Women.unigram |>
  count(word) |>
  arrange(desc(n))

Little.Women.counts |> head(10) |>
  ggplot(aes(reorder(word,-n), y=n)) +
  geom_bar(stat='identity', fill='lavender', color='black') + xlab('Frequency') + ylab('Word') +
  theme(axis.text.x = element_text(angle = 270)) 

# Monte Cristo
Monte.Cristo.counts = Monte.Cristo.unigram |>
  count(word) |>
  arrange(desc(n))

Monte.Cristo.counts |> head(10) |>
  ggplot(aes(reorder(word,-n), y=n)) +
  geom_bar(stat='identity', fill='lightblue', color='black') + xlab('Frequency') + ylab('Word') +
  theme(axis.text.x = element_text(angle = 270)) 

# Frankenstein
Frank.counts = Frank.unigram |>
  count(word) |>
  arrange(desc(n))

Frank.counts |> head(10) |>
  ggplot(aes(reorder(word,-n), y=n)) +
  geom_bar(stat='identity', fill='lightgreen', color='black') + xlab('Frequency') + ylab('Word') +
  theme(axis.text.x = element_text(angle = 270)) 


########## 4.2. NRC Positive and Negative ##########

# Little Women
Little.Women.bin = Little.Women.counts |>
  inner_join(nrc.bin , "word") |>
  filter(!(word %in% c('john', 'march')))

wordcloud(Little.Women.bin$word, Little.Women.bin$n, colors=c("firebrick1", "mediumpurple1")[factor(Little.Women.bin$sentiment)], ordered.colors=TRUE, max.words=70)

# Monte Cristo
Monte.Cristo.bin = Monte.Cristo.counts |>
  inner_join(nrc.bin , "word") |>
  filter(!(word %in% c('de', 'count')))

wordcloud(Monte.Cristo.bin$word, Monte.Cristo.bin$n, colors=c("firebrick1", "deepskyblue3")[factor(Monte.Cristo.bin$sentiment)], ordered.colors=TRUE, max.words=70)

# Frankenstein
Frank.bin = Frank.bin |>
  inner_join(nrc.bin , "word")

wordcloud(Frank.bin$word, Frank.bin$n, colors=c("firebrick1", "chartreuse3")[factor(Frank.bin$sentiment)], ordered.colors=TRUE, max.words=70)

########## 4.3. NRC Emotions ##########
colours = list("coral1", "lavender", "coral1", "coral1", 
               "lavender", "coral1", "lavender", "lavender")

Little.Women.emo = Little.Women.counts |>
  inner_join(nrc.emotion , "word") |>
  filter(!(word %in% c('john', 'march'))) |>
  group_by(sentiment) |>
  summarise(sent.count = sum(n))

Little.Women.emo |> 
  ggplot(aes(x=sentiment, sent.count)) +
  geom_bar(stat='identity', fill=colours, color='black') + xlab('Sentiment') + ylab('Frequency')

Monte.Cristo.emo = Monte.Cristo.counts |>
  inner_join(nrc.emotion , "word") |>
  filter(!(word %in% c('de', 'count'))) |>
  group_by(sentiment) |>
  summarise(sent.count = sum(n))

colours = list("coral1", "lightblue", "coral1", "coral1", 
               "lightblue", "coral1", "lightblue", "lightblue")

Monte.Cristo.emo |> 
  ggplot(aes(x=sentiment, sent.count)) +
  geom_bar(stat='identity', fill=colours, color='black') + xlab('Sentiment') + ylab('Frequency')


Frank.emo = Frank.counts |>
  inner_join(nrc.emotion , "word") |>
  group_by(sentiment) |>
  summarise(sent.count = sum(n)) |>
  ungroup()

colours = list("coral1", "lightgreen", "coral1", "coral1", 
               "lightgreen", "coral1", "lightgreen", "lightgreen")

Frank.emo |> 
  ggplot(aes(x=sentiment, sent.count)) +
  geom_bar(stat='identity', fill=colours, color='black') + xlab('Sentiment') + ylab('Frequency')


############# 5. SCORE AVERAGE BY CHAPTER ###########

LW.chapter.avg = Little.Women.unigram |>
  filter(dodds.score != 5) |>
  group_by(chapter) |>
  summarise(average = mean(dodds.score)) |>
  ungroup()

LW.chapter.avg |>
  ggplot(aes(x=chapter, y=average)) +
  geom_line(col='purple') +
  geom_vline(xintercept=24, linetype='dashed') +
  xlab("Chapter (Separated by Parts I and II)") + ylab("Average sentiment score")

Frank.chapter.avg = Frank.unigram |>
  filter(dodds.score != 5) |>
  group_by(chapter) |>
  summarise(average = mean(dodds.score)) |>
  ungroup()

Frank.chapter.avg |>
  ggplot(aes(x=chapter, y=average)) +
  geom_line(col='darkgreen') +
  xlab("Chapter") + ylab("Average sentiment score")

MC.chapter.avg = Monte.Cristo.unigram |>
  filter(dodds.score != 5) |>
  group_by(chapter) |>
  summarise(average = mean(dodds.score)) |>
  ungroup()

MC.chapter.avg |>
  ggplot(aes(x=chapter, y=average)) +
  geom_vline(xintercept=c(15, 47, 80), linetype='dashed') +
  geom_line(col='blue') +
  xlab("Chapter (Separated by points of change in trend)") + ylab("Average sentiment score")

########### 7. DIFFERENCE IN WORD APPEARANCE #######

# LITTLE WOMEN 

# Word frequencies grouped by each portion of the book
Little.Women.unigram = Little.Women.unigram |>
  mutate(book.part = ifelse(part %in% c(1,2), 1, 2))

LW.counts.byparts = Little.Women.unigram |>
  group_by(book.part) |>
  count(word) |>
  arrange(book.part, desc(n)) |>
  ungroup()

# Dataset with grouped summaries by part - overall total number of terms and average score
LW.total.parts = LW.counts.byparts |>
  left_join(dodds, by=c('word' = 'original.word')) |>
  mutate(score = ifelse(is.na(score) | word == 'march', 5, score)) |> 
  group_by(book.part) |>
  summarise(total.terms = sum(n), average = mean(score)) |>
  ungroup()

# Word frequencies 
LW.counts.byparts = LW.counts.byparts |>
  filter(book.part == 1) |>
  full_join(LW.counts.byparts[LW.counts.byparts$book.part==2, 2:3], by='word') |>
  left_join(dodds, by=c('word' = 'original.word')) |>
  mutate(score = ifelse(is.na(score) | word == 'march', 5, score)) |>
  select(word, score, n.x, n.y)

LW.counts.byparts[is.na(LW.counts.byparts)] <- 0
colnames(LW.counts.byparts) = c("word", "score", "n.1", "n.2")

LW.counts.byparts = LW.counts.byparts |>
  mutate(prop.1 = n.1 / as.numeric(LW.total.parts[1,2])) |>
  mutate(prop.2 = n.2 / as.numeric(LW.total.parts[2,2]))

LW.diff = LW.counts.byparts |>
  mutate(delta = ((prop.1 - prop.2)*(score - as.numeric(LW.total.parts[2,3]))) / (as.numeric(LW.total.parts[2,3]) - as.numeric(LW.total.parts[1,3]))) |>
  mutate(diff.freq = n.2 - n.1) |>
  select(word, score, n.1, n.2, delta, diff.freq) |>
  arrange(desc(abs(delta))) |>
  head(10)

LW.diff$word = factor(LW.diff$word, 
                      levels = LW.diff$word)

LW.diff = LW.diff |>
  mutate(Sentiment.Score = as.factor(ifelse(score > 5, "Positive", "Negative")))

LW.diff |>
  ggplot(aes(x=word, y = delta, fill=Sentiment.Score)) +
    geom_bar(stat='identity') + 
    xlab("Word") + ylab("Delta")



# COUNT OF MONTE CRISTO

# Word frequencies grouped by each portion of the book
Monte.Cristo.unigram = Monte.Cristo.unigram |>
  mutate(part.to.analyze = ifelse(chapter <= 15, 1, 
                                  ifelse(chapter > 15 & chapter <= 47, 2, 
                                         ifelse(chapter > 47 & chapter <= 80, 3, 4))))

MC.counts.byparts = Monte.Cristo.unigram |>
  group_by(part.to.analyze) |>
  count(word) |>
  arrange(part.to.analyze, desc(n)) |>
  ungroup()

# Dataset with grouped summaries by part - overall total number of terms and average score
(MC.total.parts = MC.counts.byparts |>
  left_join(dodds, by=c('word' = 'original.word')) |>
  mutate(score = ifelse(is.na(score) | word == 'de', 5, score)) |> 
  group_by(part.to.analyze) |>
  summarise(total.terms = sum(n), average = mean(score)) |>
  ungroup())

# Word frequencies, each part is in its own column
MC.counts.byparts = MC.counts.byparts |>
  filter(part.to.analyze == 1) |>
  full_join(MC.counts.byparts[MC.counts.byparts$part.to.analyze==2, 2:3], by='word') |>
  full_join(MC.counts.byparts[MC.counts.byparts$part.to.analyze==3, 2:3], by='word') |>
  full_join(MC.counts.byparts[MC.counts.byparts$part.to.analyze==4, 2:3], by='word') |>
  left_join(dodds, by=c('word' = 'original.word')) |>
  mutate(score = ifelse(is.na(score) | word == 'de', 5, score)) |>
  select(word, score, n.x, n.y, n.x.x, n.y.y)

MC.counts.byparts[is.na(MC.counts.byparts)] <- 0
colnames(MC.counts.byparts) = c("word", "score", "n.1", "n.2", "n.3", "n.4")

MC.counts.byparts = MC.counts.byparts |>
  mutate(prop.1 = n.1 / as.numeric(MC.total.parts[1,2])) |>
  mutate(prop.2 = n.2 / as.numeric(MC.total.parts[2,2])) |>
  mutate(prop.3 = n.3 / as.numeric(MC.total.parts[3,2])) |>
  mutate(prop.4 = n.4 / as.numeric(MC.total.parts[4,2]))

MC.diff.1.2 = MC.counts.byparts |>
  mutate(delta = ((prop.1 - prop.2)*(score - as.numeric(MC.total.parts[2,3]))) / (as.numeric(MC.total.parts[2,3]) - as.numeric(MC.total.parts[1,3]))) |>
  mutate(diff.freq = n.2 - n.1) |>
  select(word, score, n.1, n.2, delta, diff.freq) |>
  arrange(desc(abs(delta))) |>
  head(5)

MC.diff.1.2$word = factor(MC.diff.1.2$word, 
                      levels = MC.diff.1.2$word)

MC.diff.1.2 = MC.diff.1.2 |>
  mutate(Change = as.factor(ifelse(score > 5 & diff.freq > 0 | score < 5 & diff.freq < 0, "Positive", "Negative")))

MC.diff.1.2 |>
  ggplot(aes(x=word, y = diff.freq, fill=Change)) +
  geom_bar(stat='identity') + 
  xlab("Word") + ylab("Change in frequency") +
  ggtitle("Chapters 15-47 vs. Chapters 1-15")

MC.diff.2.3 = MC.counts.byparts |>
  mutate(delta = ((prop.2 - prop.3)*(score - as.numeric(MC.total.parts[3,3]))) / (as.numeric(MC.total.parts[3,3]) - as.numeric(MC.total.parts[2,3]))) |>
  mutate(diff.freq = n.3 - n.2) |>
  select(word, score, n.2, n.3, delta, diff.freq) |>
  arrange(desc(abs(delta))) |>
  head(10)

MC.diff.2.3$word = factor(MC.diff.2.3$word, 
                          levels = MC.diff.2.3$word)

MC.diff.2.3 = MC.diff.2.3 |>
  mutate(Change = as.factor(ifelse(score > 5 & diff.freq > 0 | score < 5 & diff.freq < 0, "Positive", "Negative")))

MC.diff.2.3 |>
  ggplot(aes(x=word, y = diff.freq, fill=Change)) +
  geom_bar(stat='identity') + 
  xlab("Word") + ylab("Change in frequency") +
  ggtitle("Chapters 48-80 vs. Chapters 15-47")

MC.diff.3.4 = MC.counts.byparts |>
  mutate(delta = ((prop.3 - prop.4)*(score - as.numeric(MC.total.parts[4,3]))) / (as.numeric(MC.total.parts[4,3]) - as.numeric(MC.total.parts[3,3]))) |>
  mutate(diff.freq = n.4 - n.3) |>
  select(word, score, n.3, n.4, delta, diff.freq) |>
  arrange(desc(abs(delta))) |>
  head(10)

MC.diff.3.4$word = factor(MC.diff.3.4$word, 
                          levels = MC.diff.3.4$word)

MC.diff.3.4 = MC.diff.3.4 |>
  mutate(Change = as.factor(ifelse(score > 5 & diff.freq > 0 | score < 5 & diff.freq < 0, "Positive", "Negative")))

MC.diff.3.4 |>
  ggplot(aes(x=word, y = diff.freq, fill=Change)) +
  geom_bar(stat='identity') + 
  xlab("Word") + ylab("Change in frequency") +
  ggtitle("Chapters 81-117 vs. Chapters 48-80")






########## 8. STABILITY ##########


n=nrow(dodds) * 0.8
R=50
set.seed(1234)

### Little Women
LW.stability <- data.frame(matrix(nrow=R, ncol=max(Little.Women.unigram$chapter)))

for (i in c(1:R)){
  index = sample(nrow(dodds), n, replace = FALSE, prob = NULL)
  lexicon_star = arrange(dodds[index,])
  
  stability.temp = Little.Women.unigram |>
    left_join(lexicon_star, by = c("word" = "original.word")) |>
    mutate(score = ifelse(is.na(score) | score == 'march', 5, score)) |>
    group_by(chapter) |>
    summarise(mean = mean(score)) |>
    ungroup()
  
  
  
  LW.stability[i, ] = t(stability.temp$mean)
  
  rm(index,lexicon_star, stability.temp)
}

# Assumes the data is of type matrix and chapters are the columns
stability.boxplots<-function(matrix) # Takes in a matrix (#rows iterations, #cols chapters/parts)
{
  ncol<-ncol(matrix)
  chapter_vector<-sort(rep(1:ncol,nrow(matrix)),decreasing = F)
  desired_form<-as.data.frame(cbind(c(matrix),chapter_vector))
  colnames(desired_form)<-c("Estimate.Distribution","Chapter")
  desired_form$Chapter<-as.factor(desired_form$Chapter) # v important!
  return(desired_form) # make sure to have a catch variable
}

catch<-stability.boxplots(as.matrix(LW.stability)) # test

ggplot(catch, aes(x = Chapter, y = Estimate.Distribution)) +
  geom_boxplot() # Plot the catch object, no need to fiddle with this too much!


### Monte Cristo
MC.stability <- data.frame(matrix(nrow=R, ncol=max(Monte.Cristo.unigram$chapter)))

for (i in c(1:R)){
  index = sample(nrow(dodds), n, replace = FALSE, prob = NULL)
  lexicon_star = arrange(dodds[index,])
  
  stability.temp = Monte.Cristo.unigram |>
    left_join(lexicon_star, by = c("word" = "original.word")) |>
    mutate(score = ifelse(is.na(score) | score == 'march', 5, score)) |>
    group_by(chapter) |>
    summarise(mean = mean(score)) |>
    ungroup()
  
  MC.stability[i, ] = t(stability.temp$mean)
  
  rm(index,lexicon_star, stability.temp)
}

catch<-stability.boxplots(as.matrix(MC.stability)) # test

ggplot(catch, aes(x = Chapter, y = Estimate.Distribution)) +
  geom_boxplot() 



############ 9. TRANSLATION COMPARISON #############

##### 9.1. Manipulating the French dataset ######
French.temp.1 = gutenberg_download(17989, mirror=my_mirror) |> filter(text != ""); French.temp.1 = French.temp.1[-c(1:5),]
French.temp.2 = gutenberg_download(17990, mirror=my_mirror) |> filter(text != ""); French.temp.2 = French.temp.2[-c(1:4),]
French.temp.3 = gutenberg_download(17991, mirror=my_mirror) |> filter(text != ""); French.temp.3 = French.temp.3[-c(1:4),]
French.temp.4 = gutenberg_download(17992, mirror=my_mirror) |> filter(text != ""); French.temp.4 = French.temp.4[-c(1:4),]

chapters = rbind(French.temp.1[1:31,2], French.temp.2[1:24,2], French.temp.3[1:29,2], French.temp.4[1:33,2])

French = rbind(French.temp.1, French.temp.2, French.temp.3, French.temp.4)
French = French[-c(49111:49601),] # Remove the bibliography

French = French |>
  filter(!text %in% chapters$text)

chapters$text<-replacePunctuation(as.character(chapters$text))
chapters$text<-gsub(' \\S*',"",as.character(chapters$text))

# Extracting the chapters into a new column
French = French |>
  mutate(chapter.empty = ifelse(text %in% chapters$text, text, ""))

French = data.table(French)
French[!nzchar(chapter.empty),chapter.empty:=NA][,chapter.word:=na.locf(chapter.empty)]

# Turning the chapter variable into numeric
French[, chapter:=rleid(chapter.word)]

# Removing the rows with CHAPTER __ in text. Also removing flagged chapter column
French = French |>
  filter(!text %in% chapters$text) |>
  select(chapter, text)

# Convert to lower case
French$text<-tolower(French$text)

# Replacing punctuation with blanks
French$text = replacePunctuation(French$text)

# Tokenizing by word, remove certain stopwords, flagging negative words
French.unigram = French |>
  unnest_tokens(output=word, input=text) |>
  anti_join(as.data.frame(stopwords('french')), by=c("word" = 'stopwords("french")')) 

French.unigram = French.unigram |>
  left_join(dodds.french, by=c("word" = "original.word")) |>
  mutate(score = ifelse(is.na(score), 5, score)) |>
  rename(dodds.score = score)

###### 9.2. Average score by chapter #######
English.chapter.avg = Monte.Cristo.unigram |>
  group_by(chapter) |>
  summarise(average = mean(dodds.score)) |>
  ungroup()

French.chapter.avg = French.unigram |>
  group_by(chapter) |>
  summarise(average = mean(dodds.score)) |>
  ungroup()

#library(writexl)
#write_xlsx(English.chapter.avg, '/Users/cristinaagatep/Desktop/School/STAT5703/Assignments/Research Project/English.chapter.avg.xlsx')
#write_xlsx(French.chapter.avg, '/Users/cristinaagatep/Desktop/School/STAT5703/Assignments/Research Project/French.chapter.avg.xlsx')

# send to patric in excel

ggplot(NULL, aes(x=chapter, y=average)) +
  geom_line(data=English.chapter.avg, aes(col="English")) +
  geom_line(data=French.chapter.avg, aes(col="French")) +
  geom_vline(xintercept=c(28, 47, 73, 95), linetype='dashed') +
  scale_color_manual(name='Language of book',
                     breaks=c('English', 'French'),
                     values=c('English'='#f04546', 'French'='#3591d1')) +
  xlab("Chapter (Separated by Parts I - V)") + ylab("Average sentiment score")


