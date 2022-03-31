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
    filter(word %in% negatives == FALSE) |>
    mutate(stemmed.word = wordStem(word))
  
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


# Used for word frequencies
Little.Women.counts = Little.Women.unigram |>
  count(stemmed.word) |>
  arrange(desc(n))



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
Monte.Cristo.unigram = Monte.Cristo.unigram |>
  left_join(dodds, by = c("word" = "original.word")) |>
  rename(dodds.score = score) |>
  mutate(dodds.score = ifelse(is.na(dodds.score), 5, dodds.score))

# If the word had a negative instance - the score is adjusted
Monte.Cristo.unigram = Monte.Cristo.unigram |>
  mutate(dodds.score = ifelse(negative.flag == 1, 5 - (dodds.score-5), dodds.score))

# Used for word frequencies
Monte.Cristo.counts = Monte.Cristo.unigram |>
  count(stemmed.word) |>
  arrange(desc(n))

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

# Used for word frequencies
Frank.counts = Frank.unigram |>
  count(stemmed.word) |>
  arrange(desc(n))



############# 4. GENERAL EXPLORATION #########

# 25 Most Common words
barplot(Little.Women.counts$n[1:25], las = 2, names.arg = Little.Women.counts$stemmed.word[1:25], 
        col = "lavender", ylab = "Word frequencies")

barplot(Monte.Cristo.counts$n[1:25], las = 2, names.arg = Monte.Cristo.counts$stemmed.word[1:25], 
        col = "lightblue", ylab = "Word frequencies")

barplot(Frank.counts$n[1:25], las = 2, names.arg = Frank.counts$stemmed.word[1:25], 
        col = "lightgreen", ylab = "Word frequencies")



############# 5. ROLLING MEANS ###########

rolling_means(Little.Women.unigram, 10000, color="purple")
rolling_means(Monte.Cristo.unigram, 10000, color="blue")
rolling_means(Frank.unigram, 5000, color="darkgreen")

## Only including words with scores more than / equal to 7 or less than / equal to 3
temp.1 = Little.Women.unigram |> filter(dodds.score >=7 | dodds.score <= 3)
temp.2 = Monte.Cristo.unigram |> filter(dodds.score >=7 | dodds.score <= 3)
temp.3 = Frank.unigram |> filter(dodds.score >=7 | dodds.score <= 3)

rolling_means(temp.1, 10000, color="purple")
rolling_means(temp.2, 1000, color="blue")
rolling_means(temp.3, 1000, color="darkgreen")



