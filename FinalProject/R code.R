library(tidyverse)
library(jsonlite)
library(reshape2)
library(stringr)
library(magrittr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(sqldf)
library(haven)
library(tidytext)
library(quanteda)
library(dplyr)
library(lda)
library(igraph)
library(ggraph)
library(SnowballC)
library(topicmodels)

pra = 
  fromJSON("done.json") %>% 
  as_tibble()

erc <- read.csv("ngpr.csv") %>% as_tibble()

# Randomly select 2500 obs from erc
#set.seed(123456)
#rs<-erc[sample(nrow(erc), 2500), ]



# Turn pra into raw in order to merge with erc
pra<-pra %>% unlist() %>% matrix( nrow=length(pra$cik),ncol=3) %>% as_tibble()
names(pra)<-c("cik", "datadate","pr")

spr <- sqldf("select a.*, b.pr
              from erc as a left join pra as b
              on a.cik=b.cik and a.datadate=b.datadate")
rm(pra)


# Replace all the line identification with space
test1<-spr %>% mutate(pr=(str_replace_all(pr, "[\r\n]" , " ")))

# Transfer to lower case
corpus <- SimpleCorpus(VectorSource(test1$pr))
corpus <- tm_map(corpus, content_transformer(tolower))

for (i in 1:length(test1$pr)) {
  test1[i,9]<-corpus[[i]]$content
}
rm(corpus)

# Most of the Press Release start with ex-99*. Thus split by "ex-99" will get most of cases correct
ex99<-colsplit(test1$pr, "ex-99", names = c("useless", "PressRelease"))

# Most of the Press Releases' useful content start after "exhibit 99"
ex99_1<-matrix(NA,nrow=length(ex99$PressRelease),ncol=2) %>% as_tibble
names(ex99_1)<-c("useless1", "PressRelease1")
for (i in 1:length(ex99$PressRelease)) {
  if (grepl("exhibit 99", ex99$PressRelease[i], fixed=TRUE)) {
    ex99_1[i,]<-colsplit(ex99$PressRelease[i],"exhibit 99",names = c("useless", "PressRelease"))
  }
  else{
    ex99_1[i,2]<-ex99$PressRelease[i] # if there is not "exhibit 99" then keep the origin value
    
  }}

# Drop original pressrelease and useless
test1<-cbind(test1,ex99_1)[-c(9:10)]
rm(ex99_1)


# The press releases start with exhibit 99, but there are cases where companies mentioned exhibit 99 in their 8-K. In case
# these firms also mentioned ex-99, we need to check if press release is empty before split useless.

ex99_2<-matrix(NA,nrow=length(ex99$PressRelease),ncol=2) %>% as_tibble
names(ex99_2)<-c("useless2", "PressRelease2")

ex99[ex99==""]<-NA # Need to change blank cell to NA for next step

for (i in 1:length(ex99$PressRelease)){
  if (is.na(ex99$PressRelease[i])){
    ex99_2[i,]<-colsplit(ex99$useless[i],"exhibit 99", names = c("useless2", "PressRelease2"))  
  }
}

ex99_2<-colsplit(ex99_2$PressRelease2,"exhibit 99", names = c("useless2", "PressRelease2"))
rm(ex99)

test1<-cbind(test1,ex99_2)[-10]
rm(ex99_2)

test1$PressRelease<-paste(test1$PressRelease1,test1$PressRelease2)

test1<-test1[-c(9:10)]

test1$ind <- seq.int(nrow(test1))

test1$PressRelease[test1$PressRelease==" "]<-NA # Need to change blank cell to NA for next step

test1 %<>% drop_na(PressRelease)

# Export to save RAM
# write_json(test1,"C:\\Jwang\\00-OKU\\Research\\Projects\\R_Code\\Machine learning\\test1.json")

rm(spr)


##################################################################################################################
##################################################################################################################
#################################### For Replication you can start here ##########################################
##################################################################################################################
##################################################################################################################

test1 = 
  fromJSON("test1.json") %>% 
  as_tibble()


# Descriptive Analyses using tidytext

# A major advantage of tidytext format is that once the text has been tidy-ed, regular R functions can be used to 
# analyze it instead of the specialized functions necessary to analyze a Corpus object. 
# An advantage of tidytext is that it removes punctuation automatically.
# tidytext automatically makes all words lower case.


# tokenization into one-word and Remove stop words
corpus <- unnest_tokens(test1, word, PressRelease) %>% anti_join(filter(stop_words, lexicon == "snowball"))

# Reorganize for two-word tokenization
corpusns <- corpus  %>% group_by(ind) %>% summarise(word = paste(word, collapse = " "))

# 2-gram analysis
# The following code will split the comments on the professors into two consecutive words. 
# Note that we set token="ngrams" and then n=2 for two words.

corpus <- unnest_tokens(corpusns, word, word, token = "ngrams", n = 2)


# the frequency of each word in barlots
count <- corpus %>% count(word, sort = TRUE)
head(count)

count %>% top_n(20) %>% mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + coord_flip()

# word cloud

count %>% with(wordcloud(word, n, max.words = 50, random.order = FALSE, rot.per = 0.35, 
                         colors = brewer.pal(8, "Dark2")))


########################################## Train Model ###################################################

########## Preprocess ##############

# Randomly select 2000 observations
set.seed(123456)
corpus_train <- corpusns[sample(nrow(corpusns), 2000), ]



# Get the output for sLDA
erc<-test1$erc[test1$ind %in% corpus_train$ind]

### Stem & Remove numbers ###

corpus2 <- unnest_tokens(corpus_train, word, word, token = "ngrams", n = 2) %>% 
  mutate(stem = wordStem(word)) %>%   # stems words and creates column
  filter(!grepl('[0-9]', word))  # Remove numbers

# Transform into DTM

test.dtm <- corpus2 %>% group_by(ind) %>% count(stem, sort = TRUE) %>% cast_dtm(ind, stem, n)
tm::inspect(test.dtm)

# the sparsity of the matrix, i.e., the number and percentage of zero values in the matrix. 
# For our example, 97% of the matrix has value 0.
# the following code only keeps the words with less than 97% sparsity. 

test.dtm <- removeSparseTerms(test.dtm, 0.97)
tm::inspect(test.dtm)
rm(corpus)
rm(corpusns)


##################################### LDA to determine the number of topics #########################################

k.topics <- 2:9
folding <- rep(1:5, each = 400)

runonce <- function(k, fold) {
  testing.dtm <- which(folding == fold)
  training.dtm <- which(folding != fold)
  
  training.model <- LDA(test.dtm[training.dtm, ], k = k)
  test.model <- LDA(test.dtm[testing.dtm, ], model = training.model, control = list(estimate.beta = FALSE))
  
  perplexity(test.model)
}

res <- NULL

for (k in 2:9) {
  for (fold in 1:5) {
    res <- rbind(res, c(k, fold, runonce(k, fold)))
  }
}



# We now have the perplexity for each fold and can take sum or average of them. For example, the total perplexity with 
# 2 to 9 topics is given below. We can observe that with 9 topics, we have the smallest perplexity. Therefore, we can 
# choose k=9 topics for analysis.

total.perp <- tapply(res[, 3], res[, 1], sum)
round(total.perp)

plot(2:9, total.perp, type = "b", xlab = "Number of topics", ylab = "Perplexity")


# LDA with 9 topics

# we can analyze the data again by setting k=9. The top 10 words related to each topic are given below. 
# The meaning of each topic can be decided based on the top words. We also print the top 2 topics for the comments 
# for the first 10 press release in the data.

test.lda <- LDA(test.dtm, k = 9)

terms(test.lda, 10)

topics(test.lda, 2)[1:2, 1:10]


################################################ sLDA modeling ###################################################

# Data needs to be in lda format

# Nest and ordered by index
test.nest <- corpus2  %>% group_by(ind) %>% summarise(word = paste(word, collapse = " "))

## change to lda data format
test.lda.data <- test.nest %>% pull(word) %>% lexicalize(lower = TRUE)

# Train model
params <- sample(c(-1, 1), 9, replace = TRUE)  ## starting values, 6 is the topics from LDA

# Get the output
erc=erc

slda_mod <- slda.em(documents = test.lda.data$documents, K = 9, vocab = test.lda.data$vocab, 
                    num.e.iterations = 100, num.m.iterations = 4, alpha = 1, eta = 0.1, params = params, 
                    variance = var(erc), annotations = erc, method = "sLDA")

summary(slda_mod$model)

########################################## Validate the predicted ERCs ############################################


# Get the predicted ERCs using the trained model
yhat <- slda.predict(test.lda.data$documents, slda_mod$topics, slda_mod$model,
                     alpha = 1.0, eta = 0.1)

y<-cbind(as_tibble(yhat),as_tiblle(erc))


est<- lm (erc ~ yhat, data=y)

summary(est)

