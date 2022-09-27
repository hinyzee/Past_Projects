#Loading packages
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)
library(seededlda)

#Setting working directory
setwd("~/Downloads/archive (3)")

#Load Data
metadata <- read.csv("ios_android.csv")

#Creating a corpus of ios_android
corpus_ia <- corpus(metadata, text_field = "Body")

#Preprocessing text
toks <- tokens(corpus_ia, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

#Trimming dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed

#Running LDA using quanteda
lda <- textmodel_lda(dfm_trimmed, k = 10)

#Most likely term for each topic
lda.terms <- terms(lda, 10)
lda.terms

#Topical content matrix
mu <- lda$phi
dim(mu) 
mu[1:10,1:20]
#Most representative words in Topic 1
mu[1,][order(mu[1,], decreasing=T)][1:10]

#Topical prevalence matrix
pi <- lda$theta
dim(pi)

#Most representative documents in Topic 1
metadata[order(pi[,1],decreasing=T),]

#STM
#Process the data to put it in STM format.  Textprocessor automatically does preprocessing
temp<-textProcessor(documents=metadata$Body,metadata=metadata)
#prepDocuments removes words/docs that are now empty after preprocessing
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

model.stm <- stm(out$documents, out$vocab, K = 10, prevalence = ~Label ,
                 data = out$meta, ) 

labelTopics(model.stm)

plot(model.stm, n=10)

model.stm.ee <- estimateEffect(1:10 ~ Label , model.stm, meta = out$meta)
plot(model.stm.ee, "Label")
plot(model.stm.ee, "Label", method="difference", cov.value1="android", cov.value2="ios")

