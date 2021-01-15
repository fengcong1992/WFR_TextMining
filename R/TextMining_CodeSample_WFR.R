#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to use text mining for wind forecasting literature review
# 2 Coder: Cong Feng        Date: 2020/01/02       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
# clear R workspace and console
rm(list=ls(all=TRUE))
cat("\014")

root_data1 <- '' # your data directory
root_save <- '' # your results directory


library(stringr)
library(plyr)
library(ggrepel)
library(rscopus)
library(rvest)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(tibble)
library(tau) 
library(corpus)
source('find_abbrev.R') # source find_abbrev.R from dazhiyang's github


load(file.path(root_data1, 'WFR_data.RData'))
data_journal <- WFR_MetaData$Source

## Journal infrastructure: data for Figs. 3 and 4
top_journal <- data.frame(count(data_journal))
top_journal <- top_journal[rev(order(top_journal$freq)),]
list_count <- NULL
for (n_journal in 1:nrow(top_journal)) {
  list_count <- c(list_count, mean(WFR_MetaData[WFR_MetaData$Source == top_journal[n_journal, 'x'], 'Cites']))
}
top_journal$citation <- list_count

# data for Table 2
df_reviewpaper <- WFR_MetaData[str_detect(WFR_MetaData$Title, 'review')|WFR_MetaData$Type=='Review', 'Title']

## Author infrastructure: data for Fig. 6. Please note that other author infrastructure analysis was performed by the VOS.
print(df_author)

## full text data collection
data_filter <- WFR_MetaData
set_api_key('') # please use your own api_key
data_fulltext <- NULL
data_badpaper <- NULL
count_goodpaper <- 1
for (n_paper in 1:nrow(data_filter)) {
  DOI <- as.character(data_filter[n_paper, 'DOI'])
  URL <- as.character(data_filter[n_paper, 'ArticleURL'])
  if (!is.na(DOI)) { # first retrieve by DOI
    x <- article_retrieval(DOI, identifier = "doi", verbose = FALSE, view = "FULL")
  } else {
    if (strsplit(URL, '[.]')[[1]][2] == 'scopus') { # by scopus id
      x <- article_retrieval(substr(URL, 65, 75), identifier = "scopus_id", verbose = FALSE, view = "FULL")
    }
    if (strsplit(URL, '[.]')[[1]][2] == 'sciencedirect') { # by pii
      list_URL_comp <- strsplit(URL, '/')[[1]]
      x <- article_retrieval(list_URL_comp[length(list_URL_comp)], identifier = "pii", verbose = FALSE, view = "FULL")
    } else {
      list_URL_comp <- strsplit(URL, '/')[[1]]
      x <- article_retrieval(list_URL_comp[length(list_URL_comp)], identifier = "doi", verbose = FALSE, view = "FULL")
    }
  }
  fulltext <- x$content$`full-text-retrieval-response`$originalText
  # clean
  if (!is.null(fulltext)) {
    fulltext_cl <- strsplit(fulltext, 'References ')
    fulltext_cl <- fulltext_cl[[1]][1:length(fulltext_cl[[1]])-1]
    fulltext_cl <- strsplit(fulltext_cl, 'Introduction')
    fulltext_cl <- fulltext_cl[[length(fulltext_cl)]][length(fulltext_cl[[1]])]
    fulltext_cl <- unlist(fulltext_cl)
    # authors
    list_author <- as.vector(as.character(unlist(x$content$`full-text-retrieval-response`$coredata$`dc:creator`)))
    list_author <- list_author[!list_author=='true']
    list_author <- paste(list_author, collapse = '; ')
    # 
    #list_string <- strsplit(as.character(data_filter[n_paper, 'PA']), ',')[[1]]
    #country <- list_string[length(list_string)]
    data_fulltext_loop <- data.frame(c(data_filter[n_paper, c('Cites', 'Authors', 'Title', 'Year', 'Source', 'ArticleURL', 'Publisher', 'Type', 'DOI', 'AuthorCount', 'Age')], list_author, fulltext_cl))
    colnames(data_fulltext_loop) <- c('Cites', 'Authors', 'Title', 'Year', 'Source', 'ArticleURL', 'Publisher', 'Type', 'DOI', 'AuthorCount', 'Age', 'ListAuthor','FT')
    data_fulltext <- rbind(data_fulltext, data_fulltext_loop)
    write.table(fulltext_cl, file = file.path(root_save1, paste0(count_goodpaper, '_', substr(data_fulltext_loop$Title, 1, 50), '.txt')), sep = "\t",row.names = TRUE, col.names = NA)
    count_goodpaper <- count_goodpaper + 1
  } else {
    data_badpaper <- rbind(data_badpaper, data_filter[n_paper,])
  }
}

## Tern frequency analysis
# 1. Creating corpus and tokenization
dfCorpus <- SimpleCorpus(VectorSource(data_fulltext))
# 2. Upper-to-lower case conversion
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
# 3. Whitespace removal
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
# 5. Punctuation removal
dfCorpus <- tm_map(dfCorpus, removePunctuation)
# 6. Stop-word removal
dfCorpus <-tm_map(dfCorpus,removeWords,stopwords("english"))
# 7. Stemming
tab1 <- read.delim(file.path(root_data1, 'lemmatization-en.txt'), header=FALSE, stringsAsFactors = FALSE)
#tab2 <- read.delim(file.path(root_data1, 'decipline.txt'), header=FALSE, stringsAsFactors = FALSE)
names(tab1) <- c("stem", "term")
tab <- rbind(tab1) # combine multiple tabs if necessary
stem_list <- function(term) {
  i <- match(term, tab$term)
  if (is.na(i)) {
    stem <- term
  } else {
    stem <- tab$stem[[i]]
  }
  stem
}
dfCorpus <- text_tokens(dfCorpus, stemmer = stem_list)
dfCorpus <- as_corpus_text(dfCorpus)
dfCorpus <- SimpleCorpus(VectorSource(dfCorpus))
# 8. document-term matrix generation
DTM <- DocumentTermMatrix(dfCorpus)

# word ngrams: Fig. 8
createNgram <-function(stringVector, ngramSize){
  ngram <- data.table()
  ng <- textcnt(stringVector, method = "string", n=ngramSize, tolower = FALSE)
  if(ngramSize==1){
    ngram <- data.table(w1 = names(ng), freq = unclass(ng), length=nchar(names(ng)))  
  }
  else {
    ngram <- data.table(w1w2 = names(ng), freq = unclass(ng), length=nchar(names(ng)))
  }
  return(ngram)
}

res <- createNgram(dfCorpus, 2)
term_freq1 <- res[rev(order(res$freq)),]
colnames(term_freq1) <- c('Word', 'Freq', 'Length')


# Abbreviation analysis
data_abbrev_detect <- NULL
data_abbrev_nondetect <- NULL

for (n_paper in 1:10) { # nrow(data_fulltext)
  cat('Proccesing ', n_paper, 'out of ', nrow(data_fulltext), '\n')
  #n_paper <- 2
  raw_text <- as.character(data_fulltext[n_paper, 'FT'])
  if (!is.na(raw_text)) {
    text <- raw_text %>% stripWhitespace(.) %>% gsub("\\s([?[:punct:]](?:\\s|$))", "\\1", .) # remove extra spaces and spaces before punctuations.
    abbrv <- find_abbrev(text)
    if (!all(is.na(abbrv$abbrev))) {
      data_loop1 <- data.frame(abbrv$abbrev, abbrv$full, data_fulltext[n_paper, 'Title'])
      data_abbrev_detect <- rbind(data_abbrev_detect, data_loop1)
    }
    if (!all(is.na(abbrv$undetect))) {
      data_loop2 <- c(abbrv$undetect, abbrv$otherabbrev, data_fulltext[n_paper, 'Title'])
      data_abbrev_nondetect <- c(data_abbrev_nondetect, data_loop2)
    }
  }
}

data_abbrev_unique  <- data_abbrev_detect[!duplicated(data_abbrev_detect$abbrv.abbrev),]
data_abbrev_unique <- ddply(data_abbrev_detect,.(abbrv.abbrev, abbrv.full,abbrv.abbrev),nrow)
data_abbrev_unique <- data_abbrev_unique[rev(order(data_abbrev_unique$V1)),]




