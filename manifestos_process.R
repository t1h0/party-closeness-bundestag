library(manifestoR)
library(magrittr)
library(stringr)
library(NLP)
library(tm)

manifestos_download <- function(manifestos_date) {
  ### Manifesto Download
  return(mp_corpus(countryname == "Germany" &
                     date == manifestos_date))  # subsetting the manifesto corpus to get the documents for current parliamentary parties
}

manifestos_tfidf <- function(d) {
  partyabbrev <-
    c("BÜNDNIS 90/DIE GRÜNEN",
      "DIE LINKE.",
      "SPD",
      "FDP",
      "CDU/CSU",
      "AfD") # These are the partyabbreviations as used in parliament protocols
  
  stopwords <- read.table("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")$V1
  tfidf <- d %>% 
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(str_replace_all),"\\W", " ") %>% # Remove non-word characters in between words
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords,stopwords) %>%
      tm_map(stripWhitespace) %>%
      tm_map(stemDocument,"german") %>%
    lapply(as.character) %>%
    lapply(str_c, collapse = " ") %>%
    VectorSource() %>%
    VCorpus() %>%
    DocumentTermMatrix(control=list(weighting=weightTfIdf))
  
  tfidf$dimnames$Docs <-
    partyabbrev # correctly naming the matrices after the respective party
  return(tfidf)
}

#### Processing of parties' manifestos------------------------------------------------------
manifestos_process <- function(manifestos_date) {
  return(
    manifestos_date %>%
      manifestos_download %>%
      manifestos_tfidf()
  )
}