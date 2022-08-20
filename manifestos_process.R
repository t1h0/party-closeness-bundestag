library(manifestoR)
library(magrittr)
library(stringr)
library(NLP)
library(tm)

#' Downloads manifestos from all german Bundestag parties.
#'
#' @param manifestos_date The date of the manifestos to download.
#'
#' @return The Manifesto corpus containing the downloaded manifestos.
manifestos_download <- function(manifestos_date) {
  ### Manifesto Download
  return(mp_corpus(countryname == "Germany" &
                     date == manifestos_date))  # subsetting the manifesto corpus to get the documents for current parliamentary parties
}

#' Creates a TF-IDF Document-Term-Matrix from a manifesto corpus containing
#' german Bundestag parties' manifestos.
#'
#' @param corpus Manifesto corpus.
#'
#' @return TF-IDF Document-Term-Matrix.
manifestos_tfidf <- function(corpus) {
  stopwords <-
    read.table(
      "https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt"
    )$V1
  tfidf <- corpus %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(str_replace_all), "\\W", " ") %>% # Remove non-word characters in between words
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument, "german") %>%
    lapply(as.character) %>%
    lapply(str_c, collapse = " ") %>%
    VectorSource() %>%
    VCorpus() %>%
    DocumentTermMatrix(control = list(weighting = weightTfIdf))
  
  return(tfidf)
}

#' Executes after another manifestos_download and manifestos_tfidf and
#' returns the output of the latter.
#'
#' @param manifestos_date The date of the manifestos to download.
#'
#' @return TF-IDF Document-Term-Matrix.
manifestos_process <- function(manifestos_date) {
  return(manifestos_date %>%
           manifestos_download %>%
           manifestos_tfidf())
}