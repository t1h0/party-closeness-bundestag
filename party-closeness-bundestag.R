#### Preparation-------------------------------------------------------------------------
library(manifestoR)
library(comprehenr)
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
library(xml2)
library(RSelenium)
library(robotstxt)
library(text2vec)
library(xtable)
`%!in%` <- Negate(`%in%`)

setwd("/")

# Path for downloads
download_path <- file.path("Protocols/")
if (!dir.exists(download_path)) {
  download_path %>%
    dir.create(recursive = TRUE)
}

# Path to file containing the API key to the manifesto API
mp_setapikey("manifesto_apikey.txt")

# 

#### I. Processing of parties' manifestos------------------------------------------------------

### Manifesto Download

df <-mp_corpus(countryname == "Germany" & date==201709) # subsetting the manifesto corpus to get the documents for current parliamentary parties
mp_main <- mp_maindataset() # info about all documents available in the manifesto corupus 
mp_germany <- subset(mp_main,countryname == "Germany" & date==201709) # info about the extracted documents
partyabbrev <- c("BÜNDNIS 90/DIE GRÜNEN","DIE LINKE","SPD","FDP","CDU/CSU","AfD") # These are the partyabbreviations as used in parliament protocols

### Text preparation

## Function for preprocessing text corpora 
process_corpus <- function(c){
  stopwords <- read.table("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")$V1
  return(
    c %>% 
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(str_replace_all),"\\W", " ") %>% # Remove non-word characters in between words
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords,stopwords) %>%
      tm_map(stripWhitespace) %>%
      tm_map(stemDocument,"german")
  )
}
df_processed <- process_corpus(df) # The processed manifestos

### Computation of TF-IDF scores
main_tfidf_manifesto <- df_processed %>%
  lapply(as.character) %>%
  lapply(str_c,collapse=" ") %>%
  VectorSource() %>%
  VCorpus() %>%
  DocumentTermMatrix(control=list(weighting=weightTfIdf))

main_tfidf_manifesto$dimnames$Docs <- partyabbrev # correctly naming the matrices after the respective party

#### II. Preprocessing of Speeches-------------------------------------------------------

### Web scraping

## Start selenium server
server <- rsDriver(verbose = TRUE, browser = "firefox")
class(selenium_server)
client <- server$client

## URL Preparation
base_url <- "https://www.bundestag.de"
dataurl <- "https://www.bundestag.de/services/opendata"

## Being Nice
dataurl_parsed <- xml2::url_parse(dataurl)
robots <- dataurl_parsed$server %>%
  robotstxt()
dataurl_parsed$path %>%
  robots$check()
robots$crawl_delay

## Scrape Links to Protocol-XMLs

client$navigate(dataurl)

## Clicking through the dynamic content to load the XML links
repeat{
  Sys.sleep(2)
  if (length(client$findElements(using = "xpath", value = "//button[@class='slick-next slick-arrow slick-disabled']"))!=0) {
    break # Stopping if end is reached, i.e. if the button to be clicked is deactivated
  }
  nextbtn <- client$findElement(using = "xpath", value = "//button[@class='slick-next slick-arrow']")
  nextbtn$clickElement()
}

## Extracting the XML Links
xml_links <- client$getPageSource() %>%
  unlist() %>%
  read_html() %>%
  html_elements(xpath="//a[@class='bt-link-dokument' and starts-with(@title,'XML')]") %>%
  xml_attrs()

## Stopping the server
server[["server"]]$stop()

## Download protocols

for (i in xml_links){
  download_xml(str_c(base_url,i[["href"]]),str_c(download_path,basename(i[["href"]])))
  #Sys.sleep(5) # Being nice anyway (though not explicitly asked by robotstxt)
}

### Extraction of speeches

## Paths to XMLs
files <- list.files(download_path, 
                    pattern = "\\.xml", full.names=TRUE)

main_speeches <- list() # Will contain all speeches, sorted by topic
main_politicians <- list() # Will contain all politicians

for (f in files){
  # Reading the file
  file <- read_xml(f)
  
  # Finding all topic sections
  tops <- file %>%
    html_elements(xpath="//tagesordnungspunkt[descendant::rede]")
  
  for (top in tops){
  
    # All speeches in that topic:
    reden <- top %>%
      html_elements(xpath="./rede")
    
    reden_clean <- list() # Will contain the cleaned speeches (all tags with actual speech text)
    
    for (rede in reden){
      
      # Extract info about speaker
      first_name <- html_element(rede,xpath="./descendant::vorname") %>%
        html_text()
      last_name <- html_element(rede,xpath="./descendant::nachname") %>%
        html_text()
      party <- html_element(rede,xpath="./descendant::name[1]/child::fraktion") %>%
        html_text()
      # Unique speaker id
      id <- html_element(rede,xpath="./descendant::redner/@id") %>%
        html_text()  %>%
        str_replace_all("[^[:alnum:]]", "")
        speaker <- str_c(last_name,first_name,sep=", ")
      
      if (is.na(last_name)){
        next # Skip speech if name of speaker is not provided
      }
      
      # Add speaker to list of politicians
      if (is.null(main_politicians[[speaker]])){
        main_politicians[[speaker]] <- list("First Name"=first_name,"Last Name" = last_name,"Party"=party,"ID"=id)
      }
      
      # Extracting tags surrounding actual speech tags
        
      # Number of all "redner" tags (containing unique speaker id)
      rednernodes <- rede %>%
        html_elements(
          xpath=str_c(
            "./p[@klasse='redner' and child::redner[@id='",
            id,
            "']]"
            )
          ) %>%
        length()
      
      # Number of all interruptions by chairman ("name" tags) or interpositions ("redner" tags with different id)
      internodes <- rede %>%
        html_elements(
          xpath=str_c(
            "./name[preceding-sibling::p[@klasse='redner']] | ./p[@klasse='redner' and child::redner[@id!='",
            id,
            "']]"
          )
          ) %>%
        length()
      nodelist <- list() # Will contain every tag containing speech text (by the speaker)
      
      # Iterating over every "redner" tag by speaker and extract those p-tags between the respective and interrupting tags
      # The idea is: Tags between first speaker tag and first interruption must have one speaker tag before them and all interrupting tags after them
      # Tags between second speaker tag and second interruption must have 2 speaker tags before and one interruption tag less behind them
      # And so on..
      for (i in 1:rednernodes) {
        nodelist <- c(nodelist,
                           rede %>%
                             html_elements(
                               xpath=str_c(
                                 "./p[count(preceding-sibling::p[@klasse='redner' and child::redner[@id='",
                                 id, # Speaker tags have this id
                                 "']]) = ",
                                 i, # Number of speaker tags before relevant tags
                                 " and count(following-sibling::name) + count(following-sibling::p[@klasse='redner' and child::redner[@id!='",
                                 id, # None-speaker tags do not have this id
                                 "']])=",
                                 internodes-i+1, # Number of interrupting tags after relevant tags
                                 "]"
                                 )
                               ) %>%
                             html_text()
        )
      }
      # Ad extracted text to the speech list of the respective speaker
      main_speeches[[speaker]] <- c(main_speeches[[speaker]],str_c(nodelist, collapse=" "))
    }
  }
}

### Preparation

## Sorting

main_speeches <- main_speeches[order(names(main_speeches))] # Sort speeches by speaker name alphabetically
main_politicians <- main_politicians[order(names(main_politicians))] # Sort politicians by name alphabetically

## Duplicates

last_names <- to_list(for (i in main_politicians) i[["Last Name"]]) # List of last names
duplicates_boolean <- to_list(for (i in main_politicians) i[["Last Name"]] %in% last_names[duplicated(last_names)])
possible_duplicates <- main_politicians[unlist(duplicates_boolean)] # Possible duplicates (politicians listed with same last name)

## Acutal manually extracted duplicated
actual_duplicates <- list(c("Cotar, Joana Eleonora","Cotar, Joana"),c("Gauland, Eberhardt Alexander","Gauland, Alexander"),c("Kuhle, Konstantin Elias","Kuhle, Konstantin"),c("Schäuble, Dr. Wolfgang","Schäuble, Wolfgang"),c("Schreiber, Eva-Maria Elisabeth","Schreiber, Eva-Maria"))

## Manually deleting duplicates from politicians list
for (i in actual_duplicates){
  main_politicians[[i[[1]]]] <- NULL
}

## Manually merging speeches from duplicates in speeches list
for (i in actual_duplicates){
  main_speeches[[i[[2]]]] <- c(main_speeches[[i[[2]]]],main_speeches[[i[[1]]]])
  main_speeches[[i[[1]]]] <- NULL
}

## Missing parties
# Download zip containing the XML with parliamentarians data
download.file("https://www.bundestag.de/resource/blob/472878/5ff47798a24a5e729d0d116f2d4c6bb2/MdB-Stammdaten-data.zip",str_c(download_path,"/parliamentarians.zip"))
# Unzip it
unzip(str_c(download_path,"/parliamentarians.zip"),exdir=str_c(download_path,"parliamentarians"))
# Find the local XML path 
parfile <- list.files(download_path, 
                    pattern = "\\.xml", full.names=TRUE)[1]
# Read the XML
parliamentarians <- read_xml(parfile)

# Looking for entries in politicians list without party and use corresponding entry in parliamentarians dataset
for (i in seq_along(main_politicians)){
  pol = main_politicians[[i]]
  if (is.na(pol$Party)){
    extracted_party <- parliamentarians %>%
      html_element(xpath=str_c("//MDB[descendant::VORNAME[text()='",pol$"First Name","'] and descendant::NACHNAME[text()='",pol$"Last Name","']]/BIOGRAFISCHE_ANGABEN/PARTEI_KURZ")) %>%
      html_text()
    if(!is.na(extracted_party)){
    main_politicians[[i]]$Party <- if(extracted_party == "CDU" | extracted_party == "CSU") "CDU/CSU" else extracted_party
    }
  }
}

## Omitting unusable data
politicians_without_party <- to_list(for(i in seq_along(main_politicians)) if(is.na(main_politicians[[i]]$Party) | main_politicians[[i]]$Party == "fraktionslos") names(main_politicians)[[i]]) %>%
  unlist()
main_politicians <- main_politicians[names(main_politicians) %!in% politicians_without_party]
main_speeches <- main_speeches[names(main_speeches) %!in% politicians_without_party]

## Adding parties in brackets to names
names(main_politicians) <- names(main_speeches) <- politicians_names <- to_list(for (i in names(main_politicians)) str_c(i," (",main_politicians[[i]]$Party,")"))

## Final preparation

main_speeches_processed <- main_speeches %>%
  lapply(str_c,collapse=" ") %>%
  VectorSource() %>%
  VCorpus() %>%
  process_corpus()

### Computation of TF-IDF scores

main_tfidf_speeches <- main_speeches_processed %>%
  DocumentTermMatrix(control=list(weighting=weightTfIdf))
main_tfidf_speeches$dimnames$Docs <- politicians_names

#### III. Compare Manifestos and Speeches------------------------------------------------

### Preparation

## Extract terms that exist both in manifestos and in speeches
keep_terms <- main_tfidf_manifesto$dimnames$Terms[main_tfidf_manifesto$dimnames$Terms %in% main_tfidf_speeches$dimnames$Terms]

## Use the extracted terms to subset the TF-IDF matrices
matr_tfidf_manifesto <- as.matrix(main_tfidf_manifesto)[,keep_terms]
matr_tfidf_manifesto <- matr_tfidf_manifesto[,order(colnames(matr_tfidf_manifesto))] # Sort columns alphabetically
matr_tfidf_speeches <- as.matrix(main_tfidf_speeches)[,keep_terms]
matr_tfidf_speeches <- matr_tfidf_speeches[,order(colnames(matr_tfidf_speeches))] # Sort columns alphabetically

### Analysis

## Compute correlations
main_cor <- sim2(matr_tfidf_speeches,matr_tfidf_manifesto,method="cosine")
party_members <- to_list(for (i in main_politicians) i$Party) # List with the politicians' party memberships

## Compute further measures of interest

# Maximum and minimum value
main_cor_max <- max(main_cor)
main_cor_min <- min(main_cor)

# Average correlation of respective party members with all parties
main_cor_avg_perparty <- to_vec(
  for (i in colnames(main_cor)) c(
    colMeans(na.omit(main_cor[party_members == i,])),
    mean(na.omit(main_cor[party_members==i,colnames(main_cor)!=i]))
    )
  ) %>%
  matrix(ncol(main_cor),byrow=TRUE,dimnames=list(lapply(colnames(main_cor),str_c," members"),c(colnames(main_cor),"Avg. with other parties")))

# Correlation per politician with own party..
main_cor_ownparty <- to_vec(for (i in 1:nrow(main_cor)) as.double(main_cor[i,][main_politicians[[i]]$Party])) %>%
  matrix(length(main_politicians),dimnames=list(politicians_names,"Own party correlation"))
# .. sorted descendantly
main_cor_ownparty_sorted <- main_cor_ownparty[order(main_cor_ownparty[,1],decreasing=TRUE),] %>% 
  as.matrix()
# Average correlation of all politicians with their own party
main_cor_avg_ownparty <- main_cor_ownparty %>%
  mean(na.rm=TRUE)

# Correlation per politician with other parties..
main_cor_otherparties <- to_vec(for (i in 1:nrow(main_cor)) mean(as.vector(main_cor[i,colnames(main_cor)!=i]))) %>%
  matrix(length(main_politicians),dimnames=list(politicians_names,"Average of correlation with other parties"))
# .. sorted descendantly
main_cor_otherparties_sorted <- main_cor_otherparties[order(main_cor_otherparties[,1],decreasing=TRUE),] %>% 
  as.matrix()
# Average correlation of all politicians with other parties
main_cor_avg_otherparties <- main_cor_otherparties %>%
  mean(na.rm=TRUE)