library(RSelenium)
library(xml2)
library(magrittr)
library(robotstxt)
library(stringr)
library(rvest)
library(comprehenr)
library(NLP)
library(tm)

bundestag_download <- function(download_path){
  ### Web scraping politicians and speeches of current legislative period
  
  ## Local Preparation
  protocols_path <- file.path(download_path,"protocols/")
  if (!dir.exists(protocols_path)) {
    protocols_path %>%
      dir.create(recursive = TRUE)
  }
  
  ## URL Preparation
  base_url <- "https://www.bundestag.de"
  data_url <- "https://www.bundestag.de/services/opendata"
  
  ## Being Nice
  data_url_parsed <- xml2::url_parse(data_url)
  robots <- data_url_parsed$server %>%
    robotstxt()
  data_url_parsed$path %>%
    robots$check() %>%
    if (!.)
      stop('Server has not given a crawl permission!')
  crawl_delay <- robots$crawl_delay
  
  ## Start selenium server
  server <- rsDriver(verbose = TRUE, browser = "firefox")
  client <- server$client
  client$navigate(data_url)
  
  ## Get politicians' meta info link (for later)
  stammdaten_link <-
    client$findElement("xpath","//a[@class='bt-link-dokument' and starts-with(@title,'Stammdaten')]")$getElementAttribute("href")[[1]]
  
  ## Clicking through the dynamic content to load the XML links of the current legislature
  term_xpath <- "//section[contains(@class,'bt-module-row-dokumente')][1]"
  term = client$findElement("xpath",term_xpath)
  slider = term$findChildElement("xpath",".//div[contains(@class,'bt-slider')]")
  while(length((currentSlideCount = strtoi(slider$getElementAttribute("data-currentslidecount")))) == 0) {}
  maxClick = round(strtoi(slider$getElementAttribute("data-allitemcount")) / currentSlideCount) - 1
  for(i in 1:maxClick){
    while (length(term$findChildElements("xpath",".//button[@class='slick-next slick-arrow slick-disabled']")) != 0) {}
    # Stopping if end is reached, i.e. if the 'next' button is deactivated
    nextbtn <-
      term$findChildElement("xpath",".//button[@class='slick-next slick-arrow']")
    while(strtoi(slider$getElementAttribute("data-currentslidecount")) == currentSlideCount){
      nextbtn$clickElement()
    }
    currentSlideCount = strtoi(slider$getElementAttribute("data-currentslidecount"))
  }
  
  ## Extracting the XML Links
  xml_links <- client$getPageSource() %>%
    unlist() %>%
    read_html() %>%
    html_elements(xpath = str_c(
      term_xpath,
      "//a[@class='bt-link-dokument' and starts-with(@title,'XML')]"
    )) %>%
    xml_attrs()
  
  ## Stopping the server
  server[["server"]]$stop()
  
  ## Download protocols
  for (i in xml_links) {
    url_absolute(i[["href"]],base_url) %>%
      robots$check() %>%
      if (!.) {
        stop('Server has not given crawl permission!')
      }
    download_xml(url_absolute(i[["href"]],base_url), file.path(protocols_path, basename(i[["href"]])))
  }
  
  ## Download zip containing the XML with parliamentarians meta data
  parliamentarians_path <- file.path(download_path,"parliamentarians_meta/")
  if (!dir.exists(parliamentarians_path)) {
    parliamentarians_path %>%
      dir.create(recursive = TRUE)
  }
  download.file(stammdaten_link,
                file.path(parliamentarians_path, "parliamentarians_meta.zip"))
  # Unzip it
  unzip(
    file.path(parliamentarians_path, "parliamentarians_meta.zip"),
    exdir = parliamentarians_path
  )
  # Find the local XML path
  parfile <- list.files(
    parliamentarians_path,
    pattern = "\\.[xml|XML]",
    full.names = TRUE
  )[1]
  # Read and return the XML
  return(c(protocols_path,parfile))
}

bundestag_extract <- function(protocols_path,parliamentarians_meta_path){
  
  ### Importing parliamentarians meta info
  
  par_meta <- read_xml(parliamentarians_meta_path)
  
  ### Extraction of speeches
  ## Paths to XMLs
  files <- list.files(protocols_path,
                      pattern = "\\.[xml|XML]", full.names = TRUE)
  
  df_return <-
    data.frame(matrix(ncol = 5, nrow = 0)) # Will contain the speeches and politicians
  colnames(df_return) <- c("id","surname","forename","party","speeches")

  message("Processing XMLs",appendLF=FALSE)
  for (f in seq_along(files)) {
    message(str_c("..",f,"/",length(files)),appendLF=FALSE)
    # Reading the file
    file <- read_xml(files[[f]])
    # Finding all topic sections
    tops <- file %>%
      html_elements(xpath = "//tagesordnungspunkt[descendant::rede]")
    
    for (top in tops) {
      # All speeches in that topic:
      speeches <- top %>%
        html_elements(xpath = "./rede")
      
      for (speech in speeches) {
        # Unique speaker id
        id <-
          speech %>%
          html_element(xpath = ".//redner/@id") %>%
          html_text()  %>%
          str_replace_all("[^[:alnum:]]", "")
        
        # Add politician to dataset if not already existing
        if(sum(df_return$id == id) == 0){
          # Extract parliament meta data about speaker
          mdb <- par_meta %>%
            html_element(xpath = str_c("//MDB[child::ID[text()='", id, "']]"))
          if(is.na(mdb)) next # skip if meta not provided
          surname <- mdb %>%
            html_element(xpath = str_c("./NAMEN/NAME/NACHNAME")) %>%
            html_text()
          forename <- mdb %>%
            html_element(xpath = str_c("./NAMEN/NAME/VORNAME")) %>%
            html_text()
          party <- mdb %>%
            html_element(xpath = str_c("./BIOGRAFISCHE_ANGABEN/PARTEI_KURZ")) %>%
            html_text() %>%
            ifelse (. == "CDU" | . == "CSU", "CDU/CSU", .)
          if ("" %in% c(surname, forename, party))
            next # skip if meta incomplete
          df_return[nrow(df_return)+1,] <- c(id,surname,forename,party,"")
        }
        
        # Extract actual speech text to the speech of the respective speaker in dataset
        df_return[df_return$id == id, ]$speeches <-
          speech %>%
          html_elements(
            xpath = str_c(
              "./p[@klasse!='redner' and preceding-sibling::*[self::name or self::p[@klasse='redner']][1][child::redner[@id='",
              id,
              "']]]"
            )
          ) %>%
          html_text() %>%
          str_c(collapse = " ") %>%
          str_c(df_return[df_return$id == id, ]$speeches, ., collapse = " ")
      }
    }
  }
  writeLines("\n")
  # sorting
  df_return <- df_return[order(df_return$surname),]
  
  return(df_return)

}

bundestag_get_tfidf <- function(d){
  stopwords <-
    read.table(
      "https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt"
    )$V1
  tfidf <- d$speeches %>%
    VectorSource() %>%
    VCorpus() %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(str_replace_all), "\\W", " ") %>% # Remove non-word characters in between words
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument, "german") %>%
    DocumentTermMatrix(control = list(weighting = weightTfIdf))
  tfidf$dimnames$Docs <- to_list(for (i in 1:nrow(d)) str_c(d[i,]$surname,", ",d[i,]$forename))
  return(tfidf)
}

bundestag_process <- function(download_path = NULL,protocols_path=NULL,parliamentarians_meta_path=NULL){
  if(is.null(download_path)) bd <- c(protocols_path,parliamentarians_meta_path)
  else bd <- bundestag_download(download_path)
  bundestag_df <- bundestag_extract(bd[[1]],bd[[2]])
  bundestag_tfidf <- bundestag_get_tfidf(bundestag_df)
  return(list(bundestag_df,bundestag_tfidf))
}
