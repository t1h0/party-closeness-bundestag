# setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Path for downloads
download_path <- "download/"

#### I. Processing of parties' manifestos---------------------------------------

## Manifestos' date to use
# View(mp_availability(countryname=="Germany")) # View all available manifestos for Germany
manifestos_date = "202109"

# Download manifestos and extract TF-IDF
source("manifestos_process.R")
mp_setapikey("manifesto_apikey.txt") # you can also put in the key directly
main_manifestos_tfidf <- manifestos_process(manifestos_date)

#### II. Preprocessing of politicians and their speeches------------------------

source("bundestag_process.R")
# main_bundestag <- bundestag_process(download_path)
main_bundestag <- bundestag_process(protocols_path = "download/protocols",parliamentarians_meta_path = "download/parliamentarians_meta/MDB_STAMMDATEN.XML")
main_bundestag_df <- main_bundestag[[1]]
main_bundestag_tfidf <- main_bundestag[[2]]

#### III. Compare Manifestos and Speeches------------------------------------------------
source("analysis.R")
analysis(main_manifestos_tfidf,main_bundestag_tfidf,main_bundestag_df)
