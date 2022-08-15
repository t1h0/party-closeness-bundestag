library(comprehenr)
library(NLP)
library(tm)
library(text2vec)
library(magrittr)
library(stringr)

analysis <- function(manifestos_tfidf, bundestag_tfidf, bundestag_df) {
  ### Preparation
  
  ## Extract terms that exist both in manifestos and in speeches
  keep_terms <-
    manifestos_tfidf$dimnames$Terms[manifestos_tfidf$dimnames$Terms %in% bundestag_tfidf$dimnames$Terms]
  
  ## Use the extracted terms to subset the TF-IDF matrices
  matr_tfidf_manifesto <-
    as.matrix(manifestos_tfidf)[, keep_terms]
  matr_tfidf_manifesto <-
    matr_tfidf_manifesto[, order(colnames(matr_tfidf_manifesto))] # Sort columns alphabetically
  matr_tfidf_bundestag <- as.matrix(bundestag_tfidf)[, keep_terms]
  matr_tfidf_bundestag <-
    matr_tfidf_bundestag[, order(colnames(matr_tfidf_bundestag))] # Sort columns alphabetically
  
  ### Analysis
  
  ## Compute correlations
  main_cor <-
    sim2(matr_tfidf_bundestag, matr_tfidf_manifesto, method = "cosine")
  party_members <-
    to_list(for (i in 1:nrow(bundestag_df))
      bundestag_df[i,]$party) # List with the politicians' party memberships
  
  ## Compute further measures of interest
  
  # Maximum and minimum value
  main_cor_max <- max(main_cor)
  main_cor_min <- min(main_cor)
  
  # Average correlation of party members with all parties
  main_cor_avg_perparty <- to_vec(for (i in colnames(main_cor))
    c(colMeans(na.omit(main_cor[party_members == i, ])),
      mean(na.omit(main_cor[party_members == i, colnames(main_cor) != i])))) %>%
    matrix(ncol(main_cor),
           byrow = TRUE,
           dimnames = list(
             lapply(colnames(main_cor), str_c, " members"),
             c(colnames(main_cor), "Avg. with other parties")
           ))
  
  # Correlation per politician with own party..
  main_cor_ownparty <-
    to_vec(for (i in 1:nrow(main_cor))
      as.double(main_cor[i, ][bundestag_df[i,]$party])) %>%
    matrix(nrow(bundestag_df),
           dimnames = list(bundestag_tfidf$dimnames$Docs, "Own party correlation"))
  # .. sorted descendantly
  main_cor_ownparty_sorted <-
    main_cor_ownparty[order(main_cor_ownparty[, 1], decreasing = TRUE), ] %>%
    as.matrix()
  # Average correlation of all politicians with their own party
  main_cor_avg_ownparty <- main_cor_ownparty %>%
    mean(na.rm = TRUE)
  
  # Correlation per politician with other parties..
  main_cor_otherparties <-
    to_vec(for (i in 1:nrow(main_cor))
      mean(as.vector(main_cor[i, colnames(main_cor) != i]))) %>%
    matrix(
      nrow(bundestag_df),
      dimnames = list(bundestag_tfidf$dimnames$Docs, "Average of correlation with other parties")
    )
  # .. sorted descendantly
  main_cor_otherparties_sorted <-
    main_cor_otherparties[order(main_cor_otherparties[, 1], decreasing = TRUE), ] %>%
    as.matrix()
  # Average correlation of all politicians with other parties
  main_cor_avg_otherparties <- main_cor_otherparties %>%
    mean(na.rm = TRUE)
}