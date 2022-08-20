library(comprehenr)
library(NLP)
library(tm)
library(text2vec)
library(magrittr)
library(stringr)

analysis <-
  function(manifestos_tfidf,
           bundestag_tfidf,
           bundestag_df) {
    ### Preparation
    manifestos_tfidf <- main_manifestos_tfidf
    bundestag_tfidf <- main_bundestag_tfidf
    bundestag_df <- main_bundestag_df
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
    cor <-
      sim2(matr_tfidf_bundestag, matr_tfidf_manifesto, method = "cosine")
    party_members <-
      to_list(for (i in 1:nrow(bundestag_df))
        bundestag_df[i, ]$party) # List with the politicians' party memberships
    
    ## Compute further measures of interest
    
    # Maximum and minimum value
    cor_max <- max(cor)
    cor_min <- min(cor)
    
    # Average correlation of party members with all parties
    cor_avg_perparty <- to_vec(for (i in colnames(cor))
      c(colMeans(na.omit(cor[party_members == i,])),
        mean(na.omit(cor[party_members == i, colnames(cor) != i])))) %>%
      matrix(ncol(cor),
             byrow = TRUE,
             dimnames = list(
               lapply(colnames(cor), str_c, " members"),
               c(colnames(cor), "Avg. with other parties")
             ))
    
    # Correlation per politician with own party..
    cor_ownparty <-
      to_vec(for (i in 1:nrow(cor))
        as.double(cor[i,][bundestag_df[i, ]$party])) %>%
      matrix(
        nrow(bundestag_df),
        dimnames = list(bundestag_tfidf$dimnames$Docs, "Own party correlation")
      )
    # .. sorted descendantly
    cor_ownparty_sorted <-
      cor_ownparty[order(cor_ownparty[, 1], decreasing = TRUE),] %>%
      as.matrix()
    # Average correlation of all politicians with their own party
    cor_avg_ownparty <- cor_ownparty %>%
      mean(na.rm = TRUE)
    
    # Correlation per politician with other parties..
    cor_otherparties <-
      to_vec(for (i in 1:nrow(cor))
        mean(as.vector(cor[i, colnames(cor) != i]))) %>%
      matrix(
        nrow(bundestag_df),
        dimnames = list(
          bundestag_tfidf$dimnames$Docs,
          "Average of correlation with other parties"
        )
      )
    # .. sorted descendantly
    cor_otherparties_sorted <-
      cor_otherparties[order(cor_otherparties[, 1], decreasing = TRUE),] %>%
      as.matrix()
    # Average correlation of all politicians with other parties
    cor_avg_otherparties <- cor_otherparties %>%
      mean(na.rm = TRUE)
    
    return(
      list(
        cor,
        cor_max,
        cor_min,
        cor_avg_perparty,
        cor_ownparty,
        cor_ownparty_sorted,
        cor_avg_ownparty,
      )
    )
  }