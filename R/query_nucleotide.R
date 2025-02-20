query_nucleotide <- function(searching_terms_lst) {
    # Initialize an empty data frame to store the search terms and their count results
    result_tbl <- data.frame(
        search_term = character(),
        result_count = integer(),
        stringsAsFactors = FALSE
    )
    
    # Set the number of iterations
    count_terms <- length(searching_terms_lst)
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = count_terms, # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         width = 50,   # Progress bar width. Defaults to getOption("width")
                         char = "=")   # Character used to create the bar
    
    # Loop through the list of search terms and query each term in the nucleotide database
    for (i in seq_along(searching_terms_lst)) {
        term <- searching_terms_lst[i]
        #cat(term, "\n")
        searching_term <- paste0('("', term, '"[Organism] OR ', 
                                 term, '[All Fields]) AND (fungi[filter] AND biomol_genomic[PROP] AND is_nuccore[filter] AND ("150"[SLEN] : "2000"[SLEN]))')
        #cat(searching_term,"\n")
        search_results <- entrez_search(db="nucleotide", term=searching_term, retmax=10)
        # Add the search term and result count to the result table
        result_tbl <- rbind(result_tbl, data.frame(search_term = term, result_count = search_results$count, stringsAsFactors = FALSE))
        
        # Update the progress bar
        setTxtProgressBar(pb, i)
    } 
    close(pb)
    return(result_tbl)
}