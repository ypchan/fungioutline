# Define the function to add lineage information to an Excel file
add_lineage_to_xlsx <- function(xlsx_file_path, query_column_name, sheet_name = "Sheet1", out_column_name = "Lineage") {
    # Define the columns that contain rank information
    rank_columns <- c("Phylum_current", "Subphylum_current", "Class_current", 
                      "Subclass_current", "Order_current", "Family_current", 
                      "Genus_current")
    
    # Combine the rank information into a single string for each row
    lineage_vector <- apply(outline[, rank_columns], 1, function(row) {
        row_combined <- paste(row, collapse = ";")
        row_combined <- paste0(row_combined, ";")
        return(row_combined)
    })

    # Read the workbook and the specified sheet into R
    wb <- loadWorkbook(xlsx_file_path)
    tbl <- read.xlsx(xlsx_file_path, sheet = sheet_name)
    
    # Extract the first term from the query column for each row
    searching_terms <- unique(tbl[[query_column_name]])
    
    # Set up parallel processing
    num_cores <- 4
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    # Use parallel processing to search for rank terms in the lineage vector
    result <- foreach(searching_term = searching_terms, .combine = 'c', .packages = 'base') %dopar% {
        lineage_info <- lineage_vector[grepl(paste0(searching_term,";"), lineage_vector, ignore.case = TRUE)]
        if (length(lineage_info) == 0) {
            lineage_info <- NA  # Return NA if no matches are found
            warning(paste("No lineage record found:", searching_term))
        }
        setNames(list(lineage_info), searching_term)
    }

    # stop parallel processing
    stopCluster(cl) 
    
    # Initialize the new lineage column in the table
    tbl[[out_column_name]] <- NA

    # Fill the new lineage column with the appropriate lineage information
    for (searching_term in as.vector(tbl[[query_column_name]])) {
        lineage_info <- result[[searching_term]]
        tbl[[out_column_name]][tbl[[query_column_name]] == searching_term] <- lineage_info
    }
    
    # Write the updated table back to the workbook and save it
    writeData(wb, sheet = sheet_name, x = tbl)
    saveWorkbook(wb, xlsx_file_path, overwrite = TRUE)
}