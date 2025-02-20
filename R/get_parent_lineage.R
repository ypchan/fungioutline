#' get_parent_lineage
#'
#' @param group_label 
#' @param deepth 
#'
#' @return
#' @export
#'
#' @examples
get_parent_lineage <- function(group_label, deepth=0) {
    # load outline table
    data(outline)
    # remove useless white spaces in string
    group_label <- trimws(group_label)
    
    # check lable level
    label_level <- "genus"
    if ( grepl(group_label, "aceae$") ) {
        label_level <- "family" 
    }
    if ( grepl(group_label, "ales$") ) {
        label_level <- "order" 
    }
    if ( grepl(group_label, "mycetidae$") ) {
        label_level <- "subclass" 
    }
    if ( grepl(group_label, "mycetes$") ) {
        label_level <- "class" 
    }
    if ( grepl(group_label, "mycotina$") ) {
        label_level <- "subphylum" 
    }
    if ( grepl(group_label, "mycota$") ) {
        label_level <- "phylum" 
    }
    
    # get lineage tbl
    if ( label_level == "genus" ) {
        lineage_info <- outline %>% filter(Genus_current == group_label) %>% 
            distinct(Genus_current, .keep_all = TRUE) %>%
            select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Family_current) %>%
            mutate(lineage=paste(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Family_current, sep = ", ")) %>%
            select(lineage) %>% pull()
    }
    if ( label_level == "family" ) {
        lineage_info <- outline %>% filter(Family_current == group_label) %>% 
            distinct(Family_current, .keep_all = TRUE) %>%
            mutate(lineage=paste(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, sep = ", ")) %>%
            select(lineage) %>% pull()
    }
    if ( label_level == "order" ) {
        lineage_info <- outline %>% filter(Order_current == group_label) %>% 
            distinct(Order_current, .keep_all = TRUE) %>%
            mutate(lineage=paste(Kingdom, Phylum, Subphylum, Class, Subclass_current, sep = ", ")) %>%
            select(lineage) %>% pull()
    }
    if ( label_level == "subclass" ) {
        lineage_info <- outline %>% filter(Subclass_current == group_label) %>% 
            distinct(Subclass_current, .keep_all = TRUE) %>%
            mutate(lineage=paste(Kingdom, Phylum, Subphylum, Class, sep = ", ")) %>%
            select(lineage) %>% pull()
    }
    if ( label_level == "class" ) {
        lineage_info <- outline %>% filter(Class_current == group_label) %>% 
            distinct(Class_current, .keep_all = TRUE) %>%  
            mutate(lineage=paste(Kingdom, Phylum, Subphylum, sep = ", ")) %>%
            select(lineage) %>% pull()
           
    }
    if ( label_level == "subphylum" ) {
        lineage_info <- outline %>% filter(Subphylum_current == group_label) %>% 
            distinct(Subphylum_current, .keep_all = TRUE) %>%  
            mutate(lineage=paste(Kingdom, Phylum, sep = ", ")) %>%
            select(lineage) %>% pull()
            
    }
    if ( label_level == "phylum" ) {
        lineage_info <- outline %>% filter(Phylum_current == group_label) %>% 
            distinct(Phylum_current, .keep_all = TRUE) %>%
            mutate(lineage=paste(Kingdom, Phylum, sep = ", ")) %>%
            select(lineage) %>% pull()
    }
    
    # output
    if ( !is.null(lineage_info) ) {
        return(lineage_info)
    }
    else {
        warning(paste0("Warning: unknown group label: ",group_label))
        return("NA")
    }
    }
