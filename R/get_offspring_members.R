#' get_offsprint_member
#'
#' @param group_label (character) A string, it can be name of genus, family, order, subclass, class, etc.
#' @param deepth (integer) A integer  
#'
#' @return (character) A table of taxonomic scheme of the corresponding groups
#' @export
#'
#' @examples
get_offspring_members <- function(group_label, deepth=0) {
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
    
    # get offspring tbl
    if ( label_level == "family" ) {
        offspring_tbl <- outline %>% filter(Current_family == group_label) %>% 
            select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family, Current_genus)
        
    } else if ( label_level == "order") {
        if (deepth == 0 ) { 
            offspring_tbl <- outline %>% filter(Current_order == group_label) %>% 
            select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family, Current_genus)
            } else if (deepth == 1) {
                offspring_tbl <- outline %>% filter(Current_order == group_label) %>% 
                    select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family) %>% 
                    distinct(Current_family)
            } else {
                warning(paste0("Warning: deepth must <= 1 for the order : ",group_label))
        }
        
    } else if ( label_level == "subclass" ) {
        if (deepth == 0 ) { 
            offspring_tbl <- outline %>% filter(Current_subclass == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family, Current_genus)
        } else if (deepth == 1) {
            offspring_tbl <- outline %>% filter(Current_subclass == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current) %>% 
                distinct(Order_current)
        }  else if (deepth == 2) {
            offspring_tbl <- outline %>% filter(Current_subclass == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family) %>% 
                distinct(Current_family) 
        } else {
            warning(paste0("Warning: deepth must <= 1 for the subclass : ",group_label))
        }
    } else if ( label_level == "class" ) {
        if (deepth == 0 ) { 
            offspring_tbl <- outline %>% filter(Current_class == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family, Current_genus)
        } else if (deepth == 1) {
            offspring_tbl <- outline %>% filter(Current_class == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current) %>% 
                distinct(Subclass_current)
        }  else if (deepth == 2) {
            offspring_tbl <- outline %>% filter(Current_class == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current) %>% 
                distinct(Order_current)
        }  else if (deepth == 3) {
            offspring_tbl <- outline %>% filter(Current_class == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family) %>% 
                distinct(Current_family) 
        } else {
            warning(paste0("Warning: deepth must <= 3 for the subclass : ",group_label))
        }
    } else if ( label_level == "subphylum" ) {
        if (deepth == 0 ) { 
            offspring_tbl <- outline %>% filter(Current_subphylum == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family, Current_genus)
        } else if (deepth == 1) {
            offspring_tbl <- outline %>% filter(Current_subphylum == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class) %>% 
                distinct(Class)
        } else if (deepth == 2) {
            offspring_tbl <- outline %>% filter(Current_subphylum == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current) %>% 
                distinct(Subclass_current)
        }  else if (deepth == 3) {
            offspring_tbl <- outline %>% filter(Current_subphylum == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current) %>% 
                distinct(Order_current)
        }  else if (deepth == 4) {
            offspring_tbl <- outline %>% filter(Current_subphylum == group_label) %>% 
                select(Kingdom, Phylum, Subphylum, Class, Subclass_current, Order_current, Current_family) %>% 
                distinct(Current_family) 
        } else {
            warning(paste0("Warning: deepth must <= 4 for the subclass : ",group_label))
        }
    } else {
        warning(paste0("Warning: unknown group label: ",group_label))
    }
    if (offspring_tbl) {
        return(offspring_tbl) 
    } else {
        print(paste0("Error: something wrong for the input: ",group_label ))
    }
}
