.fo_package_data_cache <- new.env(parent = emptyenv())

fo_get_package_data <- function(name) {
    cached <- get0(name, envir = .fo_package_data_cache, inherits = FALSE)

    if (!is.null(cached)) {
        return(cached)
    }

    namespace <- asNamespace("fungioutline")

    if (exists(name, envir = namespace, inherits = FALSE)) {
        value <- get(name, envir = namespace, inherits = FALSE)
    } else {
        rds_path <- system.file("extdata", paste0(name, ".rds"), package = "fungioutline")

        if (nzchar(rds_path) && file.exists(rds_path)) {
            value <- readRDS(rds_path)
        } else {
            data_env <- new.env(parent = emptyenv())
            utils::data(list = name, package = "fungioutline", envir = data_env)

            if (!exists(name, envir = data_env, inherits = FALSE)) {
                rlang::abort(glue::glue("Package dataset `{name}` was not found."))
            }

            value <- get(name, envir = data_env, inherits = FALSE)
        }
    }

    assign(name, value, envir = .fo_package_data_cache)
    value
}

fo_default_outline <- function() {
    fo_get_package_data("fungi_outline")
}

fo_default_taxon_index <- function() {
    fo_get_package_data("fungi_taxon_index")
}

fo_default_genome_metadata <- function() {
    fo_get_package_data("fgtdb_genome_metadata")
}
