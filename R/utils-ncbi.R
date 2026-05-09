fo_ncbi_result_columns <- function() {
    c(
        "input_taxon",
        "accepted_name",
        "rank",
        "match_type",
        "db",
        "query",
        "count",
        "ids",
        "has_sequences",
        "cached",
        "query_time",
        "error"
    )
}

fo_ncbi_count_columns <- function() {
    c(
        "input_taxon",
        "accepted_name",
        "rank",
        "match_type",
        "db",
        "query",
        "count",
        "has_sequences",
        "cached",
        "query_time",
        "error"
    )
}

fo_empty_ncbi_result <- function() {
    tibble::tibble(
        input_taxon = character(),
        accepted_name = character(),
        rank = character(),
        match_type = character(),
        db = character(),
        query = character(),
        count = integer(),
        ids = list(),
        has_sequences = logical(),
        cached = logical(),
        query_time = character(),
        error = character()
    )
}

fo_ncbi_cache_dir <- function(cache_dir = NULL) {
    if (is.null(cache_dir)) {
        cache_dir <- file.path(getOption("fungioutline.cache_path", file.path(tempdir(), "fungioutline")), "ncbi")
    }

    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir) || !nzchar(cache_dir)) {
        rlang::abort("`cache_dir` must be a single non-empty directory path.")
    }

    cache_dir
}

fo_hash_string <- function(x) {
    x <- paste(x, collapse = "\n")
    ints <- utf8ToInt(enc2utf8(x))
    hash <- 0

    for (value in ints) {
        hash <- (hash * 33 + value) %% 2147483647
    }

    sprintf("%08x", as.integer(hash))
}

fo_ncbi_cache_key <- function(db, query, retmax) {
    paste(
        "ncbi",
        stringr::str_to_lower(db),
        retmax,
        fo_hash_string(query),
        sep = "_"
    )
}

fo_ncbi_cache_path <- function(cache_key, cache_dir = NULL) {
    cache_dir <- fo_ncbi_cache_dir(cache_dir)
    safe_key <- stringr::str_replace_all(cache_key, "[^A-Za-z0-9_.-]+", "_")
    fs::path(cache_dir, paste0(safe_key, ".rds"))
}

fo_ncbi_query_time <- function(time = Sys.time()) {
    format(as.POSIXct(time), "%Y-%m-%d %H:%M:%S %Z")
}

fo_ncbi_quote <- function(x) {
    x <- stringr::str_replace_all(x, '"', '\\"')
    paste0('"', x, '"')
}

fo_build_ncbi_query <- function(taxon_name, rank = NA_character_, db = "nuccore", exact = TRUE, extra_query = NULL) {
    if (!is.character(taxon_name) || length(taxon_name) != 1L || is.na(taxon_name) || !nzchar(stringr::str_squish(taxon_name))) {
        rlang::abort("`taxon_name` must be a single non-empty taxon name.")
    }

    if (!is.character(db) || length(db) != 1L || is.na(db) || !nzchar(db)) {
        rlang::abort("`db` must be a single non-empty NCBI database name.")
    }

    check_logical_scalar(exact, "exact")

    taxon_name <- stringr::str_squish(taxon_name)
    organism_query <- if (isTRUE(exact)) {
        paste0(fo_ncbi_quote(taxon_name), "[Organism]")
    } else {
        paste0(taxon_name, "[Organism]")
    }

    query <- organism_query

    if (!is.null(extra_query)) {
        if (!is.character(extra_query) || length(extra_query) != 1L || is.na(extra_query) || !nzchar(extra_query)) {
            rlang::abort("`extra_query` must be NULL or a single non-empty character string.")
        }

        query <- paste0("(", query, ") AND (", extra_query, ")")
    }

    query
}

fo_prepare_ncbi_taxa <- function(taxon, outline = NULL, taxon_index = NULL, match_synonym = TRUE) {
    if (is.null(outline) && is.null(taxon_index)) {
        return(tibble::tibble(
            input_taxon = taxon,
            accepted_name = taxon,
            rank = NA_character_,
            match_type = "raw"
        ))
    }

    get_lineage(
        taxon = taxon,
        outline = outline,
        taxon_index = taxon_index,
        match_synonym = match_synonym,
        best_match = TRUE,
        warn_ambiguous = FALSE
    ) |>
        dplyr::transmute(
            input_taxon = .data$input_taxon,
            accepted_name = dplyr::coalesce(.data$accepted_name, .data$input_taxon),
            rank = .data$rank,
            match_type = .data$match_type
        )
}

fo_normalize_ncbi_response <- function(response) {
    if (inherits(response, "try-error")) {
        return(list(
            count = NA_integer_,
            ids = character(),
            error = as.character(response)
        ))
    }

    if (inherits(response, "error")) {
        return(list(
            count = NA_integer_,
            ids = character(),
            error = conditionMessage(response)
        ))
    }

    if (is.data.frame(response)) {
        response <- tibble::as_tibble(response)
        count <- if ("count" %in% names(response) && nrow(response) > 0L) as.integer(response$count[[1]]) else 0L
        ids <- if ("ids" %in% names(response) && nrow(response) > 0L) response$ids[[1]] else character()
        error <- if ("error" %in% names(response) && nrow(response) > 0L) as.character(response$error[[1]]) else NA_character_
        return(list(count = count, ids = as.character(unlist(ids, use.names = FALSE)), error = error))
    }

    if (is.list(response)) {
        count <- if (!is.null(response$count)) as.integer(response$count[[1]]) else 0L
        ids <- if (!is.null(response$ids)) as.character(unlist(response$ids, use.names = FALSE)) else character()
        error <- if (!is.null(response$error)) as.character(response$error[[1]]) else NA_character_
        return(list(count = count, ids = ids, error = error))
    }

    rlang::abort("NCBI search responses must be lists or data frames.")
}

fo_entrez_search_with_rentrez <- function(db, query, retmax, api_key = "") {
    args <- list(db = db, term = query, retmax = retmax)

    if (nzchar(api_key)) {
        args$api_key <- api_key
    }

    result <- tryCatch(
        do.call(rentrez::entrez_search, args),
        error = function(error) {
            if (nzchar(api_key)) {
                args$api_key <- NULL
                return(do.call(rentrez::entrez_search, args))
            }

            stop(error)
        }
    )

    list(
        count = as.integer(result$count),
        ids = as.character(result$ids),
        error = NA_character_
    )
}

fo_extract_xml_value <- function(xml, tag) {
    pattern <- paste0("<", tag, ">(.*?)</", tag, ">")
    match <- regmatches(xml, regexpr(pattern, xml, perl = TRUE))

    if (length(match) == 0L || identical(match, character())) {
        return(NA_character_)
    }

    sub(pattern, "\\1", match, perl = TRUE)
}

fo_entrez_search_with_utils <- function(db, query, retmax, api_key = "") {
    base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
    query_parts <- c(
        db = db,
        term = query,
        retmax = as.character(retmax),
        retmode = "xml"
    )

    if (nzchar(api_key)) {
        query_parts <- c(query_parts, api_key = api_key)
    }

    url <- paste0(
        base_url,
        "?",
        paste(
            paste0(names(query_parts), "=", utils::URLencode(query_parts, reserved = TRUE)),
            collapse = "&"
        )
    )

    xml <- paste(utils::readLines(url, warn = FALSE), collapse = "")
    count <- suppressWarnings(as.integer(fo_extract_xml_value(xml, "Count")))
    ids <- gregexpr("<Id>(.*?)</Id>", xml, perl = TRUE)
    ids <- regmatches(xml, ids)[[1]]
    ids <- if (length(ids) == 0L || identical(ids, character())) {
        character()
    } else {
        sub("<Id>(.*?)</Id>", "\\1", ids, perl = TRUE)
    }

    list(
        count = count,
        ids = as.character(ids),
        error = NA_character_
    )
}

fo_ncbi_search_once <- function(db, query, retmax, api_key = "", search_fun = NULL) {
    if (!is.null(search_fun)) {
        if (!is.function(search_fun)) {
            rlang::abort("`search_fun` must be NULL or a function.")
        }

        response <- tryCatch(
            search_fun(db = db, query = query, retmax = retmax, api_key = api_key),
            error = function(error) {
                error
            }
        )
        return(fo_normalize_ncbi_response(response))
    }

    response <- tryCatch(
        {
            if (requireNamespace("rentrez", quietly = TRUE)) {
                fo_entrez_search_with_rentrez(db = db, query = query, retmax = retmax, api_key = api_key)
            } else {
                fo_entrez_search_with_utils(db = db, query = query, retmax = retmax, api_key = api_key)
            }
        },
        error = function(error) {
            error
        }
    )

    fo_normalize_ncbi_response(response)
}
