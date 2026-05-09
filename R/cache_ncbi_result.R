#' Read or write a cached NCBI search result
#'
#' Stores one NCBI search result as an RDS file under a cache directory. When
#' `result` is `NULL`, the function reads from cache and returns `NULL` if no
#' cached file exists.
#'
#' @param cache_key Single cache key string.
#' @param result Optional object to write. If `NULL`, read the cached object.
#' @param cache_dir Optional cache directory. Defaults to the package cache path
#'   option under an `ncbi` subdirectory.
#' @param overwrite Logical. If `FALSE`, keep an existing cached result.
#'
#' @return The cached result object, or `NULL` when reading a missing cache key.
#' @export
#'
#' @examples
#' key <- "example_ncbi_cache_key"
#' cache_dir <- tempfile("ncbi-cache-")
#' fungioutline::cache_ncbi_result(key, list(count = 1), cache_dir = cache_dir)
#' fungioutline::cache_ncbi_result(key, cache_dir = cache_dir)
cache_ncbi_result <- function(cache_key, result = NULL, cache_dir = NULL, overwrite = TRUE) {
    if (!is.character(cache_key) || length(cache_key) != 1L || is.na(cache_key) || !nzchar(cache_key)) {
        rlang::abort("`cache_key` must be a single non-empty character string.")
    }

    check_logical_scalar(overwrite, "overwrite")

    cache_path <- fo_ncbi_cache_path(cache_key, cache_dir = cache_dir)

    if (is.null(result)) {
        if (!fs::file_exists(cache_path)) {
            return(NULL)
        }

        return(readRDS(cache_path))
    }

    fs::dir_create(fs::path_dir(cache_path))

    if (fs::file_exists(cache_path) && !isTRUE(overwrite)) {
        return(readRDS(cache_path))
    }

    saveRDS(result, cache_path)
    result
}
