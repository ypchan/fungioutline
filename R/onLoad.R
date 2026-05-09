.onLoad <- function(libname, pkgname) {
    extdata_path <- system.file("extdata", package = pkgname)
    has_extdata_path <- !is.null(extdata_path) &&
        length(extdata_path) == 1L &&
        !is.na(extdata_path) &&
        nzchar(extdata_path) &&
        dir.exists(extdata_path)

    if (isTRUE(has_extdata_path)) {
        options(fungioutline.extdata_path = extdata_path)
    } else {
        options(fungioutline.extdata_path = NULL)
    }

    if (is.null(getOption("fungioutline.cache_path"))) {
        options(fungioutline.cache_path = file.path(tempdir(), pkgname))
    }

    invisible()
}
