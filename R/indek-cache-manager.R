indek.cache.manager <- function(cachedir)
{
    cache <- structure(
        list(dirname = cachedir),
        class = "indek.cache.manager")
    if (!file.exists(cache$dirname)) { dir.create(cache$dirname) }
    cache
}


get_url <- function(cache, url) UseMethod("get_url")
get_url.indek.cache.manager <- function(cache, url) {
    
    dest <- as.local.path(url, cache)
    if (!file.exists(dest)) {
        if (!file.exists(cache$dirname)) { dir.create(cache$dirname) }
        cat("downloading ", url, "\nto ", dest, "\n")
        xml2::download_html(url, dest)
    }
    ## todo: fix this and use generic read_htmlinstead
    xml2::read_html(as.character(dest))
}

download_file <- function(cache, url, filename) {
    UseMethod("download_file")
}
download_file.indek.cache.manager <- function(cache, url, dest)
{
    if (class(url) != "character") {
        stop("download_file: url param must be character")
    }

    if (!file.exists(dest)) {
        if (!file.exists(cache$dirname)) { dir.create(cache$dirname) }
        cat("downloading ", url, "\nto ", dest, "\n")
        ## todo: fix this and use generic read_htmlinstead
        xml2::download_html(url, as.character(dest))
    }
    as.character(dest)
}
