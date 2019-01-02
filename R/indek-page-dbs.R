DBS_PAGE_URL <- "https://www.indec.gob.ar/bases-de-datos.asp"
DBS_BASE_URL <- "https://www.indec.gob.ar"
DEFAULT_CACHE_DIR <- "~/.indek.cache"

indek.dbs <- function(url = DBS_PAGE_URL,
                      baseurl = DBS_BASE_URL,
                      cachedir = DEFAULT_CACHE_DIR) {
    page <- indek.page(url, baseurl, cachedir)
    page
}
