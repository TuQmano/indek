DBS_PAGE_URL <- "https://www.indec.gob.ar/bases-de-datos.asp"
DBS_BASE_URL <- "https://www.indec.gob.ar"
DEFAULT_CACHE_DIR <- "~/.indek.cache"

#' The function 'indec.dbs' is for fetching databases in the
#' indec page: https://www.indec.gob.ar/bases-de-datos.asp
#' @examples
#' k <- indek.dbs()
#' eph <- find_hrefs(k, "EPH_usu_2_Trim_2018_txt.zip")[1]
#' x <- read_tables_zip(eph,1)[[1]]
#' summary(subset(x, PP08D1 != -9)$PP08D1)
#' @export
indec.dbs <- function(url = DBS_PAGE_URL,
                      baseurl = DBS_BASE_URL,
                      cachedir = DEFAULT_CACHE_DIR) {
    page <- indek.page(url, baseurl, cachedir)
    page
}
