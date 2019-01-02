indek.page <- function(url, baseurl, cachedir) {
    if (class(url) != "character") {
        stop("indek.page: url parameter must be character")
    }
    page <- structure(
        list(url = indek.url(url),
             baseurl = indek.url(baseurl),
             cache = indek.cache.manager(cachedir)),
        class = "indek.page")
    page$hrefs <- extract_hrefs(page)
    page
}

extract_hrefs0 <- function(o) UseMethod("extract_hrefs0")
extract_hrefs0.indek.page <- function(page) {
    html <- get_url(page$cache, page$url)
    links <- xml2::xml_find_all(html, "//a")
    urls <- sapply(xml2::xml_find_all(links, "@href"), xml2::xml_text)
    lapply(unique(trimws(urls)), indek.url)
}

extract_hrefs <- function(o) UseMethod("extract_hrefs")
extract_hrefs.indek.page <- function(page) {
    html <- get_url(page$cache, page$url)
    links <- xml2::xml_find_all(html, "//a[@href]")
    urls <- sapply(links, function(x) { xml2::xml_attr(x, "href") })
    texts <- sapply(links, function(x) { trimws(xml2::xml_text(x)) })
    indek.href.set(
        urls=sapply(links, function(x) { xml2::xml_attr(x, "href") }),
        texts=sapply(links, function(x) { trimws(xml2::xml_text(x)) }),
        baseurl = page$baseurl,
        cache = page$cache)
}


find_hrefs <- function(page, regex) UseMethod("find_hrefs")
find_hrefs.indek.page <- function(page, regexvec) {
    refs <- page$hrefs

    for (re in regexvec) {
        matches <-  grepl(re, refs$urls)
        matches <- matches | grepl(re, refs$texts)
        refs <- indek.href.set(urls = refs$urls[matches],
                               texts = refs$texts[matches],
                               baseurl = page$baseurl,
                               cache = page$cache)
    }
    refs
}

find_eph <- function(page, regex) UseMethod("find_eph")
find_eph.indek.page <- function(page, regexvec) {
    regexvec <-  c("eph/", regexvec)
    find_hrefs(page, regexvec)
}

## todo: add logical force parameter to force download
download_file.indek.page <- function(page, url) {
    if (class(url) != "indek.href") {
        stop("download_file: url param must be indek.href")
    }
    dest <- as.local.path(url$url, page$cache)
    download_file(page$cache,
                  paste(page$baseurl, url$url, sep = "/"),
                  dest)

}


unzip_list <- function(page, url) UseMethod("unzip_list")
unzip_list.indek.page <- function(page, url) {
    if (class(url) != "indek.href") {
        stop("download_file: url param must be indek.href")
    }
    path <- as.local.path(url$url, page$cache)
    if (!file.exists(path)) {
        download_file(page, url)
    }
    unzip_file(path)
}

as.urltree <- function(page) UseMethod("as.urltree")
as.urltree.indek.page  <- function() { indek.url.tree(page$hrefs$urls) }
