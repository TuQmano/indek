##' indec databases webpage url
indek.db.url <- "https://www.indec.gob.ar/bases-de-datos.asp"

##'       x: A string, a connection, or a raw vector.
html_get_links <- function(x) {
    html <- xml2::read_html(x)
    links <- xml2::xml_find_all(html, "//a")
    urls <- sapply(xml2::xml_find_all(links, "@href"), xml2::xml_text)
    unique(trimws(urls))
}
