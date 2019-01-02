indek.href <- function(url, text, baseurl, cache) {
    if (class(url) != "character" || class(text) != "character") {
        stop("indek.href: incorrect parameter types")
    }

    if (length(url) != 1 || length(text) != 1) {
        stop("indek.href: length of params must be 1")
    }
    structure(list(url = url,
                   text = text,
                   baseurl = baseurl,
                   cache = cache),
              class = "indek.href")

}



indek.href.set <- function(urls, texts, baseurl, cache) {
    if (class(urls) != "character" || class(texts) != "character") {
        stop("indek.href.set: params of incorrect type")
    }
    if (length(urls) != length(texts)) {
        stop("indek.href.set: params of different size")
    }
    if (class(cache) != "indek.cache.manager") {
        stop("indek.href.set: cache param must be of type indek.cache")
    }
    ## todo: should I do the following?:
    ## uni <- !duplicated(urls)
    ## urls <- urls[uni]
    ## texts <- texts[uni]
    structure(list(urls = urls,
                   texts = texts,
                   baseurl = baseurl,
                   cache = cache),
              class = "indek.href.set")
}

'[.indek.href.set' <- function(hrefset, i) {
    indek.href(hrefset$urls[i],
               hrefset$texts[i],
               hrefset$baseurl,
               hrefset$cache)
    ## todo: I'm not sure of leting indexin a subset, because all
    ## the idea was to ensure not to store list of hrefs because
    ## of memeory, but hrefset indetead
    
    ## sapply(1:length(hrefset),
    ##        function(x) indek.href(hrefset$urls[i], hrefset$texts[i]))
}

indek.url <- function(u) {
    if (class(u) != "character") {
        stop("indek.url: url parameter must be character")
    }

    attr(u, "class") <- "indek.url"
    u
}


indek.local.path <- function(path) {
    if (class(path) != "character") {
        stop("indek.local.path: path must be character")
    }
    attr(path, "class") <- "indek.local.path"
    path
}


download_file.indek.href <- function(href) {
    ## if (class(url) != "indek.href") {
    ##     stop("download_file: url param must be indek.href")
    ## }
    dest <- as.local.path(href$url, href$cache)
    download_file(href$cache,
                  paste(href$baseurl, href$url, sep = "/"),
                  dest)
}

as.url <- function(href) UseMethod("as.url")
as.url.indek.href  <- function(href) { indek.url(href$url) }



as.local.path <- function(href, cache) UseMethod("as.local.path")
as.local.path.indek.url  <- function(url, cache) {
    as.local.path(as.character(url), cache)
}

as.local.path.indek.href  <- function(href, cache) {
    as.local.path(href$url, cache)
}

as.local.path.character  <- function(url, cache) {
    if (class(cache) != "indek.cache.manager") {
        stop("as.local.path: cache parameter must have correct type.")
    }
    url <- sub(".*?://", "", url)
    url <- paste(cache$dirname, gsub("/", ".", url), sep = "/")

    indek.local.path(url)
}


get_path_string <- function(o, cache) UseMethod("get_path_string")
get_path_string.character  <- function(o, cache) {
    if (class(cache) != "indek.cache.manager") {
        stop("get_path_string: incorrect type param cache")
    }
    u <- as.url(o)
    print(class(u))
    as.local.path(u, cache)
}




unzip_file <- function(this, files) UseMethod("unzip_file")
unzip_file.indek.local.path <- function(this, files = NULL) {
    path <- this
    if (is.null(files)) {
        return(unzip(zipfile = path,
                     files = NULL,
                     list = TRUE))
        
    } else if (class(files) == "numeric") {
        if (min(files) < 1) {
            stop("unzip_file: negative files index")
        }
        filenames <- unzip(zipfile = path,
                           files = NULL,
                           list = TRUE)$Name
        if (length(filenames) < max(files)) {
            stop("unzip_file: files index out of range")
        }

    }

    unzip(zipfile = path,
          files = files,
          list = FALSE,
          exdir = page$cache$dirname)
    files
}


    ## download_file(href$cache,
    ##               paste(href$baseurl, href$url, sep = "/"),
    ##               as.local.path(href$url, href$cache))

unzip_list.indek.href <- function(href) {
    path <- as.local.path(href$url, href$cache)
    if (!file.exists(path)) {
        download_file(href)
    }
    unzip_file(path)
}

unzip_extract <- function(href, files) UseMethod("unzip_extract")
unzip_extract.indek.href <- function(href, files) {
    path <- as.local.path(href$url, href$cache)
    ## if (!file.exists(path)) {
    ##     download_file(href)
    ## }
    ## unzip_file(path)

    ##     path <- this
    if (is.null(files)) {
        return(unzip(zipfile = path))
        
    } else if (class(files) == "numeric") {
        if (min(files) < 1) {
            stop("unzip_file: negative files index")
        }
        filenames <- unzip(zipfile = path,
                           files = NULL,
                           list = TRUE)$Name
        if (length(filenames) < max(files)) {
            stop("unzip_file: files index out of range")
        }
        files <- filenames[files]
    }
    unzip(zipfile = path,
          files = files,
          list = FALSE,
          exdir = href$cache$dirname)
    paste(href$cache$dirname, files, sep = "/")

}

read_tables_zip <- function(href, files) UseMethod("read_tables_zip")
read_tables_zip.indek.href <- function(href, files, use.cache = TRUE) {
    if (use.cache) {
        f <- unzip_extract(href, files)
        return(lapply(unzip_extract(href, files), function(x) {
            read.table(x, header = TRUE, sep = ";") }))
    }
    
}


read_pdf <- function(href, files) UseMethod("read_pdf")
read_pdf.indek.href <- function(href, files, use.cache = TRUE) {
    if (use.cache) {
        f <-  download_file(href)
        return(pdftools::pdf_text(f))
    }
    
}
