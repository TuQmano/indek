indek.url.tree <- function(urls)
{
    if (class(urls) != "character") {
        stop("urls param must be character")
    }

    relative    <- urls[!grepl("^https?://", urls)]
    
    leafurls <- relative[!grepl("/", relative)]
    nonleafs <- relative[grepl("/",relative)]

    subnodenames <- unique(sapply(strsplit(nonleafs, "/"),
                       function(x) x[1]))
    subnodes <- lapply(subnodenames, function(subn) {
        node.re <- paste("^", subn, "/", sep="")
        res <- nonleafs[grepl(node.re, nonleafs)]
        res <- sub(node.re, "", res)
        indek.url.tree(res)
    })
    names(subnodes) <- subnodenames
    cache <- structure(
        c(list(leafurls = leafurls),
          subnodes),
        class = "indek.url.tree")
}

get_leafurls <- function(tree) UseMethod("getLeafs")
get_leafurls.indek.url.tree <- function(tree) {
    tree$leafurls
}

is.leaf <- function(tree) UseMethod("is.leaf")
is.leaf.indek.url.tree <- function(tree) { length(tree) == 1 }

as.character <- function(tree, level) UseMethod("as.character")
as.character.indek.url.tree <- function(tree, pre = "") {
    if (is.leaf(tree)) {  
        return(paste(pre, tree$leafurls, sep=""))
    }
    res <- character(0)
    subnames <- names(tree)[names(tree) != "leafurls"]
    for (name in subnames) {
        res <- c(res,
                 paste(pre, name, sep=""),
                 as.character(tree[[name]],
                              paste(pre, name, "/", sep = "")))
    }
    res
}
