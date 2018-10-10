###-------------
## methods
###-------------

### the "as.list", "rbind(a thin wrapper of bindROWS)"
### will realize all lazyIndexes and construct a new DelayedDataFrame
### (with initial lazyIndex of NULL).

setMethod("getListElement", "DelayedDataFrame", function(x, i, exact=TRUE)
{
    i2 <- normalizeDoubleBracketSubscript(
        i, x, exact = exact,
        allow.NA = TRUE,
        allow.nomatch = TRUE)
    if (is.na(i2)) 
        return(NULL)
    index <- .get_index(x, i2)
    elt <- x@listData[[i2]]
    if (!is.null(index))
        elt <- extractROWS(elt, index)
    elt
})

### "as.list" function is called in lapply("DelayedDataFrame", ) and
### names("DelayedDataFrame")...

#' DelayedDataFrame related methods.
#' @rdname DelayedDataFrame-method
#' @description \code{as.list}, \code{rbind} would incur realization
#'     of the \code{lazyIndex} slot in \code{DelayedDataFrame} object.
#' @param x \code{as.list,DelayedDataFrame}: a \code{DelayedDataFrame}
#'     object. OR, \code{[,DelayedDataFrame}: \code{DelayedDataFrame}
#'     object to be subsetted.
#' @param use.names \code{as.list,DelayedDataFrame}: whether to use
#'     the colnames of \code{DelayedDataFrame} as the names for the
#'     returned list. OR, \code{bindROWS,DelayedDataFrame}: whether to
#'     use rownames of the input arguments. Default is TRUE.
#' @aliases as.list,DelayedDataFrame-method
#' @export
#' 
setMethod("as.list", "DelayedDataFrame", function(x, use.names=TRUE)  
{
    ans <- lapply(seq_along(x), function(j) x[[j]])
    if (use.names)
        names(ans) <- names(x)
    ans
})

#' @return colnames of \code{DelayedDataFrame}
#' @rdname DelayedDataFrame-method
#' @aliases names,DelayedDataFrame-method
#' @exportMethod names
setMethod("names", "DelayedDataFrame", function(x)
{
    names(x@listData)
})

### cbind,DDF will keep and cbind the old lazyIndexes.
.cbind_DDF <- function(x, objects = list())
{
    lazyIndex_objects <- lapply(objects, lazyIndex)
    new_lazyIndex <- .cbind_lazyIndex(lazyIndex(x), lazyIndex_objects)
    listData_objects <- do.call(c, lapply(objects, function(x) x@listData))
    new_listData <- c(x@listData, listData_objects)
    ## rownames, nrows
    ## ans <- initialize(x, listData = new_listData, lazyIndex = new_lazyIndex)
    ans <- BiocGenerics:::replaceSlots(x, listData = new_listData,
                                       lazyIndex = new_lazyIndex)
    ans
}

#' @rdname DelayedDataFrame-method
#' @description \code{cbind} for DelayedDataFrame inherits the
#'     lazyIndex's if inputs have any DelayedDataFrame
#'     objects. Otherwise, return a new DelayedDataFrame with NULL
#'     lazyIndexes.
#' @param ... \code{cbind,DelayedDataFrame}: One or more vector-like
#'     or matrix-like objects. These can be given as named
#'     arguments. OR, \code{[,DelayedDataFrame}: other arguments to
#'     pass.
#' @param deparse.level See ‘?base::cbind’ for a description of this
#'     argument.
#' @aliases cbind,DelayedDataFrame-method
#' @export
#' @importMethodsFrom BiocGenerics cbind

setMethod("cbind", "DelayedDataFrame", function(..., deparse.level = 1)
{
    objects <- list(...)
    isDDF <- vapply(unname(objects), is, logical(1), "DelayedDataFrame")
    for (i in which(!isDDF)){
        a <- as(objects[[i]], "DelayedDataFrame")
        objects[[i]] <- a
    }
    x <- objects[[1]]
    objects <- objects[-1]
    .cbind_DDF(x, objects)
})

#' bindROWS is the lower-level function for \code{rbind}.
#' @rdname DelayedDataFrame-method
#' @aliases bindROWS,DelayedDataFrame-method
#' @param objects the \code{DelayedDataFrame} objects to be passed
#'     into \code{bindROWS}.
#' @param ignore.mcols Logical. This argument is ignored for
#'     \code{bindROWS,DelayedDataFrame}.
#' @param check Logical. This argument is ignored for
#'     \code{bindROWS,DelayedDataFrame}.
setMethod(
    "bindROWS", "DelayedDataFrame",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = FALSE, check = TRUE)
    {
        ans <- callNextMethod()
        lazyIndex(ans) <- LazyIndex(vector("list", 1), rep(1L, ncol(x)))
        ans
    })

###--------------------
## subsetting methods
###--------------------
#' @importFrom methods slot
.extractROWS_DelayedDataFrame <- function(x, i)
{
    i <- normalizeSingleBracketSubscript(
        i, x, exact = FALSE, allow.NAs = TRUE, as.NSBS = FALSE)
    rownames <- rownames(x)[i]
    if (!is.null(rownames))
        rownames <- make.unique(rownames)

    initialize(x, lazyIndex = lazyIndex(x)[i,],
               nrows = length(i), rownames = rownames)
}
#' @importFrom stats setNames
#' @exportMethod extractROWS
#' @aliases extractROWS,DelayedDataFrame-method
#' @rdname DelayedDataFrame-method
setMethod("extractROWS", "DelayedDataFrame",
          .extractROWS_DelayedDataFrame)

#' @rdname DelayedDataFrame-method
#' @aliases "[<-" "[<-,DelayedDataFrame-method"
#' @param value the new values in the \code{i,j} subscripts of
#'     \code{DelayedDataFrame} object.
#' @export

setReplaceMethod(
    "[", c("DelayedDataFrame", "ANY"),
    function(x, i, j, ..., value)
{
    xstub <- setNames(seq_along(x), names(x))
    if (missing(j)) {
        j <- normalizeSingleBracketSubscript(i, xstub)
        lazyIndex(x) <- .update_index(lazyIndex(x), j, NULL)
    } else {
        j <- normalizeSingleBracketSubscript(j, xstub)
        x@listData[j] <- lapply(j, function(j, x) x[[j]], x)
        lazyIndex(x) <- .update_index(lazyIndex(x), j, NULL)
    }
    callNextMethod()
})

#' @rdname DelayedDataFrame-method
#' @aliases "[" "[,DelayedDataFrame-method"
#' @param i row subscript
#' @param j col subscript
#' @param drop if drop with reduced dimension, default is TRUE.
#' @export
#' @importFrom stats setNames
#' @importFrom methods callNextMethod

setMethod("[", c("DelayedDataFrame", "ANY", "ANY", "ANY"),
          function (x, i, j, ..., drop = TRUE) 
{
    if (!isTRUEorFALSE(drop)) 
        stop("'drop' must be TRUE or FALSE")
    if (length(list(...)) > 0L) 
        warning("parameters in '...' not supported")
    list_style_subsetting <- (nargs() - (!missing(drop))) < 3L
    if (list_style_subsetting || !missing(j)) {
        if (list_style_subsetting) {
            if (!missing(drop)) 
                warning("'drop' argument ignored by list-style subsetting")
            if (missing(i)) 
                return(x)
            j <- i
        }
        if (!is(j, "IntegerRanges")) {
            xstub <- setNames(seq_along(x), names(x))
            j <- normalizeSingleBracketSubscript(j, xstub)
        }
        x <- initialize(x,
                        lazyIndex = lazyIndex(x)[j],
                        listData = extractROWS(x@listData, j),
                        elementMetadata = extractROWS(mcols(x), j))
        if (anyDuplicated(names(x))) 
            names(x) <- make.unique(names(x))
        if (list_style_subsetting) 
            return(x)
    }
    if (!missing(i)) {
        x <- extractROWS(x, i)
    }
    if (missing(drop)) 
        drop <- ncol(x) == 1L
    if (drop) {
        if (ncol(x) == 1L) 
            return(x[[1L]])
        if (nrow(x) == 1L) 
            return(as(x, "list"))
    }
    x
})


