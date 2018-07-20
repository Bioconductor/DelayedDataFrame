###----------
## LazyIndex
###----------

#' The \code{LazyIndex} class and methods.
#' @rdname LazyIndex-class
#' @description The \code{LazyIndex} class is designed to carry
#'     mapping indexes for \code{DelayedDataFrame} columns. So that
#'     some operations (e.g., subsetting) on \code{DelayedDataFrame}
#'     are delayed until a realization call is incurred. (e.g.,
#'     as.list(), DataFrame(), ...)
#' @export

.LazyIndex <- setClass(
    "LazyIndex",
    contains = "SimpleList",  ## extend list? List? 
    slots = c(index = "integer")
)

#' @rdname LazyIndex-class
#' @description \code{LazyIndex} constructor.
#' @param listData the list data for all mapping indexes that are used in
#'     corresponding \code{DelayedDataFrame} object.
#' @param index the position of mapping indexes in \code{listData} for
#'     each column of the corresponding \code{DelayedDataFrame}
#'     object.
#' @return a \code{LazyIndex} object. 
LazyIndex <-
    function(listData = list(), index = integer())
{
    .LazyIndex(listData, index = index)
}

.listData <- function(x)
    x@listData

.index <- function(x)
    x@index

.validate_LazyIndex <- function(x)
{
    msg <- character()
    ## indexes length must be same
    indexes <- .listData(x)
    indexLength <- lengths(indexes)
    uniqLen <- unique(indexLength)
    if (length(uniqLen[uniqLen != 0]) > 1)
        msg <- c(msg, "'.index(x)' must be of same length or 'NULL'")
    ## index slot must be consistent with all listData elements.
    if (!setequal(.index(x), seq_along(.listData(x))))
        msg <- c(msg, "'.index(x)' and 'seq_along(.listData(x))' are not equal")
    if (length(msg)) msg else TRUE
}

setValidity2("LazyIndex", .validate_LazyIndex)

.fulllength <- function(x) {  ## dim(), rectangular., concatenateObject (works as cbind)
    if (length(x) == 0)
        return(0)
    indexes <- .listData(x)
    indexLength <- lengths(indexes)
    uniqLen <- unique(indexLength)
    if (identical(uniqLen, 0L))
        return(NULL)
    uniqLen[uniqLen != 0]
}

### Arguments 'ignore.mcols' and 'check' are ignored. Although we have
### "cbind,LazyIndex" implemented, and so the "c,lazyIndex" works,
### but it is not used in the "DelayedDataFrame". Will keep now for
### any potential usage later.
.cbind_lazyIndex <- function(x, objects=list(), use.names = TRUE,
                             ignore.mcols = FALSE, check = TRUE)
{
    if (!is.list(objects)) 
        stop("'objects' must be a list")
    
    listData <- c(list(.listData(x)), lapply(objects, slot, "listData"))
    indexes <- c(list(.index(x)), lapply(objects, slot, "index"))

    ## index offsets
    offsets <- head(cumsum(c(0L, lengths(listData))), -1L)
    index <- unlist(indexes) + rep(offsets, lengths(indexes))
    listData <- unlist(listData, recursive=FALSE)

    .lazyIndex_compose(listData, index) }

#' @rdname LazyIndex-class
#' @aliases cbind,LazyIndex-method
#' @param ... \code{LazyIndex} objects.
#' @param deparse.level See \code{?base::cbind} for a description of
#'     this argument.
#' @details the \code{cbind,LazyIndex} method is defined to bind the
#'     LazyIndexes column-wise when \code{cbind,DelayedDataFrame}
#'     function is called.
#' @importFrom methods slot
#' @export

setMethod("cbind", "LazyIndex", function(..., deparse.level = 1)
{
    objects <- list(...)
    .cbind_lazyIndex(objects[[1L]], objects[-1L], check=FALSE)
})

#' @rdname LazyIndex-class
#' @aliases [,LazyIndex-method
#' @description the subsetting method for \code{LazyIndex} object.
#' @param x \code{LazyIndex} object.
#' @param i row subscript for \code{LazyIndex}, which will subset the
#'     \code{listData} slot.
#' @param j column subscript for \code{LazyIndex}, which will subset
#'     the \code{index} slot.
#' @param drop Logical. Wheter to drop the dimension if any of the
#'     dimensions has length 1. Default is TRUE.
#' @importFrom stats setNames
#' @export

setMethod("[", c("LazyIndex", "ANY"),
    function(x, i, j, ..., drop = TRUE)
{
    listData <- .listData(x)
    index <- .index(x)

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
                return(x)  ## return x if x[]
            j <- i
        }
        if (!is(j, "IntegerRanges")) {
            xstub <- setNames(seq_along(index), seq_along(index))
            j <- normalizeSingleBracketSubscript(j, xstub)
        }
        index <- extractROWS(index, j)
        x <- .lazyIndex_compose(listData, index)
        if (list_style_subsetting)
            return(x)
    }
    if (!missing(i)) {
        x <- .update_row(x, i)
    }
    x
})

###---------------------------------------
## Utility functions for .LazyIndex
###---------------------------------------

.lazyIndex_compose <-
    function(listData0, index0)
{
    ## 1. check if any duplicate in @listData. Modify @index
    listData1 <- unique(listData0)
    index1 <- match(listData0, listData1)[index0]

    ## 2. check if all @listData in use in @index. remove if not.
    listData2 <- listData1[seq_along(listData1) %in% index1]
    index2 <- match(listData1, listData2)[index1]

    ## 3. reorder indexes (index for 1st columns in @listData[[1]])
    index3 <- unique(index2)
    listData <- listData2[index3]
    index <- match(index2, index3)

    LazyIndex(listData, index=index)
}

.update_index <- function(lazyList, j, value)
{
    listData <- c(.listData(lazyList), list(value))
    index <- .index(lazyList)
    index[j] <- length(listData)
    .lazyIndex_compose(listData, index)
}

.update_row <- function(lazyList, i)
{
    listData <- .listData(lazyList)
    isNull <- vapply(listData, is.null, logical(1))
    if (any(lengths(listData[!isNull]) < length(i)))
        stop("subscript contains out-of-bounds indices")

    listData[isNull] <- list(i)
    listData[!isNull] <- lapply(listData[!isNull], `[`, i = i)

    .lazyIndex_compose(listData, .index(lazyList))
}

setMethod("show", "LazyIndex", function(object)
{
    lo <- length(object)
    cat(classNameForDisplay(object), " of length ", lo, "\n",
        sep = "")
    print(.listData(object))
    cat("index of each column: ", "\n", sep="")
    print(.index(object))
})
