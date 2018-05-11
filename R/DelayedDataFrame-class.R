###------------------
## DelayedDataFrame
###------------------ 

#' DelayedDataFrame-class
#' @name DelayedDataFrame
#' @exportClass DelayedDataFrame
#' @importFrom methods as initialize is new "slot<-"
#' @aliases DelayedDataFrame-class
#' @description The \code{DelayedDataFrame} class extends the
#'     \code{DataFrame} class and supports the storage of any type of
#'     object (with ‘length’ and ‘[’ methods) as columns.
#' @rdname DelayedDataFrame-class
#' @details The \code{DelayedDataFrame} inherits from \code{DataFrame}
#'     and behaves very similarily in terms of construction,
#'     subsetting, splitting, combining, etc. The most notable
#'     exception is that The additional slot of \code{lazyIndex},
#'     enables \code{DelayedArray} (with different back-ends) columns
#'     to share indexes when possible.
## refer ?DataFrame 
## #' \code{Constructor}: \code{DelayedDataFrame(..., row.names = NULL,
## #' check.names = TRUE)} constructs a ‘DelayedDataFrame’ in similar
## #' fashion to ‘DataFrame’. Each argument in \code{...} is coerced to a
## #' ‘DelayedDataFrame’ and combined column-wise.

.DelayedDataFrame <- setClass(
    "DelayedDataFrame",
    contains = "DataFrame",
    slots = c(lazyIndex = "LazyIndex")
)

###---------------------------------------
## Utility functions for DelayedDataFrame
###---------------------------------------
.get_index <- function(x, j)
{
    j <- .index(lazyIndex(x))[[j]]
    .listData(lazyIndex(x))[[j]]
}

###-------------
## constructor
###-------------

#' @export DelayedDataFrame
#' @aliases DelayedDataFrame
#' @rdname DelayedDataFrame-class
DelayedDataFrame <- function(..., row.names=NULL, check.names=TRUE)
{
    listData <- list(...)
    isDDF <- vapply(unname(listData), is, logical(1), "DelayedDataFrame")
    if (length(isDDF) && all(isDDF)) {
        ddf <- concatenateObjects(listData[[1]], listData[-1])
    } else {
        df <- DataFrame(..., row.names=row.names, check.names=check.names)
        ddf <- as(df, "DelayedDataFrame")
    }
    ddf
}

###-------------
## accessor
###-------------

setGeneric("lazyIndex", function(x) standardGeneric("lazyIndex"), signature="x")

setMethod("lazyIndex", "DelayedDataFrame", function(x) x@lazyIndex)

###-------------
## methods
###-------------

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

## "as.list" function is called in lapply("DelayedDataFrame", ) and names("DelayedDataFrame")...
setMethod("as.list", "DelayedDataFrame", function(x, use.names=TRUE)  
{
    ans <- lapply(seq_along(x), function(j) x[[j]])
    if (use.names)
        names(ans) <- names(x)
    ans
})

setMethod("names", "DelayedDataFrame", function(x)
{
    names(x@listData)
})

## FIXME: GDSArray coerced into character vector...
## setMethod("rbind", "DelayedDataFrame", function(..., deparse.level=1)
## {
##     DelayedDataFrame(callNextMethod())
## })

setMethod("cbind", "DelayedDataFrame", function(..., deparse.level=1)
{
    df <- callNextMethod()
    DelayedDataFrame(df)
})


###-------------
### Coercion
###-------------

## DelayedDataFrame has inherited from DataFrame, so it inherits
## coercion methods of DataFrame to matrix/data.frame/list (as.matrix,
## as.list, as.data.frame/as(x, "data.frame/list")). Will only need to
## define set("ANY", "DelayedDataFrame").

#' @name coerce
#' @exportMethod coerce
#' @aliases coerce,DataFrame,DelayedDataFrame-method
#' @rdname DelayedDataFrame-class
#' @param from a \code{DataFrame} object
setAs("DataFrame", "DelayedDataFrame", function(from)
{
    if (identical(dim(from), c(0L, 0L))) {
        lazyIndex <- .LazyIndex()
    } else {     
        lazyIndex <- .LazyIndex(vector("list", 1), index=rep(1L, length(from)))
    }
    .DelayedDataFrame(from, lazyIndex = lazyIndex)
})

## setAs("DelayedDataFrame", "DataFrame", function(from)
## {
##     listData <- as.list(from)
##     idx <- vapply(listData, is, logical(1), "DelayedArray")
##     listData[idx] <- lapply(listData[idx], I)
##     DataFrame(listData, row.names = rownames(from))
## })

setMethod("coerce", c("DelayedDataFrame", "DataFrame"),
          function(from, to="DataFrame", strict=TRUE)
          {
              if (!strict && is(from, "DataFrame")) {
                  return(from)
              } else {
                  listData <- as.list(from)
                  idx <- vapply(listData, is, logical(1), "DelayedArray")
                  listData[idx] <- lapply(listData[idx], I)
                  DataFrame(listData, row.names = rownames(from))
              }
          }
)

###
setAs("ANY", "DelayedDataFrame", function(from){
    df <- as(from, "DataFrame")
    as(df, "DelayedDataFrame")
})

###-----------------
## validity check
###----------------
.validate_DelayedDataFrame <- function(x)
{
    msg <- character()
    test <- .validate_LazyIndex(lazyIndex(x))
    if (!isTRUE(test))
        msg <- c(msg, test)

    if(length(.index(lazyIndex(x))) != ncol(x))
        msg <- c(msg, "'.index(x)' must be of same length of 'ncols(x)'")

    if (length(msg)) msg else TRUE
}

#' @importFrom S4Vectors setValidity2
setValidity2("DelayedDataFrame", .validate_DelayedDataFrame)

###-----------------
## subsetting
###----------------
#' @importFrom methods slot
.extractROWS_DelayedDataFrame <- function(x, i)
{
    i <- normalizeSingleBracketSubscript(
        i, x, exact = FALSE, allow.NAs = TRUE, as.NSBS = FALSE)
    rownames <- rownames(x)[i]
    if (!is.null(rownames))
        rownames <- make.unique(rownames)

    initialize(
        x, lazyIndex = lazyIndex(x)[i,], nrows = length(i), rownames = rownames
    )
}
#' @exportMethod extractROWS
#' @aliases extractROWS,DelayedDataFrame-method
#' @rdname DelayedDataFrame-class
setMethod("extractROWS", "DelayedDataFrame", .extractROWS_DelayedDataFrame)

setReplaceMethod(
    "[", c("DelayedDataFrame", "ANY"),
    function(x, i, j, ..., value)
{
    xstub <- setNames(seq_along(x), names(x))
    if (missing(j)) {
        i <- normalizeSingleBracketSubscript(i, xstub)
        lazyIndex(x) <- .update_index(lazyIndex(x), i, NULL)
    } else {
        j <- normalizeSingleBracketSubscript(j, xstub)
        x@listData[j] <- lapply(j, function(j, x) x[[j]], x)
        lazyIndex(x) <- .update_index(lazyIndex(x), j, NULL)
    }
    callNextMethod()
})

#' @importFrom methods callNextMethod
#' @exportMethod [
#' @aliases [,DelayedDataFrame-method
#' @rdname DelayedDataFrame-class
#' @param x input
#' @param i row subscript
#' @param j col subscript
#' @param drop if drop with reduced dimension, default is TRUE.
#' @param row.names rownames
#' @param check.names if check names.
#' @param ... other arguments to pass.

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
        x <- initialize(
            x, lazyIndex = lazyIndex(x)[j], listData = extractROWS(x@listData, j),
            elementMetadata = extractROWS(mcols(x), j)
        )
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

## constructing a new DelayedDataFrame
setMethod(
    "concatenateObjects", "DelayedDataFrame",
    function(x, objects = list(), use.names = TRUE, ignore.mcols = FALSE, check = TRUE)
{
    for (i in seq_len(length(objects))) {
        y <- as(objects[[i]], "DelayedDataFrame")
        lazyIndex(x) <- c(lazyIndex(x), lazyIndex(y))
    }
    ## validObject(x) ## ncol(x) != .index(lazyIndex(x))
    callNextMethod()
})

###--------------
## slot setters
###--------------

## replace method for lazyIndex(DDF)
setGeneric(
    "lazyIndex<-",
    function(x, value) standardGeneric("lazyIndex<-"),
    signature="x")

#' @exportMethod "lazyIndex<-"
#' @rdname DelayedDataFrame-class
#' @description the setter for the \code{lazyIndex} slot of \code{DelayedDataFrame} object.
#' @return the new value of \code{lazyIndex} slot for \code{DelayedDataFrame} object.
setReplaceMethod( "lazyIndex", "DelayedDataFrame", function(x, value) {
    BiocGenerics:::replaceSlots(x, lazyIndex=value, check=FALSE)
})

