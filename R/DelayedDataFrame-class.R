###------------------
## DelayedDataFrame
###------------------ 

#' DelayedDataFrame-class
#' @name DelayedDataFrame
#' @exportClass DelayedDataFrame
#' @importFrom methods as initialize is new "slot<-"
#' @import DelayedArray
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
#' 
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


#' @aliases DelayedDataFrame
#' @rdname DelayedDataFrame-class
#' @param ... the arguments to pass into construction of a new
#'     \code{DelayedDataFrame}.
#' @param row.names the \code{rownames} for the newly constructed
#'     \code{DelayedDataFrame} object.
#' @param check.names logical.  If ‘TRUE’ then the names of the
#'     variables in the \code{DelayedDataFrame} are checked to ensure
#'     that they are syntactically valid variable names and are not
#'     duplicated.  If necessary they are adjusted (by ‘make.names’)
#'     so that they are.
#' @export
#' @import S4Vectors
#' @examples
#' DDF <- DelayedDataFrame(letters, LETTERS)
#' DDF1 <- DDF[1:10,]
#' DDF1
#' lazyIndex(DDF1)
#' as(DDF1, "DataFrame")
#' 
DelayedDataFrame <- function(..., row.names=NULL, check.names=TRUE)
{
    listData <- list(...)
    isDDF <- vapply(unname(listData), is, logical(1), "DelayedDataFrame")
    if (length(listData) && any(isDDF)) {
        ans <- do.call(cbind, listData)
        if (check.names)
            names(ans) <- make.names(names(ans), unique = TRUE)
    } else { 
        ans <- DataFrame(..., row.names=row.names, check.names=check.names)
    }
    ans <- as(ans, "DelayedDataFrame", strict = FALSE)
    ans
}
### https://github.com/Bioconductor/Contributions/issues/861#issuecomment-436836411
### removing `cbind` inside constructor.
### define `cbind2()` instead of `.cbind_DDF()`? 
### column names consistency for data.frame and DataFrame? ... 
### remove `DataFrame()` calls from constructor. 

###-------------
## accessor
###-------------

setGeneric("lazyIndex", signature = "x", function(x)
    standardGeneric("lazyIndex"))

#' @rdname DelayedDataFrame-class
#' @aliases lazyIndex lazyIndex,DelayedDataFrame
#' @description the \code{lazyIndex} slot getter and setter for
#'     \code{DelayedDataFrame} object.
#' @export
setMethod("lazyIndex", "DelayedDataFrame", function(x) x@lazyIndex)

###-------------
### Coercion
###-------------

## DelayedDataFrame has inherited from DataFrame, so it inherits
## coercion methods of DataFrame to matrix/data.frame/list (as.matrix,
## as.list, as.data.frame/as(x, "data.frame/list")). Will only need to
## define set("ANY", "DelayedDataFrame").

#' @name coerce
#' @rdname DelayedDataFrame-class
#' @aliases coerce,DataFrame,DelayedDataFrame-method
#' @description the coercion method between \code{DataFrame} and
#'     \code{DelayedDataFrame} objects.
#' @param from the object to be converted.
#' @export

setAs("DataFrame", "DelayedDataFrame", function(from)
{
    ldf <- length(from)
    lazyIndex <- .LazyIndex(vector("list", as.numeric(as.logical(ldf))),
                            index=rep(1L, ldf))
    .DelayedDataFrame(from, lazyIndex = lazyIndex)
    
})

#' @rdname DelayedDataFrame-class
#' @aliases coerce,DelayedDataFrame,DataFrame-method
#' @param to the class of object to be returned by coercion.
#' @param strict Logical. Whether to force return a \code{DataFrame}. 
#' @export

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

#' @name coerce
#' @rdname DelayedDataFrame-class
#' @aliases coerce,ANY,DelayedDataFrame-method
#' @export
setAs("ANY", "DelayedDataFrame", function(from){
    df <- as(from, "DataFrame")
    as(df, "DelayedDataFrame")
})

###--------------
## slot setters
###--------------

#' @exportMethod "lazyIndex<-"
#' @rdname DelayedDataFrame-class
setGeneric(
    "lazyIndex<-",
    function(x, value) standardGeneric("lazyIndex<-"),
    signature="x")

#' @rdname DelayedDataFrame-class
#' @aliases "lazyIndex<-" "lazyIndex<-, DelayedDataFrame-class"
#' @param x the \code{DelayedDataFrame} object.
#' @param value the new value of \code{lazyIndex} slot for
#'     \code{DelayedDataFrame} object.
#' @return \code{lazyIndex<-}: the \code{DelayedDataFrame} object with
#'     new value of \code{lazyIndex} slot.
#' @details Please be very careful to use this replace method for
#'     \code{lazyIndex} slot. Because it only replace the
#'     \code{lazyIndex} slot, but not necessarily the \code{nrow} and
#'     \code{rownames} slots. If you want to have synchronized
#'     subsetting for all slots, the \code{[} method should be used.
setReplaceMethod( "lazyIndex", "DelayedDataFrame", function(x, value) {
    BiocGenerics:::replaceSlots(x, lazyIndex = value, check=FALSE)
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

setValidity2("DelayedDataFrame", .validate_DelayedDataFrame)

