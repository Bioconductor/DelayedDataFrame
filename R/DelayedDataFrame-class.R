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

### For \code{DelayedDataFrame} constructor, If the inputs are all
### DelayedDataFrame, will concatenate all existing lazyIndex's and
### cbind for listData.
#' @export DelayedDataFrame
#' @import S4Vectors
#' @aliases DelayedDataFrame
#' @rdname DelayedDataFrame-class
DelayedDataFrame <- function(..., row.names=NULL, check.names=TRUE)
{
    ## browser()
    listData <- list(...)
    isDDF <- vapply(unname(listData), is, logical(1), "DelayedDataFrame")
    if (length(isDDF) && all(isDDF)) {
        ddf <- do.call(cbind, listData)
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
## #' @param from a \code{DataFrame}, \code{DelayedDataFrame}, or \code{ANY} object.
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

###--------------
## slot setters
###--------------

#' @exportMethod "lazyIndex<-"
#' @rdname DelayedDataFrame-class
setGeneric(
    "lazyIndex<-",
    function(x, value) standardGeneric("lazyIndex<-"),
    signature="x")

#' @description the setter for the \code{lazyIndex} slot of \code{DelayedDataFrame} object.
#' @param x the \code{DelayedDataFrame} object.
#' @param value the new value of \code{lazyIndex} slot for \code{DelayedDataFrame} object.
#' @return the \code{DelayedDataFrame} object with new value of \code{lazyIndex} slot.
setReplaceMethod( "lazyIndex", "DelayedDataFrame", function(x, value) {
    BiocGenerics:::replaceSlots(x, lazyIndex=value, check=FALSE)
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

