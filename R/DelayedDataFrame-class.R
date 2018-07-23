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
DelayedDataFrame <- function(..., row.names=NULL, check.names=TRUE)
{
    ## browser()
    listData <- list(...)
    isDDF <- vapply(unname(listData), is, logical(1), "DelayedDataFrame")
    if (length(listData) > 0 && any(isDDF)) {
        ans <- do.call(cbind, listData)
    } else {
        df <- DataFrame(..., row.names=row.names, check.names=check.names)
        ans <- as(df, "DelayedDataFrame")
    }
    if (!is(ans, "DelayedDataFrame"))
        ans <- as(ans, "DelayedDataFrame")
    ans
}

### FIXME: the "check.names" in constructor is not working here yet. 


###-------------
## accessor
###-------------

setGeneric("lazyIndex", function(x) standardGeneric("lazyIndex"), signature="x")

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
    if (ncol(from) == 0) {
        lazyIndex <- .LazyIndex()
    } else {     
        lazyIndex <- .LazyIndex(vector("list", 1), index=rep(1L, length(from)))
    }
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
#' @return \code{lazyIndex<-}: the \code{DelayedDataFrame} object with new value of
#'     \code{lazyIndex} slot.
#' @details Please be very careful to use this replace method for
#'     \code{lazyIndex} slot. Because it only replace the
#'     \code{lazyIndex} slot, but not necessarily the \code{nrow} and
#'     \code{rownames} slots. If you want to have synchronized
#'     subsetting for all slots, the \code{[} method should be used.
setReplaceMethod( "lazyIndex", "DelayedDataFrame", function(x, value) {
    ## browser()
    BiocGenerics:::replaceSlots(x, lazyIndex = value, check=FALSE)
    ## fl <- .fulllength(value)
    ## if (! is.null(fl)) { ## fl == 0 (null lazyIndex with 0 index) OR
    ##                      ## fl > 0 (need to update the new
    ##                      ## .fulllength(value) )
    ##     BiocGenerics:::replaceSlots(x, nrows = fl, check = FALSE)
    ##     ld <- .listData(value)
    ##     ldisnull <- sapply(ld, is.null)
    ##     newrowidx <- ld[[ which(!ldisnull)[1] ]]
    ##     BiocGenerics:::replaceSlots(x, rownames = rownames(x)[newrowidx], check = FALSE)
    ##     ## FIXME: only use the first non-null index as the new index
    ##     ## for rownames.
    ## }
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

## setMethod("show", "DelayedDataFrame", function (object) 
## {
##     browse()
##     nhead <- get_showHeadLines()
##     ntail <- get_showTailLines()
##     nr <- nrow(object)
##     nc <- ncol(object)
##     cat(class(object), " with ", nr, ifelse(nr == 1, " row and ", 
##         " rows and "), nc, ifelse(nc == 1, " column\n", " columns\n"), 
##         sep = "")
##     if (nr > 0 && nc > 0) {
##         nms <- rownames(object)
##         if (nr <= (nhead + ntail + 1L)) {
##             out <- as.matrix(format(as.data.frame(lapply(object, 
##                 showAsCell), optional = TRUE)))
##             if (!is.null(nms)) 
##                 rownames(out) <- nms
##         }
##         else {
##             out <- rbind(as.matrix(format(as.data.frame(lapply(object, 
##                 function(x) showAsCell(head(x, nhead))), optional = TRUE))), 
##                 rbind(rep.int("...", nc)), as.matrix(format(as.data.frame(lapply(object, 
##                   function(x) showAsCell(tail(x, ntail))), optional = TRUE))))
##             rownames(out) <- .rownames(nms, nr, nhead, ntail)
##         }
##         classinfo <- matrix(unlist(lapply(object, function(x) {
##             paste0("<", classNameForDisplay(x)[1], ">")
##         }), use.names = FALSE), nrow = 1, dimnames = list("", 
##             colnames(out)))
##         out <- rbind(classinfo, out)
##         print(out, quote = FALSE, right = TRUE)
##     }
## }
