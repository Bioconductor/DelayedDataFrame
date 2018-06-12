context("DelayedDataFrame")

test_that("DelayedDataFrame constructor works", {
    obj <- DelayedDataFrame()
    expect_true(validObject(obj))
    expect_identical(c(0L, 0L), dim(obj))

    obj <- DelayedDataFrame(letters, LETTERS)
    expect_true(validObject(obj))
    expect_identical(c(26L, 2L), dim(obj))
    expect_identical(list(NULL, c("letters", "LETTERS")), dimnames(obj))

    obj <- DelayedDataFrame(letters, LETTERS, row.names=LETTERS)
    expect_identical(LETTERS, rownames(obj))

    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(letters, da1=I(da1), da2=I(da2), da3 = I(da3))
    expect_true(validObject(obj))
    expect_identical(
        list(NULL, c("letters", "da1", "da2", "da3")),
        dimnames(obj)
    )
    exp <- list(da1 = c(26L, 1L), da2 = c(26L, 2L), da3 = c(26L, 1L, 1L))
    expect_identical(exp, lapply(obj[-1], dim))

    ## DelayedDataFrame constructor over DelayedDataFrame
    ddf1 <- DelayedDataFrame(da1=I(da1))[1:10, , drop=FALSE]
    ddf2 <- DelayedDataFrame(da2=I(da2))[10:1, , drop=FALSE]
    obj <- DelayedDataFrame(ddf1, ddf2)

    exp <- LazyIndex(list(1:10, 10:1), 1:2)
    expect_identical(exp, lazyIndex(obj))
    
    ## DelayedDataFrame constructor over mix of DelayedDataFrame and others
    df2 <- DataFrame(ddf2)
    obj <- DelayedDataFrame(ddf1, df2, da2=I(da2[1:10,]))

    exp <- LazyIndex(list(NULL), rep(1L, 3))
    expect_identical(exp, lazyIndex(obj))

    exp <- list(NULL, c("da1", "da2", "da2.1"))
    expect_identical(exp, dimnames(obj))
})

test_that("DelayedDataFrame [[ works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(letters, da1=I(da1), da2=I(da2), da3 = I(da3))

    expect_identical(letters, obj[["letters"]])
    expect_equal(da1, obj[["da1"]])     # FAILS: 'package' attribute stripped (pkg: methods)
    expect_equivalent(da2, obj[["da2"]])
    expect_equivalent(da3, obj[["da3"]])

    expect_identical(letters, obj[[1]])
    expect_equivalent(da1, obj[[2]])
    expect_equivalent(da2, obj[[3]])
    expect_equivalent(da3, obj[[4]])

    expect_error(obj[[5]], "subscript is out of bounds")
    expect_identical(NULL, obj[["da4"]]) # data.frame() behaves this way
    expect_error(obj[[-1]], "\\[\\[ subscript must be >= 1")
})

test_that("DelayedDataFrame as.list() works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    ddf <- DelayedDataFrame(x = letters, da1 = I(da1))

    idx <- 1:5
    exp <- list(x=letters[idx], da1 = da1[idx,,drop=FALSE])
    expect_identical(exp, as.list(ddf[idx,]))  # FAILS: 'package' attribute stripped
    expect_equivalent(exp, as.list(ddf[idx,]))

    idx <- c(1:5, 1:5)
    exp <- list(x=letters[idx], da1 = da1[idx,,drop=FALSE])
    expect_equivalent(exp, as.list(ddf[idx,]))
    
    idx <- integer()
    exp <- list(x=letters[idx], da1 = da1[idx,,drop=FALSE])
    expect_equivalent(exp, as.list(ddf[idx,]))
})

test_that("DelayedDataFrame as(., 'DataFrame') works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    ddf <- DelayedDataFrame(x = letters, da1 = I(da1))

    exp <- DataFrame(x = letters, da1 = I(da1))
    expect_identical(exp, as(ddf, "DataFrame"))

    idx <- 5:1
    exp <- DataFrame(x = letters[idx], da1 = I(da1[idx,,drop=FALSE]))
    expect_identical(exp, as(ddf[idx,], "DataFrame"))

    ddf <- DelayedDataFrame(x = letters, da1 = I(da1), row.names = letters)
    exp <- DataFrame(x = letters, da1 = I(da1), row.names = letters)
    expect_identical(exp, as(ddf, "DataFrame"))
})

test_that("DelayedDataFrame 1-dimensional [ works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(letters, da1=I(da1), da2=I(da2), da3 = I(da3))

    ## empty
    expect_identical(obj, obj[])

    ## numeric
    expect_identical(DelayedDataFrame(letters), obj[1])
    expect_identical(DelayedDataFrame(da1 = I(da1)), obj[2])
    expect_identical(DelayedDataFrame(letters, da1 = I(da1)), obj[1:2])
    expect_identical(DelayedDataFrame(da1 = I(da1), letters), obj[2:1])
    expect_identical(DelayedDataFrame(letters, da1 = I(da1)), obj[-(3:4)])

    ## character
    expect_identical(DelayedDataFrame(letters), obj["letters"])
    expect_identical(DelayedDataFrame(da1 = I(da1)), obj["da1"])
    expect_identical(
        DelayedDataFrame(letters, da1 = I(da1)),
        obj[c("letters", "da1")]
    )
    expect_identical(
        DelayedDataFrame(da1 = I(da1), letters),
        obj[c("da1", "letters")]
    )

    ## logical
    expect_identical(
        DelayedDataFrame(letters, da2 = I(da2)),
        obj[c(TRUE, FALSE)]
    )
})

test_that("DelayedDataFrame 2-dimensional [ works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(letters, da1=I(da1), da2=I(da2), da3 = I(da3))

    ## empty
    expect_identical(obj, obj[,])

    ## columns
    expect_identical(letters, obj[,1])
    expect_identical(DelayedDataFrame(letters), obj[, 1, drop=FALSE])
    
    expect_equivalent(da1, obj[,2])
    expect_equivalent(da2, obj[,3])
    expect_equivalent(da3, obj[,4])

    expect_equivalent(DelayedDataFrame(da1 = I(da1)), obj[,2, drop=FALSE])
    expect_equivalent(DelayedDataFrame(da2 = I(da2)), obj[,3, drop=FALSE])
    expect_equivalent(DelayedDataFrame(da3 = I(da3)), obj[,4, drop=FALSE])

    expect_equivalent(DelayedDataFrame(letters, da1 = I(da1)), obj[,1:2])
    expect_equivalent(DelayedDataFrame(da1 = I(da1), letters), obj[,2:1])

    ## column names
    expect_equivalent(
        DelayedDataFrame(letters, da1 = I(da1)),
        obj[,c("letters", "da1")]
    )

    ## logical
    expect_identical(
        DelayedDataFrame(letters, da2 = I(da2)),
        obj[,c(TRUE, FALSE)]
    )
    
    ## rows
    ddf <- function(i)
        DelayedDataFrame(
            letters = letters[i],
            da1 = I(da1[i,,drop = FALSE]),
            da2 = I(da2[i,, drop=FALSE]),
            da3 = I(da3[i,,,drop=FALSE])
        )
    idx <- 1
    expect_equivalent(ddf(idx), obj[idx,])
    idx <- 1:5
    expect_equivalent(ddf(idx), obj[idx,])
    idx <- sample(nrow(obj))
    expect_equivalent(ddf(idx), obj[idx,])

    ## rownames
    obj <- DelayedDataFrame(
        letters, da1=I(da1), da2=I(da2), da3 = I(da3),
        row.names = letters
    )
    idx <- sample(rownames(obj))
    expect_equivalent(ddf(match(idx, rownames(obj))), obj[idx,])

    ## logical
    idx <- c(TRUE, FALSE)
    expect_equivalent(ddf(idx), obj[idx,])
})

test_that("[<-,DelayedDataFrame 2-D subseting works", {
    da0 <- DelayedArray(array(1:26, 26))
    obj <- DelayedDataFrame(letters, da0 = I(da0))

    obj[1:5, 1] <- rev(letters[1:5])
    expect_identical(rev(letters[1:5]), obj[[1]][1:5])
    nullList <- .LazyIndex(vector("list", 1), index=rep(1L, length(obj)))
    expect_identical(nullList, lazyIndex(obj))

    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj <- obj[10:1,]
    obj[1:5, 1] <- rev(letters[1:5])
    exp <- .LazyIndex(list(NULL, 10:1), index=1:2)
    expect_identical(exp, lazyIndex(obj))
    expect_identical(letters[c(5:1, 5:1)], obj@listData[[1]])

    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj <- obj[10:1,]
    obj["da0"] <- da0[sample(10)]
    exp <- .LazyIndex(list(10:1, NULL), index=1:2)
    expect_identical(exp, lazyIndex(obj))

    ## TODO: subset-replace a DelayedArray column

})

test_that("[[<-,DelayedDataFrame is correct", {
    da0 <- DelayedArray(array(1:26, 26))
    obj <- DelayedDataFrame(letters, da0 = I(da0))

    nullList <- .LazyIndex(vector("list", 1), index=rep(1L, length(obj)))
    expect_identical(nullList, lazyIndex(obj))

    obj <- obj[1:10,]
    exp <- .LazyIndex(list(1:10), index=rep(1L, length(obj)))
    expect_identical(exp, lazyIndex(obj))

    obj0 <- obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj <- obj[1:10,]
    expect_identical(nullList, obj0@lazyIndex)

    ## .append_list_element
    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj0 <- obj <- obj[1:10,]
    obj[["x"]] <- da0[1:10]
    exp <- .LazyIndex(list(1:10, NULL), index = c(1L, 1L, 2L))
    expect_identical(exp, lazyIndex(obj))

    exp <- .LazyIndex(list(1:10), index = c(1L, 1L))
    expect_identical(exp, obj0@lazyIndex)

    ## .remove_list_element
    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj[["da0"]] <- NULL
    exp <- .LazyIndex(list(NULL), index = 1L)
    expect_identical(exp, lazyIndex(obj))

    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj <- obj[1:10,]
    obj[["da0"]] <- NULL
    exp <- .LazyIndex(list(1:10), index = 1L)
    expect_identical(exp, lazyIndex(obj))

    ## .replace_list_element
    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj[["da0"]] <- sample(obj[["da0"]])
    expect_identical(nullList, lazyIndex(obj))

    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj[["da0"]] <- da0[sample(nrow(obj))]
    expect_identical(nullList, lazyIndex(obj))

    obj <- DelayedDataFrame(letters, da0 = I(da0))
    obj <- obj[10:1,]
    obj[["da0"]] <- da0[sample(10)]
    exp <- .LazyIndex(list(10:1, NULL), index=1:2)
    expect_identical(exp, lazyIndex(obj))
})

test_that("cbind,DelayedDataFrame works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(letters, da1=I(da1), da2=I(da2), da3 = I(da3))

    expect_equivalent(obj, cbind(obj[1:2], obj[3:4]))
})

test_that("rbind,DelayedDataFrame works", {
    da0 <- DelayedArray(array(1:26, 26))
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))
    da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
    obj <- DelayedDataFrame(
        letters, da0 = I(da0), da1=I(da1), da2=I(da2), da3 = I(da3)
    )

    expect_equivalent(obj, rbind(obj[1:10,], obj[11:26,]))
})

## test_that("arbind,DelayedDataFrame works", {
##     da0 <- DelayedArray(array(1:26, 26))
##     da1 <- DelayedArray(matrix(1:26, 26, 1))
##     da2 <- DelayedArray(matrix(1:52, 26, 2))
##     da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
##     obj <- DelayedDataFrame(
##         letters, da0 = I(da0), da1=I(da1), da2=I(da2), da3 = I(da3)
##     )

##     expect_equivalent(obj, arbind(obj[1:10,], obj[11:26,]))
## })

## test_that("acbind,DelayedDataFrame works", {
##     da0 <- DelayedArray(array(1:26, 26))
##     da1 <- DelayedArray(matrix(1:26, 26, 1))
##     da2 <- DelayedArray(matrix(1:52, 26, 2))
##     da3 <- DelayedArray(array(1:26, c(26, 1, 1)))
##     obj <- DelayedDataFrame(
##         letters, da0 = I(da0), da1=I(da1), da2=I(da2), da3 = I(da3)
##     )

##     expect_equivalent(obj, acbind(obj[1:10,], obj[11:26,]))
## })

test_that("concatenateObjects,DelayedDataFrame works", {
    da1 <- DelayedArray(matrix(1:26, 26, 1))
    da2 <- DelayedArray(matrix(1:52, 26, 2))

    ddf1 <- DelayedDataFrame(da1=I(da1))[1:10, , drop=FALSE]
    ddf2 <- DelayedDataFrame(da2=I(da2))[10:1, , drop=FALSE]
    obj <- c(ddf1, ddf2)

    expect_identical(ddf1, c(ddf1))
    expect_identical(ddf2, c(ddf2))
    expect_identical(obj, c(obj))
    
    exp <- c(lazyIndex(ddf1), lazyIndex(ddf2))
    expect_identical(exp, lazyIndex(obj))

    obj2 <- c(ddf2, ddf1)
    expect_identical(obj[2:1], obj2)

    exp <- c(lazyIndex(ddf2), lazyIndex(ddf1))
    expect_identical(exp, lazyIndex(obj2))
    expect_identical(exp, lazyIndex(obj[2:1]))
})

test_that("extractROWS,DelayedDataFrame works", {
    da0 <- DelayedArray(array(1:26, 26))
    obj <- DelayedDataFrame(letters, da0=I(da0), row.names=letters)
    obj <- extractROWS(obj, c(TRUE, FALSE))

    expect_identical(obj@listData[[1]], letters)
    expect_equivalent(obj@listData[[2]], da0)  ## 'package' attribute stripped

    expect_identical(unlist(.listData(lazyIndex(obj))), (1:26)[c(TRUE,FALSE)])
    expect_identical(dim(obj), c(13L, 2L))
    expect_identical(rownames(obj), letters[c(TRUE,FALSE)])

})
test_that("validity,DelayedDataFrame works", {
    da0 <- DelayedArray(array(1:26, 26))
    obj <- DelayedDataFrame(letters, da0=I(da0))

    exp <- LazyIndex(.listData(lazyIndex(obj)), 1L)
    expect_error(initialize(obj, lazyIndex=exp), "subscript out of bounds")
})

test_that("lazyIndex<-,DelayedDataFrame works", {
    da0 <- DelayedArray(array(1:26, 26))
    obj <- DelayedDataFrame(letters, da0=I(da0))[1:10,]

    lazyIndex(obj) <- LazyIndex(list(10:1), rep(1L,2))
    expect_identical(obj[[1]], rev(letters[1:10]))

    idx <- sample(10)
    lazyIndex(obj) <- LazyIndex(list(idx), rep(1L,2))
    expect_identical(obj[[1]], letters[idx])
    expect_identical(obj[[2]], da0[1:10, drop=FALSE][idx, drop=FALSE])
})
