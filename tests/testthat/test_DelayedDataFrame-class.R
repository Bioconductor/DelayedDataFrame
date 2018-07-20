context("DelayedDataFrame-class")

test_that("DelayedDataFrame constructor works", {
    obj <- DelayedDataFrame()
    expect_true(validObject(obj))
    expect_identical(c(0L, 0L), dim(obj))

    obj <- DelayedDataFrame(letters, LETTERS)
    expect_true(validObject(obj))
    expect_identical(c(26L, 2L), dim(obj))
    expect_identical(list(NULL, c("letters", "LETTERS")), dimnames(obj))

    ## check rownames.
    obj <- DelayedDataFrame(letters, LETTERS, row.names=LETTERS)
    expect_identical(LETTERS, rownames(obj))

    ## check column names.
    
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
    obj <- DelayedDataFrame(ddf1, ddf2)  ## FIXME, should return DDF not DF here. 

    exp <- LazyIndex(list(1:10, 10:1), 1:2)
    expect_identical(exp, lazyIndex(obj))
    
    ## DelayedDataFrame constructor over mix of DelayedDataFrame and others
    df2 <- DataFrame(ddf2)
    obj <- DelayedDataFrame(ddf1, df2, da2=I(da2[1:10,]))

    exp <- LazyIndex(list(NULL), rep(1L, 3))
    expect_identical(exp, lazyIndex(obj))

    ## exp <- list(NULL, c("da1", "da2", "da2.1"))
    ## expect_identical(exp, dimnames(obj))  ## FIXME: need to fix the "check.names=TRUE" argument here. 
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
