context("LazyIndex")

test_that(".lazyIndex_inuse works", {
    ## 0. No updates
    ll <- .LazyIndex(list(1:10, 10:1, 11:20), index=1:3)
    expect_identical(ll, .lazyIndex_compose(ll@listData, .index(ll)))

    ## 1. duplicate listData
    ll <- .LazyIndex(list(1:10, 10:1, 1:10), index=1:3)
    exp <- .LazyIndex(list(1:10, 10:1), index = c(1L, 2L, 1L))
    expect_identical(exp, .lazyIndex_compose(ll@listData, .index(ll)))

    ll <- .LazyIndex(list(1:10, 10:1, 1:10), index=c(1:3, 3L))
    exp <- .LazyIndex(list(1:10, 10:1), index = c(1L, 2L, 1L, 1L))
    expect_identical(exp, .lazyIndex_compose(ll@listData, .index(ll)))

    ## 2. listData in not in use index
    ll <- .LazyIndex(list(1:10, 10:1, 11:20), index=1:3)
    ll@index <- c(1L, 3L, 3L)
    exp <- .LazyIndex(list(1:10, 11:20), index = c(1L, 2L, 2L))
    expect_identical(exp, .lazyIndex_compose(ll@listData, .index(ll)))

    ## 3. reorder
    ll <- .LazyIndex(list(1:10, 10:1, 11:20), index=c(3L, 2L, 1L))
    exp <- .LazyIndex(list(11:20, 10:1, 1:10), index=1:3)
    expect_identical(exp, .lazyIndex_compose(ll@listData, .index(ll)))
})

test_that(".update_index works", {
    ll <- .LazyIndex(list(1:10, 10:1), index=1:2)

    lazyIndex <- sample(1:10)
    exp <- .LazyIndex(list(lazyIndex, 10:1), index=1:2)
    expect_identical(exp, .update_index(ll, 1, lazyIndex))

    exp <- .LazyIndex(list(10:1), index=c(1L, 1L))
    expect_identical(exp, .update_index(ll, 1, 10:1))

    exp <- .LazyIndex(list(NULL, 10:1), index=1:2)
    expect_identical(exp, .update_index(ll, 1, NULL))
})

test_that(".update_row works", {
    ll <- LazyIndex(list(1:10, 10:1, 11:20), index=1:3)
    exp <- LazyIndex(list(1:5, 10:6, 11:15), index = 1:3)
    expect_identical(exp, .update_row(ll, 1:5))

    ll <- LazyIndex(list(1:10, NULL), index=1:2)
    exp <- LazyIndex(list(1:5), index=rep(1L, 2))
    expect_identical(exp, .update_row(ll, 1:5))
    
    exp <- LazyIndex(list(integer(0)), index = rep(1L, 2))
    expect_identical(exp, .update_row(ll, integer(0)))
})

test_that(".validate_LazyIndex works", {
    ll <- LazyIndex(list(1:10, NULL), index=1:2)
    expect_true(validObject(ll))
    expect_true(validObject(LazyIndex()))
    expect_error(LazyIndex(list(1:10, 1:15), index=1:2))
    expect_error(LazyIndex(list(1:2, 2:1), index=1L))
})

test_that("[ subsetting for LazyIndex works", {
    ll <- LazyIndex(list(1:10, NULL), index=1:2)
    exp <- LazyIndex(list(1:5), index=1L)
    expect_identical(exp, ll[1:5, 1])

    exp <- LazyIndex(list(1:5), index=rep(1L, 2))
    expect_identical(exp, ll[1:5,])

    exp <- LazyIndex(list(1:5), index=1L)
    expect_identical(exp, ll[1:5, 1])
    
    expect_identical(ll[1], ll[,1])
    exp <- LazyIndex(list(1:10), index=1L)
    expect_identical(exp, ll[1])

    exp <- LazyIndex(list(NULL), index=1L)
    expect_identical(exp, ll[2])
})

test_that("concatenateObjects for LazyIndex works", {
    ll <- LazyIndex(list(1:10, NULL), index=1:2)

    expect_identical(ll, c(ll))

    exp <- LazyIndex(list(1:10, NULL), index=rep(1:2, 2))
    expect_identical(exp, c(ll, ll))

    exp <- LazyIndex(list(1:10, NULL), index=rep(1:2, 3))
    expect_identical(exp, c(ll, ll, ll))

    ll1 <- LazyIndex(list(NULL, 1:10), index=1:2)
    exp <- LazyIndex(list(1:10, NULL), index=c(1:2, 2:1))
    expect_identical(exp, c(ll, ll1))

    ll1 <- LazyIndex(list(1:5), index=rep(1L, 2))
    expect_error(c(ll, ll1))
})

