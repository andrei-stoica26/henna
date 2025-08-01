test_that("densityPlot returns a ggplot object", {
    x <- c(1, 1, 2, 7, 8, 0, 16, 1, 1.2, 32, 7, 5, 1.1, 1.2, 1.2)
    y <- c(1, 1.1, 0.5, 8, 18, 4, 6, 0.9, 1, 6, -24, -28, 1, 0.8, 0.9)
    z <- round(runif(15, 75, 100), 2)
    df <- data.frame(x, y, z)
    rownames(df) <- paste0('p', rownames(df))
    p <- densityPlot(df)
    expect_equal(is(p), 'gg')
})

test_that("hullPlot works", {
    pointsDF <- data.frame(x = c(1, 2, 4, 7, 10, 12, 13, 15, 16),
                           y = c(1, 1, 2, 3, 3, 2,1, 2, 1))
    expect_equal(is(hullPlot(pointsDF, 'Hull plot', 7, 1.5)), 'gg')
    expect_error(hullPlot(pointsDF, 'Hull plot', 1, 2))
    expect_equal(is(hullPlot(pointsDF, 'Hull plot', 4.1, 2)), 'gg')
})

test_that("networkPlot returns a ggraph object", {
    df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
                     gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
                     rank = c(1, 1, 3, 3, 3, 3))
    p <- networkPlot(df)
    expect_equal(is(p), 'ggraph')
})

test_that("radialPlot returns a gg object", {
    degreesDF <- data.frame(Protein = paste0('P', seq(20)),
                            Degree = sample(10, 20, replace=TRUE),
                            Group = sample(3, 20, replace=TRUE))
    p <- radialPlot(degreesDF)
    expect_equal(is(p), 'gg')
})

test_that("rankPlot returns a gg object", {
    df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
    rownames(df) <- paste('M', seq(10))
    colnames(df) <- paste('R', seq(30))
    p <- rankPlot(df)
    expect_equal(is(p), 'gg')
})

test_that("riverPlot returns a gg object", {
    df <- data.frame(x = sample(c('a','b', 'c', 'd', 'e', 'f'), 20,
                                replace=TRUE),
                     y = sample(c('p','q', 'r', 's', 't', 'u', 'v', 'w'), 20,
                                replace=TRUE),
                     z = runif(20, 1, 3))
    p <- riverPlot(df)
    expect_equal(is(p), 'gg')
})

test_that("connectedComponents works", {
    df <- data.frame(
        gene1 = paste0('G', c(1, 2, 6, 7, 8, 9,
                              11, 25, 32, 17, 18)),
        gene2 = paste0('G', c(2, 8, 8, 8, 1, 25,
                              32, 24, 24, 26, 26)))

    comps <- as.numeric(connectedComponents(df)$component)
    expect_identical(comps, c(rep(1, 5), rep(2, 4), 3, 3))
})

test_that("convexHull works", {
    pointsDF <- data.frame(a = c(1, 2, 2, 3, 3, 4, 5, 6, 8, 6, 7, 8, 6, 8, 10, 3, 1),
                           b = c(2, 3, 4, 8, 5, 6, 5, 4, 8, 11, 13, 14, 2, 1, 2, 14, 9))
    hull <- convexHull(pointsDF)
    rownames(hull) <- NULL
    expectedHull <- data.frame(x = c(10, 8, 1, 1, 3, 8),
                               y = c(2, 1, 2, 9, 14, 14))
    expect_identical(hull, expectedHull)
})
