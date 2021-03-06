library(testthat)
library(rsample)
library(purrr)
library(modeldata)

data("Smithsonian")


test_that('default param', {
    set.seed(11)
    rs1 <- spatial_clustering_cv(Smithsonian,
                                 coords = c(latitude, longitude),
                                 v = 2)
    sizes1 <- dim_rset(rs1)

    expect_true(all(sizes1$analysis + sizes1$assessment == 20))
    same_data <-
        map_lgl(rs1$splits, function(x)
            all.equal(x$data, Smithsonian))
    expect_true(all(same_data))

    good_holdout <- map_lgl(rs1$splits,
                            function(x) {
                                length(intersect(x$in_ind, x$out_id)) == 0
                            })
    expect_true(all(good_holdout))
})


test_that('bad args', {
    expect_error(spatial_clustering_cv(Smithsonian, coords = NULL))
    expect_error(spatial_clustering_cv(Smithsonian, coords = c(Species, Sepal.Width)))
})

test_that('can pass the dots to kmeans', {
    expect_error(spatial_clustering_cv(Smithsonian,
                                       coords = c(latitude, longitude),
                                       v = 2,
                                       algorithm = "MacQueen"),
                 NA)
})

test_that('printing', {
    expect_snapshot_output(
        spatial_clustering_cv(Smithsonian,
                              coords = c(latitude, longitude),
                              v = 2)
    )
})

test_that('rsplit labels', {
    rs <- spatial_clustering_cv(Smithsonian, coords = c(latitude, longitude), v = 2)
    all_labs <- map_df(rs$splits, labels)
    original_id <- rs[, grepl("^id", names(rs))]
    expect_equal(all_labs, original_id)
})
