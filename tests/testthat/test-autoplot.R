skip_if_not_installed("modeldata")
skip_if_not_installed("vdiffr")

data(ames, package = "modeldata")

test_that("autoplot is stable", {
  skip_if_not(sf::sf_use_s2())

  ames_sf <- sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
  set.seed(123)
  ames_cluster <- spatial_clustering_cv(ames_sf)

  p <- autoplot(ames_cluster)
  vdiffr::expect_doppelganger("cluster plots", p)

  p <- autoplot(ames_cluster$splits[[1]])
  vdiffr::expect_doppelganger("cluster split plots", p)

  skip_if_not(sf::sf_use_s2())
  set.seed(123)
  ames_block <- spatial_block_cv(ames_sf)

  p <- autoplot(ames_block, show_grid = FALSE)
  vdiffr::expect_doppelganger("block plots", p)

  p <- autoplot(ames_block)
  vdiffr::expect_doppelganger("block plots with grid", p)

  p <- autoplot(ames_block$splits[[1]])
  vdiffr::expect_doppelganger("block split plots", p)

  skip_if_not_installed("curl")
  skip_if_offline()
  sf::sf_proj_network(enable = TRUE)

  set.seed(123)
  boston_buffer <- spatial_block_cv(boston_canopy, buffer = 5000, radius = NULL)

  p <- autoplot(boston_buffer)
  vdiffr::expect_doppelganger("buffered rset plot", p)

  p <- autoplot(boston_buffer$splits[[1]])
  vdiffr::expect_doppelganger("buffered rsample plot", p)

  set.seed(123)
  boston_vfold_buffer <- spatial_buffer_vfold_cv(
    boston_canopy,
    v = 10,
    buffer = 5000,
    radius = NULL
  )

  p <- autoplot(boston_vfold_buffer)
  vdiffr::expect_doppelganger("buffered vfold plot", p)

  set.seed(123)
  boston_vfold_buffer <- spatial_buffer_vfold_cv(
    boston_canopy,
    v = 682,
    radius = 1,
    buffer = 5000
  )

  # chose the fourth split purely because it looks cool
  p <- autoplot(boston_vfold_buffer$splits[[4]])
  vdiffr::expect_doppelganger("buffered vfold split", p)

  set.seed(123)
  ames_neighborhoods <- spatial_leave_location_out_cv(ames_sf, Neighborhood)

  p <- autoplot(ames_neighborhoods)
  vdiffr::expect_doppelganger("buffered LLO set plot", p)

  p <- autoplot(ames_neighborhoods$splits[[1]])
  vdiffr::expect_doppelganger("buffered LLO split plot", p)

  # Not setting seed because this _should_ be deterministic
  boston_snake <- spatial_block_cv(
    boston_canopy,
    v = 10,
    method = "snake",
    relevant_only = FALSE,
    n = c(10, 23)
  )
  p <- autoplot(boston_snake)
  vdiffr::expect_doppelganger("snake flips rows the right way", p)

  set.seed(123)
  repeat_block <- spatial_block_cv(
    boston_canopy,
    v = 10,
    method = "random",
    repeats = 2
  )
  vdiffr::expect_doppelganger(
    "repeated block CV",
    autoplot(repeat_block)
  )

  set.seed(123)
  repeat_vfold <- spatial_buffer_vfold_cv(
    boston_canopy,
    radius = 1,
    buffer = 4000,
    repeats = 2
  )
  vdiffr::expect_doppelganger(
    "repeated vfold",
    autoplot(repeat_vfold)
  )

  set.seed(123)
  repeat_llo <- spatial_leave_location_out_cv(
    ames_sf,
    Neighborhood,
    repeats = 2,
    v = 10
  )
  vdiffr::expect_doppelganger(
    "repeated LLO",
    autoplot(repeat_llo)
  )
})

test_that("autoplot respects expand_bbox", {
  vdiffr::expect_doppelganger(
    "expand_bbox",
    autoplot(
      spatial_block_cv(boston_canopy, expand_bbox = 0.5, v = 4)
    )
  )
})
