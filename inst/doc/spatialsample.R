## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5.75,
  eval = requireNamespace("gifski", quietly = TRUE) && requireNamespace("modeldata", quietly = TRUE) && sf::sf_use_s2()
)

## ---- include = FALSE---------------------------------------------------------
library(ggplot2)
theme_set(theme_minimal())

## -----------------------------------------------------------------------------
data("ames", package = "modeldata")

## -----------------------------------------------------------------------------
ames_sf <- sf::st_as_sf(
  ames,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude", "Latitude"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

## ---- eval = FALSE------------------------------------------------------------
#  log10(Sale_Price) ~ Year_Built + Gr_Liv_Area + Bldg_Type

## -----------------------------------------------------------------------------
library(spatialsample)

set.seed(123)
cluster_folds <- spatial_clustering_cv(ames_sf, v = 15)

autoplot(cluster_folds)

## -----------------------------------------------------------------------------
cluster_folds

## -----------------------------------------------------------------------------
set.seed(123)
block_folds <- spatial_block_cv(ames_sf, v = 15)

autoplot(block_folds)

## -----------------------------------------------------------------------------
set.seed(123)
location_folds <-
  spatial_leave_location_out_cv(
    ames_sf,
    group = Neighborhood,
    v = 15
  )

autoplot(location_folds)

## -----------------------------------------------------------------------------
cluster_folds$type <- "cluster"
block_folds$type <- "block"
location_folds$type <- "location"

resamples <-
  dplyr::bind_rows(
    cluster_folds,
    block_folds,
    location_folds
  )

## -----------------------------------------------------------------------------
# `splits` will be the `rsplit` object
compute_preds <- function(splits) {
  # fit the model to the analysis set
  mod <- lm(log10(Sale_Price) ~ Year_Built + Bldg_Type * log10(Gr_Liv_Area),
    data = analysis(splits)
  )
  # identify the assessment set
  holdout <- assessment(splits)
  # return the assessment set, with true and predicted price
  tibble::tibble(
    geometry = holdout$geometry,
    Sale_Price = log10(holdout$Sale_Price),
    .pred = predict(mod, holdout)
  )
}

## -----------------------------------------------------------------------------
compute_preds(cluster_folds$splits[[7]])

## -----------------------------------------------------------------------------
library(purrr)
library(dplyr)

cv_res <- resamples %>%
  mutate(.preds = map(splits, compute_preds))

## -----------------------------------------------------------------------------
library(tidyr)
library(yardstick)

cv_rmse <- cv_res %>%
  unnest(.preds) %>%
  group_by(id, type) %>%
  rmse(Sale_Price, .pred)

cv_rmse

## ----fig.height=12------------------------------------------------------------
library(ggplot2)

cv_res %>%
  unnest(.preds) %>%
  left_join(cv_rmse, by = c("id", "type")) %>%
  ggplot(aes(color = .estimate)) +
  geom_sf(aes(geometry = geometry), alpha = 0.5) +
  labs(color = "RMSE") +
  scale_color_viridis_c() +
  facet_wrap(vars(type), ncol = 1)

