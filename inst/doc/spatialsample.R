## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
theme_set(theme_minimal())

## -----------------------------------------------------------------------------
data("ames", package = "modeldata")

## ---- eval = FALSE------------------------------------------------------------
#  log10(Sale_Price) ~ Year_Built + Gr_Liv_Area +  Bldg_Type

## -----------------------------------------------------------------------------
library(spatialsample)

set.seed(123)
folds <- spatial_clustering_cv(ames, coords = c("Latitude", "Longitude"), v = 15)
folds

## -----------------------------------------------------------------------------
# `splits` will be the `rsplit` object
compute_preds <- function(splits) {
  # fit the model to the analysis set
  mod <- lm(log10(Sale_Price) ~ Year_Built + Bldg_Type * log10(Gr_Liv_Area), 
            data = analysis(splits))
  # identify the assessment set
  holdout <- assessment(splits)
  # return the assessment set, with true and predicted price
  tibble::tibble(Longitude = holdout$Longitude, 
                 Latitude = holdout$Latitude,
                 Sale_Price = log10(holdout$Sale_Price), 
                 .pred = predict(mod, holdout))
}

## -----------------------------------------------------------------------------
compute_preds(folds$splits[[7]]) 

## -----------------------------------------------------------------------------
library(purrr)
library(dplyr)

cv_res <- folds %>%
  mutate(.preds = map(splits, compute_preds))

cv_res

## -----------------------------------------------------------------------------
library(tidyr)
library(yardstick)

cv_rmse <- cv_res %>%
  unnest(.preds) %>%
  group_by(id) %>%
  rmse(Sale_Price, .pred)

cv_rmse

## ----fig.width=7, fig.height=5------------------------------------------------
library(ggplot2)

cv_res %>%
  unnest(.preds) %>%
  left_join(cv_rmse) %>%
  ggplot(aes(Longitude, Latitude, color = .estimate)) + 
  geom_point(alpha = 0.5) +
  labs(color = "RMSE") +
  scale_color_viridis_c()

