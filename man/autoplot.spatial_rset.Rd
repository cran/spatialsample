% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.spatial_rset}
\alias{autoplot.spatial_rset}
\alias{autoplot.spatial_block_cv}
\title{Create a ggplot for spatial resamples.}
\usage{
\method{autoplot}{spatial_rset}(object, ..., alpha = 0.6)

\method{autoplot}{spatial_block_cv}(object, show_grid = TRUE, ..., alpha = 0.6)
}
\arguments{
\item{object}{A \code{spatial_rset} object or a \code{spatial_rsplit} object.
Note that only resamples made from
\code{sf} objects create \code{spatial_rset} and \code{spatial_rsplit} objects;
this function will not work for
resamples made with non-spatial tibbles or data.frames.}

\item{...}{Options passed to \code{\link[ggplot2:ggsf]{ggplot2::geom_sf()}}.}

\item{alpha}{Opacity, passed to \code{\link[ggplot2:ggsf]{ggplot2::geom_sf()}}.
Values of alpha range from 0 to 1, with lower values corresponding to more
transparent colors.}

\item{show_grid}{When plotting \link{spatial_block_cv} objects, should the grid
itself be drawn on top of the data? Set to FALSE to remove the grid.}
}
\value{
A ggplot object with each fold assigned a color, made using
\code{\link[ggplot2:ggsf]{ggplot2::geom_sf()}}.
}
\description{
This method provides a good visualization method for spatial resampling.
}
\details{
The plot method for \code{spatial_rset} displays which fold each observation
is assigned to. Note that if data is assigned to multiple folds
(which is common if resamples were created with a non-zero \code{radius}) only
the "last" fold for each observation will appear on the plot.
Consider adding \code{ggplot2::facet_wrap(~ fold)} to visualize all members of
each fold separately.
Alternatively, consider plotting each split using the \code{spatial_rsplit} method
(for example, via \code{lapply(object$splits, autoplot)}).
}
\examples{

boston_block <- spatial_block_cv(boston_canopy, v = 2)
autoplot(boston_block)
autoplot(boston_block$splits[[1]])

}
