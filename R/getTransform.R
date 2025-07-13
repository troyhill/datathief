#' getTransform
#' @description
#' Generates a transform function that converts between two coordinate systems.
#'
#' @param knownData a dataframe with data points with known locations in both coordinate systems, created by getPoints
#' @param x.dest name of column with x coordinates in destination coordinate system
#' @param y.dest name of column with y coordinates in destination coordinate system
#' @param x.arb name of column with x coordinates in the arbitrary coordinate system
#' @param y.arb name of column with y coordinates in the arbitrary coordinate system
#' @param n a number. don't adjust this.
#'
#' @returns a matrix
#' @export
#'
#' @examples getTransform
getTransform <- function(knownData, # three axis points (origin, ymax, xmax) defined via input_known_points = known_vals
                         x.dest = "x_true", y.dest = "y_true", # columns with known x and y data
                         x.arb = "x_arb", y.arb = "y_arb",
                         n = 3) {
  tiePoints <- knownData[1:n, ]

  # re-format to get the datasets in the same order
  tiePoints$fill.col <- 1
  tiePoints$x.un <- tiePoints$y.un <- NA
  # get known coordinates
  u <- as.matrix(tiePoints[c(x.arb, y.arb, "fill.col")])
  k <- as.matrix(tiePoints[, c(x.dest, y.dest, "fill.col")])

  A <- t(solve(u, k)) # matches matlab output
}
