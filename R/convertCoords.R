#' convertCoords
#' @description
#' Converts coordinates between coordinate systems, using a transform matrix produced by `getTransform`
#'
#'
#' @param inputData dataframe with coordinates in the arbitrary coordinate system
#' @param transformMatrix transformation matrix, produced by `getTransform`.
#' @param colNames vector of length 2. Name of columns that will receive data.
#'
#' @returns dataframe with coordinates converted to the destination coordinate system.
#' @export
#'
#' @examples convertCoords

convertCoords <- function (inputData, # dataframe of unknown values
                       transformMatrix, # from genTransform
                       colNames = c('x', 'y')) {
  xyPoints = inputData
  # add columns to receive data
  inputData[, colNames[2]] <- inputData[, colNames[1]] <- NA

  xyPoints$fill.col <- 1
  if (!nrow(xyPoints) == 1) {
    for (i in 1:nrow(xyPoints)) {
      outputTemp <- transformMatrix %*% as.numeric(xyPoints[i, ])
      inputData[i, colNames[1]] <- outputTemp[1]
      inputData[i, colNames[2]] <- outputTemp[2]
    }
  } else {
    outputTemp <- transformMatrix %*% as.numeric(xyPoints)
    inputData[colNames[1]] <- outputTemp[1]
    inputData[colNames[2]] <- outputTemp[2]
  }

  return(inputData)
}
