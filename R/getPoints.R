#' getPoints
#' @description
#' Function lets the user identify points on a displayed plot.
#'
#' @details
#' TBD
#'
#' @param locations number of points to be selected
#' @param type the type of points you will be selecting. Can be either `known` or `unknown`. If `known`, the user is asked for coordinates after selecting each point.
#'
#' @returns a dataframe of xy coordinates. If `type = known`, the dataframe can be used to calculate a matrix transformation to reconcile two coordinate systems.
#' @export
#'
#' @examples
#' getPoints
#'
getPoints <- function(locations = 3, type = 'known'){
  if(!grepl(x = tolower(trimws(type)), pattern = '^known$|^unknown$')) stop('`type` argument not recognized in `getPoints`')

  cat('\n')
  if(grepl(x = tolower(trimws(type)), pattern = '^unknown$')) {
    for (i in 1:locations){
      message(paste0('Select an unknown point (', i, ' of ', locations,') for conversion.'))
      img_1 <- terra::click(n = 1)
      if (i == 1) {
        outdf <- img_1
      } else {
        outdf <-  rbind(outdf, img_1)
      }
    }
    row.names(outdf) <- 1:nrow(outdf)
  } else if(grepl(x = tolower(trimws(type)), pattern = '^known$')) {
    for (i in 1:locations){
      img_1 <- terra::click(n = 1)
      if (interactive()){
        known1 <- readline(paste0("What are the coordinates of known point ", i, "? Format must be `x,y`?"))
      } else{
        #  non-interactive
        cat(paste0("What are the coordinates of known point ", i, "? Format must be `x,y`?"));
        known1 <- readLines("stdin",n=10)
      }
      cat('\n')

      ### look for c() notation, add it if raw coordinates were provided
      if(!grepl(x = known1, pattern = '^c(.*)$')) {
        known1 <- paste0('c(', trimws(known1), ')')
      }
      cat('input received: ', known1, '\n')

      known1xy <- eval(parse(text = trimws(known1)))
      names(known1xy) <- c('x', 'y')
      if (i == 1) {
        outlist <- list(figure_coords = img_1,
                        known_coords  = known1xy
        )
      } else {
        outlist$figure_coords <-  rbind(outlist$figure_coords, img_1)
        outlist$known_coords  <-  rbind(outlist$known_coords, known1xy)
      }
    }
    row.names(outlist$figure_coords) <- 1:nrow(outlist$figure_coords)
    row.names(outlist$known_coords) <- 1:nrow(outlist$known_coords)
    outdf <- data.frame(do.call(cbind, Map(cbind, outlist)))
    names(outdf) <- c('x_arb', 'y_arb', 'x_true', 'y_true')
  }

  invisible(outdf)
}
