

#' datathief
#'
#' @param png_address file address of png file (screenshot of data). Currently this must be a png.
#' @param type type of process to run, either `auto` (all data in graph space pulled) or `manual` (only user-defined points are collected).
#' @param unknown_points *only used if `type = manual`. This is the initial number of unknown values to estimate. function will ask if you have more data to enter.
#' @param buffer_size For future use. buffer size around pixel clusters before creating a new data point.
#' @param layer_to_use  layer of img to use for clustering. Adjusting this *may* help if clustering isn't working well.
#' @param solid_lines logical. should every data point be reported, or should 'clumps' be aggregated? Setting this to TRUE can be helpful if the image contains solid lines that you want data from. If TRUE, all data in the plot space is provided. Note that if FALSE, data can be lost if the centroid falls on empty graph space.
#' @param x_interval decimal places to round the x axis values to. if this is a numeric value, e.g., 3 (i.e., x interval is 0.001), it can help avoid having thousands of data points for a line. if all data is desired, set to 'max'.
#'
#' @returns a dataframe
#' @export
#'
#' @examples datathief
#' \dontrun{
#' ### manual mode works but can be labor intensive
#' out_man <- datathief(png_address =
#'     system.file('extdata', 'example_data2.png', package = 'datathief'),
#'     type = 'manual')
#' ### using auto can be helpful if there's not debris on the plot
#' out_auto <- datathief(
#'     system.file('extdata', 'example_data2.png', package = 'datathief'),
#'     type = 'auto')
#' ### we can do lines too, kind of.
#' ### setting `x_interval = max` gives a full-resolution data dump.
#' out_line <- datathief(
#'     system.file('extdata', 'example_data3.png', package = 'datathief'),
#'     type = 'auto',  solid_lines = TRUE, x_interval = 'max')
#' plot(y ~ x, col = group, pch = 19, data = out_line, las = 1, cex = 0.2)
#'
#' ### using `x_interval = [positive integer]` aggregates the points somewhat
#' out_line <- datathief(
#'     system.file('extdata', 'example_data3.png', package = 'datathief'),
#'     type = 'auto',  solid_lines = TRUE, x_interval = 1)
#' plot(y ~ x, col = group, pch = 19, data = out_line, type = 'l')
#' }
#'

datathief <- function(png_address = system.file('extdata', 'example_data2.png', package = 'datathief'),
                           type = 'manual',
                      unknown_points = 10, # initial number of unknown values to estimate. function will ask if you have more data to enter.
                      buffer_size = 0, # buffer size around pixel clusters, before creating a new data point.
                           layer_to_use = 1, # experimental. layer of img to use for clustering. May help if clustering isn't working well.
                           solid_lines = FALSE,
                           x_interval = 'max' # decimal places to round the x axis values to. if this is a numeric value, e.g., 0.001, it can help avoid having thousands of data points for a line. if all data is desired, set to 'max'
) {
  # load image
  input_file <- png_address
  img        <- png::readPNG(input_file)
  ### convert to raster
  img2 <- terra::rast(img)[[layer_to_use]]

  ### create plot
  # plot(0:200, type='n', xaxs = 'i', yaxs = 'i', yaxt = 'n', xaxt = 'n',  bty="n", ylab = '', xlab = '')
  # graphics::rasterImage(img, 0, 0, 200, 200)
  terra::plot(img2, col = terra::map.pal("sepia", 100), axes = F)

  ### get known points
  cat('\nSelect three known coordinates. Typically these are the origin, ymax, and xmax\n')
  known_vals   <- getPoints(locations = 3, type = 'known')

  ### set transform matrix
  transform_mat <- getTransform(knownData = known_vals, y.dest = "y_true", x.dest = "x_true", # columns with known x and y data
                                y.arb = "y_arb", x.arb = "x_arb",
                                n = 3)


  if (all(grepl(x = trimws(tolower(type)), pattern = 'manual'))) {
    if (exists('unknown_vals_fin')) rm('unknown_vals_fin')
    needed_points <- unknown_points
    while(needed_points > 0) {
      ### get unknown points
      unknown_vals1 <- getPoints(locations = needed_points, type = 'unknown')

      if(!exists('unknown_vals_fin')) {
        unknown_vals_fin <- unknown_vals1
      } else if(exists('unknown_vals_fin')) {
        unknown_vals_fin <- rbind(unknown_vals1, unknown_vals_fin)
      }
      ### check if there's a need for more unknown data points
      needed_points <- as.numeric(makeDialogBox(vars = c('How many additional unkown data points are needed? Enter `0` if you are done.')))
      if (is.na(needed_points)) {needed_points <- 0}
    }
    df2 <- data.frame(unknown_vals_fin[, c('x', 'y')])
    df2$group <- 1

  } else if (all(grepl(x = trimws(tolower(type)), pattern = 'auto'))) {
    ### auto mode: user identifies axis limits and values, then all data in plotting space is analyzed
    ### extract all pixel values as matrix, convert white (or most common color) to NA, get cluster centroids (x,y, color)
    # hist(img2) # main value clusters. 1 always white/background?

    ### this could cause issues, but helps remove plot background color, which will otherwise be identified as a clump
    img2[img2 > 0.99] <- NA

    ### TODO: replace with terra functions
    #tst <- terra::patches(img2) # crashes R, but is a known issue under active development (last mod 2 days ago)
    tst <- raster::clump(raster::raster(img2), gaps = FALSE)  # loads igraph
    clump_id <- raster::getValues(tst)
    xy <- raster::xyFromCell(tst,1:raster::ncell(tst))
    df <- data.frame(xy, clump_id, is_clump = tst[] %in% raster::freq(tst, useNA = 'no')[,1])
    ### end TODO

    # plot(tst)
    # text(df[df$is_clump == T, 1:2], labels = df[df$is_clump == T, 3]) # lines show up here
    ### Remove clumps outside axes (defined by known_vals)
    df <- df[df$x > (min(known_vals$x_arb) + 5), ]
    df <- df[df$y > (min(known_vals$y_arb) + 5), ]
    ### extract initial values (color)
    df2 <- df[df$is_clump == T, c('clump_id', 'x','y')]
    df2$group <- terra::extract(img2, df2[, 2:3])[, 2]
    df2 <- df2[!is.na(df2$group), ] # remove clumps falling on NAs in original plot

    if (solid_lines == TRUE) {
      ### for lines, clump_id performs better than group (100+ groups for one color line)
      ### will likely still require post-processing.
      df2$group <- df2$clump_id
    }

    if (solid_lines == FALSE) {
      ### if no solid lines, get clump centroids
      df2 <- plyr::ddply(df2, c('clump_id'), plyr::summarise,
                         x = mean(x),
                         y = mean(y),
                         group = mean(group))
    }
    df2$group <- factor(df2$group, labels = 1:length(unique(df2$group)))

    ### diagnostic: visualize clumps
    # terra::plot(tst) # plot(img2)
    # if (solid_lines == FALSE) {
    #     terra::text(df2[, 2:3], labels = df2$group)
    # }
    # if (solid_lines == TRUE) {
    #   terra::points(df2[, 2:3], col = df2$clump_id, pch = 19) # group meaningless for lines
    # }
  } else stop('`type` not recognized. try `manual` or `auto`.\n')

  ### transform all data
  outDat <- convertCoords(inputData   = df2[, c('x', 'y')],
                          transformMatrix = transform_mat,
                          colNames        = c('x', 'y'))
  outDat$group <- df2$group

  ### some final processing for auto-detected data
  if (all(grepl(x = trimws(tolower(type)), pattern = 'auto'))) {
    if (!grepl(x = tolower(as.character(x_interval)), pattern = 'max')) {
      ### if x-axis conservation is desired
      if (is.numeric(x_interval)) {
        outDat$x <- round(outDat$x, x_interval)
        outDat   <- plyr::ddply(outDat, c('x'), plyr::summarise,
                                x = mean(x),
                                y = mean(y),
                                group = utils::head(sort(group, decreasing = TRUE),1))
      }
    }
    if (!(buffer_size == 0)) {
      ### if x-axis conservation is desired
      if (is.numeric(buffer_size)) {
        #
      }
    }
  }

  outDat           <- outDat[order(outDat$group, outDat$x), ]
  rownames(outDat) <- 1:nrow(outDat)

  plot(y ~ x, col = group, pch = 19, data = outDat, ylab = '', xlab = '', main = 'Extracted data', las = 1)

  # img3 <- convert img2 to new coordinate system

  invisible(#list(data =
                   outDat#,
             #    plot = img3) # TODO: provide a converted image
            )
}

# ### manual mode works but could be labor intensive
# out_man <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data2.png",
#                   type = 'manual')
# out_auto <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data2.png",
#                      type = 'auto')
# out_line <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data3.png",
#                        type = 'auto',  solid_lines = TRUE, x_interval = 'max') # use 'max' to have a full-resolution data dump
# plot(y ~ x, col = group, pch = 19, data = out_line, ylab = '', xlab = '', main = 'Extracted data', las = 1, cex = 0.2)
# out_line <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data3.png",
#                        type = 'auto',  solid_lines = TRUE, x_interval = 1) # x_interval aggregates the points somewhat
# plot(y ~ x, col = group, pch = 19, data = out_line, ylab = '', xlab = '', main = 'Extracted data', las = 1, type = 'l')


#
# ### this looks like shit - tons of points identified. could be resolved by implementing  buffer
# out1 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data2.png",
#                   type = 'auto',  solid_lines = TRUE)
# ### Fixed by using solid_lines = FALSE (averages by clump ID; doesn't report every data point)
# out1 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data2.png",
#                   type = 'auto',  solid_lines = FALSE)
#
# # shows that dashed trend line registers as a bunch of points with different groups (blurring causes color differences, I guess)
# out2 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data.png")
# ### some low res figures will alwyas looks like garbage with auto detection.
# out2 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data.png",
#                        solid_lines = TRUE) # solid_lines argument just dumps every feature it detects, including duplication of clumps
# # adjusting x_interval reduces the data detected but can include artifacts
# out3 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data3.png",
#                        solid_lines = TRUE, x_interval = 2) # if there aren't dramatic changes in the dataset setting x_interval to the # of decimal places you want can help reduce the dataset size.
# ### works much better
# out4 <- datathief(png_address = "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/Documents/RDATA/datathief_exp/example_data3.png",
#                        solid_lines = TRUE, x_interval = 'max') # use 'max' to have a full-resolution data dump
