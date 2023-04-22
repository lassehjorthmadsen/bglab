# plumber_api.R

#' @apiTitle `ggboard()` function to generate backgammon positions
#' @apiDescription Expose `ggboard()` from `backgammon` as an API endpoint.

# Import required packages
library(backgammon)
library(gridExtra)
library(ggplot2)

#* @param xgid Input parameter for ggboard
#* @serializer png
#* @get /ggboard
function(req, res, xgid) {
  # Call ggboard with the provided xgid
  encoded_xgid <- URLencode(xgid)
  plot <- ggboard(encoded_xgid)

  # Create a temporary file to store the PNG
  tmpfile <- tempfile(fileext = ".png")

  # Save the ggplot object as a PNG
  ggsave(filename = tmpfile, plot = plot, device = "png")

  # Read the PNG file and return it as a binary response
  res$setHeader("Content-Type", "image/png")
  res$body <- readBin(tmpfile, "raw", n = file.info(tmpfile)$size)
  return(res)
}
