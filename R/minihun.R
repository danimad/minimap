#' Make a tile grid map of Hungary
#' based on: https://github.com/seankross/minimap
#' changed the minicanada function
#'
#' @param pt A vector of Hungarian counties, Budapest and abbreviations.
#' This vector must be some permutation of \code{hungary_abb}.
#' @param pt_colors A vector of "colors" in the R sense. For example strings
#'  (\code{"blue"}), hex codes (\code{"#D0C7B9"}), etc. The ith color in this
#'  vector will be the color of square that represents the ith element of
#'  \code{pt}.
#' @param border_colors Like \code{pt_colors} but specifying the border
#'  of the square.
#' @param pt_names Should the postal codes for each province or territory be
#' displayed in the center of the province or territory? The default value is
#' \code{TRUE}.
#' @param pt_name_colors Like \code{pt_colors} but specifying the color
#'  of the text displayed in each province or territory.
#' @param pt_name_cex The size of the text displayed inside of each province or
#'  territory.
#' @param font The font of the text displayed inside of each province or
#'  territory. The values \code{"serif"}, \code{"sans"}, and \code{"mono"} are
#'  safest to use. Use other fonts at your own risk. If \code{NULL} a
#'  sans-style font will be used.
#'
#' @export
#' @examples
#' \dontrun{
#'  minihun(hungary_abb, 1:20)
#' }
minihun <- function(pt, pt_colors, border_colors = rep("white", 20),
                    pt_names = TRUE, pt_name_colors = rep("white", 20),
                    pt_name_cex = 1, font = NULL){
  # Make sure all parameters are specified
  if(any(unlist(lapply(list(
    pt, pt_colors, border_colors, pt_name_colors
  ), length)) != 20)){
    stop("Please make sure parameters contain exactly 20 elements.")
  }

  map <- data.frame(State = c("NO", "BO", "GY", "KO", "BP", "HE", "HA", "SZ",
                              "VA", "VE", "FE", "PE", "JÁ", "BÉ", "ZA", "SO",
                              "TO", "BÁ", "CS", "BA"),
                    X = c(4:5, 1:6, 0:5, 0:4, 2),
                    Y = c(rep(4, 2), rep(3, 6), rep(2, 6), rep(1, 5), rep(0, 1)),
                    stringsAsFactors = FALSE)

  # Make sure all counties are present
  if(!all(map$State %in% pt)){
    stop("It appears some counties names are repeated or missing.")
  }

  user_map <- data.frame(State = pt, scol = pt_colors, bcol =
                           border_colors, sncol = pt_name_colors,
                         stringsAsFactors = FALSE)

  map <- merge(map, user_map, by = "State")

  text_ <- NULL
  if(pt_names){
    text_ <- map$State
  }

  plotbox(map$X, map$Y, map$scol, map$bcol,
          n_xboxes = 7, n_yboxes = 5, text_ = text_,
          text_col = map$sncol, text_cex = pt_name_cex, text_font = font)

}