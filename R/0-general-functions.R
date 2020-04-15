#' This finds the closest date to a given date that exists in a dataset.
#'
#' @param data A data frame that includes a column `timestamp`
#' @param date The date you want to match`
#' @param type Default "all", which returns closest date overall. Other options are "before", "after", "beforeequal", and "afterequal"`
#' @param force_val Default True. If True, then the function will return the "all" type if there is no date given for the type
#' @keywords
#' @export
#' @examples
#' closest_date(data = df, date = "2020-01-01", type = "all", force_val = T)

closest_date <- function(
  data,
  date,
  type = "all",
  force_val = T
){

  # Put both into data format
  data$timestamp <- ymd(data$timestamp)
  date <- ymd(date)

  # Calculate the difference between the date you gave and each date in the dataset
  data$diff_with_date <- abs(as.numeric(data$timestamp - date))

  # If we have a date, we want to get the closest date available in the dataset with
  # a few options

  # If we want the date that is closest AND before
  before <- data %>% filter(timestamp <  date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)

  # If we want the date that is closest AND after
  after <- data %>% filter(timestamp > date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)

  # If we want the date that is closest AND (before or equal)
  beforeequal <- data %>% filter(timestamp <=  date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)

  # If we want the date that is closest AND (after or equal)
  afterequal <- data %>% filter(timestamp >= date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)

  # If we want the date that is closest regardless of everything else
  all <- data %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)


  # The type argument lets us set up which option we want
  if(type == "before") out <- before
  if(type == "after") out <- after
  if(type == "beforeequal") out <- beforeequal
  if(type == "afterequal") out <- afterequal
  if(type == "all") out <- all

  # force_val allows us to default to `all`
  if(force_val  & length(out) == 0) out <- all

  # We return the result as a Date
  out <- ymd(out)
  return(out)

}


#' Colorscheme
#'
#' @param col A data frame that includes a column `timestamp`
#' @keywords
#' @export
#' @examples
#' colorschemer("red")

colorschemer <- function(col){

  # Every colorscheme has seven colors.
  # - hicol: primary color
  # - locol: subdued version of that color
  # - nucol: neutral color
  # - opcol: opposite color
  # - highcol: intense version of the color
  # - lowcol: opposite color
  # - midcol: midway between highcol and lowcol

  # First we look at the namespace of the function. If any of these exist,
  # we use them. If not, we set them to NA
  pe <- parent.frame()
  hicol <- if(exists("hicol", pe)) get("hicol", pe) else {NA}
  locol <- if(exists("locol", pe)) get("locol", pe) else {NA}
  nucol <- if(exists("nucol", pe)) get("nucol", pe) else {NA}
  opcol <- if(exists("opcol", pe)) get("opcol", pe) else {NA}
  highcol <- if(exists("highcol", pe)) get("highcol", pe) else {NA}
  lowcol <- if(exists("lowcol", pe)) get("lowcol", pe) else {NA}
  midcol <- if(exists("midcol", pe)) get("midcol", pe) else {NA}


  # This is a sample colorscheme. It will replace any value not found in the
  # above with a color in the theme.
  if(col == "blue"){
    getPalette <- colorRampPalette(brewer.pal(100, "RdBu"))
    if(is.null(hicol) || is.na(hicol)) assign("hicol",    getPalette(1000)[900], pos=parent.frame())
    if(is.null(locol) || is.na(locol)) assign("locol",    getPalette(1000)[700], pos=parent.frame())
    if(is.null(nucol) || is.na(nucol)) assign("nucol",    getPalette(1000)[550], pos=parent.frame())
    if(is.null(opcol) || is.na(opcol)) assign("opcol",    getPalette(1000)[200], pos=parent.frame())
    if(is.null(highcol) || is.na(highcol)) assign("highcol",  getPalette(1000)[900], pos=parent.frame())
    if(is.null(lowcol) || is.na(lowcol)) assign("lowcol",   getPalette(1000)[200], pos=parent.frame())
    if(is.null(midcol) || is.na(midcol)) assign("midcol",   getPalette(1000)[550], pos=parent.frame())
  }

  if(col == "red"){
    getPalette <- colorRampPalette(brewer.pal(100, "RdBu"))
    if(is.null(hicol) || is.na(hicol)) assign("hicol",    rev(getPalette(1000))[900], pos=parent.frame())
    if(is.null(locol) || is.na(locol)) assign("locol",    rev(getPalette(1000))[700], pos=parent.frame())
    if(is.null(nucol) || is.na(nucol)) assign("nucol",    rev(getPalette(1000))[550], pos=parent.frame())
    if(is.null(opcol) || is.na(opcol)) assign("opcol",    rev(getPalette(1000))[200], pos=parent.frame())
    if(is.null(highcol) || is.na(highcol)) assign("highcol",  rev(getPalette(1000))[900], pos=parent.frame())
    if(is.null(lowcol) || is.na(lowcol)) assign("lowcol",   rev(getPalette(1000))[200], pos=parent.frame())
    if(is.null(midcol) || is.na(midcol)) assign("midcol",   rev(getPalette(1000))[550], pos=parent.frame())
  }

  if(col == "jamaim"){
    getPalette <- colorRampPalette(brewer.pal(100, "RdBu"))
    if(is.null(hicol) || is.na(hicol)) assign("hicol",    "#2F5763", pos=parent.frame())
    if(is.null(locol) || is.na(locol)) assign("locol",    "#F7941D", pos=parent.frame())
    if(is.null(nucol) || is.na(nucol)) assign("nucol",    "#BDD1DA", pos=parent.frame())
    if(is.null(opcol) || is.na(opcol)) assign("opcol",    "#F7941D", pos=parent.frame())
    if(is.null(highcol) || is.na(highcol)) assign("highcol",  "#2F5763", pos=parent.frame())
    if(is.null(lowcol) || is.na(lowcol)) assign("lowcol",   "white",      pos=parent.frame())
    if(is.null(midcol) || is.na(midcol)) assign("midcol",   "#C3D6DE", pos=parent.frame())
  }
}


#' samplemean
#'
#' @param x vector
#' @param d indices
#' @keywords
#' @export
#' @examples

samplemean <- function(x, d) {
  # This is a convenience function to give us the sample mean of a vector. Useful for bootstrapping.
  return(mean(x[d]))
}


#' min0
#'
#' @param x vector
#' @param d indices
#' @keywords
#' @export
#' @examples


minpos <- function(x) {
  # This makes it so that any numeric value is at least positive.
  return(ifelse(x <= 0, 0.00000001, x))
}
