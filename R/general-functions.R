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

  data$timestamp <- ymd(data$timestamp)
  date <- ymd(date)
  data$diff_with_date <- abs(as.numeric(data$timestamp - date))

  before <- data %>% filter(timestamp <  date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)
  after <- data %>% filter(timestamp > date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)
  beforeequal <- data %>% filter(timestamp <=  date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)
  afterequal <- data %>% filter(timestamp >= date) %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)
  all <- data %>% filter(diff_with_date == min(.$diff_with_date, na.rm = T)) %>% pull(timestamp)

  if(type == "before") out <- before
  if(type == "after") out <- after
  if(type == "beforeequal") out <- beforeequal
  if(type == "afterequal") out <- afterequal
  if(type == "all") out <- all

  if(force_val  & length(out) == 0) out <- all

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

  pe <- parent.frame()
  hicol <- if(exists("hicol", pe)) get("hicol", pe) else {NA}
  locol <- if(exists("locol", pe)) get("locol", pe) else {NA}
  nucol <- if(exists("nucol", pe)) get("nucol", pe) else {NA}
  opcol <- if(exists("opcol", pe)) get("opcol", pe) else {NA}
  highcol <- if(exists("highcol", pe)) get("highcol", pe) else {NA}
  lowcol <- if(exists("lowcol", pe)) get("lowcol", pe) else {NA}
  midcol <- if(exists("midcol", pe)) get("midcol", pe) else {NA}

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
    if(is.null(locol) || is.na(locol)) assign("locol",    "#C3D6DE", pos=parent.frame())
    if(is.null(nucol) || is.na(nucol)) assign("nucol",    "#BDD1DA", pos=parent.frame())
    if(is.null(opcol) || is.na(opcol)) assign("opcol",    "#F7941D", pos=parent.frame())
    if(is.null(highcol) || is.na(highcol)) assign("highcol",  "#2F5763", pos=parent.frame())
    if(is.null(lowcol) || is.na(lowcol)) assign("lowcol",   "white",      pos=parent.frame())
    if(is.null(midcol) || is.na(midcol)) assign("midcol",   "#C3D6DE", pos=parent.frame())
  }
}
