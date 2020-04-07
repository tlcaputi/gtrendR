#' multi_term
#'
#' @param df A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @param interrupt The date where things change. ARIMA will be predicted on all days before the interrupt.
#' @param beginperiod How far back you want the "pre" period to go
#' @param preperiod This creates a beginperiod but with a number of days instead of a date
#' @param endperiod How far after the interruption you want to go
#' @param scaletitle Title of the scale
#' @param scalelimits vector of two values for min and max for the scale
#' @param linecol Line color
#' @param lowcol Color for low values
#' @param midcol Color for mid values
#' @param highcol Color for high values
#' @param save Default is True, If False, don't save
#' @param width Width of file in inches
#' @param height Height of file in inches
#' @param outfn Output filename
#' @keywords
#' @export
#' @examples
#' pct_change_state(
#'   df = read.csv("./temp/data.csv", header = T, stringsAsFactor = F),
#'   interrupt = "2020-03-01",
#'   beginperiod = NA,
#'   preperiod = 90,
#'   endperiod = "2020-03-23",
#'   scaletitle = "Pct. Increase in Searches",
#'   linecol = "gray",
#'   lowcol = "red",
#'   midcol = "white",
#'   highcol = "dodgerblue4"
#' )
