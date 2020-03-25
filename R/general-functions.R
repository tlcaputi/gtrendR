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
