#' multi_term_arima
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
#' multiterms <- multi_term_arima(
#'
#'   ## A folder containing all of your gtrends data
#'   input_dir = "./input",
#'
#'   ## Which data to use
#'   geo = "US", # Geography you want to use
#'   terms_to_use = NA, # Terms you'd like to analyze. If NA then all terms
#'   timeframe_to_use = NA, # Only analyze data with filenames that contain a certain timeframe. If NA then all timeframes
#'
#'
#'   ## Parameters of time periods
#'   beginperiod = T, # Beginning of the before period, if T then beginning of data
#'   preperiod = 90, # If beginperiod is logical, preperiod is the number of days before interrupt to include in before period
#'   endperiod = T, # End of the end period, if T then end of data
#'   interrupt = "2020-03-01", # Date for interruption, splitting before and after periods
#'
#'
#'   ## Analytical arguments
#'   bootstrap = T, # Bootstrap CIs
#'   bootnum = 1000, # Number of bootstraps
#'   kalman = T # If T, impute with Kalman
#' )


multi_term_arima <- function(
  input_dir = "C:/Users/tcapu/Google Drive/modules/gtrendR/READMEcode/input",
  geo = "US",
  terms_to_use = NA,
  timeframe_to_use = NA,
  beginperiod = T,
  preperiod = 90,
  endperiod = T,
  interrupt = "2020-03-01",
  bootstrap = T,
  bootnum = 1000,
  kalman = T
  ){


  if(!is.na(preperiod) & is.na(beginperiod)){
    beginperiod <- ymd(interrupt) - preperiod
  }

  interrupt <- ymd(interrupt)

  files <- dir(input_dir, pattern=".csv", full.names = T)

  if(!is.na(terms_to_use)){
    files <- grep(paste0(terms_to_use, collapse = "|"), files, value = T)
  }

  if(!is.na(timeframe_to_use)){
    files <- grep(timeframe_to_use, files, value = T)
  }

  dat <- list(); ct <- 1
  for (f in files){

    term <- basename(f)
    term <- gsub("day|week|month|year|[.]csv|[_]", "", term)

    df <- read.csv(f, header = T, stringsAsFactor = F)
    df$timestamp <- ymd(df$timestamp)
    names(df) <- gsub(geo, "geo", names(df))
    if(is.logical(beginperiod)) beginperiod <- min(df$timestamp, na.rm = T)
    if(is.logical(endperiod)) endperiod <- max(df$timestamp, na.rm = T)
    df <- df %>% filter(timestamp %within% interval(beginperiod, endperiod))
    freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)

    time_series <- ts(df$geo, freq = 365.25/freq, start = decimal_date(beginperiod))
    ts_training <- ts(df %>% filter(timestamp <= interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(beginperiod))
    ts_test <- ts(df %>% filter(timestamp > interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(interrupt))

    if(kalman){
      time_series <- na_kalman(time_series, model="auto.arima")
      ts_training <- na_kalman(ts_training, model="auto.arima")
      ts_test <- na_kalman(ts_test, model="auto.arima")
      df$geo <- as.numeric(time_series)
    }

    mod <- auto.arima(ts_training)


    ## EXTRACT FITTED VALUES
    if(bootstrap){
      set.seed(1234)
      fitted_values <- forecast(mod, h = length(ts_test), bootstrap = TRUE, npaths = bootnum)
    } else{
      fitted_values <- forecast(mod, h = length(ts_test))
    }

    tmp <- data.frame(
      "actual" = as.numeric(ts_test),
      "fitted" = fitted_values$mean,
      "lo" = fitted_values$lower,
      "hi" = fitted_values$upper
    )


    names(tmp) <- gsub("[.]", "", names(tmp))


    out <- data.frame(
      "term" = term,
      "mean" = mean(tmp$actual / tmp$fitted, na.rm = T),
      "lo95" = mean(tmp$actual / tmp$hi95, na.rm = T),
      "hi95" = mean(tmp$actual / tmp$lo95, na.rm = T)
    )

    dat[[ct]] <- out; ct <- ct + 1

  }

  out <- do.call(rbind.data.frame, dat)
  out <- as.data.frame(out)

  return(out)


}





#' multiterm_barplot: Use the data from multi_term_arima to create a barplot
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
#' panG <- multiterm_barplot(
#'   df = multiterms,
#'
#'   ## Graphing Parameters
#'   title = NULL, # If NULL, no Title
#'   xlab = "Terms", # x axis label
#'   label_df = NA, # Use a two-column dataframe to label the barplot x axis
#'   ylab = "Greater than Expected (%)", # y axis label
#'   space = 0.8, # space between bars
#'
#'   ## Set a colorscheme
#'   colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"
#'
#'   # ... customize any color using these
#'   hicol = NA, # Color of bars
#'
#'   ## Saving arguments
#'   save = T, # If T, save plot
#'   outfn = './output/panG.png', # Location to save plot
#'   width = 6, # Width in inches
#'   height = 3 # Height in inches
#' )

multiterm_barplot <- function(
  df,
  label_df = NA,
  title = NULL, # If NULL, no Title
  xlab = "Terms", # x axis label
  ylab = "Greater than Expected (%)", # y axis label
  ylim = NULL, # length-2 vector with ymin and ymax, default NULL
  space = 0.8, # space between bars
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"
  hicol = NA, # Actual color
  save = T, # If T, save plot
  outfn = './output/panG.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches
  ){

  colorschemer(colorscheme)

  if(!is.na(label_df)){
    names(label_df) <- c("original", "label")
    df$term <- terms_df$label[match(df$term, terms_df$original)]
  }

  p <- ggplot(df)
  p <- p + geom_bar(aes(x = term, y = mean, fill = hicol), stat = "identity", position=position_dodge(width=space))
  p <- p + geom_errorbar(aes(x = term, ymin = lo95, ymax = hi95), width=.2)
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + scale_y_continuous(
    limits = ylim,
    labels = function(x) paste0(x*100, "%")
  ) # Multiply by 100 & add Pct
  p <- p + theme_classic()
  p <- p + theme(legend.position="none")

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  return(p)
}
