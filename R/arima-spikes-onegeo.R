#' Run this first. It applies the ARIMA to predict future values and then produces a dataset with the actual and fitted values.
#'
#' @param df A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @param interrupt The date where things change. ARIMA will be predicted on all days before the interrupt.
#' @param geo The column name of the geography of searches that you want.
#' @param polycolor What color do you want the polygon to be?.
#' @keywords
#' @export
#' @examples
#' run_arima(df = data, interrupt = ymd("2019-12-19"), geo = "US")


run_arima <- function(
  df,
  interrupt,
  begin = T,
  end = T,
  geo = "US",
  kalman = F
  ){


  tmpdf <- df[, c("timestamp", geo)]
  names(tmpdf) <- c("timestamp", "geo")

  ## SET PARAMETERS
  tmpdf$timestamp <- ymd(tmpdf$timestamp)
  if(begin == T) begin <- min(ymd(tmpdf$timestamp))
  if(end == T) end <- max(ymd(tmpdf$timestamp))

  begin <- closest_date(data = tmpdf, date = begin, type = "beforeequal")
  end <- closest_date(data = tmpdf, date = end, type = "afterequal")
  interrupt <- closest_date(data = tmpdf, date = interrupt, type = "before")

  begin <- ymd(begin)
  end <- ymd(end)
  interrupt <- ymd(interrupt)

  tmpdf <- tmpdf %>% filter(timestamp %within% interval(begin, end))

  freq <- min(as.numeric(diff.Date(tmpdf$timestamp)), na.rm = T)

  ## RUN ARIMA ON THE TIME SERIES
  # ts_training <- window(time_series, end = decimal_date(interrupt) - 0.5/365)
  # ts_test <- window(time_series, start = decimal_date(interrupt))

  time_series <- ts(tmpdf$geo, freq = 365.25/freq, start = decimal_date(begin))
  ts_training <- ts(tmpdf %>% filter(timestamp <= interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(begin))
  ts_test <- ts(tmpdf %>% filter(timestamp > interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(interrupt))

  if(kalman){
    time_series <- na_kalman(time_series, model="auto.arima")
    ts_training <- na_kalman(ts_training, model="auto.arima")
    tmpdf$geo <- as.numeric(time_series)
  }


  mod <- auto.arima(ts_training)

  ## EXTRACT FITTED VALUES
  fitted_values <- forecast(mod, length(ts_test))

  ## ADD THE FITTED VALUES TO THE DATA FRAME
  tmp1 <- data.frame(matrix(NA, nrow=nrow(tmpdf) - length(fitted_values$mean), ncol=5))
  tmp2 <- data.frame(fitted=fitted_values$mean, lo=fitted_values$lower, hi=fitted_values$upper)
  names(tmp1) <- names(tmp2)
  df_to_cbind <- rbind.data.frame(tmp1, tmp2)

  names(df_to_cbind) <- gsub("[.]", "", names(df_to_cbind))
  finaldf <- cbind.data.frame(tmpdf, df_to_cbind)
  # finaldf$polycolor <- polycolor
  finaldf$fitted <- ifelse(is.na(finaldf$fitted), finaldf$geo, finaldf$fitted)

  names(finaldf) <- gsub("geo", geo, names(finaldf))

  return(finaldf)
}



#' line_plot: Creates a simple line plot of searches over time
#'
#' @param df The dataframe as outputted by \code{run_arima}.
#' @param geo The column name of the geography of searches that you want.
#' @param beginplot The date you want the plot to start. Default is beginning of the time series.
#' @param endplot The date you want the plot to end. Default is end of the time series.
#' @param interrupt The date of the interruption (should be the same as \code{run_arima})
#' @param linelabel Label next to the vertical interruption line
#' @param title The title of the figure. The default is no title.
#' @param xlab The label for the x axis
#' @param ylab The label for the y axis
#' @param lbreak The distance between tick marks in the x axis, i.e., "one year" or "3 month"
#' @param lwd Line width
#' @param width Width of the plot in inches
#' @param height Height of the plot in inches
#' @param save Default is True. If False, the plot is not saved.
#' @param outfn Where to save the plot.
#' @keywords
#' @export
#' @examples
#' line_plot(
#'   df,
#'   beginplot = T,
#'   endplot = T,
#'   interrupt = ymd("2019-12-19")
#'   linelabel = "Interruption",
#'   title = NULL,
#'   xlab = "Date",
#'   ylab = "Query Fraction (Per 10 Million Searches)",
#'   lbreak = "1 year",
#'   lwd = 0.3,
#'   width = 6,
#'   height = 3,
#'   save = T,
#'   outfn = './output/panA.png'
#' )


line_plot <- function(
  df,
  geo = 'US',
  beginplot = T,
  endplot = T,
  interrupt,
  linelabel = "Interruption",
  linelabelpos = 0.02,
  title = NULL,
  xlab = "Date",
  ylab = "Query Fraction\n(Per 10 Million Searches)",
  lbreak = "1 year",
  hicol = NA,
  locol = NA,
  nucol = NA,
  opcol = NA,
  colorscheme = "red",
  xfmt = date_format("%b %Y"),
  lwd = 0.3,
  extend = F,
  width = 6,
  height = 3,
  save = T,
  outfn
  ){

  colorschemer(colorscheme)


  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  interrupt <- ymd(interrupt)
  if(freq == 1){
    interrupt_line <- interrupt -1
  } else{
    interrupt_line <- interrupt
  }


  if(beginplot==T) beginplot <- ymd(min(ymd(df$timestamp), na.rm = T))
  if(endplot==T) endplot <- ymd(max(ymd(df$timestamp), na.rm = T))


  if(!extend){
    beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
    endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  }
  interrupt <- closest_date(data = df, date = interrupt, type = "beforeequal")

  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)

  names(df) <- gsub(geo, "geo", names(df))

  maxval <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)

  p <- ggplot(df)
  p <- p + annotate("text", x = interrupt_line - as.numeric(as.numeric(endplot - beginplot) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey74")
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1), color=hicol, linetype="solid", size=lwd)
  p <- p + geom_point(aes(x = maxtime, y = maxval), size=2, color=opcol)
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + theme_classic()

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  names(df) <- gsub("geo", geo, names(df))

  return(p)

}


#' This uses the output from \code{run_arima} to create a figure showing the
#' difference between the actual and expected searches for a single geography.
#'
#' @param df The dataframe as outputted by \code{run_arima}.
#' @param geo The column name of the geography of searches that you want.
#' @param beginplot The date you want the plot to start
#' @param endplot The date you want the plot to end.
#' @param interrupt The date of the interruption (should be the same as \code{run_arima})
#' @param linelabel Label next to the vertical interruption line
#' @param title The title of the figure. The default is no title.
#' @param xlab The label for the x axis
#' @param ylab The label for the y axis
#' @param lbreak The distance between tick marks in the x axis, i.e., "one year" or "3 month"
#' @param lwd Line width
#' @param width Width of the plot in inches
#' @param height Height of the plot in inches
#' @param save Default is True. If False, the plot is not saved.
#' @param outfn Where to save the plot.
#' @keywords
#' @export
#' @examples
#' arima_plot(
#'            df,
#'            title = "Searches to Purchase Cigarettes - US",
#'            xlab = "Date",
#'            ylab = "Query Fraction",
#'            outfn = './output/fig.pdf',
#'            beginplot = "2019-09-01",
#'            endplot = "2020-01-15",
#'            lbreak = "1 year",
#'            linelabel = "Tobacco 21 Signed",
#'            interrupt = ymd("2019-12-19"),
#'            width = 6,
#'            height = 3,
#'            lwd = 0.3,
#'            save = T
#'            )
arima_plot <- function(
  df,
  geo = 'US',
  title = NULL,
  xlab = "Date",
  xfmt = date_format("%b %Y"),
  ylab = "Query Fraction\n(Per 10 Million Searches)",
  outfn = './output/fig.pdf',
  beginplot,
  endplot,
  lbreak = "1 year",
  linelabel = "Interruption",
  linelabelpos = 0.02,
  hicol = NA,
  locol = NA,
  nucol = NA,
  opcol = NA,
  colorscheme = "red",
  polyalpha = 0.9,
  interrupt,
  width = 6,
  height = 3,
  lwd = 0.3,
  save = T,
  extend = F
  ){


  colorschemer(colorscheme)


  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  interrupt <- ymd(interrupt)
  if(freq == 1){
    interrupt_line <- interrupt -1
  } else{
    interrupt_line <- interrupt
  }


  if(beginplot==T) beginplot <- ymd(min(ymd(df$timestamp), na.rm = T))
  if(endplot==T) endplot <- ymd(max(ymd(df$timestamp), na.rm = T))


  names(df) <- gsub(geo, "geo", names(df))

  maxval <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)


  if(!extend){
    beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
    endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  }
  interrupt <- closest_date(data = df, date = interrupt, type = "before")

  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)

  df$polycolor <- nucol

  ## CREATE PLOT
  poly <- with(df %>% filter(timestamp %within% interval(interrupt, endplot)),
              data.frame(x = c(timestamp, rev(timestamp)), y = c(geo, rev(fitted)), polycolor=nucol))
  p <- ggplot(df)
  p <- p + annotate("text", x = interrupt_line - (as.numeric((endplot - beginplot)) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_polygon(data = poly, aes(x = x, y = y, fill=nucol), fill=nucol, alpha=polyalpha)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey74")
  p <- p + geom_line(aes(x=timestamp, y=fitted, group=1, color=locol), linetype="solid", size=lwd)
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1, color=hicol), linetype="solid", size=lwd)
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + scale_colour_manual(name = 'Legend', values=c(hicol, locol), labels = c('Actual','Expected'))
  p <- p + scale_fill_manual(values=nucol)
  p <- p + theme_classic()
  p <- p + theme(legend.position=c(0.1,0.9))
  p <- p + theme(plot.title = element_text(hjust = 0.5))

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  names(df) <- gsub("geo", geo, names(df))

  return(p)

}
