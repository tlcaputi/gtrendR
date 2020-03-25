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
  polycolor = "grey81"
){


  tmpdf <- df[, c("timestamp", geo)]
  names(tmpdf) <- c("timestamp", "geo")

  ## SET PARAMETERS
  tmpdf$timestamp <- ymd(tmpdf$timestamp)
  if(begin == T) begin <- min(ymd(tmpdf$timestamp))
  if(end == T) end <- max(ymd(tmpdf$timestamp))
  interrupt <- ymd(interrupt) - 1

  freq <- min(as.numeric(diff.Date(tmpdf$timestamp)), na.rm = T)


  ## RUN ARIMA ON THE TIME SERIES
  ts <- ts(tmpdf$geo, freq = 365.25/freq, start = decimal_date(begin))

  ts_training <- window(ts, end = decimal_date(interrupt-1))
  ts_test <- window(ts, start = decimal_date(interrupt))
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
  finaldf$polycolor <- polycolor
  finaldf$fitted <- ifelse(is.na(finaldf$fitted), finaldf$geo, finaldf$fitted)

  names(finaldf) <- gsub("geo", geo, names(finaldf))

  return(finaldf)
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
#'            linelabel = "Tobacco 21\nSigned",
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
  interrupt,
  width = 6,
  height = 3,
  lwd = 0.3,
  save = T
  ){


  names(df) <- gsub(geo, "geo", names(df))

  maxval <- df %>% filter(timestamp > interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp > interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)

  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)

  ## CREATE PLOT
  poly <- with(df  %>% filter(timestamp %within% interval(interrupt - 1, endplot)), data.frame(x = c(timestamp, rev(timestamp)), y = c(geo, rev(fitted)), polycolor="grey81"))
  p <- ggplot(df)
  p <- p + annotate("text", x = interrupt - (as.numeric((endplot - beginplot)) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_vline(xintercept=as.numeric(interrupt - 1), linetype="dashed", color="grey74")
  p <- p + geom_polygon(data = poly, aes(x = x, y = y, fill="grey80"), fill="grey80")
  p <- p + geom_line(aes(x=timestamp, y=fitted, group=1, color="red"), linetype="solid", size=lwd)
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1, color="blue"), linetype="solid", size=lwd)
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + scale_colour_manual(name = 'Legend', values =c('blue'='blue','red'='red'), labels = c('Actual','Expected'))
  p <- p + scale_fill_manual(values="grey80")
  p <- p + theme_classic()
  p <- p + theme(legend.position=c(0.1,0.9))
  p <- p + theme(plot.title = element_text(hjust = 0.5))

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  names(df) <- gsub("geo", geo, names(df))


  return(p)

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
#'   ylab = "Query Fraction\n(Per 10 Million Searches)",
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
  xfmt = date_format("%b %Y"),
  lwd = 0.3,
  width = 6,
  height = 3,
  save = T,
  outfn
){

  if(beginplot==T) beginplot <- ymd(min(ymd(df$timestamp), na.rm = T))
  if(endplot==T) endplot <- ymd(max(ymd(df$timestamp), na.rm = T))

  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)

  names(df) <- gsub(geo, "geo", names(df))

  maxval <- df %>% filter(timestamp > interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp > interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)

  p <- ggplot(df)
  p <- p + annotate("text", x = interrupt - as.numeric(as.numeric(endplot - beginplot) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_vline(xintercept=as.numeric(interrupt), linetype="dashed", color="grey74")
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1), color="blue", linetype="solid", size=lwd)
  p <- p + geom_point(aes(x = maxtime, y = maxval), size=2, color="red")
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
