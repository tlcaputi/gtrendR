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
  kalman = F,
  bootstrap = F,
  bootnum = 1000,
  linear = F,
  rsv = F
  ){


  # For data periods larger than 1 day, Google gives data with the date equal to
  # the first date in each period. This doesn't look good in plots. So we need to
  # make it so that the dates in the dataset are the last date in each period. To
  # complicate things, Google will report preliminary data for the most recent
  # period even if that period is not complete.

  # First we figure out how many dates are in each period
  df$timestamp <- ymd(df$timestamp)
  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)

  # If the end of the last period hasn't even occurred yet, we remove it from the dataset
  maxdate <- max(df$timestamp, na.rm = T)
  if(Sys.Date() < maxdate + freq) df <- df %>% filter(timestamp != maxdate)

  # Finally, we move the dates to be the end of the period. Note that if it
  # is daily data, freq is 1 and so the dates do not actually move.
  df$timestamp <- ymd(df$timestamp) + freq - 1



  # We take only the data that we need. This is the timestamp and the one
  # geography.
  tmpdf <- df[, c("timestamp", geo)]
  names(tmpdf) <- c("timestamp", "geo")

  # We need timestamp to be a date object.
  tmpdf$timestamp <- ymd(tmpdf$timestamp)

  # This allows us to use T as a default for begin and after.
  # If either is T, it just takes the beginning/end of the data
  if(begin == T) begin <- min(ymd(tmpdf$timestamp))
  if(end == T) end <- max(ymd(tmpdf$timestamp))


  # We need to select a begin and end point that is a datapoint. Therefore, we
  # use the closest_date function to select something before or equal to begin...
  begin <- closest_date(data = tmpdf, date = begin, type = "beforeequal")
  # ... and after or equal to end
  end <- closest_date(data = tmpdf, date = end, type = "afterequal")


  # There are two possibilities. Either interupt occurs on a date that we have
  # data for or it occurs between two data points.

  # Case 1: If the interrupt date is actually in the data frame, then this is fairly simple.
  # This keeps interrupt as the same date, because it is equal. The interrupt line
  # looks best on the date immediately before the interruption -- this way it is
  # looks like the fitted line diverges from the actual line right at the interruption
  # line.

  # Case 2: If our interruption is between two dates in the dataset, it's a bit more
  # complicated. We assume that the period including your interruption should
  # be part of the "post-period". Google data is formatted such that the date
  # corresponds to the last date in the period. Therefore, the date after your
  # interruption corresponds to the week that your interruption happened.
  # We use this as the interruption. We still need the line to come at the date
  # before the interruption, so we use "before" to set our interruption.

  # All together, we correct the interrupt to be either the same as it was
  # (case 1) or the next date (case 2). From there, we use the date before then
  # for the interuption line.

  interrupt <- closest_date(data = df, date = interrupt, type = "afterequal")
  interrupt_line <- closest_date(data = df, date = interrupt, type = "before")


  # We want to ensure that all of these are date objectss
  begin <- ymd(begin)
  end <- ymd(end)
  interrupt <- ymd(interrupt)


  # Now we limit the data to only the data between the begin and end period.
  tmpdf <- tmpdf %>% filter(timestamp %within% interval(begin, end))


  # This is an option to change query fractions from searches per 10M to
  # Relative Search Values (RSV), i.e., everything is relative to the max value,
  # which is set to 100.

  if(rsv){
    rsvmaxval <- max(tmpdf$geo, na.rm = T)
    tmpdf$geo <- tmpdf$geo / rsvmaxval * 100
  }


  # We need to know what kind of data this is. We infer with diff.date.
  # If the minimum difference is 1 day, its daily. If it's 7, it's weekly. Etc.
  freq <- min(as.numeric(diff.Date(tmpdf$timestamp)), na.rm = T)


  # Here we put the searches into time series objects.

  # time_series is the entire timeline
  time_series <- ts(tmpdf$geo, freq = 365.25/freq, start = decimal_date(begin))

  # ts_training is just the pre-period
  ts_training <- ts(tmpdf %>% filter(timestamp < interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(begin))

  # ts_test is just the post-period
  ts_test <- ts(tmpdf %>% filter(timestamp >= interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(interrupt))


  # na_kalman lets us impute missing data in the time series objects if the
  # kalman object is set to T
  if(kalman){
    time_series <- na_kalman(time_series, model="auto.arima")
    ts_training <- na_kalman(ts_training, model="auto.arima")
    ts_test <- na_kalman(ts_test, model="auto.arima")
    tmpdf$geo <- as.numeric(time_series)
  }



  # If linear is True, then we use a linear model. If not...
  if(!linear){

    # We run the model
    mod <- auto.arima(ts_training, approximation = F)

    # We use the built-in arima.forecast function. The bootstrap option
    # lets us set the options on the forecast.
    if(bootstrap){
      set.seed(1234)
      # h (horizon) should be equal to the length of the after-period
      # bootnum is the number of paths it's allowed to take
      fitted_values <- forecast(mod, h = length(ts_test), bootstrap = TRUE, npaths = bootnum)
    } else{
      fitted_values <- forecast(mod, h = length(ts_test))
    }

  } else{


    # If linear is True, we have to create that model.

    # First, we create a dataframe of train data and fit the model.
    x_train <- 1:length(ts_training)
    y_train <- as.numeric(ts_training)
    train <- data.frame("x" = x_train, "y" = y_train)
    mod <- lm(y ~ x, data = train)

    # Next, we create a data frame for the test data.
    x_test <- (length(ts_training) + 1):(length(ts_training) + length(ts_test))
    ft <- data.frame("x" = x_test, "y" = NA)


    if(bootstrap){

      # If bootstrap, we use boot_predict to forecast the values for the test data
      # based upon the model fit on the train data
      conf95 <- boot_predict(mod, newdata = ft, R=1000, condense = F)
      conf95 <- data.frame(conf95)
      names(conf95)[3:5] <- c("fit", "lwr", "upr")
      fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)


    } else{

      # If not bootstrap, we use predict.lm to forecast the values for the test data
      # based upon the model fit on the train data.
      conf95 <- predict(mod, newdata = ft, interval = "confidence", level = 0.95)
      conf95 <- data.frame(conf95)
      fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)
    }



  }


  # Now we need to add the fitted data to the data.frame.

  # tmp2 is a data frame of forecasts on the test data
  tmp2 <- data.frame(fitted=fitted_values$mean, lo=fitted_values$lower, hi=fitted_values$upper)

  # tmp1 is just an empty data.frame for the train data
  tmp1 <- data.frame(matrix(NA, nrow=nrow(tmpdf) - length(fitted_values$mean), ncol=ncol(tmp2)))
  names(tmp1) <- names(tmp2)

  # By binding them together, we get a data.frame with the same number of rows
  # as the tmpdf
  df_to_cbind <- rbind.data.frame(tmp1, tmp2)


  # Depending upon which fitted values we use, the names can come out strange. This
  # fixes that so we at least have a `lo95` and `hi95` column.
  names(df_to_cbind) <- gsub("[.]", "", names(df_to_cbind))
  names(df_to_cbind) <- gsub("^lo$", "lo95", names(df_to_cbind))
  names(df_to_cbind) <- gsub("^hi$", "hi95", names(df_to_cbind))


  # finaldf combines the actual data with the fitted values
  finaldf <- cbind.data.frame(tmpdf, df_to_cbind)

  # For convenience, we set "fitted" to be the same as "actual" for all rows that
  # were originally in the train data.
  finaldf$fitted <- ifelse(is.na(finaldf$fitted), finaldf$geo, finaldf$fitted)

  # We rename "geo" to the actual name for the geography
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
  ylim = NULL,
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


  # Set a colorscheme
  colorschemer(colorscheme)

  # This allows us to use T as a default for begin and after.
  # If either is T, it just takes the beginning/end of the data
  if(beginplot == T) beginplot <- min(ymd(df$timestamp), na.rm = T)
  if(endplot == T) endplot <- max(ymd(df$timestamp), na.rm = T)


  # If extend is true, you make the plot fit around existing data points.
  if(!extend){
    # We need to select a begin and end point that is a datapoint. Therefore, we
    # use the closest_date function to select something before or equal to begin...
    # ... and after or equal to end
    beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
    # This gives us an option where
    endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  }


  # There are two possibilities. Either interupt occurs on a date that we have
  # data for or it occurs between two data points.

  # Case 1: If the interrupt date is actually in the data frame, then this is fairly simple.
  # This keeps interrupt as the same date, because it is equal. The interrupt line
  # looks best on the date immediately before the interruption -- this way it is
  # looks like the fitted line diverges from the actual line right at the interruption
  # line.

  # Case 2: If our interruption is between two dates in the dataset, it's a bit more
  # complicated. We assume that the period including your interruption should
  # be part of the "post-period". Google data is formatted such that the date
  # corresponds to the last date in the period. Therefore, the date after your
  # interruption corresponds to the week that your interruption happened.
  # We use this as the interruption. We still need the line to come at the date
  # before the interruption, so we use "before" to set our interruption.

  # All together, we correct the interrupt to be either the same as it was
  # (case 1) or the next date (case 2). From there, we use the date before then
  # for the interuption line.

  interrupt <- closest_date(data = df, date = interrupt, type = "afterequal")
  interrupt_line <- closest_date(data = df, date = interrupt, type = "before")


  # We need to know what kind of data this is. We infer with diff.date.
  # If the minimum difference is 1 day, its daily. If it's 7, it's weekly. Etc.
  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)


  # We want to ensure that all of these are date objectss
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)



  # Change the name of the geography to something that's easier to use
  names(df) <- gsub(geo, "geo", names(df))


  # freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  # interrupt <- ymd(interrupt)
  # if(freq == 1){
  #   interrupt_line <- interrupt -1
  # } else{
  #   interrupt_line <- interrupt
  # }
  #
  #
  # if(beginplot==T) beginplot <- ymd(min(ymd(df$timestamp), na.rm = T))
  # if(endplot==T) endplot <- ymd(max(ymd(df$timestamp), na.rm = T))
  #
  #
  # interrupt <- closest_date(data = df, date = interrupt, type = "beforeequal")





  # We just want to set the maximum time and search value occuring after the interruption.
  # We use these to place a point right at the peak.
  maxval <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)



  # Here we create the figure.
  p <- ggplot(df)

  # The first step is putting in the vertical line and vertical line label in the right spot.
  # Here, taking the difference between endplot and beginplot standardizes the x axis and linelabelpos
  # can be used to change how close or far away the label is.
  p <- p + annotate("text", x = interrupt_line - as.numeric(as.numeric(endplot - beginplot) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey74")

  # Next, we draw the line of actual values
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1), color=hicol, linetype="solid", size=lwd)

  # We put a point at the peak
  p <- p + geom_point(aes(x = maxtime, y = maxval), size=2, color=opcol)


  # We pass options along to the axes
  p <- p + scale_y_continuous(limits = ylim)
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = ymd(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )

  # And use the default theme
  p <- p + theme_classic()


  # If save, we save the figure
  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)


  # We change the name of the geography back in the dataframe.
  names(df) <- gsub("geo", geo, names(df))


  # Return the ggplot
  return(p)

}


#' arima_ciplot This uses the output from \code{run_arima} to create a figure showing the
#' confidence interval of the ARIMA
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
#' @param ylim length-2 vector with ymin and ymax, Default NULL
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
arima_ciplot <- function(
  df,
  geo = 'US',
  title = NULL,
  xlab = "Date",
  xfmt = date_format("%b %Y"),
  ylab = "Greater Than Expected (%)",
  ylim = NULL,
  outfn = './output/fig.pdf',
  vlinelwd = 1,
  vlinecol = "grey74",
  beginplot = T,
  endplot = T,
  hline = T,
  vline = T,
  lbreak = "1 month",
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

  # Set a colorscheme
  colorschemer(colorscheme)

  # This allows us to use T as a default for begin and after.
  # If either is T, it just takes the beginning/end of the data
  if(beginplot == T) begin <- min(ymd(df$timestamp))
  if(endplot == T) end <- max(ymd(df$timestamp))

  # If extend is true, you make the plot fit around existing data points.
  if(!extend){
    # We need to select a begin and end point that is a datapoint. Therefore, we
    # use the closest_date function to select something before or equal to begin...
    # ... and after or equal to end
    beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
    # This gives us an option where
    endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  }


  # There are two possibilities. Either interupt occurs on a date that we have
  # data for or it occurs between two data points.

  # Case 1: If the interrupt date is actually in the data frame, then this is fairly simple.
  # This keeps interrupt as the same date, because it is equal. The interrupt line
  # looks best on the date immediately before the interruption -- this way it is
  # looks like the fitted line diverges from the actual line right at the interruption
  # line.

  # Case 2: If our interruption is between two dates in the dataset, it's a bit more
  # complicated. We assume that the period including your interruption should
  # be part of the "post-period". Google data is formatted such that the date
  # corresponds to the last date in the period. Therefore, the date after your
  # interruption corresponds to the week that your interruption happened.
  # We use this as the interruption. We still need the line to come at the date
  # before the interruption, so we use "before" to set our interruption.

  # All together, we correct the interrupt to be either the same as it was
  # (case 1) or the next date (case 2). From there, we use the date before then
  # for the interuption line.

  interrupt <- closest_date(data = df, date = interrupt, type = "afterequal")
  interrupt_line <- closest_date(data = df, date = interrupt, type = "before")


  # We need to know what kind of data this is. We infer with diff.date.
  # If the minimum difference is 1 day, its daily. If it's 7, it's weekly. Etc.
  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)


  # We want to ensure that all of these are date objectss
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)


  # Change the name of the geography to something that's easier to use
  names(df) <- gsub(geo, "geo", names(df))


  # freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  # interrupt <- ymd(interrupt)
  # # interrupt_line <- interrupt
  #
  # if(beginplot==T) beginplot <- ymd(interrupt)
  # if(endplot==T) endplot <- ymd(max(ymd(df$timestamp), na.rm = T))
  #
  # names(df) <- gsub(geo, "geo", names(df))
  #
  # maxval <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  # maxtime <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)
  #
  #
  # if(!extend){
  #   beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
  #   endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  # }
  # interrupt <- closest_date(data = df, date = interrupt, type = "afterequal")
  # interrupt_line <- closest_date(data = df, date = interrupt, type = "before")
  # # interrupt <- closest_date(data = df, date = interrupt, type = "before")
  #
  # beginplot <- ymd(beginplot)
  # endplot <- ymd(endplot)
  # interrupt <- ymd(interrupt)
  #
  # if(!extend){
  #   beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
  #   endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  # }
  #
  # beginplot <- ymd(beginplot)
  # endplot <- ymd(endplot)
  # interrupt <- ymd(interrupt)



  # The CI_plot focuses on the period after the interruption, so from that data...
  tmp <- with(df %>% filter(timestamp %within% interval(interrupt_line, endplot)),
                # we form a dataset with the pctdiff, CI, and color of the polygon
                data.frame(
                  "timestamp" = timestamp,
                  "pctdiff" = geo/fitted - 1,
                  "lo95" = geo/hi95 - 1,
                  "hi95" = geo/lo95 - 1,
                  "polycolor" = nucol
                ))


  # Notice that we include data from the interrupt-line (the data period before the interruption).
  # This is because we don't want to start the polygon when there is a difference between
  # fitted and actual. We want the polygon to fill the diverging lines.


  # Of course, the interrupt_line data point is actually part of the pre-period
  # so we place the confidence interval at that point very close to the actual point.
  mintime <- min(ymd(tmp$timestamp), na.rm = T)
  tmp$lo95 <- ifelse(ymd(tmp$timestamp) == mintime, tmp$pctdiff*0.99, tmp$lo95)
  tmp$hi95 <- ifelse(ymd(tmp$timestamp) == mintime, tmp$pctdiff*1.01, tmp$hi95)

  ## CREATE PLOT

  # We create a "circular" data frame for the polygon
  poly <- with(tmp,
              data.frame(x = c(timestamp, rev(timestamp)), y = c(lo95, rev(hi95)), polycolor=nucol))


  # We start the plot
  p <- ggplot(tmp)

  # We create the polygon giving the CI for the pctdiff
  p <- p + geom_polygon(data = poly, aes(x = x, y = y, fill=nucol), fill=nucol, alpha=polyalpha)

  # On top of that, we plot the percent difference
  p <- p + geom_line(aes(x=timestamp, y=pctdiff, group=1, color=hicol), color=hicol, linetype="solid", size=lwd)


  # If vline or hline, we put a vertical line at the interruption_line or a horizontal
  # line at 0
  if(vline){
    p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color=vlinecol, lwd = vlinelwd)
  }
  if(hline){
    p <- p + geom_hline(yintercept=0, linetype="dashed", color=vlinecol, lwd = vlinelwd)
  }

  # We pass arguments onto the axes
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + scale_y_continuous(
   limits = ylim,
   labels = function(x) paste0(x*100, "%")
  )
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )

  # Use a default theme and get rid of the legend
  p <- p + theme_classic()
  p <- p + theme(legend.position="none")


  # Save if necessary
  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)


  # Rename the data frame back to the actual name of the geography
  names(df) <- gsub("geo", geo, names(df))


  # return plot
  return(p)

}



#' arima_plot: This uses the output from \code{run_arima} to create a figure showing the
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
  ylim = NULL,
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
  extend = F,
  labels = T,
  bootnum = 1000,
  alpha =0.05,
  labsize = 0.5
  ){

  # Set a colorscheme
  colorschemer(colorscheme)

  # This allows us to use T as a default for begin and after.
  # If either is T, it just takes the beginning/end of the data
  if(beginplot == T) begin <- min(ymd(df$timestamp))
  if(endplot == T) end <- max(ymd(df$timestamp))

  # If extend is true, you make the plot fit around existing data points.
  if(!extend){
    # We need to select a begin and end point that is a datapoint. Therefore, we
    # use the closest_date function to select something before or equal to begin...
    # ... and after or equal to end
    beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
    # This gives us an option where
    endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  }


  # There are two possibilities. Either interupt occurs on a date that we have
  # data for or it occurs between two data points.

  # Case 1: If the interrupt date is actually in the data frame, then this is fairly simple.
  # This keeps interrupt as the same date, because it is equal. The interrupt line
  # looks best on the date immediately before the interruption -- this way it is
  # looks like the fitted line diverges from the actual line right at the interruption
  # line.

  # Case 2: If our interruption is between two dates in the dataset, it's a bit more
  # complicated. We assume that the period including your interruption should
  # be part of the "post-period". Google data is formatted such that the date
  # corresponds to the last date in the period. Therefore, the date after your
  # interruption corresponds to the week that your interruption happened.
  # We use this as the interruption. We still need the line to come at the date
  # before the interruption, so we use "before" to set our interruption.

  # All together, we correct the interrupt to be either the same as it was
  # (case 1) or the next date (case 2). From there, we use the date before then
  # for the interuption line.

  interrupt <- closest_date(data = df, date = interrupt, type = "afterequal")
  interrupt_line <- closest_date(data = df, date = interrupt, type = "before")


  # We need to know what kind of data this is. We infer with diff.date.
  # If the minimum difference is 1 day, its daily. If it's 7, it's weekly. Etc.
  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)


  # We want to ensure that all of these are date objectss
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)
  df$timestamp <- ymd(df$timestamp)


  # Change the name of the geography to something that's easier to use
  names(df) <- gsub(geo, "geo", names(df))


  # We just want to set the maximum time and search value occuring after the interruption.
  # We use these to place a point right at the peak.
  maxval <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(geo)
  maxtime <- df %>% filter(timestamp >= interrupt) %>% filter(geo == max(geo, na.rm = T)) %>% pull(timestamp)



  ## CREATE PLOT

  # Create a circular data frame for the polygon
  poly <- with(df %>% filter(timestamp %within% interval(interrupt_line, endplot)),
              data.frame(x = c(timestamp, rev(timestamp)), y = c(geo, rev(fitted)), polycolor=nucol))


  # We create the plot including...
  p <- ggplot(df)
  # the polygon
  p <- p + geom_polygon(data = poly, aes(x = x, y = y, fill=nucol), fill=nucol, alpha=polyalpha)
  # vertical line and line label
  p <- p + annotate("text", x = interrupt_line - (as.numeric((endplot - beginplot)) * linelabelpos), y = maxval*0.98, label = linelabel, hjust=1, vjust = 1)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey74")
  # the fitted line, which is equal to the actual line for the entire pre period
  p <- p + geom_line(aes(x=timestamp, y=fitted, group=1, color=locol), linetype="solid", size=lwd)
  # the actual line, which covers the fitted line for the pre period
  p <- p + geom_line(aes(x=timestamp, y=geo, group=1, color=hicol), linetype="solid", size=lwd)

  # pass axis arguments
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + scale_y_continuous(limits = ylim)

  # This creates a legend for the expected and actual values
  p <- p + scale_colour_manual(name = 'Legend', values=c(hicol, locol), labels = c('Actual','Expected'))
  p <- p + scale_fill_manual(values=nucol)
  p <- p + theme(legend.position=c(0.1,0.9))

  # Classic is the default theme
  p <- p + theme_classic()
  p <- p + theme(plot.title = element_text(hjust = 0.5))




  ## We are likely interested in the percentage difference between ARIMA forecasted
  ## and actual searches. We print that out automatically.


  # Set seed for bootstrapping
  set.seed(1234)

  # Make sure everything is in Date format still
  df$timestamp <- ymd(df$timestamp)
  interrupt <- ymd(interrupt)
  endplot <- ymd(endplot)

  # Get a vector of the expected and actual searches
  expectedsearches <- df %>% filter(timestamp %within% interval(interrupt, endplot)) %>% pull(fitted)
  actualsearches <-   df %>% filter(timestamp %within% interval(interrupt, endplot)) %>% pull(geo)

  # Use the boot package to create vectors of bootstrapped means from these
  ratiomeans <- boot(data = na.omit(actualsearches / expectedsearches - 1), statistic = samplemean, R = bootnum)
  # expectedmeans <- boot(data = expectedsearches, statistic = samplemean, R = bootnum)
  # actualmeans <- boot(data = actualsearches, statistic = samplemean, R = bootnum)

  # Put these bootstrapped vectors together and calculate percent diff
  # bootdf <- data.frame("expectedmeans" = expectedmeans$t, "actualmeans" = actualmeans$t)
  # bootdf <- bootdf %>% mutate(
  #   pctdiff = ((actualmeans / expectedmeans) - 1)
  # )

  # Extract the bootstrapped percent difference as a vector
  # booted_vec <- bootdf %>% pull(pctdiff)
  booted_vec <- ratiomeans$t

  # Report the mean and CI of this vector
  mn <- mean(actualsearches / expectedsearches - 1, na.rm = T)
  hi95 <- as.numeric(quantile(booted_vec, 1-(alpha/2), na.rm = T))
  lo95 <- as.numeric(quantile(booted_vec, (alpha/2), na.rm = T))
  lab <- sprintf("%1.0f%% Increase (95%%CI %1.0f to %1.0f)", mn*100, lo95*100, hi95*100)
  print(lab)


  # If labels are on, this will print that result on the top of your figure
  if(labels){
    g = grobTree(textGrob(lab, x=0.5, hjust=0.5, y=1, vjust=1, gp=gpar(fontsize=labsize*12)))
    p <- p + annotation_custom(g)
  }

  # Save if necessary
  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  # Change geo name back to geography
  names(df) <- gsub("geo", geo, names(df))

  # Return plot
  return(p)

}





#' get_rawcounts using estimates from Comscore
#'
#' @param df The dataframe as outputted by \code{run_arima}.
#' @param month 1 or 2, 1 is the earlier month and 2 is the later. Default 2
#' @param pct_desktop Assumption of the percentage of searches that are executed on Desktops. Default 0.35
#' @param geo Location
#' @param interrupt The date of the interruption (should be the same as \code{run_arima})
#' @param qf_denominator Denominator for fractions given in gtrendspy. Default 10M.
#' @param endperiod Compute raw counts from the interrupt to an enddate
#' @keywords
#' @export
#' @examples
#' get_rawcounts <- function(
#'   df,
#'   month = 2,
#'   pct_desktop = 0.35,
#'   geo = "US",
#'   interrupt = "2020-03-01",
#'   qf_denominator = 10000000,
#'   endperiod = T
#'   )


get_rawcounts <- function(
  df,
  month = 2,
  pct_desktop = 0.35,
  geo = "US",
  interrupt = "2020-03-01",
  qf_denominator = 10000000,
  endperiod = T,
  numobspermonth = NA
  ){


    # First, we need to parse comscore's website. We get the data as an rvest object
    html_data <- read_html("https://www.comscore.com/Insights/Rankings?cs_edgescape_cc=US#tab_search_query/")

    #... and pull out the url for the Google spreadsheet containing the data
    spts <- html_data %>% html_nodes("script")
    spts_text <- spts %>% html_text()
    script_with_spreadsheet <- grep("public_spreadsheet_url", spts_text, value = T)
    sheeturl <- strapplyc(script_with_spreadsheet, 'public_spreadsheet_url = "(.+pubhtml)";') %>% unlist()

    # We read the spreadsheet (authentication not required)
    sheets_deauth()
    d <- read_sheet(sheeturl, sheet = "search_query")
    names(d)[2] <- "engines"

    # ... and find the value for Google searches
    google_searches <- d %>% filter(grepl("google", tolower(engines)))


    # We choose the month and the value depending upon which month the user wants
    if(month==1){
      monthname <- names(d)[3]
      num_searches <- d[1, 3]  %>% as.numeric()
    } else {
      monthname <- names(d)[4]
      num_searches <- d[1, 4] %>% as.numeric()
    }

    print(sprintf("Using Comscore estimates for %s: %s Million Searches", monthname, num_searches))


    # We then take our data and figure out how many observations are in the month
    # we pulled
    tmpdf <- df
    names(tmpdf) <- gsub(geo, "geo", names(tmpdf))
    tmpdf$month <- month(tmpdf$timestamp)
    month_as_date <- as.Date(tolower(paste0(monthname,"-15")), format = "%b-%Y-%d")
    month_month <- month(month_as_date)
    month_year <- year(month_as_date)
    num_obs_in_month <- tmpdf %>%
            filter(
              month(timestamp) == month_month,
              year(timestamp) == month_year
            ) %>% nrow()


    # If the month we pulled is not in the dataset, we just use the frequency of the data
    if(num_obs_in_month == 0){
      freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
      num_obs_in_month <- 30 / freq
    }

    if(!is.na(numobspermonth)) num_obs_in_month <- numobspermonth

    print(sprintf("Assuming %s observations per month", num_obs_in_month))


    # Now we have the total number of searches for the month, the number of observations
    # in our dataset per month, and our query fraction. The rest is arithmetic.


    tmpdf <- tmpdf %>% mutate(
      # comscore reports in units of 1M searches
      num_desktop_searches_per_month = num_searches * 1000000,

      # From above
      num_obs_in_month = num_obs_in_month,

      # Puts total searches in the same scale as the observations
      num_desktop_searches_per_obs = num_desktop_searches_per_month / num_obs_in_month,

      # From function arguments
      pct_searches_desktop = pct_desktop,

      # Bakc out the number of total searches per observation
      num_total_searches_per_obs = num_desktop_searches_per_obs / pct_searches_desktop
    )

    tmpdf <- tmpdf %>% mutate(
      # Actual number of searches
      rawcount_actual = geo * num_total_searches_per_obs / qf_denominator,
      # Fitted number of searches
      rawcount_fitted = fitted * num_total_searches_per_obs / qf_denominator,
    )

    # only take those columns we need
    tmpdf <- tmpdf %>% select(
      timestamp,
      geo,
      fitted,
      num_desktop_searches_per_month,
      num_obs_in_month,
      num_desktop_searches_per_obs,
      pct_searches_desktop,
      num_total_searches_per_obs,
      rawcount_actual,
      rawcount_fitted
    )


    # This gives us the option to report some summary data
    if(!is.na(interrupt)){

      # If endperiod is T, take the last date in the data
      if(is.logical(endperiod)) endperiod <- max(tmpdf$timestamp, na.rm = T)

      # Make sure everything is in date format
      endperiod <- ymd(endperiod)
      interrupt <- ymd(interrupt)

      # Sum up all actual searches since the interruption
      totalsearches <- sum(tmpdf %>% filter(timestamp %within% interval(interrupt, endperiod)) %>% pull(rawcount_actual), na.rm = T)
      # Sum up all fitted searches since the interruption
      expectedsearches <- sum(tmpdf %>% filter(timestamp %within% interval(interrupt, endperiod)) %>% pull(rawcount_fitted), na.rm = T)
      # Take the difference
      excesssearches <- totalsearches - expectedsearches

      # Report results
      print(sprintf("Actual Searches from %s to %s: %s", interrupt, endperiod, totalsearches))
      print(sprintf("Expected Searches from %s to %s: %s", interrupt, endperiod, expectedsearches))
      print(sprintf("Excess Searches from %s to %s: %s", interrupt, endperiod, excesssearches))
    }

    # Replace the name of geo with the actual geography
    names(tmpdf) <- gsub("geo", geo, names(tmpdf))


    # Return the data frame with raw counts
    return(tmpdf)

}
