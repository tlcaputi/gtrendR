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
  bootnum = 1000
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
    ts_test <- na_kalman(ts_test, model="auto.arima")
    tmpdf$geo <- as.numeric(time_series)
  }

  mod <- auto.arima(ts_training)


  ## EXTRACT FITTED VALUES
  if(bootstrap){
    set.seed(1234)
    fitted_values <- forecast(mod, h = length(ts_test), bootstrap = TRUE, npaths = bootnum)
  } else{
    fitted_values <- forecast(mod, h = length(ts_test))
  }


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
  beginplot,
  hline = T,
  endplot,
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



  colorschemer(colorscheme)


  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  interrupt <- ymd(interrupt)
  interrupt_line <- interrupt

  if(beginplot==T) beginplot <- ymd(interrupt)
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

  # if(!extend){
  #   beginplot <- closest_date(data = df, date = beginplot, type = "beforeequal")
  #   endplot <- closest_date(data = df, date = endplot, type = "afterequal")
  # }
  #
  # beginplot <- ymd(beginplot)
  # endplot <- ymd(endplot)
  # interrupt <- ymd(interrupt)

  tmp <- with(df %>% filter(timestamp %within% interval(interrupt, endplot)),
                data.frame(
                  "timestamp" = timestamp,
                  "pctdiff" = geo/fitted - 1,
                  "lo95" = geo/hi95 - 1,
                  "hi95" = geo/lo95 - 1,
                  "polycolor" = nucol
                ))


  ## CREATE PLOT
  poly <- with(tmp,
              data.frame(x = c(timestamp, rev(timestamp)), y = c(lo95, rev(hi95)), polycolor=nucol))
  p <- ggplot(tmp)
  p <- p + geom_polygon(data = poly, aes(x = x, y = y, fill=nucol), fill=nucol, alpha=polyalpha)
  p <- p + geom_line(aes(x=timestamp, y=pctdiff, group=1, color=hicol), color=hicol, linetype="solid", size=lwd)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey74")
  if(hline){
    p <- p + geom_hline(xintercept=0, linetype="dashed", color="grey74")
  }
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = as.Date(c(beginplot, endplot)))
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + scale_y_continuous(
    limits = ylim,
    labels = function(x) paste0(x*100, "%")
  )
  p <- p + theme_classic()
  p <- p + theme(legend.position="none")

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
  endperiod = T
  ){

    html_data <- read_html("https://www.comscore.com/Insights/Rankings?cs_edgescape_cc=US#tab_search_query/")
    spts <- html_data %>% html_nodes("script")
    spts_text <- spts %>% html_text()
    script_with_spreadsheet <- grep("public_spreadsheet_url", spts_text, value = T)
    sheeturl <- strapplyc(script_with_spreadsheet, 'public_spreadsheet_url = "(.+pubhtml)";') %>% unlist()

    sheets_deauth()
    d <- read_sheet(sheeturl, sheet = "search_query")
    names(d)[2] <- "engines"

    google_searches <- d %>% filter(grepl("google", tolower(engines)))

    if(month==1){
      monthname <- names(d)[3]
      num_searches <- d[1, 3]  %>% as.numeric()
    } else {
      monthname <- names(d)[4]
      num_searches <- d[1, 4] %>% as.numeric()
    }

    print(sprintf("Using Comscore estimates for %s: %s Million Searches", monthname, num_searches))

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

    if(num_obs_in_month == 0){
      freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
      num_obs_in_month <- 30 / freq
    }

    print(sprintf("Assuming %s observations per month", num_obs_in_month))


    tmpdf <- tmpdf %>% mutate(
      num_desktop_searches_per_month = num_searches * 1000000,
      num_obs_in_month = num_obs_in_month,
      num_desktop_searches_per_obs = num_desktop_searches_per_month / num_obs_in_month,
      pct_searches_desktop = pct_desktop,
      num_total_searches_per_obs = num_desktop_searches_per_obs / pct_searches_desktop
    )

    tmpdf <- tmpdf %>% mutate(
      rawcount_actual = geo * num_total_searches_per_obs / qf_denominator,
      rawcount_fitted = fitted * num_total_searches_per_obs / qf_denominator,
    )

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

    if(!is.na(interrupt)){

      if(is.logical(endperiod)) endperiod <- max(tmpdf$timestamp, na.rm = T)
      endperiod <- ymd(endperiod)
      interrupt <- ymd(interrupt)

      totalsearches <- sum(tmpdf %>% filter(timestamp %within% interval(interrupt, endperiod)) %>% pull(rawcount_actual), na.rm = T)
      expectedsearches <- sum(tmpdf %>% filter(timestamp %within% interval(interrupt, endperiod)) %>% pull(rawcount_fitted), na.rm = T)
      excesssearches <- totalsearches - expectedsearches

      print(sprintf("Actual Searches from %s to %s: %s", interrupt, endperiod, totalsearches))
      print(sprintf("Expected Searches from %s to %s: %s", interrupt, endperiod, expectedsearches))
      print(sprintf("Excess Searches from %s to %s: %s", interrupt, endperiod, excesssearches))
    }

    names(tmpdf) <- gsub("geo", geo, names(tmpdf))

    return(tmpdf)



}
