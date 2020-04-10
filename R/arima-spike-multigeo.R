#' pct_change_state: Create a map of states based upon the percentage increase in searches before or after the interruption
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

state_pct_change = function(
  df,
  interrupt,
  beginperiod = NA,
  preperiod,
  endperiod,
  scaletitle = "Pct. Increase\nin Searches",
  scalelimits = NULL,
  linecol = "gray",
  lowcol = NA,
  midcol = NULL,
  highcol = NA,
  colorscheme = "red",
  save = T,
  width = 6,
  height = 4,
  bootstrap = T,
  outfn = "./output/fig.png",
  bootnum = 1000,
  alpha = 0.05,
  return_df = T
  ){


  colorschemer(colorscheme)

  if(!is.na(preperiod) & is.na(beginperiod)){
    beginperiod <- ymd(interrupt) - preperiod
  }

  names(df) <- gsub("US_", "", names(df))

  df$timestamp <- ymd(df$timestamp)
  interrupt <- ymd(interrupt)
  beginperiod <- ymd(beginperiod)
  endperiod <- ymd(endperiod)

  tmp <- df %>% filter(timestamp %within% interval(beginperiod, endperiod)) %>%
    mutate(
      before = case_when(timestamp < interrupt ~ 1, timestamp >= interrupt ~ 0)
    ) %>%
    select(
      timestamp, before, state.abb
    )

  tmp_long <- melt(tmp, id.vars = c("timestamp", "before"), variable.name = "abbr", value.name = "searches")
  statedf <- tmp_long %>%
                group_by(abbr) %>%
                summarise(
                  searches_after = mean(searches[before==0], na.rm = T),
                  searches_before = mean(searches[before==1], na.rm = T)
                ) %>% ungroup() %>% mutate(
                  change = ((searches_after / searches_before) - 1)
                )
  statedf$state <- state.name[match(statedf$abbr, state.abb)]

  if(bootstrap){
    set.seed(1234)
    statedf$hi95 <- NA
    statedf$lo95 <- NA
    locs <- statedf$abbr
    for(loc in locs){
      beforesearches <- tmp %>% filter(before == 1) %>% pull(loc)
      aftersearches <- tmp %>% filter(before == 0) %>% pull(loc)

      beforemeans <- boot(data = beforesearches, statistic = samplemean, R = bootnum)
      aftermeans <- boot(data = aftersearches, statistic = samplemean, R = bootnum)

      bootdf <- data.frame("beforemeans" = beforemeans$t, "aftermeans" = aftermeans$t)
      bootdf <- bootdf %>% mutate(
        pctdiff = ((aftermeans / beforemeans) - 1)
      )

      booted_vec <- bootdf %>% pull(pctdiff)
      hi95 <- as.numeric(quantile(booted_vec, 1-(alpha/2), na.rm = T))
      lo95 <- as.numeric(quantile(booted_vec, (alpha/2), na.rm = T))

      statedf$hi95 <- ifelse(statedf$abbr == loc, hi95, statedf$hi95)
      statedf$lo95 <- ifelse(statedf$abbr == loc, lo95, statedf$lo95)

    }
  }



  p <- plot_usmap(
          data = statedf,
          values = "change",
          color = linecol
        ) +
        scale_fill_gradient2(
          name = scaletitle,
          label = scales::percent,
          low = lowcol,
          mid = midcol,
          high = highcol,
          limits = scalelimits
        ) + theme(legend.position = c(0.95,0.3))

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  if(return_df){
    out <- list(statedf, p)
  } else{
    out <- p
  }

  return(out)

}



#' state_arima: Run this first to do ARIMA by state functions
#'
#' @param data A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @param begin Use data after this date to make a prediction
#' @param end Predict up to this date
#' @param interrupt The date where things change. ARIMA will be predicted on all days before the interrupt.
#' @keywords
#' @export
#' @examples
#' state_list <- state_arima(
#'   data = read.csv("./temp/data.csv", header = T, stringsAsFactor = F),
#'   interrupt = "2019-03-01"
#' )


state_arima = function(
  data,
  begin = T,
  end = T,
  interrupt = "2020-03-01",
  kalman = F
  ){

  data$timestamp <- ymd(data$timestamp)
  if(begin == T) begin <- min(ymd(data$timestamp), na.rm = T)
  if(end == T) end <- max(ymd(data$timestamp), na.rm = T)

  begin <- closest_date(data = data, date = begin, type = "beforeequal")
  end <- closest_date(data = data, date = end, type = "afterequal")
  interrupt <- closest_date(data = data, date = interrupt, type = "beforeequal")

  begin <- ymd(begin)
  end <- ymd(end)
  interrupt <- ymd(interrupt)

  df <- data %>% filter(timestamp %within% interval(begin, end))
  names(df) <- gsub("US_", "", names(df))
  freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)
  states_in_dataset <- names(df)[names(df) %in% state.abb]
  states_in_dataset <- c(states_in_dataset, "US")

  ## Spaghetti plot dataset (each state for each time point)
  arima_spaghetti_df <- data.frame(matrix(NA, nrow = nrow(df), ncol = (length(states_in_dataset)*3) + 1))
  names(arima_spaghetti_df) <- c("timestamp", states_in_dataset, paste0(states_in_dataset, "_fitted"), paste0(states_in_dataset, "_pctdiff"))
  arima_spaghetti_df$timestamp <- ymd(df$timestamp)

  ## Summary (one data point for each state)
  arima_summary_df <- data.frame(state = states_in_dataset, actual = NA, fitted = NA)

  for(st in states_in_dataset){

    print(sprintf("[%s] Processing state %s", Sys.time(), st))

    time_series <- ts(df[, st], freq = 365.25/freq, start = decimal_date(begin))
    if(kalman){
      time_series <- na_kalman(time_series, model="auto.arima")
      df[, st] <- as.numeric(time_series)
    }

    ts_train <- window(time_series, end = decimal_date(interrupt))
    ts_test <- window(time_series, start = decimal_date(interrupt))

    if(!all(is.na(ts_train)) & !all(is.na(ts_test))){

      mod <- auto.arima(ts_train)

      fitted_values <- forecast(mod, length(ts_test))
      fitted_values$mean <- ifelse(fitted_values$mean<0, NA, fitted_values$mean)

      vname1 <- paste0(st, "_fitted")
      vname2 <- paste0(st, "_pctdiff")
      arima_spaghetti_df[, st] <- c(ts_train, ts_test)
      arima_spaghetti_df[, vname1] <- c(ts_train, fitted_values$mean)

      timestamp <- arima_spaghetti_df[, "timestamp"]
      pctdiff <- (arima_spaghetti_df[, st] / arima_spaghetti_df[, vname1]) - 1
      arima_spaghetti_df[, vname2] <- pctdiff

      arima_summary_df$actual <- ifelse(arima_summary_df$state == st, mean(ts_test, na.rm = T), arima_summary_df$actual)
      arima_summary_df$fitted <- ifelse(arima_summary_df$state == st, mean(fitted_values$mean, na.rm = T) , arima_summary_df$fitted)

    }

  }

  arima_spaghetti_df$timestamp <- as.character(ymd(arima_spaghetti_df$timestamp))
  out <- list("spaghetti" = arima_spaghetti_df, "summary" = arima_summary_df, "interrupt"=interrupt)


  return(out)

}





#' state_arima_spaghetti: Use the output of state_arima to create a state-level spaghetti plot of differences between the ARIMA model and actual searches
#'
#' @param state_arima_list A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @param interrupt The date where things change. ARIMA will be predicted on all days before the interrupt.
#' @param beginplot When the plot begins
#' @param endplot When the plot ends
#' @param ylim A 2-item vector for the limits of the y axis
#' @param title Title of the figure
#' @param xlab x-axis label, default is "Date"
#' @param lbreak space between tick marks on x axis
#' @param xfmt date format for x axis
#' @param ylab y-axis label, default is "Actual Versus Model-Fitted Search Queries (Pct Diff.)"
#' @param linelabel label next to interruption line
#' @param lwd line width
#' @param states_with_labels vector of states that you want to have labels
#' @param states_to_exclude vector of states not to include in the figure
#' @param save default T, if false does not save
#' @param width width in inches
#' @param height height in inches
#' @param outfn output filename
#' @keywords
#' @export
#' @examples
#' state_arima_spaghetti(
#'   state_list,
#'   interrupt = "2019-03-01",
#'   title = NULL,
#'   xlab = "Date",
#'   ylab = "Actual Versus Model-Fitted Search Queries (Pct Diff.)",
#'   data = read.csv("./temp/data.csv", header = T, stringsAsFactor = F),
#'   linelabel = "COVID-19 Outbreak",
#'   lbreak = "1 week",
#'   lwd = 0.4,
#'   beginplot = ymd("2019-03-01")-(7*1),
#'   endplot = ymd("2019-03-18"),
#'   states_with_labels = c("CA", "NY", "US", "IA"),
#'   states_to_exclude = c("IA"),
#'   save = T,
#'   width = 6,
#'   height = 4,
#'   outfn = "./output/fig.png"
#' )

state_arima_spaghetti = function(
  state_arima_list,
  beginplot,
  endplot,
  interrupt = NA,
  ylim = NULL,
  title = NULL,
  xlab = "Date",
  ylab = "Actual Versus Model-Fitted\nSearch Queries (% Diff.)",
  linelabel = "Interruption",
  lbreak = "1 week",
  lwd = 0.4,
  right_margin = 0.1,
  xfmt = date_format("%d %b"),
  states_with_labels = c("CA", "NY", "US", "IL", "TX"),
  states_to_exclude = c(),
  hicol = NA,
  locol = NA,
  nucol = NA,
  opcol = NA,
  colorscheme = "red",
  spaghettialpha = 0.25,
  extend = F,
  save = T,
  width = 6,
  height = 4,
  outfn = "./output/fig.png"
  ){


  colorschemer(colorscheme)

  if(class(state_arima_list)=="list"){
    arima_spaghetti_df <- state_arima_list[[1]]
    if(is.na(interrupt)) interrupt <- state_arima_list[[3]]
  } else{
     arima_spaghetti_df <- state_arima_list
  }

  arima_spaghetti_df$timestamp <- ymd(arima_spaghetti_df$timestamp)
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)
  interrupt <- ymd(interrupt)
  diff <- abs(as.numeric(endplot - beginplot))


  freq <- min(as.numeric(diff.Date(arima_spaghetti_df$timestamp)), na.rm = T)
  interrupt <- ymd(interrupt)
  interrupt_line <- interrupt
  # if(freq == 1){
  # interrupt_line <- interrupt -1
  # } else{
  # }


  if(!extend){
    beginplot <- closest_date(data = arima_spaghetti_df, date = beginplot, type = "beforeequal")
    endplot <- closest_date(data = arima_spaghetti_df, date = endplot, type = "afterequal")
  }
  interrupt <- closest_date(data = arima_spaghetti_df, date = interrupt, type = "beforeequal")


  arima_spaghetti_df$timestamp <- ymd(arima_spaghetti_df$timestamp)
  arima_spaghetti_df <- arima_spaghetti_df %>% filter(timestamp %within% interval(beginplot, endplot))

  freq <- min(as.numeric(diff.Date(arima_spaghetti_df$timestamp)), na.rm = T)
  states_in_dataset <- names(arima_spaghetti_df)[names(arima_spaghetti_df) %in% state.abb]
  states_in_dataset <- c(states_in_dataset, "US")


  p <- ggplot(arima_spaghetti_df)
  p <- p + geom_vline(xintercept=interrupt_line, linetype="dashed", color="grey72")

  maxval <- 0
  ct <- 0


  for(st in states_in_dataset){
    ct <- ct + 1

    pctdiffvname <- paste0(st, "_pctdiff")

    timestamp <- arima_spaghetti_df$timestamp
    pctdiff <- arima_spaghetti_df[, pctdiffvname]
    maxval <- max(c(maxval, pctdiff), na.rm = T)

    tmpdf <- data.frame(x = timestamp, y = pctdiff)

    if(!(st %in% states_to_exclude)){
      if(st=="US"){
        p <- p + geom_line(tmpdf, mapping = aes(x=x, y=y), color=hicol, linetype="solid", alpha=1, size=lwd*2)
      } else{
        p <- p + geom_line(tmpdf, mapping = aes(x=x, y=y), color=locol, linetype="solid", alpha=spaghettialpha, size=lwd)
      }
      if(st %in% states_with_labels){
        p <- p + annotate("text", x=max(timestamp[!is.na(pctdiff)])+1, y=pctdiff[length(pctdiff)], label=st)
      }
    }
  }

  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = c(ymd(beginplot), ymd(endplot) + diff*right_margin))
  p <- p + scale_y_continuous(
    limits = ylim,
    labels = function(x) paste0(x*100, "%")
  ) # Multiply by 100 & add Pct
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + geom_hline(yintercept=0, linetype="dashed", color="grey72")
  p <- p + theme_classic()

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  return(p)

}




#' state_arima_pctdiff: Run this first to do ARIMA by state functions
#'
#' @param state_arima_list A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @param linecol Color of the state boundaries
#' @param scaletitle Title on the scale
#' @param lowcol Color at lower end of scale
#' @param midcol Color at mid of scale
#' @param highcol Color of high end of scale
#' @param save default T, if false does not save
#' @param width width in inches
#' @param height height in inches
#' @param outfn output filename
#' @keywords
#' @export
#' @examples
#' state_arima_pctdiff(
#'   state_list,
#'   linecol = "gray",
#'   scaletitle = "Pct Diff nin Searches",
#'   lowcol = "white",
#'   midcol = NULL,
#'   highcol = "dodgerblue4",
#'   save = T,
#'   width = 6,
#'   height = 4,
#'   outfn = "./output/fig.png"
#' )

state_arima_pctdiff = function(
  state_arima_list,
  linecol = "gray",
  scaletitle = "% Diff.\nin Searches",
  lowcol = NA,
  midcol = NULL,
  highcol = NA,
  colorscheme = "red",
  save = T,
  width = 6,
  height = 4,
  outfn = "./output/fig.png"
){

  colorschemer(colorscheme)

    if(class(state_arima_list)=="list"){
      arima_summary_df <- state_arima_list[[2]]
    } else{
      arima_summary_df <- state_arima_list
    }

    ## Uses the summary DF from the previous for loop
    arima_summary_df <- arima_summary_df %>% mutate(pctdiff = actual / fitted - 1)
    p <- plot_usmap(data = arima_summary_df, values = "pctdiff", color = linecol) +
            scale_fill_gradient2(
              name = scaletitle, label = scales::percent,
              low=lowcol, mid = midcol, high=highcol
            ) +
            theme(legend.position = c(0.95,0.3))

    if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

    return(p)

}
