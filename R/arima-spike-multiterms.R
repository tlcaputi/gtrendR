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
  kalman = T,
  include_data = T,
  linear = F,
  min0 = T
  ){



  # We want users to be able to use "pre-period" to set the number of days in
  # their before period even without a date.
  if(!is.na(preperiod) & is.na(beginperiod)){
    beginperiod <- ymd(interrupt) - preperiod
  }


  # Make sure the interruption is a Date object
  interrupt <- ymd(interrupt)


  # Find all files in the input directory
  files <- dir(input_dir, pattern=".csv", full.names = T)

  # These can be used to include or exclude certain terms
  if(!is.na(terms_to_use)){
    files <- grep(paste0(terms_to_use, collapse = "|"), files, value = T)
  }

  if(!is.na(timeframe_to_use)){
    files <- grep(timeframe_to_use, files, value = T)
  }


  # We are going to collect data through a for loop. ct is just a counter.
  summ_dat <- list()
  full_dat <- list()
  ct <- 1
  for (f in files){


    # For every individual term file, we first collect what the term is.
    term <- basename(f)
    term <- gsub("day|week|month|year|[.]csv", "", term)
    term <- trimws(term)

    # Then we read in the data
    df <- read.csv(f, header = T, stringsAsFactor = F)

    # Ensure that timestamp is a date object and that the geography is a standard name
    df$timestamp <- ymd(df$timestamp)
    names(df) <- gsub(geo, "geo", names(df))

    # If beginperiod/endperiod is T, then we use the min/max date from the dataset
    if(is.logical(beginperiod)) beginperiod <- min(df$timestamp, na.rm = T)
    if(is.logical(endperiod)) endperiod <- max(df$timestamp, na.rm = T)


    # We restrict the dataset using the beginperiod/endperiod
    df <- df %>% filter(timestamp %within% interval(beginperiod, endperiod))

    # We need to know what kind of data this is. We infer with diff.date.
    # If the minimum difference is 1 day, its daily. If it's 7, it's weekly. Etc.
    freq <- min(as.numeric(diff.Date(df$timestamp)), na.rm = T)


    # Here we put the searches into time series objects.

    # time_series is the entire timeline
    time_series <- ts(df$geo, freq = 365.25/freq, start = decimal_date(beginperiod))

    # ts_training is just the pre-period
    ts_training <- ts(df %>% filter(timestamp < interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(beginperiod))

    # ts_test is just the post-period
    ts_test <- ts(df %>% filter(timestamp >= interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(interrupt))


    # na_kalman lets us impute missing data in the time series objects if the
    # kalman object is set to T
    if(kalman){
      time_series <- na_kalman(time_series, model="auto.arima")
      ts_training <- na_kalman(ts_training, model="auto.arima")
      ts_test <- na_kalman(ts_test, model="auto.arima")
      df$geo <- as.numeric(time_series)
    }



    # If linear is True, then we use a linear model. If not...
    if(!linear){

      # We run the model
      mod <- auto.arima(ts_training)

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


    # Create a data frame with the same number of rows as ts_test for the predicted values
    preds <- data.frame(
      "actual" = as.numeric(ts_test),
      "fitted" = fitted_values$mean,
      "lo" = fitted_values$lower,
      "hi" = fitted_values$upper
    )

    # Fix the column names
    names(preds) <- gsub("[.]", "", names(preds))
    names(preds) <- gsub("^lo$", "lo95", names(preds))
    names(preds) <- gsub("^hi$", "hi95", names(preds))


    # Also create a data frame that's the same size as the original data

    # Create an empty data frame with rows = length(ts_train)
    tmp <- data.frame(matrix(NA, nrow = length(ts_training), ncol = ncol(preds)))
    names(tmp) <- names(preds)
    tmp$actual <- as.numeric(ts_training)
    # bind it
    full <- data.frame(rbind(tmp, preds))

    # change column names with the term
    names(full) <- paste0(term, "_", names(full))
    full$timestamp <- df$timestamp

    # This ensures that each value in both data frame is positive. When modelling a low
    # volume term, lo95 is often negative, which doesn't make sense (can't have negative
    # searches).

    if(min0){
      preds <- preds %>% mutate_if(is.numeric, minpos)
      full <- full %>% mutate_if(is.numeric, minpos)
    }




    # Summarizes the ratio of actual to fitted searches using only those
    # observations where the actual value is not 0
    summ <- with(preds %>% filter(actual > 0),
      data.frame(
        "term" = term,
        "mean" = (mean(actual, na.rm = T) / mean(fitted, na.rm = T)) - 1,
        "lo95" = (mean(actual, na.rm = T) / mean(hi95, na.rm = T)) - 1,
        "hi95" = (mean(actual, na.rm = T) / mean(lo95, na.rm = T)) - 1
    ))

    # print(term)
    # print(preds$actual)
    # print(preds$hi95)
    # with(preds %>% filter(actual > 0), print(mean(actual)))
    # with(preds %>% filter(actual > 0), print(mean(hi95)))
    # with(preds %>% filter(actual > 0), print(mean(lo95)))

    # Place these data.frames in the list
    summ_dat[[ct]] <- summ
    full_dat[[ct]] <- full
    ct <- ct + 1

  }

  # bind the summ_dat together
  summary <- do.call(rbind.data.frame, summ_dat)

  # Merge the full data together
  full <- Reduce(function(x,y) merge(x = x, y = y, by = "timestamp"), full_dat)

  # Return born in a list format.
  out <- list("summary" = summary, "full" = full)
  return(out)

    #
    #
    #
    # # Now we need to add the fitted data to the data.frame.
    #
    # # tmp2 is a data frame of forecasts on the test data
    # tmp2 <- data.frame(fitted=fitted_values$mean, lo=fitted_values$lower, hi=fitted_values$upper)
    #
    # # tmp1 is just an empty data.frame for the train data
    # tmp1 <- data.frame(matrix(NA, nrow=nrow(tmpdf) - length(fitted_values$mean), ncol=ncol(tmp2)))
    # names(tmp1) <- names(tmp2)
    #
    # # By binding them together, we get a data.frame with the same number of rows
    # # as the tmpdf
    # df_to_cbind <- rbind.data.frame(tmp1, tmp2)
    #
    #
    # # Depending upon which fitted values we use, the names can come out strange. This
    # # fixes that so we at least have a `lo95` and `hi95` column.
    # names(df_to_cbind) <- gsub("[.]", "", names(df_to_cbind))
    # names(df_to_cbind) <- gsub("^lo$", "lo95", names(df_to_cbind))
    # names(df_to_cbind) <- gsub("^hi$", "hi95", names(df_to_cbind))
    #
    #
    # # finaldf combines the actual data with the fitted values
    # finaldf <- cbind.data.frame(tmpdf, df_to_cbind)
    #
    # # For convenience, we set "fitted" to be the same as "actual" for all rows that
    # # were originally in the train data.
    # finaldf$fitted <- ifelse(is.na(finaldf$fitted), finaldf$geo, finaldf$fitted)
    #
    #
    #
    #
    # time_series <- ts(df$geo, freq = 365.25/freq, start = decimal_date(beginperiod))
    # ts_training <- ts(df %>% filter(timestamp <= interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(beginperiod))
    # ts_test <- ts(df %>% filter(timestamp > interrupt) %>% pull(geo), freq = 365.25/freq, start = decimal_date(interrupt))
    #
    # if(kalman){
    #
    #   if(sum(!is.na(ts_training)) < 3 | sum(!is.na(ts_test)) < 3){
    #     warning("Kalman doesn't work with so few observations")
    #     next
    #   }
    #   time_series <- na_kalman(time_series, model="auto.arima")
    #   ts_training <- na_kalman(ts_training, model="auto.arima")
    #   ts_test <- na_kalman(ts_test, model="auto.arima")
    #   df$geo <- as.numeric(time_series)
    # }
    #
    # if(!linear){
    #   mod <- auto.arima(ts_training)
    #
    #   ## EXTRACT FITTED VALUES
    #   if(bootstrap){
    #     set.seed(1234)
    #     fitted_values <- forecast(mod, h = length(ts_test), bootstrap = TRUE, npaths = bootnum)
    #   } else{
    #     fitted_values <- forecast(mod, h = length(ts_test))
    #   }
    #
    # } else{
    #
    #   x_train <- 1:length(ts_training)
    #   y_train <- as.numeric(ts_training)
    #   train <- data.frame("x" = x_train, "y" = y_train)
    #
    #   mod <- lm(y ~ x, data = train)
    #   x_test <- (length(ts_training) + 1):(length(ts_training) + length(ts_test))
    #   ft <- data.frame("x" = x_test, "y" = NA)
    #
    #
    #   if(bootstrap){
    #
    #     conf95 <- boot_predict(mod, newdata = ft, R=1000, condense = F)
    #     conf95 <- data.frame(conf95)
    #     names(conf95)[3:5] <- c("fit", "lwr", "upr")
    #     fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)
    #
    #
    #   } else{
    #
    #     conf95 <- predict(mod, newdata = ft, interval = "confidence", level = 0.95)
    #     conf95 <- data.frame(conf95)
    #     fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)
    #   }
    #
    # }
    #

}






#' multiterm_barplot: Use the data from multi_term_arima to create a barplot
#'
#' @param multiterm_list A dataframe including time as \code{timestamp} and searches for your given geography in one column.
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
  multiterm_list,
  label_df = NA,
  title = NULL,
  xlab = "Terms",
  ylab = "Greater than Expected (%)",
  ylim = NULL,
  space = 0.8,
  colorscheme = "blue",
  hicol = NA,
  save = T,
  outfn = './output/panG.png',
  width = 6,
  height = 3,
  barlabels = T
  ){

    # Choose a colorscheme
  colorschemer(colorscheme)

  # You may use the output from multi-term ARIMA. If that output is a list
  if(class(multiterm_list)=="list"){
    # we need the first object
    df <- multiterm_list[[1]]
  } else{
    # it's a data frame and we can use the whole thing
    df <- multiterm_list
  }


  # If we are given a label data frame, we can add labels to the barplot
  if(!is.na(label_df)){
    # just match the term in the multi_term_arima df to the label
    names(label_df) <- c("original", "label")
    df$term <- terms_df$label[match(df$term, terms_df$original)]
  }
  # When that isn't provided (or it is), we want _ to be spaces
  df$term <- gsub("_", " ", df$term)



  # Create the plot
  p <- ggplot(df)

  # Barplot, ordering the terms from highest mean to lowest mean
  p <- p + geom_bar(aes(x = reorder(term, -mean), y = mean), fill = hicol, stat = "identity", position=position_dodge(width=space))
  # Add errorbars from lo95 and hi95
  p <- p + geom_errorbar(aes(x = reorder(term, -mean), ymin = lo95, ymax = hi95), width=.3)

  # If user wants barlabels
  if(barlabels){
    p <- p  + geom_text(aes(
                x = reorder(term, -mean),
                # adds label above if mean is pos or below if mean is neg
                y = ifelse(mean >=0, hi95, lo95),
                label = sprintf("%1.0f%%", mean * 100),
                vjust = ifelse(mean >= 0, 0, 1)
              ))
  }

  # Pass axis arguments
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


  # Save if necessary
  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)


  # Return plot
  return(p)
}



#' multiterm_spaghetti: Use the data from multi_term_arima to create a spaghetti plot
#'
#' @param multiterm_list # from multi_term_arima
#' @keywords
#' @export
#' @examples

multiterm_spaghetti <- function(
  multiterm_list,
  interrupt = "2020-03-01",
  terms_to_use = NA,
  terms_to_exclude = NA,
  normalize = T,

  ## Plot Arguments
  beginplot = "2020-03-01", # Start date for the plot. If T, beginning of data
  endplot = "2020-04-03", # End date for the plot. If T, end of data
  title = NULL, # If NULL, no Title
  xlab = "Date", # x axis label
  lbreak = "1 week", # Space between x-axis tick marks
  xfmt = date_format("%b-%d"), # Format of dates on x axis
  ylab = "Query Fraction\n(Per 10 Million Searches)", # y axis label
  lwd = 1, # Width of the line
  spaghettilwd = 0.2, #width of spaghetti
  vlinecol = "grey72", # color of vertical line
  vlinelwd = 1, # width of vertical line

  ylim = c(NA, NA), # y axis limts

  ## Spaghetti specific adjustments
  spaghettialpha = 0.25, # How transparent do you want the spaghetti lines

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  hicol = NA, # Color of US line
  locol = NA, # Color of other lines

  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panE.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches
  ){


    # Choose a colorscheme
  colorschemer(colorscheme)

  # You may use the output from multi-term ARIMA. If that output is a list
  if(class(multiterm_list)=="list"){
    # we need the first object
    df <- multiterm_list[[2]]
  } else{
    # it's a data frame and we can use the whole thing
    df <- multiterm_list
  }

  # We only want the actual values for each term
  actual <- df %>% select(timestamp, ends_with("_actual"))

  # This allows us to easily include or exclude certain terms
  if(!is.na(terms_to_use)){
    grepstring <- paste(terms_to_use, collapse="|")
    names_to_use <- grep(grepstring, names(actual), value = T)
    actual <- actual %>% select("timestamp", names_to_use)
  }

  if(!is.na(terms_to_exclude)){
    grepstring <- paste(terms_to_exclude, collapse="|")
    names_to_exclude <- grep(grepstring, names(actual), value = T)
    actual <- actual %>% select(-grepstring)
  }


  # We transform the data from wide to long, which works better with ggplot.
  actual_long <- melt(actual, id.vars = "timestamp", variable.name = "term", value.name = "searches")
  # the data frame is now just timestamp, term, and searches
  actual_long$term <- gsub("_actual$", "", actual_long$term)
  # Ensure timestamp is a date object
  actual_long$timestamp <- ymd(actual_long$timestamp)


  # If normalize, scale everything
  if(normalize){
    actual_long <- actual_long %>% group_by(term) %>% mutate(searches = scale(searches)) %>% ungroup()
  }


  # Ensure these are date objects
  interupt <- ymd(interrupt)
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)

  # We want the mean value for all the terms included in this analysis. We do this with group_by
  grouped_data <- actual_long %>% group_by(timestamp) %>% summarise(mean = mean(searches, na.rm = T)) %>% ungroup()
  grouped_data$timestamp <- ymd(grouped_data$timestamp)
  grouped_data <- as.data.frame(grouped_data)


  # We start the plot
  p <- ggplot(actual_long)
  # For each term, we draw a spaghetti line
  p <- p + geom_line(aes(x = timestamp, y = searches, group = term), col = locol, alpha = spaghettialpha, lwd = spaghettilwd)
  # We draw a darker line for the mean value
  p <- p + geom_line(data = grouped_data, aes(x = timestamp, y = mean), col = hicol, lwd = lwd)

  # Pass axis arguments
  p <- p + scale_x_date(date_breaks = lbreak,
                   labels=xfmt,
                   limits = c(ymd(beginplot), ymd(endplot)))
  p <- p + scale_y_continuous(
    limits = ylim
  ) # Multiply by 100 & add Pct
  p <- p + labs(
    title= title,
    x = xlab,
    y = ylab
  )
  p <- p + geom_vline(xintercept=ymd(interrupt), linetype="dashed", color=vlinecol, lwd = vlinelwd)
  p <- p + theme_classic()


  # Save if necessary
  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  # Return ggplot
  return(p)

}
