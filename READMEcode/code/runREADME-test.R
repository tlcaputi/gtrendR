

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


  files <- dir(input_dir, pattern=".csv", full.names = T)

  if(!is.na(terms_to_use)){
    files <- grep(paste0(terms_to_use, collapse = "|"), files, value = T)
  }

  if(!is.na(timeframe_to_use)){
    files <- grep(timeframe_to_use, files, value = T)
  }

  dat <- list(); ct <- 1
  for (f in files){
    print(sprintf("[%s] Processing %s", Sys.time(), f))

    term <- basename(f)
    term <- gsub("day|week|month|year|[.]csv|[_]", "", term)

    df <- read.csv(f, header = T, stringsAsFactor = F)
    names(df) <- gsub(geo, "geo", names(df))

    df$timestamp <- ymd(df$timestamp)
    if(is.logical(beginperiod)) beginperiod <- min(df$timestamp, na.rm = T)
    if(is.logical(endperiod)) endperiod <- max(df$timestamp, na.rm = T)

    beginperiod <- ymd(beginperiod)
    endperiod <- ymd(endperiod)
    interrupt <- ymd(interrupt)

    df$timestamp <- ymd(df$timestamp)
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
      "hi95" = mean(tmp$actual / tmp$lo95, na.rm = T),
      stringsAsFactor = F
    )

    dat[[ct]] <- out; ct <- ct + 1

  }

  out <- do.call(rbind.data.frame, dat)
  out <- as.data.frame(out)

  return(out)


}


multiterm <- multi_term_arima(
  input_dir = "C:/Users/tcapu/Google Drive/modules/gtrendR/READMEcode/input",
  geo = "US",
  terms_to_use = NA,
  timeframe_to_use = NA,
  beginperiod = T,
  endperiod = T,
  interrupt = "2020-03-01",
  bootstrap = T,
  bootnum = 1000,
  kalman = T
)


terms_df <- data.frame("original" = c("handwashing", "socialisolation"), "label" = c("Handwashing", "Social Isolation"))
multiterm_barplot(
  multiterm,
  terms_df = terms_df
)
