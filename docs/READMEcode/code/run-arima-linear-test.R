

# Run this every time
library("gtrendR")
pacman::p_load(finalfit)
ROOTPATH <- "C:/Users/tcapu/Google Drive/modules/gtrendR/docs/READMEcode"
setwd(ROOTPATH)



run_arima <- function(
  df,
  interrupt,
  begin = T,
  end = T,
  geo = "US",
  kalman = F,
  bootstrap = F,
  bootnum = 1000,
  linear = F
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

  if(!linear){
    mod <- auto.arima(ts_training)

    ## EXTRACT FITTED VALUES
    if(bootstrap){
      set.seed(1234)
      fitted_values <- forecast(mod, h = length(ts_test), bootstrap = TRUE, npaths = bootnum)
    } else{
      fitted_values <- forecast(mod, h = length(ts_test))
    }

  } else{

    x_train <- 1:length(ts_training)
    y_train <- as.numeric(ts_training)
    train <- data.frame("x" = x_train, "y" = y_train)

    mod <- lm(y ~ x, data = train)

    x_test <- (length(ts_training) + 1):(length(ts_training) + length(ts_test))
    ft <- data.frame("x" = x_test, "y" = NA)


    if(bootstrap){

      conf95 <- boot_predict(mod, newdata = ft, R=1000, condense = F)
      conf95 <- data.frame(conf95)
      names(conf95)[3:5] <- c("fit", "lwr", "upr")
      fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)


    } else{

      conf95 <- predict(mod, newdata = ft, interval = "confidence", level = 0.95)
      conf95 <- data.frame(conf95)
      fitted_values <- data.frame("mean" = conf95$fit, "lower" = conf95$lwr, "upper" = conf95$upr)
    }



  }


  ## ADD THE FITTED VALUES TO THE DATA FRAME
  tmp2 <- data.frame(fitted=fitted_values$mean, lo=fitted_values$lower, hi=fitted_values$upper)
  tmp1 <- data.frame(matrix(NA, nrow=nrow(tmpdf) - length(fitted_values$mean), ncol=ncol(tmp2)))
  names(tmp1) <- names(tmp2)
  df_to_cbind <- rbind.data.frame(tmp1, tmp2)

  names(df_to_cbind) <- gsub("[.]", "", names(df_to_cbind))
  names(df_to_cbind) <- gsub("^lo$", "lo95", names(df_to_cbind))
  names(df_to_cbind) <- gsub("^hi$", "hi95", names(df_to_cbind))

  finaldf <- cbind.data.frame(tmpdf, df_to_cbind)
  # finaldf$polycolor <- polycolor
  finaldf$fitted <- ifelse(is.na(finaldf$fitted), finaldf$geo, finaldf$fitted)

  names(finaldf) <- gsub("geo", geo, names(finaldf))

  return(finaldf)
}


US_df <- run_arima(
  df = read.csv("./input/handwashing_day.csv", header = T, stringsAsFactor = F), # Data from gtrends
  interrupt = "2020-03-01", # Interruption point in your data
  geo = "US", # geography you want to use
  kalman = T, # If True, uses Kalman method to impute time series
  linear = T,
  bootstrap = T
)
