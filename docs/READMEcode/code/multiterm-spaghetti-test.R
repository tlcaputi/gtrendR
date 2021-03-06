library("gtrendR")
ROOTPATH <- "C:/Users/tcapu/Google Drive/modules/gtrendR/docs/READMEcode"
setwd(ROOTPATH)


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
  include_data = T
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

  summ_dat <- list()
  full_dat <- list()
  ct <- 1
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

    preds <- data.frame(
      "actual" = as.numeric(ts_test),
      "fitted" = fitted_values$mean,
      "lo" = fitted_values$lower,
      "hi" = fitted_values$upper
    )
    names(preds) <- gsub("[.]", "", names(preds))


    tmp <- data.frame(matrix(NA, nrow = length(ts_training), ncol = ncol(preds)))
    names(tmp) <- names(preds)
    tmp$actual <- as.numeric(ts_training)

    full <- data.frame(rbind(tmp, preds))


    names(full) <- paste0(term, "_", names(full))
    full$timestamp <- df$timestamp

    summ <- data.frame(
      "term" = term,
      "mean" = mean(preds$actual / preds$fitted, na.rm = T),
      "lo95" = mean(preds$actual / preds$hi95, na.rm = T),
      "hi95" = mean(preds$actual / preds$lo95, na.rm = T)
    )

    summ_dat[[ct]] <- summ
    full_dat[[ct]] <- full
    ct <- ct + 1

  }

  summary <- do.call(rbind.data.frame, summ_dat)
  full <- Reduce(function(x,y) merge(x = x, y = y, by = "timestamp"), full_dat)
  out <- list("summary" = summary, "full" = full)

  return(out)


}





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
  height = 3
  ){

  colorschemer(colorscheme)

  if(class(df)=="list"){
    df <- multiterm_list[[1]]
  } else{
    df <- multiterm_list
  }

  if(!is.na(label_df)){
    names(label_df) <- c("original", "label")
    df$term <- terms_df$label[match(df$term, terms_df$original)]
  }

  p <- ggplot(df)
  p <- p + geom_bar(aes(x = term, y = mean), fill = hicol, stat = "identity", position=position_dodge(width=space))
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


multiterm_spaghetti <- function(
  multiterm_list,
  interrupt = "2020-03-01",
  terms_to_use = NA,

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

  colorschemer(colorscheme)

  if(class(multiterm_list)=="list"){
    df <- multiterm_list[[2]]
  } else{
    df <- multiterm_list
  }

  actual <- df %>% select(timestamp, ends_with("_actual"))
  if(!is.na(terms_to_use)){
    grepstring <- paste0(terms_to_use, collapse="|")
    actual <- actual %>% select(contains(grepstring))
  }

  actual_long <- melt(actual, id.vars = "timestamp", variable.name = "term", value.name = "searches")
  actual_long$term <- gsub("_actual$", "", actual_long$term)
  actual_long$timestamp <- ymd(actual_long$timestamp)

  interupt <- ymd(interrupt)
  beginplot <- ymd(beginplot)
  endplot <- ymd(endplot)


  grouped_data <- actual_long %>% group_by(timestamp) %>% summarise(mean = mean(searches, na.rm = T)) %>% ungroup()
  grouped_data$timestamp <- ymd(grouped_data$timestamp)
  grouped_data <- as.data.frame(grouped_data)

  p <- ggplot(actual_long)
  p <- p + geom_line(aes(x = timestamp, y = searches, group = term), col = locol, alpha = spaghettialpha, lwd = spaghettilwd)
  p <- p + geom_line(data = grouped_data, aes(x = timestamp, y = mean), col = hicol, lwd = lwd)

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
  p <- p + geom_vline(xintercept=ymd(interrupt), linetype="dashed", color="grey72")
  p <- p + theme_classic()

  if(save) ggsave(p, width=width, height=height, dpi=300, filename=outfn)

  return(p)




}



multiterms <- multi_term_arima(

  ## A folder containing all of your gtrends data and ONLY your gtrends data
  input_dir = "./input",

  ## Which data to use
  geo = "US", # Geography you want to use
  terms_to_use = NA, # Terms you'd like to analyze. If NA then all terms
  timeframe_to_use = NA, # Only analyze data with filenames that contain a certain timeframe. If NA then all timeframes


  ## Parameters of time periods
  beginperiod = T, # Beginning of the before period, if T then beginning of data
  preperiod = 90, # If beginperiod is logical, preperiod is the number of days before interrupt to include in before period
  endperiod = T, # End of the end period, if T then end of data
  interrupt = "2020-03-01", # Date for interruption, splitting before and after periods


  ## Analytical arguments
  bootstrap = T, # Bootstrap CIs
  bootnum = 1000, # Number of bootstraps
  kalman = T # If T, impute with Kalman
)



multiterm_spaghetti(
  multiterm_list = multiterms,
  interrupt = "2020-03-01",
  terms_to_use = NA,

  ## Plot Arguments
  beginplot = "2020-01-01", # Start date for the plot. If T, beginning of data
  endplot = "2020-04-03", # End date for the plot. If T, end of data
  title = NULL, # If NULL, no Title
  xlab = "Date", # x axis label
  lbreak = "1 week", # Space between x-axis tick marks
  xfmt = date_format("%b-%d"), # Format of dates on x axis
  ylab = "Query Fraction\n(Per 10 Million Searches)", # y axis label
  lwd = 1, # Width of the line
  ylim = c(NA, NA), # y axis limts

  ## Spaghetti specific adjustments
  spaghettialpha = 0.6, # How transparent do you want the spaghetti lines

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  hicol = NA, # Color of US line
  locol = NA, # Color of other lines

  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panH.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches
  )
