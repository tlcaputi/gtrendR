# # Only run this once
# library(devtools)
# devtools::install_github("tlcaputi/gtrendR")

# Run this every time
library("gtrendR")
ROOTPATH <- "C:/Users/tcapu/Google Drive/modules/gtrendR/READMEcode"
setwd(ROOTPATH)

US_df <- run_arima(
  df = read.csv("./input/handwashing_day.csv", header = T, stringsAsFactor = F), # Data from gtrends
  interrupt = "2020-03-01", # Interruption point in your data
  geo = "US", # geography you want to use
  kalman = T # If True, uses Kalman method to impute time series
)

panA <- line_plot(
  US_df, # data from run_arima
  geo = 'US', # geography you wnat to use

  ## Create a vertical "interruption" line in your plot
  interrupt = "2020-03-01", # Date of an interruption
  linelabel = "COVID19",

  ## Plot arguments
  beginplot = T, # Start date for the plot. If T, beginning of data
  endplot = T, # End date for the plot. If T, end of data
  title = NULL, # If NULL, no Title
  xlab = "Date", # x axis label
  lbreak = "3 month", # Space between x-axis tick marks
  xfmt = date_format("%b %Y"), # Format of dates on x axis
  ylab = "Query Fraction\n(Per 10 Million Searches)", # y axis label
  lwd = 0.3, # Width of the line

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  hicol = NA, # Searches line color
  opcol = NA, # Color of point on top of spike


  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panA.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches
)



panB <- arima_plot(
  US_df, ## data from run_arima

  ## Create a vertical "interruption" line in your plot
  interrupt = "2020-03-01", # Date of an interruption
  linelabel = "COVID19",
  linelabelpos = 0.02, # Where the label goes near the interruption line

  ## Plot Arguments
  beginplot = "2019-09-01", # Start date for the plot. If T, beginning of data
  endplot = "2020-04-01", # End date for the plot. If T, end of data
  title = NULL, # If NULL, no Title
  xlab = "Date", # x axis label
  lbreak = "1 month", # Space between x-axis tick marks
  xfmt = date_format("%b %Y"), # Format of dates on x axis
  ylab = "Query Fraction\n(Per 10 Million Searches)", # y axis label
  lwd = 1, # Width of the line

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  hicol = NA, # Actual line color
  locol = NA, # Expected line color
  nucol = NA, # Excess polygon color


  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panA.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches

)

title <- ggdraw() +
  draw_label(
    "Google Searches",
    fontface = 'bold',
    hjust = 0.5
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

fig <- plot_grid(panA, panB, labels=c(LETTERS[1:2]), ncol=1, nrow=2, rel_heights=c(1,1))
fig <- plot_grid(title, fig, ncol = 1, rel_heights = c(0.1, 1))
save_plot("./output/Fig1.png", fig, base_width=6, base_height=6)


out <- state_pct_change(
  df = read.csv("./input/handwashing_day.csv", header = T, stringsAsFactor = F), ## Data from gtrends

  ## You will need to decide on the timeframes for "before" and "after"
  beginperiod = NA, # If not NA, this is the start of the "before" period
  preperiod = 90, # If beginperiod is NA, this uses 90 days before the interruption
  interrupt = "2020-03-01", # The date of the interruption
  endperiod = "2020-04-01", # The after period is the interruption to the endperiod

  ## Scale Legend
  scaletitle = "% Increase\nin Searches",
  scalelimits = NULL, # Vector of length 2 with lower and upper limit

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  highcol = NA, # Color for highest percent change
  midcol = NA, # Color for 0 percent change
  lowcol = NA, # Color for lowest percent change
  linecol = "gray", # Line between states

  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panC.png', # Location to save plot
  width = 6, # Width in inches
  height = 3, # Height in inches

  ## Get data back from this function
  return_df = T,
  # If this is True...
  bootstrap = T, ## Bootstrap confidence intervals for pct change
  bootnum = 1000, # Number of bootstraps
  alpha = 0.05 # Alpha value for CIs

)

panC <- out[[2]]


state_list <- state_arima(
  data = read.csv("./input/handwashing_day.csv", header = T, stringsAsFactor = F), ## Data from gtrends
  interrupt = "2020-03-01", ## Interruption point
  begin = T, ## Beginning of the time period to use
  end = T, ## End of the time period to use
  kalman = T ## If True, Kalman impute NAs in the time series
)



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


  freq <- min(as.numeric(diff.Date(arima_spaghetti_df$timestamp)), na.rm = T)
  interrupt <- ymd(interrupt)
  if(freq == 1){
    interrupt_line <- interrupt -1
  } else{
    interrupt_line <- interrupt
  }




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
  p <- p + geom_vline(xintercept=closest_date(data = arima_spaghetti_df, date = interrupt, type="before"), linetype="dashed", color="grey72")

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
                   limits = c(as.Date(beginplot), as.Date(endplot) + 5))
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

panD <- state_arima_spaghetti(
  state_list, # data from state_arima
  interrupt = "2020-03-01", # should be the same as state_arima

  ## Plot Arguments
  beginplot = ymd(interrupt) - 7, # Start date for the plot. If T, beginning of data
  endplot = "2020-04-01", # End date for the plot. If T, end of data
  title = NULL, # If NULL, no Title
  xlab = "Date", # x axis label
  lbreak = "1 month", # Space between x-axis tick marks
  xfmt = date_format("%b %Y"), # Format of dates on x axis
  ylab = "Query Fraction\n(Per 10 Million Searches)", # y axis label
  lwd = 1, # Width of the line

  ## Spaghetti specific adjustments
  spaghettialpha = 0.25, # How transparent do you want the spaghetti lines
  states_with_labels = c("US"), ## Add labels to the end of these
  states_to_exclude = c("IA"), ## Don't include these

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  hicol = NA, # Color of US line
  locol = NA, # Color of other lines

  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panD.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches
)


panE <- state_arima_pctdiff(
  state_list, # data from state_arima

  ## Set a colorscheme
  colorscheme = "blue",  # Color schemes set in this package "red", 'blue" or "jamaim"

  # ... customize any color using these
  highcol = NA, # Color for highest percent change
  midcol = NA, # Color for 0 percent change
  lowcol = NA, # Color for lowest percent change
  linecol = "gray", # Line between states

  ## Scale Arguments
  scaletitle = "% Diff.\nin Searches",

  ## Saving arguments
  save = T, # If T, save plot
  outfn = './output/panE.png', # Location to save plot
  width = 6, # Width in inches
  height = 3 # Height in inches

)



title <- ggdraw() +
  draw_label(
    "Google Searches",
    fontface = 'bold',
    hjust = 0.5
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
fig <- plot_grid(panC, panD, panE, labels=c(LETTERS[3:5]), ncol=1, nrow=3, rel_heights=c(1.1, 1, 1.1))
fig <- plot_grid(title, fig, ncol = 1, rel_heights = c(0.05, 1))
save_plot("./output/Fig2.png", fig, base_width=7, base_height=12)
