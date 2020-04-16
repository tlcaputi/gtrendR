
## Arguments
terms <- c(
  'commit suicide',
  'suicide statistics',
  'suicide',
  'suicide hotline number',
  'suicide quotes + suicidal quotes',
  'suicide prevention',
  'suicidal thoughts',
  'how to commit suicide',
  'teen suicide',
  'suicide song + suicide songs'
)

timeframe <- 'day'
ROOTPATH <- "C:/Users/tcapu/Google Drive/modules/gtrendR/docs/gtrends-variance"


## Load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, gsubfn, psych, lubridate, kable, kableExtra)


## Build Data

# Get all the file names for that term
setwd(ROOTPATH)
files <- dir("./input", ".csv", full.name = T)
files <- grep(timeframe, files, value = T)
names <- gsub(" [+] | ", "_", terms)


# Create empty lists to fill in through a for loop
summ_data_list <- list()
full_data_list <- list()
ct <- 1

# For each term
for(name in names){

  # We take all the files with that term
  same_files <- grep(name, files, value = T)
  # ... read them in and merge them into a list
  dat <- list()
  for(f in same_files){
    term <- basename(f)
    num <- strapplyc(term, "_([0-9]+)_[A-Za-z]+.csv$") %>% as.numeric() + 1
    df <- read.csv(f, header = T, stringsAsFactor = F)
    names(df) <- c("timestamp", paste0("run", num))
    dat[[num]] <- df
  }

  # Which we then combine
  df <- Reduce(function(x, y) merge(x, y, by="timestamp", all = F), dat)

  # We only use those dates where there was no missing values across pulls
  df <- df %>% filter(complete.cases(df))


  # We can get some summary statistics on the rows

  # `tmp' allows us to use the rows without worrying about timestamp`
  tmp <- df %>% select(-timestamp)
  summ_data <- data.frame(
    timestamp = ymd(df$timestamp),
    # we get the sd by row
    sd = apply(tmp, 1, function(x) sd(x)),
    # the mean by row
    mean = apply(tmp, 1, function(x) mean(x)),
    # the min by row
    min = apply(tmp, 1, function(x) min(x)),
    # the max by row
    max = apply(tmp, 1, function(x) max(x))
    ) %>% mutate(
    # compute a few interesting numbers
    sd_over_mean = sd / mean,
    range = max - min,
    range_over_mean = range / mean
  )

  # We add these to lists of data
  summ_data_list[[ct]] <- summ_data
  names(summ_data_list)[ct] <- name
  full_data_list[[ct]] <- df
  names(full_data_list)[ct] <- name
  ct <- ct + 1

}

full_data_list[[1]] %>% head() %>% kable(format = "markdown")
lapply(summ_data_list, function(x) sprintf("%.2f%%", mean(x$range_over_mean) * 100)) %>% data.frame() %>% t() %>% kable(format = "markdown")
lapply(summ_data_list, function(x) sprintf("%.2f%%", max(x$range_over_mean) * 100)) %>% data.frame() %>% t() %>% kable(format = "markdown")
lapply(summ_data_list, function(x) sprintf("%.2f", mean(x$range))) %>% data.frame() %>% t() %>% kable(format = "markdown")
lapply(summ_data_list, function(x) sprintf("%.2f", max(x$range))) %>% data.frame() %>% t() %>% kable(format = "markdown")




pacman::p_load(reshape2)
set.seed(1234)
df <- full_data_list[[1]]
long_df <- melt(df %>% filter(complete.cases(.)) %>% sample_n(60), id = "timestamp", value.name = "searches", variable.name = "run")
long_df$run <- gsub("run", "", long_df$run)
long_df$timestamp <- ymd(long_df$timestamp)
long_df <- long_df %>% arrange(timestamp)

grouped_df <- long_df %>% group_by(timestamp) %>% summarise(meansearches = mean(searches, na.rm = T)) %>% ungroup()

long_df%>% tail(40)


p <- ggplot(long_df)
p <- p + geom_vline(aes(xintercept = timestamp), linetype = "dotted")
p <- p + geom_point(aes(x = timestamp, y = searches, col = run))
s1 <- seq.Date(min(ymd(long_df$timestamp)), max(ymd(long_df$timestamp)) + 30, by = "1 month")
p <- p + scale_x_date(
    lim = c(min(s1), max(s1)),
    breaks = s1,
    labels = function(x) format(x, format = "%b %Y")
  )

p <- p + geom_line(data = grouped_df, aes(x=timestamp, y = meansearches))
# p <- p + geom_line(data = long_df %>% filter(run == 1), aes(x=timestamp, y = searches))
p <- p + theme_classic()
p <- p + theme(axis.text.x = element_text(angle = 55, hjust = 1))
p <- p + labs(
  x = "Date",
  y = "Query Fraction for 'Commit Suicide'",
  col = "Run"
)
p

ggsave("./output/commitsuicide_dotplot.png", p, width = 10, height = 4)

range(long_df$searches)

full_data_list[[1]]
summ_data_list[[1]]
lapply(summ_data_list, function(x) range(x$range_over_mean))
lapply(full_data_list, nrow)
