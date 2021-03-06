% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima-spikes-onegeo.R
\name{arima_plot}
\alias{arima_plot}
\title{arima_plot: This uses the output from \code{run_arima} to create a figure showing the
difference between the actual and expected searches for a single geography.}
\usage{
arima_plot(
  df,
  geo = "US",
  title = NULL,
  xlab = "Date",
  xfmt = date_format("\%b \%Y"),
  ylab = "Query Fraction\\n(Per 10 Million Searches)",
  outfn = "./output/fig.pdf",
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
  alpha = 0.05,
  labsize = 0.5
)
}
\arguments{
\item{df}{The dataframe as outputted by \code{run_arima}.}

\item{geo}{The column name of the geography of searches that you want.}

\item{title}{The title of the figure. The default is no title.}

\item{xlab}{The label for the x axis}

\item{ylab}{The label for the y axis}

\item{outfn}{Where to save the plot.}

\item{beginplot}{The date you want the plot to start}

\item{endplot}{The date you want the plot to end.}

\item{lbreak}{The distance between tick marks in the x axis, i.e., "one year" or "3 month"}

\item{linelabel}{Label next to the vertical interruption line}

\item{interrupt}{The date of the interruption (should be the same as \code{run_arima})}

\item{width}{Width of the plot in inches}

\item{height}{Height of the plot in inches}

\item{lwd}{Line width}

\item{save}{Default is True. If False, the plot is not saved.}
}
\description{
arima_plot: This uses the output from \code{run_arima} to create a figure showing the
difference between the actual and expected searches for a single geography.
}
\examples{
arima_plot(
           df,
           title = "Searches to Purchase Cigarettes - US",
           xlab = "Date",
           ylab = "Query Fraction",
           outfn = './output/fig.pdf',
           beginplot = "2019-09-01",
           endplot = "2020-01-15",
           lbreak = "1 year",
           linelabel = "Tobacco 21 Signed",
           interrupt = ymd("2019-12-19"),
           width = 6,
           height = 3,
           lwd = 0.3,
           save = T
           )
}
