## Getting Absolute Number of Searches using Comscore

You may be interested in calculating an estimate of the absolute number of searches executed since your interruption or as estimate for the absolute number of "excess" searches (searches above what you would expect if your interruption didn't occur). For this, we use `get_rawcounts` -- but PLEASE read the following explanation before you use it. The process described below is [standard in the Google search trends literature](https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2719193) but deserves some explanation because it rest on several key assumptions.

This function uses the estimates of monthly Desktop search volumes from [Comscore](https://www.comscore.com/Insights/Rankings?cs_edgescape_cc=US#tab_search_query/) to extrapolate the count of volumes for your searches. Comscore data is quite limited. At any given time, Comscore provides estimates for the total volume of Desktop-based (i.e., not mobile) Google searches for the United States for two somewhat recent months. For example, as of April 10, 2020, they have estimates for January 2020 and February 2020. The function actively pulls these numbers from Comscore's website, so they will always be the two most current months available from Comscore. We make a number of assumptions to turn our query fractions from the API (searches per 10M) and the Comscore Desktop searches into estimates for absolute counts.

1. We first make an assumption as to which month is most correct for your analysis. If you set `month` to 1, it will take the volume estimate for the earlier month. If you leave `month` as the default (2), it will take volume estimate for the later month. We assume that the raw count of Desktop Google searches is unchanging among months in your dataset.

2. Next, we need to assume how these Desktop Google searches are distributed within months. The function assumes that the searches are distributed uniformly, i.e., each day or week has the same number of searches. If the Comscore month is in your dataset, the function figures out how many observations were in that month, divides the raw count by that number, and sets the result as the raw count of Desktop searches for every observation. If the Comscore month is not in your dataset, the function divides the raw count by 30 and multiplies it by the minimum number of days between any two observations in your dataset. For example, if it daily data, it divides the raw count by 30 and multiplies it by 1 to get daily Desktop Google search count.

3. Then, we have to make an assumption about the percentage of all Google searches that are executed on Desktops. You can set this parameter through `pct_desktop`. We use this number to back out the total number of Google searches (i.e., desktop and mobile). If there is 100 Desktop Google Searches, and you expect 35% of Google searches to be Desktop searches, then the total number of Google searches is 100/0.35 = 285.

This function prints the raw counts (the actual number of searches for your keywords, the expected number of searches, and the excess number of searches) between your `interrupt` and `endperiod`, as well as several of the assumptions from above. It also returns a data.frame that contains the raw search counts for each observation in the input data. I highly recommend you check the output data to ensure that the assumptions are reasonable.


### raw_counts
```r
rawcounts_df <- get_rawcounts(
  df = US_df, # data from run_arima

  ## Analysis arguments
  interrupt = "2020-03-01", # Beginning of period to calculate raw counts
  endperiod = T, # Default T, estimates will be given until the last available date in the dataset
  geo = "US", # the geography you're interested in

  ## Comscore Arguments
  month = 2, # Default is 2
  pct_desktop = 0.35,

  ## Google Trends API Argument
  qf_denominator = 10000000 # Denominator of query fractions, should be 10M, do not change
)
```

This function prints the following:

```text
[1] "Using Comscore estimates for Feb-2020: 11127 Million Searches"
[1] "Assuming 29 observations per month"
[1] "Actual Searches from 2020-03-01 to 2020-04-05: 1405660.54909038"
[1] "Expected Searches from 2020-03-01 to 2020-04-05: 316628.32025804"
[1] "Excess Searches from 2020-03-01 to 2020-04-05: 1089032.22883234"

```

This indicates that there were approximately 1.4 million searches for hand washing between March 1, 2020 and April 5, 2020, where approximately 300,000 were expected. Therefore, there were approximately 1.1 million excess searches for hand washing in this period.
