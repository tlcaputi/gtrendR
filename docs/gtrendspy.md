
## The First Step: Pulling Data

## The NEW Method: You can now download data directly in R!

You can now download data directly in R without having to install Python or a Python package. This feature is new, so there may be issues and not everything is properly documented yet. If you come across any issues, please post a bug report or contribute a fix!

```r
library(gtrendR)

Sys.setenv(GOOGLE_TRENDS_KEY="YOUR_API_KEY") # Save your API key as an environment variable

getTimelinesForHealth(
    batch_size = 1, # How many terms to process at once
    year_batch = "1 year", # How much time to process at once
    time.startDate = "2019-06-15", # When to start collecting data
    time.endDate = "2020-01-01", # When to stop collecting data
    timelineResolutions = c(
        "month" # can be month, year, week, or day
    ),
    terms = c( # The terms you want to pull
        "summer + winter + fall + spring", 
        "cat + cat food + dog + dog food"
    ),
    names = c( # The aliases for those pulls (what you want the CSV files to be named)
        "seasons",
        "pets"
    ),
    geoRestriction.regions = c( # Which regional geographies you want
        "US-NY", 
        "US-CA"
    ),
    geoRestriction.countries = c( # What countries you want
        "GB",
        "US"
    ),
    geoRestriction.dmas = c( # What dmas you want
    ),
    output_directory = "../output" # Where you want the output data saved
    )
)

```

This will save CSV files to the output directory with the filename format `{name}_{timelineResolution}.csv`. You can then use those CSVs for the rest of the functions in this package. The CSVs will have the form:

```
|timestamp |US         |US_AL      |US_CA      |US_NY      |
|----------|-----------|-----------|-----------|-----------|
|2020-01-02|642.8568888|636.164136 |262.0138526|991.5688604|
|2020-01-03|969.2211805|696.3971518|578.4875232|248.9556789|
|2020-01-04|232.1583943|655.6860359|189.5345507|279.1872892|
|2020-01-05|488.0699387|471.8936588|953.0010047|131.028145 |
|2020-01-06|758.2366717|997.2484335|740.3822249|558.1017193|
|2020-01-07|443.525007 |211.6926334|358.489257 |240.2757544|
|2020-01-08|947.7052461|664.2961719|346.3216015|907.9927533|
|2020-01-09|415.2533228|448.5096531|222.1345994|333.3310304|
|2020-01-10|919.4877736|254.382975 |811.7631744|134.159574 |
```



## The OLD Method: Possibly More Reliable But Probably Less Convenient

Before you begin using this package, pull the Google Trends data using the [gtrendspy package for Python3](https://www.github.com/tlcaputi/gtrendspy).

### Basic Download
We will use the following data pull to demonstrate the features of the package. Unfortunately, I cannot share the raw data.

### theo_timeline
```python

from gtrendspy import timeline

timeline.theo_timeline(
    terms = ['hand washing', 'social isolation'],
    names = ['handwashing', 'socialisolation'],
    start = '2019-01-01',
    end = '2020-06-01',
    timeframe_list = ['day'],
    geo_country_list = ['US'],
    us_states = True,
    worldwide = False,
    timestep_years = 1,
    batch_size = 2,
    outpath = "/path/to/ROOTPATH/input",
    creds = "/path/to/info.py"
)
```

To use the [gtrendspy package for Python3](https://www.github.com/tlcaputi/gtrendspy), you'll need to request an API Key from Google. Don't let that discourage you -- it's easy! Complete the short application [here](https://docs.google.com/forms/d/e/1FAIpQLSenHdGiGl1YF-7rVDDmmulN8R-ra9MnGLLs7gIIaAX9VHPdPg/viewform). If you do not wish to use the gtrends package for Python, you'll need to format your data to match the following and save it as a CSV:

```
|timestamp |US         |US_AL      |US_CA      |US_NY      |
|----------|-----------|-----------|-----------|-----------|
|2020-01-02|642.8568888|636.164136 |262.0138526|991.5688604|
|2020-01-03|969.2211805|696.3971518|578.4875232|248.9556789|
|2020-01-04|232.1583943|655.6860359|189.5345507|279.1872892|
|2020-01-05|488.0699387|471.8936588|953.0010047|131.028145 |
|2020-01-06|758.2366717|997.2484335|740.3822249|558.1017193|
|2020-01-07|443.525007 |211.6926334|358.489257 |240.2757544|
|2020-01-08|947.7052461|664.2961719|346.3216015|907.9927533|
|2020-01-09|415.2533228|448.5096531|222.1345994|333.3310304|
|2020-01-10|919.4877736|254.382975 |811.7631744|134.159574 |
```

Notice that the column with dates is titled "timestamp" and all other column names correspond to geographies. For example, the search value for the US on 2020-01-02 is 642.9. The search value for the same date for Alabama (US_AL) is 636.2.

(NOTE: These are randomly generated values that do not correspond to actual search volumes for anything.)

### Download with Related Terms

You may be interested not just in a particular search term but in a series of related search terms. In this case, you may consider using Google Trends' built-in "Top Queries" feature. You can implement that through the following function:

### theo_timeline_top
```python
from gtrendspy import topterms

topterms.theo_timeline_top(
        root_terms = ['commit suicide', 'how suicide', 'depression help', 'suicide help'], # a list of the root terms you're interested in
        num_terms_per_root = 10, # how many additional terms you want per root term
        start = '2019-01-01', # the start date
        end = '2020-04-10', # the end date
        timeframe_list = ['week'], # the timeframe you want
        outpath = "/path/to/ROOTPATH/input/individual",
        creds = "/path/to/creds.txt",
        geo_country_list = ['US'], # the region you're interested in. ONLY CHOOSE 1 or None
        batch_size = 5, # how many terms you want in each batch
        timestep_years = 1, # how many years you want to pull at once
        get_all = True, # if True, pull a data file for all of hte terms together
        all_path = "/path/to/ROOTPATH/input" # where you want the total file
        )

```

This will automatically pull the top 10 search queries related to each of your root terms and pull the appropriate timeline files. Right now it only works with one region at a time. This could be useful for creating a [multi-term barplot](/latest/en/arima-multi-terms/#multiterm_barplot) or [spaghetti plot](/latest/en/arima-multi-terms/#multiterm_spaghetti).
