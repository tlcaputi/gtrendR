
Before you begin using this package, pull the Google Trends data using the [gtrendspy package for Python3](https://www.github.com/tlcaputi/gtrendspy). For example, we will use the following data pull to demonstrate the features of the package. Unfortunately, I cannot share the raw data.

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

To use the [gtrendspy package for Python3](https://www.github.com/tlcaputi/gtrendspy), you'll need to request an API Key from Google. You can do that easily [here](https://docs.google.com/forms/d/e/1FAIpQLSenHdGiGl1YF-7rVDDmmulN8R-ra9MnGLLs7gIIaAX9VHPdPg/viewform). If you do not wish to use the gtrends package for Python, you'll need to format your data to match the following and save it as a CSV:

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

Notice that the column with dates is titled "timestamp" and all other column names correspond to geographies. For example, the search value for the US on 2020-01-02 is 642.9. The search value for the same date for Alabama (US_AL) is 636.2.

(NOTE: These are randomly generated values that do not correspond to actual search volumes for anything.)
