from gtrendspy import timeline
from datetime import datetime, timedelta
import re
from random import randint
from time import sleep


terms = [
        # 'suicide chat',
        # 'suicide is painless',
        # 'national suicide hotline',
        # 'suicide prevention chat',
        # 'painless suicide',
        # 'how to kill yourself', 'suicide hotline',
        # 'how to suicide',
        # 'suicidal ideation',
        # 'suicidal',
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
]

initial_date = datetime.strptime("2019-03-01", "%Y-%m-%d")
step = 2
interval = 360
numruns = 10

for index in range(0, numruns):

    start = initial_date + timedelta(days = step * index)
    start_string = start.strftime("%Y-%m-%d")
    end = start + timedelta(days = interval)
    end_string = end.strftime("%Y-%m-%d")

    print("start date is {}".format(start_string))
    print("end date is {}".format(end_string))

    timeline.theo_timeline(
        terms = terms,
        names = ["{}_{}".format(re.sub(" \+ | ", "_", x), index) for x in terms],
        start = start_string,
        end = end_string,
        timeframe_list = ['day'],
        geo_country_list = ['US'],
        us_states = False,
        worldwide = False,
        timestep_years = 10,
        batch_size = 5,
        outpath = "C:/Users/tcapu/Google Drive/modules/gtrendR/docs/gtrends-variance/input",
        creds = "C:/Users/tcapu/Google Drive/modules/gtrendspy/info_theo.py"
    )

    sleep(randint(10,20))
