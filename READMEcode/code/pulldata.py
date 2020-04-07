from gtrends import timeline

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
    outpath = "C:/Users/tcapu/Google Drive/modules/gtrendR/READMEcode/input",
    creds = "C:/Users/tcapu/Google Drive/modules/gtrends/info_theo.py"
)
