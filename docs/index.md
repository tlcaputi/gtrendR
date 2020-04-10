# gtrendR: Analyze and Visualize data from the Google Trends API with R

## Features

While more functions may be added in the future, the functionality of this package currently focuses on analyzing "spikes" (or other increases) in search volumes from Google Trends occurring as the result of some event ("interruption"). An increase in searches following an event may indicate that the event spurred increased public interest in a health topic or behavior. Specifically, this package allows users to:

- Perform ARIMA-based interrupted time series analysis
- Analyze and visualize the difference between actual and expected searches
- Compare search volumes before and after an interruption
- Conduct ARIMA analyses analyses over several states
- Compare search volume growth among several search terms
- Easily compute estimates of the absolute counts of Google searches using Comscore

## Graph Gallery

Click on any of the following graph types to see how you can make one using your own data.

#### line_plot
[![Line Plot](READMEcode/output/panA.png)](/en/latest/arima-one-geo/#line_plot)

#### arima_plot
[![ARIMA Plot](READMEcode/output/panB.png)](/en/latest/arima-one-geo/#arima_plot)

#### arima_ciplot
[![ARIMA Confidence Intervals](READMEcode/output/panC.png)](/en/latest/arima-one-geo/#arima_ciplot)

#### state_pct_change
[![Percent Change by State](READMEcode/output/panD.png)](/en/latest/arima-multi-geo/#state_pct_change)

#### state_arima_spaghetti
[![Spaghetti Plot by State](READMEcode/output/panE.png)](/en/latest/arima-multi-geo/#state_arima_spaghetti)

#### state_arima_pctdiff
[![Percent Difference by State](READMEcode/output/panF.png)](/en/latest/arima-multi-geo/#state_arima_pctdiff)

#### multiterm_barplot
[![Percent Change by Term](READMEcode/output/panG.png)](/en/latest/arima-multi-terms/#multiterm_barplot)


## Disclaimer
This project is a work-in-progress. It works in some cases but may not work in many others (and may not be flexible enough for some users), and some extra code exists for functions that are not yet operational. Most things are not properly documented, and you may need to refer to the source code. This program has only been tested in Windows 10.


## Citation

If you find this package useful, please consider citing me in your work.

```text
Caputi TL. 2020. gtrendspy and gtrendR: Packages for analyzing Google Trends Data.
```

<!--
## Project layout

    docs/
        index.md  # The documentation homepage.
        gtrendspy.md   # Pull data from gtrendspy
        installation.md   # Install the gtrendR package
        armia-one-geo.md   # Analyze a spike in searches
        arima-multi-geo.md   # Compare several geographies
        arima-multi-terms.md   # Compare several terms
        absolute-counts.md   # Retrieve absolute counts of searches from Comscore -->
