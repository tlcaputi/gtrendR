# gtrendR: Analyze and Visualize data from the Google Trends API with R

## Disclaimer
This project is a work-in-progress. It works in some cases but may not work in many others (and may not be flexible enough for some users), and some extra code exists for functions that are not yet operational. Most things are not properly documented, and you may need to refer to the source code. This program has only been tested in Windows 10.

## Features

- Perform ARIMA-based interrupted time series analysis
- Analyze and visualize the difference between actual and expected searches
- Compare search volumes before and after an interruption
- Conduct analyses over several states
- Compare search volume growth among several search terms
- Easily compute estimates of the absolute counts of Google searches using Comscore

## Graph Gallery

#### line_plot
[![Line Plot](READMEcode/output/panA.png)](/arima-one-geo/#line_plot)

#### arima_plot
[![ARIMA Plot](READMEcode/output/panB.png)](/arima-one-geo/#arima_plot)

#### arima_ciplot
[![ARIMA Confidence Intervals](READMEcode/output/panC.png)](/arima-one-geo/#arima_ciplot)

#### state_pct_change
[![Percent Change by State](READMEcode/output/panD.png)](/arima-multi-geo/#state_pct_change)

#### state_arima_spaghetti
[![Spaghetti Plot by State](READMEcode/output/panE.png)](/arima-multi-geo/#state_arima_spaghetti)

#### state_arima_pctdiff
[![Percent Difference by State](READMEcode/output/panF.png)](/arima-multi-geo/#state_arima_pctdiff)

#### multiterm_barplot
[![Percent Change by Term](READMEcode/output/panG.png)](/arima-multi-terms/#multiterm_barplot)

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
