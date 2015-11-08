# hydroutils
An R package for handling hydrology data and plotting hydrologic frequency curves.  Forked from DSSRip to provide these functions in a cross-platform environment.

```wateryear```, similar to the ```lubridate``` package's ```year``` function, returns the water year for a timestamp (starting 01 Oct of the previous calendar year). ```wymonth``` returns the month of the water year, starting with October, and the ```wy.month.abb``` constant contains the month abreviations in this order.

For assessing the fit of hydrologic models, the functions ```nash.sutcliff```, ```rmse```, and ```excelR2``` are provided.

```flowBreaks``` and ```probBreaks``` are provided for generating breaks on a plot with logarithmic and normal-deviate axes, such as those used for flow frequency graphics.  ```weibullProbs``` returns the corresponding Weibull plotting position for a given vector.

```hydro_flow_trans``` and ```hydro_prob_trans``` can be used with ```ggplot```'s ```scale_continuous``` for axes to automatically apply flowBreaks and probBreaks.

# An example:

## Frequency curve
annual_peaks_data.dss contains a record with a USGS flow frequency record.

```r
require(dssrip) # use your own data, or the nile dataset included in R.
require(ggplot2)
require(scales)

testfile = opendss("C:/path/to/annual_peaks_data.dss")
peaks = getFullTSC(testfile, getPaths(testfile, "C=FLOW-*")[1])
colnames(peaks) = "FLOW"
peaks = fortify(peaks) ## xts to data.frame for ggplot2

ggplot(peaks, aes(y=FLOW, x=weibullProbs(FLOW))) + geom_point() + 
  theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
  scale_y_continuous(trans=hydro_flow_trans()) + 
  scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) + 
  stat_smooth(method="glm", family=gaussian(link="log"))
```

## Quick monthly summary hydrograph
test.dss contains a daily flow record downloaded with the USGS Import tool in DSSVue for the Delaware River at Trenton, NJ (USGS #01463500).  This isn't the best example of a summary hydrograph, and definitely not the best way to do it, but gives an idea of seasonality.
```r
require(dssrip)
require(ggplot2) # for plotting and fortify command
mydssfile = opendss("F:/test.dss")

delawareflows = getFullTSC(mydssfile, getPaths(mydssfile, "A=DELAWARE RIVER B=TRENTON NJ C=FLOW"))
plot(delawareflows)
## uses fortify.xts from ggplot, creates column for index.
deflow = fortify(delawareflows)
## Let's label months and assign them a WY order (October thru Sept)
deflow$MONTH = factor(x=wymonth(deflow$Index), labels=wymonth.abb)
## plot
ggplot(deflow, aes(x=MONTH, y=FLOW, group=MONTH)) + geom_boxplot()
