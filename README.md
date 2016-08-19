# hydroutils
An R package for handling hydrology data and plotting hydrologic frequency curves.  Forked from DSSRip to provide these functions in a cross-platform environment.  This package should not require the links to DSSVue and should be useful for more general hydrologic analysis.

`wateryear`, similar to the `lubridate` package's `year` function, returns the water year for a timestamp (starting 01 Oct of the previous calendar year). `wymonth` returns the month of the water year, starting with October, and the `wy.month.abb` constant contains the month abreviations in this order.  `wyday` returns the day of the wateryear, with 1 being Oct 1st and 356 or 366 being Sept 30th depending on if it is a leap year or not.

For assessing the fit of hydrologic models, the functions `nash.sutcliffe`, `rmse`, and `excelR2` are provided.  The `nash.sutcliffe` function supports an argument `x.alt` to replace the mean of the `x.obs` dataset with a different alternative model to provide more resolution in the resulting score.  This can either be the best 'dumb' model output, where no calibration is required to get a reasonable answer.

`weibullProbs` returns the corresponding Weibull plotting position for a given vector to plot on a flow-frequency graphic.

`hydro_flow_trans` and `hydro_prob_trans` can be used with `ggplot`'s `scale_continuous` for axes to automatically apply flowBreaks and probBreaks.  These call the methods `flowBreaks` and `probBreaks` for generating breaks on a plot with logarithmic and normal-deviate axes, such as those used for flow frequency graphics.

# Examples:

The example below has been superceeded by [one in the examples directory](https://github.com/eheisman/hydroutils/blob/master/examples/hydroutil_examples.md) which retrieves data from the USGS via their [`dataRetrieval` package](https://github.com/USGS-R/dataRetrieval).  If you do not want to install (on Windows) or cannot install (on Mac and Linux) the `dssrip` library, the DSS references the files below can be replaced with either the `nile` dataset or data obtained through the USGS's `dataRetrieval` package)*

## Frequency curve
annual_peaks_data.dss contains a record with a USGS flow frequency record.

```r
require(hydroutils)
require(dssrip) # or use your own data or the R example 'nile' dataset
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
test.dss contains a daily flow record downloaded with the USGS Import tool in DSSVue for the Delaware River at Trenton, NJ (USGS #01463500).  This isn't the best example of a summary hydrograph, and definitely not the best way to do it, but gives an idea of seasonality and creating a WY based plot with `wymonth` and `wymonth.abb`.

```r
require(hydroutils)
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
```
