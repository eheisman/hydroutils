
## Water specific date functions
#################################

#' @name hydrologic_date_functions
#' @title Hydrologic date helper functions
#' @author Evan Heisman
#' @description Provides 'lubridate' type functions for getting numeric values from `Date` and `POSIXt` objects based on the water year.
#' @note \code{wy} is no longer provided due to conflicts with the common use in hydrologic programs
#' @usage
#' \code{wateryear(t)}
#' \code{wymonth(t)}
#' \code{wyday(t)}
#' \code{wymonth.abb}
#' \code{months14()(t)}
#'
#' @param t timestamp object (e.g. \code{Date}, \code{POSIXt} classes)
#' @param splitMonths numerals for months that are to be split in two for 14 (or n) period models, defaults to April (7) and August (11).
#' @param splitDay day on which months should be split, defaults to 16.
#' @param monthAbbrevs the ordered abbreivations to use, defaults to wymonth.abb.  Use month.abb if you want calendar year order, month.name if you want full month names
#' @param sep separator between month and periodNames
#' @param case function to convert cases, \code{toupper} for upper case months, \code{tolower} for lower case, null (default) for no change
#' @param periodNames - naming convention for periods, defaults to 1 and 2, for example, could be I and II, or A and B.
#' @return
#'
#' \code{wateryear} water year for a given timestamp (October of previous calendar year through September)
#'
#' \code{wymonth} month of water year (Oct = 1, Sept = 12)
#'
#' \code{wyday} day of water year (01Oct=1, 30Sep=365+leapyear(y))
#'
#' \code{wymonth.abb} abbreviated months of year sorted by water year order
#'
#' \code{months14} returns a function that when diven a timestamp, returns a factor of the 'abbreviated' 14-period months, can be more split periods than just typical 14-periods.
#'
#' @aliases wateryear wymonth wyday wymonth.abb months14 months14 wy

#' @export
wateryear <-  function(t) {
  require(lubridate)
  year(t) + ifelse(month(t) >= 10, 1, 0)
}

#' @export
wymonth = function(t){
  require(lubridate)
  (month(t) + 2) %% 12 + 1
}

#' @export
wyday <- function(t) {
  require(lubridate)
  as.integer(as.POSIXct(t) - as.POSIXct(paste0(wateryear(t)-1, "-10-01")))
}

#' @export
wymonth.abb = month.abb[c(10:12,1:9)]

#' @export
months14=function(splitMonths=c(7,11), splitDay=16, monthAbbrevs=wymonth.abb, sep="", case=NULL, periodNames=c("1","2"), ...) {
  require(plyr)
  require(lubridate)
  if(is.numeric(splitMonths)){
    splitMonths = monthAbbrevs[splitMonths]
  }
  if(length(splitDay) == 1){
    splitDay = rep(splitDay, length(splitMonths))
  }
  ## Do nothing with the case if not specified

  caseFunc = function(x) x
  if(!is.null(case)){
    caseFunc = case
  }
  splitMonths = caseFunc(splitMonths)
  ## compute the periods list
  periods = unlist(llply(monthAbbrevs, function(m){
    if(m %in% splitMonths){
      return(paste(m, periodNames,sep=sep))
    } else {
      return(m)
    }
  }))
  return(function(d){
    md = month(d, label=TRUE, abbr=TRUE)
    factor(ifelse(md %in% splitMonths,
                  paste0(md,ifelse(day(d)<splitDay, periodNames[1], periodNames[2]),sep=sep),
                  paste0(md)),
           levels=periods, ordered=TRUE)
  })
}
