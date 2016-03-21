## Frequency Curve Axes for ggplot2
###################################
##
## Transformations for ggplot / scales package
## with inspiration from probability_trans and log_trans in the scales package.
##
## TODO:  reimplement probBreaks and flowBreaks into proper _breaks functions.
## TODO:  implement nice formatters that behave similarly.
##
## such that
## ggplot(data) + geom_line(aes(x=flow, y=probs)) +
##    scale_x_continuous(trans=hydro_flow_trans()) +
##    scale_y_continuous(trans=hydro_probs_trans())
## produces a decent flow frequency plot by default
##
## TODO: implement ways to pass arguments to flowBreaks and probBreaks through these.
##
## these are the probability_trans and log10_trans, but with special break and
## format functions.
##
## Example that works:
## ggplot(peaks, aes(y=PEAK_DAILY_FLOW, x=PROB)) + geom_point() +
##   theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
##   scale_y_continuous(trans=hydro_flow_trans()) +
##   scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) +
##   stat_smooth(method="glm", family=gaussian(link="log"))

#' @name hydro_axis_trans
#' @title Hydrologic axis transforms for ggplot2
#' @author Evan Heisman
#' @description
#' Axis transforms for hydrologic plots in ggplot2.
#' @details
#'
#' \code{hydro_prob_trans} and \code{hydro_prob_breaks} for probability axes on frequency plots
#'
#' \code{hydro_flow_trans} and \code{hydro_flow_breaks} for log axes with additional breaks between powers of ten.
#'
#' @param Q values for flow to create scale along
#' @param labels First digit of breaks to label.  Defaults to 1,2,3,5,7 for log axes; 1,2,5 for probability axes.  Multiples of these are assumed.
#' @param maxLevel smallest place to which breaks should be created for probability axes.
#' @param lines which breaks should have lines for probability axes, must be superset of labels
#' @param invert switch the direction of the probability axes
#' @param as.percent Show labels by percent instead of probability
#' @param byPeriod Show labels with return interval (1 / probability)
#' @param periodSuffix String to concatenate to return interval
#' @param distribution change distribution for probability axis, defaults to normal.  Untested with other distributions.
#' @param distArgs arguments pasted to distribution specified in \code{distribution}
#'
#' @aliases hydro_flow_trans hydro_prob_trans hydro_prob_breaks hydro_flow_breaks flowBreaks probBreaks
#' @examples
#' ggplot(peaks, aes(y=PEAK_DAILY_FLOW, x=weibullProbs(PEAK_DAILY_FLOW))) + geom_point() +
#'   theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
#'   scale_y_continuous(trans=hydro_flow_trans()) +
#'   scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) +
#'   stat_smooth(method="glm", family=gaussian(link="log"))

#' @export
flowBreaks <- function(Q, labels=c(1,2,3,5,7), blankLines=TRUE){
  logRange = log10(range(Q))
  lower = 10^floor(logRange[1])
  upper = 10^ceiling(logRange[2])
  cap = lower
  ybreaks = NULL
  ynames = NULL
  while(cap < upper){
    if(blankLines){
      newBreaks = seq(cap, cap*10-1, by=cap/10)
    } else {
      newBreaks = seq(cap, cap*10-1, by=cap)
    }
    ybreaks = c(ybreaks, newBreaks)
    ynames = c(ynames, ifelse(newBreaks %in% (labels*cap),
                              as.character(newBreaks), ""))
    cap = cap*10
  }
  names(ybreaks) = ynames
  return(ybreaks)
}

#' @export
probBreaks <- function(maxLevel=3, lines=c(1,2,5), labels=c(1,2,5), invert=TRUE, as.percent=TRUE, byPeriod=FALSE, periodSuffix=" yr"){
  probBreaks = NULL
  probLabels = NULL
  level = -1

  # TODO - assign maximum level from max(log10(weibullProbs(Q)))
  while(level >= -maxLevel){
    p = 10^level*lines
    p = c(p, 1-p)
    if(as.percent){
      labs = ifelse(c(lines, lines) %in% labels, paste0(as.character(100*p), "%"), "")
      labs = ifelse(p==0.5, "50%", labs)
    } else {
      labs = ifelse(c(lines, lines) %in% labels, as.character(p), "")
    }
    if(byPeriod){
      warning("Generating breaks with reoccurance periods shown - You shouldn't be using '-year' events!")
      period = 1 / p
      period = paste0(period, periodSuffix)
      labs = ifelse(p <= 0.5 & labs != "", paste(labs, period, sep="\n"), labs)
    }
    probBreaks = c(probBreaks, p)
    probLabels = c(probLabels, labs)
    level = level - 1
  }
  if(invert){
    probBreaks = 1-probBreaks
  }
  names(probBreaks) = probLabels
  return(probBreaks)
}

#' @export
hydro_prob_trans <- function(distribution="norm", distArgs=list(), ...){
  require(scales)

  qfun <- match.fun(str_c("q", distribution))
  pfun <- match.fun(str_c("p", distribution))

  return(trans_new(
    name=str_c("hydro_probs_", distribution),
    transform=function(x) { qDistArgs = distArgs; qDistArgs$p = x; do.call(qfun, qDistArgs)},
    inverse=function(x) { pDistArgs = distArgs; pDistArgs$q = x; do.call(pfun, pDistArgs)},
    breaks=hydro_prob_breaks(...),
    format=format_format(),
    domain=c(1e-9, 1-1e-9)))
}

#' @export
hydro_flow_trans <- function(...){
  require(scales)
  return(trans_new(
    name="hydro_flow",
    transform=log10_trans()$transform, #function(x) log(x, base=10),
    inverse=log10_trans()$inverse, #function(x) x^10,
    breaks=hydro_flow_breaks(...),
    format=format_format(),
    domain=c(1e-100,Inf)))
}

#' @export
hydro_prob_breaks <- function(...){
  return(function(x){
    magnitude = ceiling(abs(log10(min(min(x),1-max(x)))))
    return(probBreaks(maxLevel=magnitude, ...))
  })
}

#' @export
hydro_flow_breaks <- function(labels=NULL){
  return(function(x){
    magnitude = diff(log10(range(x)))
    print(magnitude)
    if(is.null(labels)){
      if(magnitude <= 1){
        return(flowBreaks(x, labels=seq(1,9)))
      } else if(magnitude > 4){
        return(flowBreaks(x, labels=c(1)))
      } else {
        return(flowBreaks(x, labels=c(1,2,3,5,7)))
      }
    } else {
      return(flowBreaks(x, labels))
    }

  })
}

# function to merge plots
#' @name mergePlots
#' @title Merge two ggplot plots with a legend on the right hand side such that the plots have the same width.
#' @author Evan Heisman
#' @description
#' Used for merging plots of different heights (unlike facets) with a common x-axis.
#' @param ... ggplot grobs to merge
#' @param plotTitle title to place at top of plot
#' @param heights vector of weights (e.g. \code{c(2,1,1)} for half page and two quarter page plots) for heights
#' @return grob of merged plots
#' @export
mergePlots <- function(..., plotTitle="", heights=NULL){
  require(plyr)
  require(gridExtra)
  require(ggplot2)
  require(grid)
  items = list(...)
  if(is.null(heights)){
    heights = rep(1, length(items))
  }
  grobs = llply(items, ggplotGrob)
  widths = llply(grobs, function(grob) grob$widths[2:5])
  maxWidths = as.list(do.call(grid::unit.pmax,widths))
  grobs = llply(grobs, function(grob){
    grob$widths[2:5] = maxWidths
    return(grob)
  })
  return(do.call(arrangeGrob, append(list(main=plotTitle, heights=heights, ncol=1), grobs)))
}
