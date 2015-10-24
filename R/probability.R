#' @name weibullProbs
#' @title Compute Weibull plotting positions for flow frequency curves
#' @author Evan Heisman
#' @description
#' generates weibull plotting positions for given vector.  Often used in hydrologic 'flow frequency curves'
#' @param Qs list of points for which weibull plotting positions are needed
#' @param exceedance boolean that determines if plotting positions should be exceedance probabilities or non-exceedance probabilities.
#' @param as.points boolean if ties should get independent points or not.
#' @return probabilities in range of (0,1) corresponding to each point in input vector.
#' @export
weibullProbs <- function(Qs, exceedance=FALSE, as.points=FALSE){
  ## Ties.method as 'min' if diplsaying points, "first" if displaying as a line.  Series of points with same value having different probabilities doesn't make sense.
  return(abs(exceedance - rank(Qs, ties.method=ifelse(as.points, "min", "first"), na.last=FALSE) / (length(Qs)+1)))
}
