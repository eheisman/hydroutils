## Model Performance Functions
################################
#' @name model_error_measurement
#' @aliases rmse nash.sutcliffe excelR2
#' @title Model error measurement metrics
#' @author Evan Heisman
#' @description Common interface for metrics commonly for assessing quality of hydrologic models, statistical or otherwise.
#' @usage
#' \code{excelR2(x.obs, x.model)}
#' \code{rmse(x.obs, x.model)}
#' \code{rmse(residuals)}
#' \code{nash.sutcliffe(x.obs, x.model)}
#' \code{nash.sutcliffe(x.obs, x.model, x.alt)}
#'
#' @param x.obs - observed values
#' @param x.model - modeled values
#' @param x.alt - alternate model for comparison (Nash Sutcliffe only, defaults to mean of observed)
#' @details
#'
#' \code{excelR2} returns the R^2 as reported by Excel's curve fits.  It is provided as people are comfortable with it, but is a terrible measure of model accuracy.  See the references below.
#'
#' \code{rmse} returns the root mean square error.  With one parameter assumes x.obs is residuals, with two, computes residuals between x.obs and x.model
#'
#' \code{nash.sutcliffe} returns the Nash-Sutcliffe model coefficent, greater than 0 if `x.model` is a better fit than x.alt, 1 if a perfect fit, and between 0 and -infinity if a worse fit than x.alt.
#'
#' @references Hopper, T. (2014), Can We do Better than R-squared? http://www.r-bloggers.com/can-we-do-better-than-r-squared/ retrieved 16 May 2014
#' @references Nash, J. E. and J. V. Sutcliffe (1970), River flow forecasting through conceptual models part I - A discussion of principles, Journal of Hydrology, 10 (3), 282-290.
#' @export

#' @title Excel-like R^2 function
#' @name excelR2
excelR2 = function(x.obs, x.model, warn=FALSE){
  if(warn){
    warning("Why are you using excelR2?  Didn't you read the manual!")
  }
  cor(x.obs, x.model, method="pearson")**2
}
#' @title RMSE function
#' @name rmse
#' @export
rmse = function(x.obs, x.model=NULL, residuals=x.obs){
  if(!is.null(x.model)){
    residuals = x.obs - x.model
  }
  return(sqrt(mean((residuals)**2)))
}

#' @title Nash-Sutcliffe model coefficient function
#' @name nash.sutcliffe
#' @export
nash.sutcliffe = function(x.obs, x.model, x.alt=mean(x.obs)){
  return(1 - sum((x.obs - x.model)**2) / sum((x.obs - x.alt)**2))
}


