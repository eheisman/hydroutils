## Constants
#############

#' @title Hydrologic constants
#' @aliases AF_PER_CFS_DAY CFS_DAY_PER_AF MGD_PER_CFS CFS_PER_MGD
#' @description used for converting between flows in (k)cfs and volumes in (K)AF or between cfs and MGD
#' @author Evan Heisman
#' @note acre-feet to CFS-day is an exact conversion as cubic feet per acre-foot is 43560, seconds in a day is 86400, simplifing the conversion gets 24/12.1
#' @export
AF_PER_CFS_DAY = 24/12.1  ## This is an exact conversion.  == (24hr/day * 60min/hr * 60s/min)/(43560 ft^2/acre * 1 ft)
CFS_DAY_PER_AF = 12.1/24
MGD_PER_CFS = 1.547
CFS_PER_MGD = 1/1.547
