NULL

#'
#' Transfroms a data.frame containing \code{year,month,day,hour} into a "zoo" object
#'
#' @param x data frame
#' @param station names of data.frame columns to be considered
#' @param tz timezone. Default is \code{"A"}.
#' @param ... further arguments
#' 
#' @return a 'zoo' object
#' 
#' 
#' @export
#' 
#' 


dataframe2zoo <- function(x,station=NULL,tz="A",...) {
	
	
	
	if (is.null(station)) station <- names(x)[!(names(x) %in% c("year","month","day","hour","minute","second"))]
	
	
	
	year <- array(1979,nrow(x))
	month <- array(1,nrow(x))
	day <- array(1,nrow(x))
	hour <- array(0,nrow(x))
	minute <- array(0,nrow(x))
	second <- array(0,nrow(x))
	
	if (!is.null(x$year)) year <-  x$year
	if (!is.null(x$month)) month <-  x$month
	if (!is.null(x$day)) day <-  x$day	
	if (!is.null(x$hour)) hour <- x$hour
	if (!is.null(x$minute)) minute <- x$minute
	if (!is.null(x$second)) second <- x$second
	
	
	
	dates <- ISOdatetime(year,month,day,hour,minute,second,tz=tz,...)
#	str(dates) print
	
#	print(dates[dates[-length(dates)]==dates[-1]])
	
	out <- zoo(x[,station],dates)
	
	
	return(out)
}
	

