NULL

#'
#' It calcultes air umidity from hourly observation of temperature and daily time series of dew point temperature
#'
#' @param T_hourly hourly air temperature time-series 
#' @param Td daily air dew point temperature
#' @param percent logical value. If \code{TRUE} (default) the relative humidity is returned in per cent
#' @note TBoth the variables \code{T_hourly} and \code{Td} must be \code{zoo} object referred to the same period 
#' @export
#' 
#' 
#' @return hourly time series of relative humidity (\code{zoo} object)
#' 
#' @seealso \code{\link{TDEW}}
#'
#'
#' @examples
#'
#' rm(list=ls())
#' 
#' library(RHUMIDITY)
#' library(zoo)
### library(lattice)
#' data(pine) # contains pine hourly datasat 
#' index(pine) <- as.POSIXlt(index(pine),tz="A") # changes index class type
#' 
#' 
#' 
#' ## get daily temperature Time series
#' 
#' station <- c("PINE")
#' 
#' # Calibratipn coefficien for polygonal relatioships between DDI and Daily Daw Point Temperature by Emanuele Eccel 
#' 
#' intercept_prec <- c(1.2)
#' names(intercept_prec) <- station 
#' 
#' coeff_prec <- c(-0.37)
#' names(coeff_prec) <- station 
#' 
#' 
#' intercept_noprec <- c(2.7)
#' names(intercept_noprec) <- station 
#' 
#' coeff_noprec <- c(-1.5)
#' names(coeff_noprec) <- station #adjusted!!
#' 
#' T_hourly <- pine$T
#' prec_hourly <- pine$prec
#' RH_hourly <- pine$RH
#' 
#' 
#' ## daily aggregation 
#' 
#' prec <- aggregate(prec_hourly,by=as.Date(index(T_hourly)),FUN=sum)
#' Tm <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=mean)
#' Tn <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=min)
#' Tx<- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=max)
#' 
#' ### Daily dew temperature
#' 
#' Td <- TDEW(Tx=Tx,Tn=Tn,Tmean=Tm,prec=prec,lag=10,valmin_prec=0.5,intercept_prec=intercept_prec,intercept_noprec=intercept_noprec,coeff_prec=coeff_prec,coeff_noprec=coeff_noprec,DDI_lim=NULL)
#' 
#' 
#' 
#' RH_hourly_calc <- air_humidity(T_hourly=T_hourly,Td=Td)
#' 
#' # Date from which the measurement instrument work well!!!
#' data0 <- as.POSIXlt("2011-04-01 00:00:00",tz="A")
#' 
#' # start and end dates for a possible visualization!!!
#' 
#' start <- as.POSIXlt("2012-05-01 00:00:00",tz="A")
#' end <- as.POSIXlt("2012-05-15 23:00:00",tz="A")
#' 
#' days_calc <- (index(RH_hourly_calc)>=start & index(RH_hourly_calc)<=end)
#' days <- (index(RH_hourly)>=start & index(RH_hourly)<=end)
#' 
#' day_correct <-  (index(RH_hourly)>=data0)
#' 
#' RH_hourly_calc_m <- RH_hourly_calc[days_calc]
#' RH_hourly_m <- RH_hourly[days]
#' T_hourly_m <- T_hourly[days]
#' prec_hourly_m <- prec_hourly[days]
#' 
#' # temporary plot 
#' 
#' plot(RH_hourly_m)
#' lines(RH_hourly_calc_m,col=2)
#' 
#' # daily aggregation of relative humidity 
#' #
#' RHm <- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=mean)
#' RHn <- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=min)
#' RHx<- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=max)
#' 
#' RHm_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=mean)
#' RHn_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=min)
#' RHx_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=max)








air_humidity <- function(T_hourly,Td,percent=TRUE) {
	

	row <- julian(as.Date(index(T_hourly)))-julian(as.Date(index(T_hourly)[1]))+1


	
#	print(summary(row))
#	print(median(row))
	out <- T_hourly
	# FARE MODIFICHE!!!
	temp <- array(NA,dim(T_hourly))
	
	td <- as.matrix(Td)[row,]
	tn <- as.matrix(T_hourly)
	temp <- saturated_water_vapor_pressure(td)/saturated_water_vapor_pressure(tn)
	

	if (percent) temp <- temp*100
	
	if (ncol(as.matrix(out))>1) {
		
		out[,] <- temp
		
	} else {
		
		out[] <- temp
	}
	
	
	return(out)
	
		
} 

NULL 
#'
#' It calculates the saturated water vapor pressure head (expressed in hPa) in function of air temperature 
#'
#'@param T temperature expressed in Celsius degrees
#'@param root trial value of saturated water vapor pressure. It is 0 (Default) and is only used by \code{\link{temperature_from_saturated_water_vapor_pressure}}
#'@references Eccel at al, 2012
#'@return The water vapor pressure expressed in hPa 
#' 
#' @export 

saturated_water_vapor_pressure <- function(T,root=0) {
	
	out <- 6.1078*exp(17.269*T/(T+237.3))-root
	return(out)
} 

NULL

#'
#' It is the inverse of \code{\link{temperature_from_saturated_water_vapor_pressure}} 
#' @param es saturated water vapor pressure in hPa
#' @param interval Temperature interval see \code{\link{uniroot}}. Default is \code{c(-100,100)}
#' @param root logical value. If \code{TRUE} (Default) only root value is returned, it is used only if \code{es}  has length 1.
#' @param ... arguments for \code{\link{uniroot}}
#' 
#' @export
#' @rdname temperature_from_saturated_water_vapor_pressure
#' 



temperature_from_saturated_water_vapor_pressure <- function(es,interval=c(-100,100),root=TRUE,...) {
	
	
	if (length(es)>1) {
		
		out <- es*NA
	
		
		str(as.vector(es))
		print(as.vector(es))
		out[,] <- unlist(lapply(X=as.vector(es),FUN=temperature_from_saturated_water_vapor_pressure,interval=interval,root=TRUE,...))
	#	for (i in 1:length(es)) {
	#		
	#		out[i] <- temperature_from_saturated_water_vapor_pressure(as.vector(es)[i],interval=interval,root=TRUE,...)
	#		
	#		
	#	}
	} else if (!is.na(es)) {
	    
		out <- uniroot(f=saturated_water_vapor_pressure,root=es,interval=interval,...)
	    if (root) out <- out$root
	} else {
		out <- NA
		
	}
	
	
	return(out)
	
}

NULL
#'
#' It calculates dew temperature from air temperature and relative humidity
#' 
#' @param T temperature
#' @param RH relative humidity 
#' @param percent logical value. If \code{TRUE} (default) the relative humidity is returned in per cent
#' @param ... arguments for \code{\link{temperature_from_saturated_water_vapor_pressure}}
#' 
#'@export

dew_temperature_from_temperature_RH <- function (T,RH,percent=TRUE,...) {
	
	esat <- saturated_water_vapor_pressure(T)
	
	if (percent) RH <- RH/100
	
	ev <- esat*RH
	
	out <- temperature_from_saturated_water_vapor_pressure(ev,...)
	
	
}




