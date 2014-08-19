
NULL
#'
#' It aggragetes \code{x1} subdaily variable at the daily scale according to \code{x2} aggregation
#' 
#' @param x1 subdaily variable to aggregate, e.g. temperature
#' @param x2 subdaily secondary variable to which \code{FUN} is applied for aggregagation of \code{x1}, e.g. relative humdity
#' @param FUN function used for aggregation of \code{x1}. It is recommened that \code{FUN} be \code{max} or \code{min}.
#' 
#' @return The daily aggragation requested for \code{x1}, e.g. the temparature at the time instant in which relative humidity is maximum.
#' @export
#' 
#' 
#' 

aggregate_daily <- function(x1,x2,FUN=max) {
	
	out <- NULL 
	x2m <- as.matrix(x2)
	x1m <- as.matrix(x1)
	
	x2aggr <- aggregate(x2,by=as.Date(index(x2)),FUN=FUN)
	row <- julian(as.Date(index(x2)))-julian(as.Date(index(x2)[1]))+1
	x2a <-  as.matrix(x2aggr)[row,]
	
	x1m[x2m!=x2a] <- -9999
	
	
	temp <- x1
	temp[,] <- x1m
	out <- aggregate(temp,by=as.Date(index(temp)),FUN=max)
	
	
	
	return(out)
	
}
## # FARE FUNZIONE 
## use.dew_temperature_from_temperature_RH,...)
## 
## RH_dis <- RH 
## RH_dis[,] <- as.matrix(RHx)[row,]
## Td_matrix <- as.matrix(Tdew)
## Td_matrix[RH!=RH_dis] <- 0
## Tdh <- Tdew
## Tdh[,] <- Td_matrix
## 
## Td <- aggregate(Tdh,by=as.Date(index(Tdh)),FUN=sum)
## O <- eval(call(.Generic,T))
## 
## 
## 
## 





