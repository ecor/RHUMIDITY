NULL
#'
#' Calculates Daily Extraterrestrial radiation (DER)  according to the formules indicated by FAO and Daily Potential Evapotranspiration (ET) according to Hargerves' formula and Drought Daily Index (DDI) and Daily Dew Point Temperature (TDEW)  
#'
#' @param doy day of the year
#' @param lat latitude expressed in degrees North  
#' @param unit output measurement unit 
#' @param vaporization_latent_heat water vaporization latent heat expressed in MJ per Kg. Default is 2.45.
#' @param divide_by_latent_heat logical values. If \code{TRUE} \code{unit} is set equal to \code{"Kg_per_square_meter_per_day"}. Default is \code{FALSE}.
#' @param prec daily precipitation expressed in mm

#' @title Empirical Potential Evapotranspiration and Daily Drought Index
#' @author Emanuele Cordano
### @note \url{http://www.fao.org/docrep/X0490E/x0490e07.htm#estimating%20missing\%20climatic%20data}
#### @references \url{http://onlinelibrary.wiley.com/doi/10.1002/met.258/abstract} 
#' @references See the URLs in DESCRIPTION files
#' @details These functions implement the theory illustrated in the following paper:  from \url{http://onlinelibrary.wiley.com/doi/10.1002/met.258/abstract}
#' @rdname der
#' @export
#' @seealso \code{\link{air_humidity}}
#' 
#' @examples  
#' # See examples in "air_humidity" function
#' 
#' 
#' 
#' 
#' 
#' 



DER <- function(doy=1,lat=46,unit="MJ_per_square_meter_per_day",vaporization_latent_heat=2.45,divide_by_latent_heat=FALSE) {
	
	
#	if (class(Tx)=="zoo") {
#		
#		doy <- as.numeric(julian(index(Tx)))+1
#		
#	}
	
	
	phi <- lat/189*pi
	dr <- 1+0.033*cos(2*pi/365.25*doy)
	delta <- 0.409*sin(2*pi/365.25*doy-1.39)
	epsilon <- -tan(phi)*tan(delta)
	epsilon[epsilon<0] <- 0
	epsilon[epsilon>1] <- 1
	omega_s <- acos(epsilon)
	gsc <- 0.0820
	
	out <- 24*60/pi*gsc*dr*(omega_s*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(omega_s))
	if (divide_by_latent_heat) unit="Kg_per_square_meter_per_day"
	if (unit=="Kg_per_square_meter_per_day") out <- out/vaporization_latent_heat
	
	return(out)	
	
}

NULL
#'
#' 
#' @param Tx,Tn,Tmean Daily Maximum, Minimun and Mean Temperature expressed in Celsius Degrees

#' @name ET
#' @rdname der
#' @export
#' 
ET <- function(doy=1,lat=46,vaporization_latent_heat=2.45,Tn=10,Tx=20,Tmean=14) {
	
	if (class(Tx)=="zoo") {
		
		doy <- as.numeric(julian(index(Tx)))+1
		
	}
	
	
	R <- DER(doy=doy,lat=lat,vaporization_latent_heat=vaporization_latent_heat,divide_by_latent_heat=TRUE) 
	out <- 0.0023*R*(Tmean+17.8)*(Tx-Tn)^0.5
	
	return(out)
}

NULL
#'
#' 
#' @rdname der
#' 
#' @param DDI_lim a vector in case \code{DDI} must returns numerical catagories or integer number. Default is \code{NULL} and \code{DDI} is returned as a continuous numerical value
#' @param ops operation used for \code{DDI} computation. Default is "/"
#' @param lag number of days used for calculation of DDI. See reference.

#' 
#'  @export
#' 
#' 
DDI <- function(doy=1,lat=46,vaporization_latent_heat=2.45,Tn=10,Tx=20,Tmean=14,prec=0,lag=6,DDI_lim=c(0,0.5,1,3),ops="/") {
	
	pet <- ET(doy=doy,lat=lat,vaporization_latent_heat=vaporization_latent_heat,Tn=Tn,Tx=Tx,Tmean=Tmean)

	out <- eval(call(ops,prec,pet))
#	out <- prec/pet

	if (length(out)>0) {
		
		temp <- out
		out <- Tx
	
		vect <- c(0,array(1,lag))
		
		if (ncol(as.matrix(out))>1) {
			out[,] <- filter(temp,vect,sides=1,circular=FALSE)

		} else {
			
			out[] <- filter(temp,vect,sides=1,circular=FALSE)
		}
			
		

		
	}
	# INSERIRE UNA 
	
#	if (!is.null(signif)) {
	
	
	if (!is.null(DDI_lim)) {	
		
		# 
		min_DDI <- max(DDI_lim)
		max_DDI <- max(as.matrix(out[,]),na.rm=TRUE)+10
		
		for (i in 1:length(DDI_lim)) {
			
			j <- length(DDI_lim)-i+1
			out[(as.matrix(out)>=min_DDI) & (as.matrix(out)<max_DDI) & (!is.na(as.matrix(out)))] <- DDI_lim[j]
			max_DDI <- min_DDI
			min_DDI <- DDI_lim[j-1]
			
		}
		
#		out[,] <- signif(out[,],signif)
	}
	
	return(out)	

}
NULL
#'
#' @param valmin_prec precipitation threshold value under which no daily precipitation is considered. Default is 0.5 mm. 
#' @param intercept_prec,intercept_noprec,coeff_prec,coeff_noprec values for piecewise linear function for dew temperature correctio. See FAO method in the Reference.

#' @rdname der
#' @export
#' 
#' 
 TDEW <- function(doy=1,lat=46,vaporization_latent_heat=2.45,Tn=10,Tx=20,Tmean=14,prec=0,lag=6,valmin_prec=0.5,intercept_prec,intercept_noprec,coeff_prec,coeff_noprec,DDI_lim=NULL) {

	 ddi <- DDI(doy=doy,lat=lat,vaporization_latent_heat=vaporization_latent_heat,Tn=Tn,Tx=Tx,Tmean=Tmean,prec=prec,lag=lag) 
	 out <- ddi
#	 print(diag(coeff_prec,nrow=length(coeff_prec)))
#	 print(as.matrix(ddi))
	corr <- as.matrix(ddi) %*% diag(coeff_prec,nrow=length(coeff_prec))+ array(1,dim(as.matrix(ddi))) %*% diag(intercept_prec,nrow=length(intercept_prec))
	corr_noprec <- as.matrix(ddi) %*% diag(coeff_noprec,nrow=length(coeff_noprec))+ array(1,dim(as.matrix(ddi))) %*% diag(intercept_noprec,nrow=length(intercept_noprec))

	corr[corr<0] <- 0
	corr_noprec[corr_noprec<0] <- 0
#	print(min(corr,na.rm=TRUE))
#	print(min(corr_noprec,na.rm=TRUE))
	
	corr[(prec<=valmin_prec) & (!is.na(prec))] <- corr_noprec[(prec<=valmin_prec) & (!is.na(prec))]
	
	if (ncol(as.matrix(out))>1) {
		
		out[,] <- as.matrix(Tn)-corr
	} else {
		
		out[] <- as.vector(Tn)-corr
		
	}
	
	

	
	return(out)
	
 }
 
 NULL




