#
#
#
rm(list=ls())
library(Interpol.T)
library(RMAWGEN)
library(RHUMIDITY)
library(zoo)
data(Trentino_hourly_T) # contiene le tabelle di calibrazione




##load("/home/ecor/Dropbox/iasma/RMAWGENdev/RHUMIDITY/data/gen_2001_2002_00003.rda")
data(gen_2001_2002_00003)
## get daily temperature Time series

station <- c("SMICH","T0083")

# Calibratipn coefficien for polygonal relatioships between DDI and Daily Daw Point Temperature by Emanuele Eccel 

intercept_prec <- c(1.12487884192312,1.70075167424242)
names(intercept_prec) <- station 

coeff_prec <- c(-0.374959613974372,-0.363722090772410)
names(coeff_prec) <- station 


intercept_noprec <- c(2.52111983703096,3.08358876530956)
names(intercept_noprec) <- station 

coeff_noprec <- c(-0.84037327901032,-1.02786292176985)
names(coeff_noprec) <- station #adjusted!!

Tn0 <-list$Tn_gen[c("year","month","day",station)]
Tx0 <-list$Tx_gen[c("year","month","day",station)]
prec0 <- list$prec[c("year","month","day",station)]

year_min<-min(Tn0$year)
year_max<-max(Tn0$year)

origin <- paste(year_min,"1","1",sep="-")
Th_int_list <- (Th_int_series(cal_times=calibration_l, cal_shape=calibration_shape, TMIN=Tn0, TMAX=Tx0, start_year=year_min, end_year=year_max))
T_hourly0 <- as.data.frame(cbind(Th_int_list$Date,as.data.frame(Th_int_list[station])))
T_hourly <- dataframe2zoo(T_hourly0,station=station)

# Daily Maximum, Minimun and Mean Temperature as zoo objects

Tn <- dataframe2zoo(Tn0,station=station)
Tx <- dataframe2zoo(Tx0,station=station)
prec <- dataframe2zoo(prec0,station=station)
Tm <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=mean)
Tnc <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=min)
Txc <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=max)


index(Tx) <- index(Tm)
index(Tn) <- index(Tm)
index(prec) <- index(Tm)
# dew temperature 

Td <- TDEW(Tx=Tx,Tn=Tnc,Tmean=Tm,prec=prec,lag=6,valmin_prec=0.5,intercept_prec=intercept_prec,intercept_noprec=intercept_noprec,coeff_prec=coeff_prec,coeff_noprec=coeff_noprec)

RH_hourly <- air_humidity(T_hourly=T_hourly,Td=Td)


# humidity aggregation

RHm <- aggregate(RH_hourly,by=as.Date(index(RH_hourly)),FUN=mean)
RHn <- aggregate(RH_hourly,by=as.Date(index(RH_hourly)),FUN=min)
RHx <- aggregate(RH_hourly,by=as.Date(index(RH_hourly)),FUN=max)




RH_leaf_wetness_threshold <- c(84,80)
names(RH_leaf_wetness_threshold) <- station 

leaf_wetness_hourly <- (RH_hourly > RH_hourly^0 %*% diag(RH_leaf_wetness_threshold))

leaf_wetness <- aggregate(leaf_wetness_hourly,by=as.Date(index(T_hourly)),FUN=sum)

# add to list 

list$RHm <- adddate(data=as.data.frame(RHm[,]),origin=index(RHm)[1])
list$leaf_wetness <- adddate(data=as.data.frame(leaf_wetness[,]),origin=index(RHm)[1])
