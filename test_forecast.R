library(forecast)
load('forecast_testdata.Rdata')

fit_model=function(xdata,ydata,scale_y=TRUE,switch_vars=FALSE) {
	
	if (switch_vars) {
		print('switching X and Y variables')
		tmp=xdata
		xdata=ydata
		ydata=tmp
	}
	dates=c(xdata$date,ydata$date)
	xdates=xdata$date
	ydates=ydata$date
	alldays = seq(min(dates), max(dates), by='1 day')
	xdata$date=NULL
	ydata$date=NULL
	xnames=names(xdata)
	ynames=names(ydata)
	
	x=as.numeric(as.character(xdata[,1]))
	x = (x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
	x_alldays=array(NA,dim=length(alldays))
	x_alldays[alldays %in% xdates]=x[xdates %in% alldays]
	x=as.ts(zoo(x_alldays,alldays))
	 		
	if (scale_y) {
		print('scaling Y variable')
		y = (ydata[,1] - mean(ydata[,1],na.rm=TRUE))/sd(ydata[,1],na.rm=TRUE)
	} else { 
		print('not scaling Y variable')
		y=ydata[,1]
		}
	
	y_alldays=array(NA,dim=length(alldays))
	y_alldays[alldays %in% ydates]=y[ydates %in% alldays]
	y=as.ts(zoo(y_alldays, alldays))
	fit.loop = auto.arima(y, xreg = x)
	print(fit.loop$coef) # summary(fit.loop))

}




print(Sys.info()['sysname'])

xdata=subset(testdata,select=c(date, email.LIWC_CDI))
ydata=subset(testdata,select=c(date, prevevening.Guthealth))
fit_model(xdata,ydata)
fit_model(xdata,ydata,scale_y=FALSE)
fit_model(xdata,ydata,switch_vars=TRUE)
fit_model(xdata,ydata,scale_y=FALSE,switch_vars=TRUE)

