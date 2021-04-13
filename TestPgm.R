#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE COMPARISON                         #
#                                                                           #
#                                                                           #
#        Program Name        :  TestPgm.R                                   #
#                                                                           #
#                                                                           #
#        Creation Date       :  10.08.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.1.2    (2014)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#       'TestPgm' is a test program for different purposes.                 #
#                                                                           #
#############################################################################


setwd("C:\\LKO\\Programs\\R")
TZ=Sys.timezone(location = TRUE)

library(astrolibR)  # Load 'astrolibR'-package
library(suncalc)  # Load 'astroFns'-package

# source("BDcomparSub.R")
# source("CalcDobson.R")
source("DateZeit.R")
source("ReadInstTables.R")
# source("TreatDobsonAE.R")
# source("TreatBrewerData.R")

statusOK <- 0

# Reads the path-file 'Recalcoz.pth' with the adress of the ini-file
# 'Recalcoz.ini' and reads the initialization parameters from the latter
# (Nparams designs the number of parameters to read from ini-file)

Nparams=13
iniParams = ReadIniFile(Nparams, "CalcSunRiseSet.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Read the atmospheric constants from the constants file which is usually
	# called "C:\LKO\Programs\Tabelle\Constant.txt"
	
	ATconst = ReadConstantsFile(iniPar[15],iniPar[17],iniPar[23],iniPar[27])
	iniOk  = ATconst[[1]][[1]]
}

if (iniOk)  # Continue, if previous files were properly read
{
	# check input data format: if the substring 'LV' appear in the name of
	# the input-path, the respective data format is selected, otherwise
	# 'AE' is the default format)

	if (grep("LV",iniPar[13],ignore.case=FALSE)) inputFmt="LV" else inputFmt="AE"

	# Treat desired period

	day  = as.integer(substr(iniPar[3],1,2))
	mon  = as.integer(substr(iniPar[3],4,5))
	year = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	yearE = as.integer(substr(iniPar[4],7,10))

	dobson = iniPar[2]
	calib  = iniPar[3]
	lvpath = iniPar[13]
	day;mon;year;dobson;calib;lvpath

	if (inputFmt=="AE")
	{
		dataAE = ReadDobsonAE (day, mon, year, iniPar[2], iniPar[3], iniPar[13])
	} else
	{
		dataAE = ReadDobsonLV (day, mon, year, iniPar[2], iniPar[3], iniPar[13])
	}
	inFile="C:\\LKO\\Dayoz_2018.101"
	outFile="C:\\LKO\\Brewer\\Dayoz_2018.101"
	file.copy(inFile, outFile)

####################################################################################
#
#  Astronomical routines from 'astrolibR'-package
#
####################################################################################

# Load 'astrolibR' and 'suncalc'-packages

library(astrolibR)  # Load 'astrolibR'-package
library(suncalc)  # Load 'suncalc'-package

# Convert Gregorian dates to Julian days

	Tstr="07:20:00"

	hr=StringToTime(Tstr)
	jd=jdcnv(year, mon, day, hr)

# Compute the Right Ascension and Declination of the Sun at specified Julian date(s)

	aLibsunpos<-sunpos(jd, radian=F)
	RiAs=aLibsunpos$ra
	Decl=aLibsunpos$dec
	jd;RiAs;Decl

# Compute the Right Ascension and Declination of the Moon at specified Julian date(s)

	aLibmoonpos<-moonpos(jd, radian=F)
	RiAs=aLibmoonpos$ra
	Decl=aLibmoonpos$dec

# Convert celestial (ra-dec) coords to local horizon coords (alt-az)

	#eq2hor(ra, dec, jd, lat, lon, ws, obsname, b1950, precess_, nutate_, refract_,aberration_, altitude, ...)

	#eq2hor(ra, dec, jd, lat, lon, ws=F, obsname=NULL, b1950=F, precess_, nutate_, refract_,aberration_, altitude, ...)

	altitude=StaHeight*1000
	sunposGeo<-eq2hor(RiAs, Decl, jd, lat, lon, ws=F, obsname=NULL, b1950=F, precess_=0, nutate_=0, refract_=0,aberration_=0, altitude=427)

# Set Date and UTC-time for calculation with 'astrolibR'

	DateStr = "2017-06-21"
	Tstr="03:34:26"

	year= as.integer(substr(DateStr,1,4))
	mon = as.integer(substr(DateStr,6,7))
	day = as.integer(substr(DateStr,9,10))
	inDate<-as.Date(DateStr)
	hr=StringToTime(Tstr)
	jd=jdcnv(year, mon, day, hr)
	year;mon;day;hr;jd
	jd=jd+0.5

# Compute the Right Ascension, Declination, Elevation and Azimuth of the Sun 
# at specified Julian date(s)

	aLibsunpos<-sunpos(jd, radian=F)
	RiAs=aLibsunpos$ra
	Decl=aLibsunpos$dec
	sunposGeo<-eq2hor(RiAs, Decl, jd, StaLat, StaLong, aberration_=1, refract_=1)
	sunposGeo

# Compute the Right Ascension, Declination, Elevation, Azimuth and the illuminated 
# fraction of the Moon at specified Julian date(s)

	aLibmoonpos<-moonpos(jd, radian=F)
	RiAs=aLibmoonpos$ra
	Decl=aLibmoonpos$dec

	moonposGeo<-eq2hor(RiAs, Decl, jd, StaLat, StaLong)
	moonposGeo

	moonTimes<-getMoonTimes(inDate, StaLat, StaLong)
	moonTimes

	moonIllum<-getMoonIllumination(inDate)
	moonIllum

# general formulae

	sunTimes<-getSunlightTimes(inDate, StaLat, StaLong,
	keep = c("solarNoon", "sunrise", "sunset", "sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"), tz = "UTC")

	moonTimes<-getMoonTimes(date = NULL, lat = NULL, lon = NULL, data = NULL, keep = c("rise", "set", "alwaysUp", "alwaysDown"), tz = "UTC", inUTC = FALSE)
	moonIllum<-getMoonIllumination(inDate, keep = c("fraction", "phase","angle"))

	moonTimes2<-getMoonPosition(date = NULL, lat = NULL, lon = NULL, data = NULL, keep = c("altitude", "azimuth", "distance", "parallacticAngle"))

	# multiple date

	data <- data.frame(getSunlightTimes(date = seq.Date(inDate, inDate+31), StaLat, StaLong)

# multiple date + subset

	ST7<-getSunlightTimes(date = seq.Date(inDate, inDate+6, by=1), StaLat, StaLong, 
	keep = c("solarNoon", "sunrise", "sunset", "sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"), tz = "UTC")

	ST31<-getSunlightTimes(date = seq.Date(inDate, inDate+31, by = 1), StaLat, StaLong, keep = c("sunrise", "sunsetStart"))
	#getSunlightTimes(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 	ST31<-getSunlightTimes(date = seq.Date(inDate, inDate+31, by = 1), StaLat, StaLong, keep = c("sunrise", "sunsetStart")), lat = 50.1, lon = 1.83, tz = "CET")

		MT31<-getMoonTimes(date = seq.Date(inDate, inDate+31, by = 1), StaLat, StaLong)

	# dates in characters

	d01="2017-06-21 03:32:42"
	d02="2017-06-21 03:34:00"
	d03="2017-06-21 03:34:10"
	d04="2017-06-21 03:34:15"
	d05="2017-06-21 03:34:20"
	d06="2017-06-21 03:34:30"
	d07="2017-06-21 03:34:40"
	d08="2017-06-21 03:35:00"
	d09="2017-06-21 03:36:00"
	d10="2017-06-21 03:36:39"
	d11="2017-06-21 03:37:00"
	md11=c(d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11)

	SP1<-getSunlightPosition(date = md11, StaLat, StaLong)
	SP1deg<-180*SP1[4]/pi
	SP1deg

	# test 1

	hr=hr+1/1440  # correct by one min
	jd=jdcnv(year, mon, day, hr)
	aLibsunpos<-sunpos(jd, radian=F)
	RiAs=aLibsunpos$ra
	Decl=aLibsunpos$dec
	sunposGeo<-eq2hor(RiAs, Decl, jd, StaLat, StaLong, refract_=0,aberration_=0)
	#sunposGeo<-eq2hor(RiAs, Decl, jd, StaLat, StaLong, refract_=1,aberration_=1)
	sunposGeo
	alHMS=sixty(hr)
	alHMS

}

# end of program 'TestPgm.R'
