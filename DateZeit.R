#############################################################################
#                                                                           #
#                                                                           #
#             LIBRARY-ROUTINES  DATE, TIME and SUNPOS                       #
#                                                                           #
#                                                                           #
#        Function Name       :  DateZeit.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  28.06.2015                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               05.04.2020                                  #
#                               10.01.2020                                  #
#                               31.01.2019                                  #
#                               29.11.2017                                  #
#                               13.10.2017                                  #
#                               26.12.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'DateZeit' contains a collection of calendar- and time-functions   #
#        and of function to calculate sun position data.                    #
#                                                                           #
#                                                                           #
#        List of functions:                                                 #
#                                                                           #
#          - AzimuthSunhei                                                  #
#          - CalcMoonCulm                                                   #
#          - CalcTrueNoon                                                   #
#          - CheckConvertDate                                               #
#          - CompareDate                                                    #
#          - ConvertDate                                                    #
#          - LeapYear                                                       #
#          - ReplaceCalInstYear                                             #
#          - ReplaceDayMonthYear                                            #
#          - SunposKomhyr                                                   #
#          - StringToTime                                                   #
#          - TimeToString                                                   #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 26.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          add of functions CalcTrueNoon, SunposKomhyr, StringToTime,       #
#          TimeToString                                                     #
#                                                                           #
#        - 13.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          add of function 'AzimuthSunhei'                                  #
#                                                                           #
#                                                                           #
#        - 29.11.2017 by Herbert Schill:                                    #
#                                                                           #
#          add of function 'LeapYear'                                       #
#                                                                           #
#                                                                           #
#        - 31.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          add functions 'ReplaceCalInstYear', 'ReplaceDayMonthYear'        #
#          (transferred from program file 'ReadInstTables.R')               #
#                                                                           #
#                                                                           #
#        - 10.01.2020 by Herbert Schill:                                    #
#                                                                           #
#          add of function 'CheckConvertDate'                               #
#                                                                           #
#                                                                           #
#        - 05.04.2020 by Herbert Schill:                                    #
#                                                                           #
#          add of function 'CalcMoonCulm'                                   #
#                                                                           #
#############################################################################


AzimuthSunhei <- function (day, mon, year, TimeHr0, StaLat, StaLong, 
                           StaHeight, O3Layer, AtmLayer, RadiusEarth)

#############################################################################
#                                                                           #
#        Function 'AzimuthSunhei' calculates the position of the sun        #
#        (Sunheigh, Azimuth) and the Airmass for Ozone for a specific time, #
#        date and location according the formulae of "Walter D. Komhyr,     #
#        WMO Report No. 6, June 1980"                                       #
#                                                                           #
#                                                                           #
#        Function call:  AzimuthSunhei (day, mon, year, TimeHr0, StaLat,    #
#                                       StaLong, StaHeight, O3Layer,        #
#                                       AtmLayer, RadiusEarth)              #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Inxxxx       day, month, year [yyyy] to calculate                  #
#        TimeHr0      time in UTC of format 'h.h' [hour, fraction of it]    #
#        StaLong      Station Longitude [deg; west negative]                #
#        StaLat       Station Latitude [deg; south negative]                #
#        StaHeight    Station altitude [km]                                 #
#        O3Layer      Barycentric height of ozone layer [km]                #
#        AtmLayer     Barycentric height of atmosphere [km]                 #
#        RadiusEarth  Earth radius [km]                                     #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        List containing Sunheight, Azimuth and AirmassO3 (Mu)              #
#                                                                           #
#          Sunheight  [deg]                                                 #
#          SunAzimuth [deg]                                                 #
#          AirmassO3                                                        #
#                                                                           #
#############################################################################
{

	# Initialisation
	#
	#   Pi = 3.14159265358979
	#
	#   ufac = pi/180.0, deg-rad conversion factor

	Pi = 3.14159265358979
	ufac = 0.017453293

	# Calculation of azimuth; this is done in a 2-step iteration

	az1=180
	for (i in -1:0)
	{
		TimeHr = TimeHr0 + i * 0.02
		sunpos = SunposKomhyr (day, mon, year, TimeHr, StaLong, StaLat, 
		                       StaHeight, O3Layer, AtmLayer, RadiusEarth)

		cosSH = cos(sunpos$Sunhei * ufac)
		sitr = sin((sunpos$TrueSolarTime-12)*Pi / 12)
		awi = cos(sunpos$DeclRad)*sitr / cosSH
		# sunpos$Sunhei;sunpos$TrueSolarTime;sunpos$DeclRad;cosSH;sitr;awi
		if (awi<(-1))
			{ az2 = 90 } else if (awi>1)
			{ az2 =270 } else
			{ az2 = asin(awi) / ufac+180 }
  	
		az3 = az2
		if (az2<az1)
		{
			if (az2>=180)
			{ az2 = 540-az2 } else
			{ az2 = 180-az2 }
		}
		SunAzimut = az2
		az1 = az3
		# TimeHr;SunAzimut;az1;az2;az3
	}

	# Write data in lists

	sunList = list(sunpos$Sunhei, SunAzimut, sunpos$Airmass)
	names(sunList) = c("Sunhei", "Azimuth", "AirmassO3")
	return (sunList)

}  # end of function 'AzimuthSunhei'


CalcMoonCulm <- function (inDate, mtc, StaLat, StaLong)

#############################################################################
#                                                                           #
#        Function 'CalcMoonCulm' calculates time and value of maximal       #
#        moonheight (culmination) of the day for the given latitude and     #
#        longitude.                                                         #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        CalcMoonCulm (inDate, mtc, StaLat, StaLong)                        #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        inDate    Date to calculate                                        #
#        mtc       Time [hr] to start loop for calculation                  #
#        StaLat    Latitude for true noon [deg]                             #
#        StaLong   Longitude for true noon [deg]                            #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        MoonCulm  List with: TimeCulm [hr], mCulmStr ["hh:mm"], deltaDay   #
#                             (-1 if culmination on previous day, 0 if the  #
#                              same day, +1 if the following day)           #
#                                                                           #
#############################################################################
{

	inDate0=inDate
	if (mtc<0)
	{
		inDate=inDate-1
		mtc=mtc+24
	}
	moonAlt=0.2
	moonAlt0=moonAlt-0.1

	while (moonAlt>moonAlt0)
	{
		mtc=mtc+1/120  # step 30 sec
		if (mtc>24)
		{
			inDate=inDate+1
			mtc=mtc-24
		}
		moonAlt0=moonAlt
		mCulmStr=TimeToString(mtc)
		mcDateTime = sprintf ("%s %s", inDate, mCulmStr)
		moonTimes2<-getMoonPosition(mcDateTime, StaLat, StaLong)
		moonAlt<-180*moonTimes2$altitude/pi
		mcDateTime;moonAlt0;moonAlt
	}
	TimeCulm=mtc-1/120
	mCulmStr=TimeToString(TimeCulm)
	mcDateTime = sprintf ("%s %s", inDate, mCulmStr)
	deltaDay=0
	if (inDate<inDate0) deltaDay=-1
	if (inDate>inDate0) deltaDay=1

# Write data in list

MoonCulm = list(TimeCulm, mcDateTime, moonAlt0, deltaDay)
names(MoonCulm) = c("culmHR","mcDateTime","moonAlt","deltaDay")

return (MoonCulm)

}  # end of function 'CalcMoonCulm'


CalcTrueNoon <- function (InDay, InMon, InYear, StaLong)

#############################################################################
#                                                                           #
#        Function 'CalcTrueNoon' calculates time and value of maximal       #
#        sunheight of the true noon of the day for the given longitude,     #
#        using the Komhyr algorithms.                                       #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        CalcTrueNoon (InDay, InMon, InYear, StaLong)                       #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Inxxxx    day, month, year [yyyy] to calculate                     #
#        StaLong   Longitude for true noon [deg]                            #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        TrueNoon  List with: TimeHMax [hr], HMax "hh:mm:ss", SunheiMax     #
#                                                                           #
#############################################################################
{

	# Use default values for atmosperic constants

	StaLat=46.7828
	StaHeight=1.847
	O3Layer=21
	AtmLayer=5.5
	RadiusEarth=6371

	# Calculates time and value of maximal sunheight of the true noon of the day,
	# using the Komhyr algorithms; in a first step, mean noon is used as input, 
	# then the resulting time is used for the second iteration step

	TimeHMax = 12-(StaLong/15)
	Sunpos = SunposKomhyr (InDay, InMon, InYear, TimeHMax, StaLong, StaLat, 
                         StaHeight, O3Layer, AtmLayer, RadiusEarth)
	InDay;InMon;InYear;TimeHMax;StaLong;StaLat;StaHeight;O3Layer;AtmLayer;RadiusEarth
	TimeHMax = 12-Sunpos[[1]]
	Sunpos = SunposKomhyr (InDay, InMon, InYear, TimeHMax, StaLong, StaLat, 
                         StaHeight, O3Layer, AtmLayer, RadiusEarth)
	SunheiMax = Sunpos[[3]]
	TimeHMS = TimeToString(TimeHMax)

# Write data in list

TrueNoon = list(TimeHMax, TimeHMS, SunheiMax)
names(TrueNoon) = c("TimeHR","TimeHMS","SunheiMax")

return (TrueNoon)

}  # end of function 'CalcTrueNoon'


CheckConvertDate <- function (cDateStr)

#############################################################################
#                                                                           #
#  Calculates the date-array 'cdate' out of the input date string, which    #
#  might be of one of the following formats:                                #
#                                                                           #
#   'dd.mm.yy', 'dd.mm.yyyy', 'jjj.yy', 'jjj.yyyy'                          #
#                                                                           #
#  Format cDate={day, mon, year, jdn, leap}                                 #
#                                                                           #
#############################################################################

{

# check date format

if (substr(cDateStr,4,4)=='.')  # input-format is 'jjj.yy' or 'jjj.yyyy'
{
	jdn  = as.integer(substr(cDateStr,1,3))
	year = as.integer(substr(cDateStr,5,8))
	dirc = -1
} else  # input-format is 'dd.mm.yy' or 'dd.mm.yyyy'
{
	day  = as.integer(substr(cDateStr,1,2))
	mon  = as.integer(substr(cDateStr,4,5))
	year = as.integer(substr(cDateStr,7,10))
	dirc = 1
}

# test on leap year

leap = LeapYear(year)

if (dirc>0)  # input-format is 'dd.mm.yy' or 'dd.mm.yyyy'
{
	# set proper julian day of the year

	if (leap==0) 
		jdn = switch(mon, 0,31,59,90,120,151,181,212,243,273,304,334) + day
	else
		jdn = switch(mon, 0,31,60,91,121,152,182,213,244,274,305,335) + day

}
else if (dirc<0)  # input-format is 'jjj.yy' or 'jjj.yyyy'
{
	m = array()
	m[1]=m[3]=m[5]=m[7]=m[8]=m[10]=m[12]=31
	m[4]=m[6]=m[9]=m[11]=30
	if (leap==0) 
		m[2]=28
	else
		m[2]=29

	# reset jdn if necessary

	if  (leap>0)
	  i=366
	else
		i=365
	if  (jdn<1)  jdn=1
	if  (jdn>i)  jdn=i

	# Calculate day and month

	i=0;
	mm=1;
	while  (i<jdn)
	{
		i= i+m[mm]
		mm=mm+1
	}
	day= jdn+m[mm-1]-i
	mon=mm-1
}  # if dirc

# Write data in record

cDate[1] = day
cDate[2] = mon
cDate[3] = year
cDate[4] = jdn
cDate[5] = leap

return (cDate)

}  # end of function 'CheckConvertDate'


CompareDate <- function (date1, date2)
#############################################################################
#                                                                           #
# Compares two dates of string-format "dd.mm.yyyy" and returns the          #
# appropriate value of 'i':                                                 #
#                                                                           #
#   date1 < date2   i=-1                                                    #
#   date1 = date2   i= 0                                                    #
#   date1 > date2   i= 1                                                    #
#                                                                           #
#############################################################################
{

d1=as.integer(substr(date1,1,2))
m1=as.integer(substr(date1,4,5))
y1=as.integer(substr(date1,7,10))
d2=as.integer(substr(date2,1,2))
m2=as.integer(substr(date2,4,5))
y2=as.integer(substr(date2,7,10))
vdate1=y1*10000+m1*100+d1
vdate2=y2*10000+m2*100+d2

i=0
if  (vdate1<vdate2)  i=-1
if  (vdate1>vdate2)  i=1

return (i)

}  # end of function 'CompareDate'


ConvertDate <- function (cDate, dirc)

#############################################################################
#                                                                           #
#  Calculates JDN and leap out of day, month, year, if dirc>0, resp.        #
#  Calculates day, month and leap out of JDN and year , if dirc<0.          #
#                                                                           #
#  Format cDate={day, mon, year, jdn, leap}                                 #
#                                                                           #
#############################################################################

{

# test on leap year

year = as.integer(cDate[3])
leap = LeapYear(year)

if (dirc>0)
{
	day  = as.integer(cDate[1])
	mon  = as.integer(cDate[2])

	# set proper julian day of the year

	if (leap==0) 
		jdn = switch(mon, 0,31,59,90,120,151,181,212,243,273,304,334) + day
	else
		jdn = switch(mon, 0,31,60,91,121,152,182,213,244,274,305,335) + day

}
else if (dirc<0)
{
	m = array()
	m[1]=m[3]=m[5]=m[7]=m[8]=m[10]=m[12]=31
	m[4]=m[6]=m[9]=m[11]=30
	if (leap==0) 
		m[2]=28
	else
		m[2]=29

	# reset jdn if necessary
	
	jdn = as.integer(cDate[4])
	if  (leap>0)
	  i=366
	else
		i=365
	if  (jdn<1)  jdn=1
	if  (jdn>i)  jdn=i

	# Calculate day and month

	i=0;
	mm=1;
	while  (i<jdn)
	{
		i= i+m[mm]
		mm=mm+1
	}
	day= jdn+m[mm-1]-i
	mon=mm-1
}  # if dirc

# Write data in record

cDate[1] = day
cDate[2] = mon
cDate[3] = year
cDate[4] = jdn
cDate[5] = leap

return (cDate)

}  # end of function 'ConvertDate'


LeapYear <- function (year)

#############################################################################
#                                                                           #
#  Check on leap year, returns 1 for leap year, 0 for normal year           #
#                                                                           #
#############################################################################

{

leap=0
if  (year/4==floor(year/4))      leap=1
if  (year/100==floor(year/100))  leap=0
if  (year/400==floor(year/400))  leap=1
return (leap)

}  # end of function 'LeapYear'


ReplaceCalInstYear <- function (inpStr, calib, instrum, lyear)

#############################################################################
#                                                                           #
#        'ReplaceCalInstYear.R' replaces in a string 'inpStr' the  sub-     #
#        string 'Calxxx' by the calibration version 'calib', the 'instrum'  #
#        substring 'iii' by the instrument identifier 'instrum' and the     #
#        sub-tring 'yyyy'by the year 'lyear', if occurring one or more      #
#        time.                                                              #
#                                                                           #
#        e.g. inpStr-string "C:\PMOD\Dobson\Data\AE\iii\Calxxx\yyyy\"       #
#             is changed to "C:\PMOD\Dobson\Data\AE\101\Cal15a\2016\"       #
#                                                                           #
#        e.g. inpStr-string "C:\PMOD\Brewer\Data\DS\iii\Calxxx\yyyy\"       #
#             is changed to "C:\PMOD\Brewer\Data\DS\156\Cal15a\2016\"       #
#                                                                           #
#        e.g. inpStr-string "C:\PMOD\Brewer\iii\ICF\Calxxx\ICF00114.iii"    #
#             is changed to "C:\PMOD\Brewer\156\ICF\Cal16a\ICF00114.156"    #
#                                                                           #
#############################################################################

{

# Replace "Calxxx", "iii" and "yyyy" in string 'inpStr' by calibration, 
# instrument identifier and year, if occurring
 
subold="Calxxx"
subnew=calib

for (k in 1:3)
{
	occ=1
	while (occ>0)
	{
		occ=0
		len=nchar(inpStr)
		len0=nchar(subold)
		for (i in 1:(len-len0+1))	if (substr(inpStr,i,i+len0-1)==subold) occ=i
		if (occ>=1) inpStr  = sprintf("%s%s%s", substr(inpStr,1,occ-1), subnew, substr(inpStr,occ+len0,len))
		if (k==1)
		{
			subold="iii"
			subnew=instrum
		}
		if (k==2)
		{
		subold="yyyy"
		subnew=lyear
		}
	}
}
return (inpStr)

}  # end of function 'ReplaceCalInstYear'


ReplaceDayMonthYear <- function (inpStr, day, month, lyear)

#############################################################################
#                                                                           #
#        'ReplaceDayMonthYear.R' replaces in a string 'inpStr' the  sub-    #
#        string 'dd' by the string of 'day', the substring 'mm' by the      #
#        string of 'month', the substring 'yyyy' resp. 'yy' by the string   #
#        of 'lyear' resp. the last two characters of 'lyear', if occurring  #
#        one or more time, and the substring 'jjj' by the string of the     #
#        julian day number [001..366], which is calculated from the date.   #
#                                                                           #
#        e.g. inpStr-string "C:\PMOD\Dobson\Data\AEddmmyyyy.iii"            #
#             is changed to "C:\PMOD\Dobson\Data\AE23102016.iii"            #
#                                                                           #
#        e.g. inpStr-string "C:\PMOD\Brewer\Data\Bjjjyy.iii"                #
#             is changed to "C:\PMOD\Brewer\Data\B30116.iii"                #
#                                                                           #
#        e.g. inpStr-string "Luxcurves_yyyymm.051"                          #
#             is changed to "Luxcurves_201603.051"                          #
#                                                                           #
#############################################################################

{

# Replace "dd", "mm", "jjj" and "yyyy" resp. "yy" in string 'inpStr' by day, 
# month, julian day number jdn, and year resp. short year, if occurring
 
cDate = array()
subold = "yyyy"
subnew = sprintf("%04d",lyear)
if (nchar(lyear)==4) syear=substr(subnew,3,4) else syear=subnew

for (k in 1:5)
{
	occ=1
	while (occ>0)
	{
		occ=0
		len=nchar(inpStr)
		len0=nchar(subold)
		for (i in 1:(len-len0+1))	if (substr(inpStr,i,i+len0-1)==subold) occ=i
		if (occ>=1) inpStr  = sprintf("%s%s%s", substr(inpStr,1,occ-1), subnew, substr(inpStr,occ+len0,len))
		if (k==1)
		{
			subold = "yy"
			subnew = syear
		} else if (k==2)
		{
			subold="mm"
			subnew = sprintf("%02d",month)
		} else if (k==3)
		{
			subold="dd"
			subnew = sprintf("%02d",day)
		} else if (k==4)
		{
			cDate[1] = day
			cDate[2] = month
			cDate[3] = lyear
			cDate = ConvertDate (cDate, 1)
			subold="jjj"
			subnew = sprintf("%03d",cDate[4])
		}
	}
}
return (inpStr)

}  # end of function 'ReplaceDayMonthYear'


SunposKomhyr <- function (InDay, InMon, InYear, TimeHr, StaLong, StaLat, 
                          StaHeight, O3Layer, AtmLayer, RadiusEarth)

#############################################################################
#                                                                           #
#        Function 'SunposKomhyr' calculates the position of the sun         #
#        (SZA, Sunheigh, Airmass) for a specific time, date and location    #
#        according the formulae of "Walter D. Komhyr, WMO Report No. 6,     #
#        June 1980"                                                         #
#                                                                           #
#        Function call:  SunposKomhyr (InDay, InMon, InYear, TimeHr,        #
#                                      StaLong, StaLat, StaHeight,          #
#                                      O3Layer, AtmLayer, RadiusEarth)      #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Inxxxx       day, month, year [yyyy] to calculate                  #
#        TimeHr       time in UTC of format 'h.h' [hour, fraction of it]    #
#        StaLong      Station Longitude [deg; west negative]                #
#        StaLat       Station Latitude [deg; south negative]                #
#        StaHeight    Station altitude [km]                                 #
#        O3Layer      Barycentric height of ozone layer [km]                #
#        AtmLayer     Barycentric height of atmosphere [km]                 #
#        RadiusEarth  Earth radius [km]                                     #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        List containing TSTcorr, SZA, Sunheight, Airmass, Bemporad         #
#                                                                           #
#          TSTcorr    True Solar Time Correction [hrs]                      #
#          SZA        Solar Zenith Angle [deg]                              #
#          Sunheight  90-SZA                                                #
#          Airmass    mu                                                    #
#          Bemporad   m/mu                                                  #
#                                                                           #
#                                                                           #
#############################################################################
{

	# Initialisation
	#
	#   Pi = 3.14159265358979
	#
	#   ufac = pi/180.0, deg-rad conversion factor

	Pi = 3.14159265358979
	ufac = 0.017453293

	# Calculation of time elapsed in days 'DD', since 00.01.1900 12:00 UTC
	# and number of Julian centuries 'TT' from 1900.0 (Julian date 2415020.0)
	# (Komhyr p. 105; "INT" of Komhyr has to be replaced by "trunc" resp.
	# "floor" commands for a proper calculation of negative numbers)

	AA = (InMon - 14) / 12
	BB = trunc(AA)
	CC = (1461 * (InYear + 4800 + BB)) / 4
	UU = floor(CC)
	EE = 367 * (InMon - 2 - 12 * BB) / 12
	VV = trunc(EE)
	GG = (InYear + 4900 + BB) / 100
	HH = 3 * trunc(GG) / 4
	WW = -trunc(HH)
	DD = InDay - 2447095.5 + UU + VV + WW + TimeHr / 24
	TT = DD / 36525
	#AA;BB;CC;UU;EE;VV;GG;HH;WW;DD;TT
	
	# Calculation of mean longitude of sun 'SunLDeg/SunlRad', equation of time 'ET' [secs],
	# tangens of ecliptic obliquity 'tanEcl', apparent right ascension of sun 'RA' [deg],
	# apparent declination of sun 'Decl' [deg] and great hour angle 'GHA' [deg] (Komhyr p. 104)

  SunlDeg = 279.697 + 36000.769 * TT
  while (SunlDeg > 360)  SunlDeg = SunlDeg-360

  SunlRad = SunlDeg*ufac
  
  T2 = TT * TT
  set1 = -(93 + 14.23 * TT - 0.0144 * T2) * sin(SunlRad)
  cet1 = -(432.5 - 3.71 * TT - 0.2063 * T2) * cos(SunlRad)
  set2 = (596.9 - 0.81 * TT - 0.0096 * T2) * sin(SunlRad*2)
  cet2 = (1.4 + 0.28 * TT) * cos(SunlRad*2)
  set3 = (3.8 + 0.6 * TT) * sin(SunlRad*3)
  cet3 = (19.5 - 0.21 * TT - 0.0103 * T2) * cos(SunlRad*3)
  set4 = -(12.8 - 0.03 * TT) * sin(SunlRad*4)
	# set1;cet1;set2;cet2;set3;cet3;set4
  
  ET = set1 + cet1 + set2 + cet2 + set3 + cet3 + set4
  tanEcl = 0.43382 - 0.00027 * TT
  ecl = atan(tanEcl) / ufac
  RA = SunlDeg - ET / 240
  DeclRad = (atan(tanEcl * sin(RA * ufac)))
  DeclDeg = DeclRad / ufac
  GHA = TimeHr * 15 + ET / 240 + 180
	# ET;tanEcl;ecl;RA;DeclRad;DeclDeg;GHA

	# Calculation of local time correction [hr], solar time correction 'TSTCor' [hrs],
	# true solar time 'TST' [hrs] and hour angle 'HARrad' [rad]

  LocalTimeCorr = StaLong / 15
  TSTcor = LocalTimeCorr + ET / 3600
  TST = TimeHr + TSTcor
  HARdeg = (TST * 15 - 180)
  HARrad = HARdeg * ufac
	# LocalTimeCorr;TSTcor;TST;HARdeg;HARrad
  
	# Calculation of solar zenith angle SZA (Komhyr pp 121) and Sunheight [deg]

	LatRad = StaLat*ufac
  cosSZA = cos(HARrad) * cos(DeclRad) * cos(LatRad) + sin(DeclRad) * sin(LatRad)
  SZArad = acos(cosSZA)
  SZAdeg = SZArad/ufac
  Sunheight = 90 - SZAdeg
	# LatRad;cosSZA;SZArad;SZAdeg;Sunheight

	# Calculation of airmass and Bemporad (m/mu)

  O3Factor = (RadiusEarth + StaHeight) / (RadiusEarth + O3Layer)
  O3Factor = O3Factor*O3Factor
  AtmFactor = (RadiusEarth + StaHeight) / (RadiusEarth + AtmLayer)
  AtmFactor = AtmFactor*AtmFactor
  AirmassMu = 1 / sqrt(1 + O3Factor * (cosSZA * cosSZA - 1))
  AirmassM   = 1 / sqrt(1 + AtmFactor * (cosSZA * cosSZA - 1))
  Bemporad  = AirmassM/(AirmassMu)
	# O3Factor;AtmFactor;AirmassMu;AirmassM;Bemporad

	# Write data in lists

	sunList = list(TSTcor, SZAdeg, Sunheight, AirmassMu, Bemporad, TST, DeclRad)
	names(sunList) = c("TSTcorr", "SZA", "Sunhei", "Airmass", "Bemporad", 
	                   "TrueSolarTime", "DeclRad")
	return (sunList)

}  # end of function 'SunposKomhyr'


StringToTime <- function (TimeStr)
#############################################################################
#                                                                           #
# Converts a time of string-format "hhmmss" or "hh:mm:ss" to double format  #
# 'h.hh'; no plausibility-check is done.                                    #
#                                                                           #
#############################################################################
{

	if (nchar(TimeStr)==6)
	{
		mHour = as.integer(substr(TimeStr,1,2))
		mMin  = as.integer(substr(TimeStr,3,4))
		mSec  = as.integer(substr(TimeStr,5,6))
	} else
	{
		mHour = as.integer(substr(TimeStr,1,2))
		mMin  = as.integer(substr(TimeStr,4,5))
		mSec  = as.integer(substr(TimeStr,7,8))
	}
	TimeHr = as.double(mHour+mMin/60+mSec/3600)

return (TimeHr)

}  # end of function 'StringToTime'


TimeToString <- function (TimeHr)
#############################################################################
#                                                                           #
# Converts a time of format 'h.hh' to string-format "hh:mm:ss"              #
#                                                                           #
#############################################################################
{

	mHour = as.integer(TimeHr)
	mMin  = as.integer((TimeHr-mHour)*60)
	mSec  = as.integer((TimeHr-mHour)*3600-mMin*60+0.5)
	if (mSec==60)
	{
		mSec=0
		mMin=mMin+1
		if (mMin==60)
		{
			mMin=0
			mHour=mHour+1
		}
	}
	strTime = sprintf("%02d:%02d:%02d",mHour,mMin,mSec)


return (strTime)

}  # end of function 'TimeToString'


####  end of DateZeit.R  #################################################
