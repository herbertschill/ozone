#############################################################################
#                                                                           #
#                                                                           #
#                            SUN AND MOON POSITION CALCULATION              #
#                                                                           #
#                                                                           #
#        Program Name        :  SunMoonRiseSet.R                            #
#                                                                           #
#                                                                           #
#        Creation Date       :  19.04.2020                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert                              #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 3.6.1    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        Calculation of the times of rising, culmination and setting of     #
#        sun and moon, of the culmination heights, the moon phase, the      #
#        daylength, and the times of the civil (h*=-6°) , nautical          #
#        (h*=-12°)or astronomical (h*=-18°) dawn and dusk for a required    #
#        period and station.                                                #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'SunMoonRiseSet.ini'.                                   #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

# Load packages

library(astrolibR)  
library(suncalc)
library(timechange)
library(timeDate)

source("ReadInstTables.R")
source("DateZeit.R")

#  from:               import:
#
#  ReadInstTables.R    ReadIniFile, ReadStationsFile
#
#  DateZeit.R          AzimuthSunhei, CalcMoonCulm, ConvertDate


test = 0

# Open the ini-file 'CalcSunRiseSet.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=10
iniParams = ReadIniFile(Nparams, "SunMoonRiseSet.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Read the values for the station from the station list file

	StationPara = ReadStationsFile (iniPar[8],iniPar[2])
	StationName = StationPara[[2]][[1]]
	StaLong = StationPara[[3]][[1]]
	StaLat = StationPara[[3]][[2]]
	StaHeight = StationPara[[3]][[3]]
	iniOk = StationPara[[1]][[1]]
}

if (iniOk)  # Continue, if previous files were properly read
{
	# Check some parameters

	RefractCorr=iniPar[5]
	if (toupper(substr(iniPar[6],1,1))=="U") upperEdge=1 else upperEdge=0
	DawnType=toupper(substr(iniPar[7],1,1))

	# Set desired period

	dayA  = as.integer(substr(iniPar[3],1,2))
	monA  = as.integer(substr(iniPar[3],4,5))
	yearA = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	yearE = as.integer(substr(iniPar[4],7,10))

	# Open output file and write header

	outFile = sprintf ("%s%s", iniPar[10], iniPar[9])
	df = file(outFile, open="w")

	outl = sprintf ("%s%4d%s", "\n    Rise, Culmination and Set of Moon and Sun, Moonphase, Daylength, Dawn and Dusk  ", yearA, "\n\n")
	cat (outl, file=df)
	outl = sprintf ("                              %s%s", StationName, "\n\n")
	cat (outl, file=df)
	outl = sprintf ("%s%9.4f%s%9.4f%s","           [Long=", StaLong, " deg, Lat=", StaLat, " deg    All times in UTC]\n\n")
	cat (outl, file=df)
	outl = sprintf ("%s", "  Date              Moon                            Sun              Day-    Dawn Dusk      Date\n")
	cat (outl, file=df)
	outl = sprintf ("%s", "           Rise   Set   Culm  HMax Phas    Rise   Set   Culm   HMax length   Begin  End\n\n")
	cat (outl, file=df)

	# Treat year; set proper month range

	cat (sprintf("%s", "  Calculating ."), file="")

	for (year in yearA:yearE)
	{
		year=yearA

		leap = LeapYear(year)
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

		# Treat month; set proper days in month 'mdays'

		for (mon in mon1:mon2)
		{
			# mon=mon1
			if (leap==1) 
				{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
				{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

			day1=1
			day2=mdays
			if ((year==yearA) & (mon==monA)) day1=dayA
			if ((year==yearE) & (mon==monE)) day2=dayE

			cat (sprintf("%s", "."), file="")

			# Treat days in month **********************************************

			for (day in day1:day2)
			{
				# day=day1
				# day=day+1

				jdn=0
				cDate = array()
				cDate[1] = day
				cDate[2] = mon
				cDate[3] = year
				cDate[4] = jdn
				cDate[5] = leap
				cDate = ConvertDate(cDate, 1)
				jdn = cDate[4]
				jdnStr = sprintf("%03d",jdn)

				# Create date string "yyyy-mm-dd" for the day

				DateStr = sprintf ("%04d-%02d-%02d", year, mon, day)
				inDate<-as.Date(DateStr)

				# Calculate required time values for the sun (times of sunrise and sunset refer
				# to upper edge of sun: correct by -/+ 95 secs for center of the dial)

				if (DawnType=="N") # nautical
				{
					sunTimes<-getSunlightTimes(inDate, StaLat, StaLong,
					keep = c("solarNoon", "sunrise", "sunset", "nauticalDawn", "nauticalDusk"), tz = "UTC")
				} else if (DawnType=="A") # astronomical
				{
					sunTimes<-getSunlightTimes(inDate, StaLat, StaLong,
					keep = c("solarNoon", "sunrise", "sunset", "nightEnd", "night"), tz = "UTC")
				} else # civil
				{
					sunTimes<-getSunlightTimes(inDate, StaLat, StaLong,
					keep = c("solarNoon", "sunrise", "sunset", "dawn", "dusk"), tz = "UTC")
				}

				if (upperEdge==0)
				{
					sRiseStr = substr(time_round(sunTimes$sunrise+95, "minute"),12,16)
					sSetStr = substr(time_round(sunTimes$sunset-95, "minute"),12,16)
				} else
				{
					sRiseStr = substr(time_round(sunTimes$sunrise, "minute"),12,16)
					sSetStr = substr(time_round(sunTimes$sunset, "minute"),12,16)
				}

				sCulmStr = substr(time_round(sunTimes$solarNoon, "minute"),12,16)
				SunDurHr = sunTimes$sunset-sunTimes$sunrise
				DurHr = as.double(SunDurHr)
				sDurStr = TimeToString(DurHr)
				sdDateTime = sprintf ("%s %s", DateStr, TimeToString(as.double(SunDurHr)))
				sDurMin = as.integer(substr(time_round(as.POSIXct(sdDateTime), "minute"),15,16))
				sDurHr = as.integer(SunDurHr)
				duskStr = substr(time_round(sunTimes[[7]], "minute"),12,16)
				dawnStr = substr(time_round(sunTimes[[8]], "minute"),12,16)
				sRiseStr;sSetStr;sCulmStr;duskStr;dawnStr

				# Convert Gregorian date and time of true noon to Julian days

				Tstr = substr(sunTimes$solarNoon,12,19)
				hr = StringToTime(Tstr)
				jd = jdcnv(year, mon, day, hr)

				# Compute the Right Ascension, Declination, Elevation and Azimuth of the Sun 
				# at specified Julian date (true noon of the day)

				aLibsunpos<-sunpos(jd, radian=F)
				RiAs=aLibsunpos$ra
				Decl=aLibsunpos$dec
				sunposGeo<-eq2hor(RiAs, Decl, jd, StaLat, StaLong, aberration_=1, refract_=1)
				sunposGeo

				# Calculate required time values for the moon

				moonTimes<-getMoonTimes(inDate, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")

				moonIllum<-getMoonIllumination(inDate, keep = c("fraction"))
				moonTimes;moonIllum

				if (is.na(moonTimes[[4]]))  
				{
					noRise=1
					mRisStr="-----"
				} else
				{
					noRise=0
					mRisStr=substr(time_round(as.POSIXct(moonTimes[[4]]), "minute"),12,16)
				}
				if (is.na(moonTimes[[5]]))  
				{
					noSet=1
					mSetStr="-----"
				} else
				{
					noSet=0
					mSetStr=substr(time_round(as.POSIXct(moonTimes[[5]]), "minute"),12,16)
				}
				mRisStr;mSetStr;noRise;noSet

				if ((noRise+noSet)==0)  # time rise AND time set exist > cases 1-4
				{
					if ((moonTimes[[4]]<moonTimes[[5]]))  # time rise < time set: calculate culmination > case 1
					{
						mtr=StringToTime(substr(moonTimes[[4]],12,19))
						mts=StringToTime(substr(moonTimes[[5]],12,19))
						mdr=as.integer(substr(moonTimes[[4]],9,10))
						mds=as.integer(substr(moonTimes[[5]],9,10))
						mdr;mtr;mds;mts
						if (mdr<day)
						{
							mtr=mtr-24
							moonTimes1<-getMoonTimes(inDate+1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
							mdr1=as.integer(substr(moonTimes1[[4]],9,10))
							if (mdr1==day)
							{
								mdr=mdr1
								mRisStr=substr(time_round(as.POSIXct(moonTimes1[[4]]), "minute"),12,16)
							}
						}
						mtc=(mtr+mts)/2-0.1
						if (mtc>24) mtc=mtc-24
						moonCulm = CalcMoonCulm(inDate, mtc, StaLat, StaLong)  # case 1
						mCulmStr=substr(time_round(as.POSIXct(moonCulm$mcDateTime), "minute"),12,16)
					} else # time rise > time set: culm may exist > cases 2-4
					{
						mtr=StringToTime(substr(moonTimes[[4]],12,19))
						mts=StringToTime(substr(moonTimes[[5]],12,19))
						mdr=as.integer(substr(moonTimes[[4]],9,10))
						moonTimes1<-getMoonTimes(inDate+1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
						mts1=StringToTime(substr(moonTimes1[[5]],12,19))
						moonTimes1
						mds1=as.integer(substr(moonTimes1[[5]],9,10))
						if (mds1==day)
						{
							mtc=(mtr+mts1)/2-1.0
							mds=mds1
							mSetStr=substr(time_round(as.POSIXct(moonTimes1[[5]]), "minute"),12,16)
						} else
						{
							mtc=(mtr+mts1+24)/2-1.0
							mds=as.integer(substr(moonTimes[[5]],9,10))
						}
						if (mtc>24) mtc=mtc-24
						moonCulm = CalcMoonCulm(inDate, mtc, StaLat, StaLong)
						moonCulm
						if (moonCulm$deltaDay==0)  # case 2: culmination after rise
						{
							mCulmStr=substr(time_round(as.POSIXct(moonCulm$mcDateTime), "minute"),12,16)
						} else  # case 3 or 4
						{
							moonTimes0<-getMoonTimes(inDate-1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
							mtr0=StringToTime(substr(moonTimes0[[4]],12,19))
							mtc=(mtr0+mts-24)/2-1.0
							if (mtc>24) mtc=mtc-24
							moonCulm = CalcMoonCulm(inDate, mtc, StaLat, StaLong)
							moonCulm
							if (moonCulm$deltaDay==0)  # case 4: culmination before rise
							{
								mCulmStr=substr(time_round(as.POSIXct(moonCulm$mcDateTime), "minute"),12,16)
							} else  # case 3: no culmination this day
							{

								# get maximal moon altitude of the day

								mcDateTime = sprintf ("%s %s", inDate, "00:00:01")
								moonTimes2<-getMoonPosition(mcDateTime, StaLat, StaLong)
								moonAlt0<-180*moonTimes2$altitude/pi
								mcDateTime = sprintf ("%s %s", inDate, "23:59:59")
								moonTimes2<-getMoonPosition(mcDateTime, StaLat, StaLong)
								moonAlt1<-180*moonTimes2$altitude/pi
								moonCulm$moonAlt = max(moonAlt0,moonAlt1)
								mCulmStr="-----"
							}
						}
					}

					# probably time of moonrise or moonset are another day: check

					#mdr=as.integer(substr(moonTimes[[4]],9,10))
					#mds=as.integer(substr(moonTimes[[5]],9,10))
					if (mdr!=day) mRisStr="-----"
					if (mds!=day) mSetStr="-----"

				} else  # time rise OR time set exists
				{
					if (noRise==0)  # only time rise exists: culmination later this day (case 6)
					{
						moonTimes1<-getMoonTimes(inDate+1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
						mts1=StringToTime(substr(moonTimes1[[5]],12,19))
						mds1=as.integer(substr(moonTimes1[[5]],9,10))
						if (mds1==day) mtc=(mtr+mts1)/2-1.0 else mtc=(mtr+mts1+24)/2-1.0
						if (mtc>24) mtc=mtc-24
						moonCulm = CalcMoonCulm(inDate, mtc, StaLat, StaLong)  # case 6

						# probably time of moonset exists yet at this day: check

						mdr=substr(moonTimes[[4]],1,10)
						mds1=substr(moonTimes1[[5]],1,10)
						if (mds1==mdr) mSetStr=substr(time_round(as.POSIXct(moonTimes1[[5]]), "minute"),12,16)
					} else  # only time set exists: culmination earlier this day (case 5)
					{
						moonTimes0<-getMoonTimes(inDate-1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
						mtr0=StringToTime(substr(moonTimes0[[4]],12,19))
						mtc=(mtr0+mts-24)/2-1.0
						moonCulm = CalcMoonCulm(inDate, mtc, StaLat, StaLong)  # case 5

						# probably time of moonrise exists yet at this day: check

						moonTimes1<-getMoonTimes(inDate+1, StaLat, StaLong, keep = c("rise", "set"), tz = "UTC")
						mds=substr(moonTimes[[5]],1,10)
						mdr1=substr(moonTimes1[[4]],1,10)
						if (mds==mdr1) mRisStr=substr(time_round(as.POSIXct(moonTimes1[[4]]), "minute"),12,16)
					}
					mCulmStr=substr(time_round(as.POSIXct(moonCulm$mcDateTime), "minute"),12,16)
				}
				moonTimes;moonCulm;mCulmStr
				mRisStr;mSetStr;mCulmStr;moonCulm$moonAlt;moonIllum$fraction


				outl = sprintf (" %02d.%02d.   %s %s  %s%7.2f%4.0f", day, mon, mRisStr, mSetStr, mCulmStr, moonCulm$moonAlt, moonIllum$fraction*100)
				outl = sprintf ("%s   %s %s  %s%7.2f", outl, sRiseStr, sSetStr, sCulmStr, sunposGeo$alt)
				outl = sprintf ("%s  %02d:%02d   %s %s   %02d.%02d.%s", outl, sDurHr, sDurMin, duskStr, dawnStr, day, mon, "\n")
				cat (outl, file=df)

			}  # for day=day1:day2

		}  # for mon=mon1:mon2
	}  # for year=yearA:yearE

	close(df)  # close output file

	# Termination message

	infoFile = sprintf("%s%s%s", "\n\n\n  Results written to:\n\n  ", outFile, "\n\n")
	cat (infoFile, file="")

}  # end if iniOk

#### end of program 'SunMoonRiseSet.R' ######################################
