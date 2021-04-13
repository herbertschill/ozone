#############################################################################
#                                                                           #
#                                                                           #
#                            SUN POSITION CALCULATION                       #
#                                                                           #
#                                                                           #
#        Program Name        :  CalcSunRiseSet.R                            #
#                                                                           #
#                                                                           #
#        Creation Date       :  26.12.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'CalcSunRiseSet.R' calculates the times of the mathematical or     #
#        the real sunrise, culmination and sunset for a location, or the    #
#        elevations of the real sunrise and sunset for a location, whose    #
#        horizon is given by a vector of azimuths and elevations.           #
#                                                                           #
#        Calculations are done for a required period.                       #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'CalcSunRiseSet.ini'.                                   #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\LKO\\Programs\\R")
Sys.setenv(TZ="UTC")

source("ReadInstTables.R")
source("DateZeit.R")

#  from:               import:
#
#  ReadInstTables.R    ReadIniFile, ReadStationsFile
#
#  DateZeit.R          AzimuthSunhei, ConvertDate


test = 0

# Open the ini-file 'CalcSunRiseSet.ini' and read the initialization parameters
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

	# Read the values for the station from the station list file

	StationPara = ReadStationsFile (iniPar[6],iniPar[2])
	StationName = StationPara[[2]][[1]]
	StaLong = StationPara[[3]][[1]]
	StaLat = StationPara[[3]][[2]]
	StaHeight = StationPara[[3]][[3]]
	O3Layer = StationPara[[3]][[5]]
	AtmLayer = StationPara[[3]][[6]]
	RadiusEarth = StationPara[[3]][[7]]
	iniOk = StationPara[[1]][[1]]
}

if (iniOk)  # Continue, if station list file was properly read
{
	# If real sunrise and sunset are required, read the horizon values 
	# (azimuth, elevation) for the station from the horizon file

	azi = array()
	ele = array()

	if ((iniPar[5]=="R") | (iniPar[5]=="r"))
	{
		RealHor=1
		separ="	"
		hf = file(iniPar[7],open="r")
		mv = array()
		mv = scan(hf,what="character",sep=separ,skip=1,nlines=0,quiet=TRUE)
		close(hf)  # close horizon file
		vlen=length(mv)/4
		ThresHold = as.double(iniPar[11])
		MinSunhei = as.double(iniPar[12])
		for (i in 1:vlen)
		{
			azi[i] = as.double(mv[(i-1)*4+1])
			ele[i] = as.double(mv[(i-1)*4+2])
			if (ele[i]<MinSunhei) ele[i]=MinSunhei
		}
	} else  # mathematical horizon
	{
		RealHor=0
		vlen=360
		ThresHold = 0
		MinSunhei = 0
		for (i in 1:vlen)
		{
			azi[i] = as.double(i)
			ele[i] = 0
		}
	}

	Horizont = list(azi, ele)
	names(Horizont) = c("azim", "elev")

	if (vlen>=180) {iniOk=1} else {iniOk=0}
}

if (iniOk)  # Continue, if previous files were properly read
{
	# Open output file and write header

	outDetail = as.integer(iniPar[13])

	outFile = sprintf ("%s%s", iniPar[9], iniPar[8])
	df = file(outFile, open="w")

	if (outDetail==1)
	{
		outl = sprintf ("%s%s", "Sunheights [deg] of Sunrise and Sunset  ", StationName)
	} else if (outDetail==2)
	{
		outl = sprintf ("%s%s", "Times [UTC] of Sunrise and Sunset  ", StationName)
	} else if (outDetail==3)
	{
		outl = sprintf ("%s%s", "Times [UTC] of Sunrise, Culmination and Sunset  ", StationName)
	} else if (outDetail==4)
	{
		outl = sprintf ("%s%s", "Azimuth, Sunheight and Times [UTC] of Sunrise and Sunset  ", StationName)
	}
	outl = sprintf ("%s%s%4.1f%s%4.1f%s", outl, "    Threshold (Sunhei-Horizon) = ", ThresHold, "°, Minimal Sunheight = ", MinSunhei, "° \n")
	cat (outl, file=df)

	if (outDetail<3)
	{
		outl = sprintf ("%s%s", "JDN    Rise    Sett", "\n")
	} else if (outDetail==3)
	{
		outl = sprintf ("%s%s", "dd.mm.yyyy  JDN  SRise   Culmination SunSet   SunDuration", "\n")
	} else if (outDetail==4)
	{
		outl = sprintf ("%s%s", "dd.mm. jdn    i    AZ[i] AZ[i+1]   AZ[Y]   EL[i] EL[i+1]   ST[i] ST[i+1]   RiseY HMrise    i    AZ[i] AZ[i-1]   AZ[Y]   EL[i] EL[i-1]   ST[i] ST[i-1]    SetY  HMset", "\n")
	}
	cat (outl, file=df)

	# Time-step and array length for suntrack calculation

	dT = as.integer(iniPar[10])/60
	STdim=1440/as.integer(iniPar[10])
	Sec30=1/2880

	# Treat desired period

	dayA  = as.integer(substr(iniPar[3],1,2))
	monA  = as.integer(substr(iniPar[3],4,5))
	yearA = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	yearE = as.integer(substr(iniPar[4],7,10))

	# Treat year; set proper month range

	cat (sprintf("%s", "  Calculating ."), file="")

	for (year in yearA:yearE)
	{
		# year=yearA

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

			# Treat days in month

			for (day in day1:day2)
			{
				# day=day1

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

				# Calculate suntrack for the day

				azimut=array()
				sunhei=array()
				sttime=array()

				for (j in 1:STdim)
				{
					sttime[j] = (j-1)*dT
					sunpos = AzimuthSunhei (day, mon, year, sttime[j], StaLat, StaLong, 
                                  StaHeight, O3Layer, AtmLayer, RadiusEarth)

					sunhei[j] = sunpos$Sunhei
					azimut[j] = sunpos$Azimuth
				}
				Suntrack = list(azimut, sunhei, sttime)
				names(Suntrack) = c("azimut", "sunhei", "sttime")

				# TrueNoon-list with: TimeHMax [hr], HMax "hh:mm:ss", SunheiMax

				TN = CalcTrueNoon (day, mon, year, StaLong)

				if (RealHor==0)  # mathematical horizon
				{
					# Calculate height[deg] and time [hr/hh:mm] of mathematical sunrise (sunhei=0°)

					i=1
					while (Suntrack$sunhei[i]<0) i=i+1
					sh0=Suntrack$sunhei[i-1]
					sh1=Suntrack$sunhei[i]
					tt0=Suntrack$sttime[i-1]
					tt1=Suntrack$sttime[i]
					TRise=tt0+(tt1-tt0)*(-sh0)/(sh1-sh0)
					HMrise=substr(TimeToString(TRise+Sec30),1,5)
					#hm0=TimeToString(tt0)
					#hm1=TimeToString(tt1)
					#hmr=TimeToString(TRise)

					# Calculate height[deg] and time [hr/hh:mm] of mathematical sunset (sunhei=0°)

					i=length(Suntrack$sunhei)
					while (Suntrack$sunhei[i]<0) i=i-1
					sh0=Suntrack$sunhei[i]
					sh1=Suntrack$sunhei[i+1]
					tt0=Suntrack$sttime[i]
					tt1=Suntrack$sttime[i+1]
					TSet=tt0+(tt1-tt0)*(-sh0)/(sh1-sh0)
					#hm0=TimeToString(tt0)
					#hm1=TimeToString(tt1)
					#hms=TimeToString(TSet)
					HMset=substr(TimeToString(TSet+Sec30),1,5)
					SunDurHr = TSet-TRise
					SunDurHM = substr(TimeToString(SunDurHr+Sec30),1,5)

					if (outDetail==2)
					{
						outl = sprintf ("%s   %s   %s%s", jdnStr, HMrise, HMset, "\n")
					} else
					{
						TSHmax = substr(TimeToString(TN$TimeHR+Sec30),1,5)
						outl = sprintf ("%02d.%02d.%4d  %s  %s  %s%7.2f  %s%7.2f  %s%s", day, mon, year, jdnStr, HMrise, TSHmax, TN$SunheiMax, HMset, SunDurHr, SunDurHM, "\n")
					}

				} else  # real horizon
				{
					# Calculate the splined STspl-values (azimut,sunhei) of the suntrack 
					# for the xspl-values (azim) of the real horizon

					x<-Suntrack$azimut
					y<-Suntrack$sunhei
					z<-Suntrack$sttime
					nxs=length(Horizont$azim)
					xspl<-Horizont$azim
					yspl<-Horizont$elev

					STspl <- spline(x, y, xout=xspl)
					TTspl <- spline(x, Suntrack$sttime, xout=xspl)
					if (test>0)
					{
						require(graphics)
						plot(x, y, main = paste("spline through", floor(nxs), "points"))
						lines(spline(x, y))
						lines(yspl, col = 3)
					}

					# Calculate height[deg] and time [hr/hh:mm] of real sunrise

					nnDY=length(xspl)
					nnAM=length(xspl[xspl<=180])
					i=nnAM
					while (STspl$y[i]>yspl[i] & i>0) i=i-1
					e = (yspl[i+1]-yspl[i])-(STspl$y[i+1]-STspl$y[i])
					if (e==0)
					{ 
						AZx = xspl[i]
						TRise = TTspl$y[i]
						SunriseY = yspl[i] + ThresHold
					} else
					{
						f = (STspl$y[i]-yspl[i])*(xspl[i+1]-xspl[i])
						AZx = xspl[i]+f/e
						TRise = TTspl$y[i]+(TTspl$y[i+1]-TTspl$y[i])*f/e
						SunriseY = yspl[i]+(AZx-xspl[i])/(xspl[i+1]-xspl[i])*(yspl[i+1]-yspl[i]) + ThresHold
					}
					HMrise=substr(TimeToString(TRise+Sec30),1,5)
					if (outDetail==4)
					{
						outl = sprintf ("%02d.%02d. %s%6d%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f  %s",day,mon,jdnStr, i,xspl[i],xspl[i+1],AZx,yspl[i],yspl[i+1],STspl$y[i],STspl$y[i+1],SunriseY,HMrise)
					}

					# Calculate height[deg] and time [hr/hh:mm] of real sunset

					i=nnAM+1
					while (STspl$y[i]>yspl[i] & i<nnDY) i=i+1
					e = (yspl[i-1]-yspl[i])-(STspl$y[i-1]-STspl$y[i])
					if (e==0)
					{ 
						AZx = xspl[i]
						TSet = TTspl$y[i]
						SunsetY = yspl[i] + ThresHold
					} else
					{
						f = (STspl$y[i]-yspl[i])*(xspl[i-1]-xspl[i])
						AZx = xspl[i-1]-f/e
						TSet = TTspl$y[i]+(TTspl$y[i+1]-TTspl$y[i])*f/e
						SunsetY = yspl[i]+(AZx-xspl[i])/(xspl[i-1]-xspl[i])*(yspl[i-1]-yspl[i]) + ThresHold
					}
					HMset = substr(TimeToString(TSet+Sec30),1,5)
					SunDurHr = TSet-TRise
					SunDurHM = substr(TimeToString(SunDurHr+Sec30),1,5)

					if (outDetail==1)
					{
						outl = sprintf ("%s%8.2f%8.2f%s", jdnStr, SunriseY, SunsetY, "\n")
					} else if (outDetail==2)
					{
						outl = sprintf ("%s   %s   %s%s", jdnStr, HMrise, HMset, "\n")
					} else if (outDetail==3)
					{
						TSHmax = substr(TimeToString(TN$TimeHR+Sec30),1,5)
						outl = sprintf ("%02d.%02d.%4d  %s  %s  %s%7.2f  %s%7.2f  %s%s", day, mon, year, jdnStr, HMrise, TSHmax, TN$SunheiMax, HMset, SunDurHr, SunDurHM, "\n")
					} else if (outDetail==4)
					{
						outl = sprintf ("%s%6d%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f  %s%s",outl, i,xspl[i],xspl[i-1],AZx,yspl[i],yspl[i-1],STspl$y[i],STspl$y[i-1],SunsetY,HMset, "\n")
					}
				}  # end if RealHor=0/1

				cat (outl, file=df)

			}  # for day=day1:day2
		}  # for mon=mon1:mon2
	}  # for year=yearA:yearE

	close(df)  # close output file

	# Termination message

	infoFile = sprintf("%s%s%s", "\n\n\n  Results written to:\n\n  ", outFile, "\n\n")
	cat (infoFile, file="")

}  # end if iniOk

#### end of program 'CalcRealSunset.R' ######################################
