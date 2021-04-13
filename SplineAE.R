#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  SplineAE.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  18.07.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               31.10.2020                                  #
#                               12.08.2018                                  #
#                               09.08.2018                                  #
#                               27.10.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   Meteoswiss/PMOD-WRC        #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'SplineAE.R' reads the raw data from the 'AEyyyymmdd.iii'-file or  #
#        from the 'Dyyyymmdd.iii'-file, calculates the exact, cubic spline  #
#        through the R-values for standard IC-reference times (beginning    #
#        at the full hour with wl C, then D and A, followed by one minute   #
#        pause etc., or using the IC-times of the reference instrument)     #
#        and writes the values to the RDCC standard format on a             #
#        "IDyyjjj.iii"-file.                                                #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'SplineAE.ini'.                                         #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 27.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          - change date of 'ID'-file from 'jjjyy' to 'yyjjj'               #
#                                                                           #
#                                                                           #
#        - 09.08.2018 by Herbert Schill:                                    #
#                                                                           #
#          - allow different time zone than UTC for output                  #
#                                                                           #
#          - allow different time grids for comparison output               #
#                                                                           #
#          - allow use of all or only of unflagged measurements             #
#                                                                           #
#                                                                           #
#        - 12.08.2018 by Herbert Schill:                                    #
#                                                                           #
#          - allow use of reference instrument measurement times for        #
#            spline x-values                                                #
#                                                                           #
#                                                                           #
#        - 31.10.2020 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in use of reference instrument measurement times    #
#            for spline x-values (bug affected only wl=A values)            #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("ReadInstTables.R")
source("TreatDobsonData.R")

#  from:               import:
#
#  DateZeit.R          LeapYear, TimeToString
#
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear
#
#  TreatDobsonData.R   ReadDobsonAE, ReadDobsonLV


statusOK <- 0

# Open the ini-file 'RecalcAE.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=12
iniParams = ReadIniFile(Nparams, "SplineAE.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# check input data format: if the substring 'LV' appear in the name of
	# the input-path, the respective data format is selected, otherwise
	# 'AE' is the default format)

	if (length(grep("LV",iniPar[6],ignore.case=FALSE))>0) inputFmt="LV" else inputFmt="AE"

	# Treat desired day

	day  = as.integer(substr(iniPar[5],1,2))
	mon  = as.integer(substr(iniPar[5],4,5))
	year = as.integer(substr(iniPar[5],7,10))
	sYearStr=substr(sprintf("%04d", year),3,4)
	leap = LeapYear(year)

	if (leap==0) 
		{jdn = switch(mon, 0,31,59,90,120,151,181,212,243,273,304,334) + day} else
		{jdn = switch(mon, 0,31,60,91,121,152,182,213,244,274,305,335) + day}

	# Read header data and measurement data of the day from file "AEyyyymmdd.iii"
	# or "Dyyyymmdd.iii"

	if (inputFmt=="AE")
	{
		dataAE = ReadDobsonAE (day, mon, year, iniPar[3], iniPar[4], iniPar[6])
	} else
	{
		dataAE = ReadDobsonLV (day, mon, year, iniPar[3], iniPar[4], iniPar[6])
	}
	nn=dataAE$HeaderData$NumberMeas

	if (nn>0)  # measurements exist of this day
	{
		# If only unflagged measurements are desired, the sum of the 3 flags
		# of a measurement set must be zero; reduce dataset accordingly

		unFlagged=as.integer(iniPar[8])
		if (unFlagged>0)
		{
			f<-dataAE$Measurements$Flags
			fs<-f[1,]+f[2,]+f[3,]
			nn=length(grep (0,fs,))  # number of unflagged measurement sets
		}

		if (nn>0)  # (unflagged) measurements exist of this day
		{
			# Get measurement times and R-values; calculate x-values (time), or 
			# get measurement times of reference instrument, for the spline,
			# if required, and calculate the splined y-values (R-values)

			if (iniPar[12]>0)  {require(graphics)}
			yspl = array()
			xsstr = array()
			dTimeZone = as.double(iniPar[10])

			# Get measurement times of reference instrument, if required

			if (iniPar[9]==9)
			{
				fpathname = sprintf("%s%s", iniPar[7], iniPar[11])
				rff = file(fpathname,open="r")
				inRef = scan(rff,what="character",nlines=999,quiet=TRUE)
				nxs=length(inRef)/13
				dim(inRef) = c(13,nxs)
				xspl0 = array()
				xsstr = inRef[8,]
				xspl0 = as.double(substr(xsstr,1,2))+as.double(substr(xsstr,3,4))/60+as.double(substr(xsstr,5,6))/3600
				k=0
				close(rff)
			} else
			{
				timeGrid=60/as.double(iniPar[9])
				dxs=1/timeGrid
			}

			# Proceed wavelength pairs C, D, A

			for (w in 1:3)
			{
				if (unFlagged>0)
				{
					x<-dataAE$Measurements$MeasTime[w,][fs==0]+dTimeZone
					y<-dataAE$Measurements$Rvalues[w,][fs==0]
				} else
				{
					x<-dataAE$Measurements$MeasTime[w,]+dTimeZone
					y<-dataAE$Measurements$Rvalues[w,]
				}

				if (iniPar[9]<9)
				{
					if (w==1)  # all 3 wl-vectors must have the same dimension
					{
						xs1=floor(x[1])+(w-1)/60
						while (xs1<x[1])
						{
							xs1=xs1+dxs
						}
						xs2=ceiling(x[nn])+(w-1)/60
						while (xs2>x[nn])
						{
							xs2=xs2-dxs
						}
						nxs=(xs2-xs1)*timeGrid+1
					}
					k=0
					if (w>1)
					{
						if (timeGrid==60) k=1
						xs1=xs1+1/60
					}
					xspl = array()
					for (i in 1:nxs)
					{
						xspl[i] = xs1+dxs*(i-1)
						if (w==1)
						{
							u = TimeToString(xspl[i])
							xsstr[i+k] = sprintf ("%s%s%s", substr(u,1,2),substr(u,4,5),substr(u,7,8))
						}
					}  # for i=1..nxs
				} else  # iniPar[9]=9
				{ 
					#	xspl = xspl+(w-1)/60   !! erronous code used in 2018 !!
					xspl = xspl0+(w-1)/60
				}  # end if iniPar[9]:9

				svspl <- spline(x, y, xout=xspl)
				if (iniPar[12]>0)
				{
					plot(x, y, main = paste("spline through", round(nxs), "points, wl=", w))
					lines(spline(x, y))
					lines(svspl, col = 3)
				}
				for (i in 1:nxs)
				{
					yspl[(w-1)*nxs+i+k]=svspl$y[i]
				}
			}  # for w=1:3

			dim(yspl) = c(nxs+k,3)

			# write splined R-values in IC-format to "Idyyjjj.iii"

			idpath = ReplaceCalInstYear (iniPar[7], iniPar[4], iniPar[3], year)
			filename = sprintf("%s%s%03d.%s", "Id", sYearStr, jdn, iniPar[3])
			fpathname = sprintf("%s%s", idpath, filename)
			idf = file(fpathname,open="w")

			for (i in 1:nxs)
			{
				outl = sprintf ("%6.1f%6.1f%6.1f", yspl[i,1], yspl[i,2], yspl[i,3])
				outl = sprintf ("%s%s%02d", outl, "   0.0   0.0I DSGQP ", day)
				outl = sprintf ("%s %s %02d %s", outl, xsstr[i], mon, sYearStr)
				outl = sprintf ("%s%s", outl, " C               ")
				outl = sprintf ("%s  %s  %s\n", outl, iniPar[3], iniPar[2])
				cat (outl, file=idf)
			}
			close(idf)

		}  # end if nn>0 ((unflagged) measurements exist of this day)
	}  # end if measurements exist of this day

}  # end if iniOk

#### end of program 'SplineAE.R' ############################################
