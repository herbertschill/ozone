#############################################################################
#                                                                           #
#                                                                           #
#                            DATA FILE CONVERSION                           #
#                                                                           #
#                                                                           #
#        Program Name        :  LuxValuesMerging.R                          #
#                                                                           #
#                                                                           #
#        Creation Date       :  26.11.2015                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               13.07.2018                                  #
#                               27.01.2018                                  #
#                               07.12.2015                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'LuxValuesMerging.R' takes for a period the Umkehr raw value       #
#        files without lux values from one Dobson and merges the lux        #
#        values from annother Dobson of the same day, if available.         #
#        Merging is done by using a spline function.                        #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'LuxValuesMerging.ini'.                       #
#                                                                           #
#        Log-file 'LuxValuesMergingyyyy.log' contains information about     #
#        the treated files                                                  #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 07.12.2015 by Herbert Schill:                                    #
#                                                                           #
#          - change Lux-value output format to higher precision             #
#                                                                           #
#                                                                           #
#        - 27.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - adapt program to new 'ReadDobsonUM' and other routines         #
#                                                                           #
#          - reduce parameters in 'ini'-file to the needs                   #
#                                                                           #
#                                                                           #
#        - 13.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in log output                                       #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")

source("DateZeit.R")
source("ReadInstTables.R")
source("TreatDobsonData.R")

#  from:               import:
#
#  DateZeit.R          CalcTrueNoon, LeapYear, StringToTime
#
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear
#
#  TreatDobsonData.R   ReadDobsonUM

statusOK=0
isTest=0

# Open the ini-file 'LuxValuesMerging.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=12
iniParams = ReadIniFile(Nparams, "LuxValuesMerging.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar) iniPar[n] = iniParams[[2]][[n]]
	for (n in 22:11)  iniPar[n+6] = iniPar[n]

	refDobson = iniPar[2]
	dobson = iniPar[3]
	dayA  = as.integer(substr(iniPar[4],1,2))
	monA  = as.integer(substr(iniPar[4],4,5))
	yearA = as.integer(substr(iniPar[4],7,10))
	dayE  = as.integer(substr(iniPar[5],1,2))
	monE  = as.integer(substr(iniPar[5],4,5))
	yearE = as.integer(substr(iniPar[5],7,10))
	wavelength = iniPar[6]
	nHeader = as.integer(iniPar[10])
	StaLong = as.double(iniPar[12])

	if (isTest)  # test only
	{
		day=dayA
		mon=monA
		year=yearA
	}

	# Treat year; set proper month range

	for (year in yearA:yearE)
	{
		# Set proper input/output pathes for the year

		refPathname = ReplaceCalInstYear (iniPar[7], "", refDobson, year)
		inPathname = ReplaceCalInstYear (iniPar[8], "", dobson, year)
		outPathname = ReplaceCalInstYear (iniPar[9], "", dobson, year)

		## Create and open yearly log-file

		logFilename = sprintf("%s%s%4d%s", outPathname, "LuxValuesMerging",year,".log")
		log = file(logFilename,open="w")
		logl = sprintf ("\n%s%s%s%s\n\n","    LuxValuesMerging from D", refDobson, " to D", dobson)
		logl = sprintf ("%s%27s%s\n",logl," ","Dobs RefD     Dobs AM   Dobs PM   RefD AM   Dobs PM")
		logl = sprintf ("%s%6s%s\n\n",logl," ","Date     Dobs RefD   ndat nref    uaa  uae  upa  upe  raa  rae  rpa  rpe")
		cat (logl, file=log)

		# Set proper month range

		leap = LeapYear(year)
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

		# Treat month; set proper days in month 'mdays'

		for (mon in mon1:mon2)
		{

			mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
			if (leap==1 && mon==2) mdays=29
			day1=1
			day2=mdays
			if ((year==yearA) & (mon==monA)) day1=dayA
			if ((year==yearE) & (mon==monE)) day2=dayE

			# Treat days in month

			for (day in day1:day2)
			{

				measDate = sprintf("%04d%02d%02d", year, mon, day)
				logl = sprintf ("%s%02d%s%02d%1s%04d","    ",day,".",mon,".",year)

				# create filenames and open input/output files

				inFilename = sprintf("%s%s%s%s%s%s", inPathname, "um", measDate, wavelength, ".", dobson)
				outFilename = sprintf("%s%s%s%s%s%s", outPathname, "um", measDate, wavelength, ".", dobson)
				refFilename = sprintf("%s%s%s%s%s%s", refPathname, "um", measDate, wavelength, ".", refDobson)
				ex1=file.exists(refFilename)
				ex2=file.exists(inFilename)

				logl = ifelse(ex2, sprintf ("%s%s%s",logl,"  ",dobson), sprintf ("%s%s",logl,"     "))
				logl = ifelse(ex1, sprintf ("%s%s%s",logl,"  ",refDobson), sprintf ("%s%s",logl,"     "))

				if (ex1 && ex2)
				{
					# Read umkehr measurement data of the day from file "umyyyymmddw.iii"
					# and from the reference file containing lux values

					dataUM = ReadDobsonUM (day, mon, year, dobson, wavelength, inPathname, nHeader)
					drefUM = ReadDobsonUM (day, mon, year, refDobson, wavelength, refPathname, nHeader)

					# Get time of true noon of the day, using the Komhyr algorithms 

					timeNoon = CalcTrueNoon (day, mon, year, StaLong)
					timeHMax = timeNoon$TimeHR/24

					# rearrangement of arrays

					ndat=dataUM[[1]][[4]]
					nref=drefUM[[1]][[4]]
					umkTime = array()
					umkLux  = array()
					refTime = array()
					refLux  = array()
					raa=rae=rpa=rpe=0
					uaa=uae=upa=upe=0

					# Convert times from "hh:mm:ss" to fraction of day; get indices for AM and PM

					for (i in 1:ndat)
					{
						umkTime[i] = (StringToTime(dataUM[[2]][[2]][[i]]))/24
						if(umkTime[i]<=timeHMax)
						{
							uae=uae+1
						}
						else
						{
							upe=upe+1
						}
					}  # next i
					if (uae>0) uaa=1
					if (upe>0)
					{
						upa=uae+1
						upe=uae+upe
					}

					for (i in 1:nref)
					{
						refLux[i] =drefUM[[2]][[6]][[i]]
						refTime[i] = (StringToTime(drefUM[[2]][[2]][[i]]))/24
						if(refTime[i]<=timeHMax)
						{
							rae=rae+1
						}
						else
						{
							rpe=rpe+1
						}
					}  # next i
					if (rae>0) raa=1
					if (rpe>0)
					{
						rpa=rae+1
						rpe=rae+rpe
					}

					logl = sprintf ("%s%7d%5d%7d%5d%5d%5d%5d%5d%5d%5d",logl,ndat,nref,uaa,uae,upa,upe,raa,rae,rpa,rpe)

					# Separation of AM and PM

					if (uaa>0 && raa>0)  # AM
					{
						umkTimeHD = array()
						umkXY     = array()
						k=1
						for (i in uaa:uae)
						{
							umkTimeHD[k]=umkTime[i]
							k=k+1
						}
						umkXY  = spline(refTime, refLux, xout=umkTimeHD)
						for (i in uaa:uae) {umkLux[i] = umkXY[[2]][[i]]}

						# graphics for test

						if (isTest>0)
						{
							require(graphics)
							op = par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
							plot(refTime, refLux, main = paste("spline[fun](.) through", k, "points"))
							lines(spline(refTime, refLux))
							lines(umkLux, col = 3)
						}
					}  # end if uaa>0 & raa>0

					if (upa>0 && rpa>0)  # PM
					{
						umkTimeHD = array()
						umkXY     = array()
						k=1
						for (i in upa:upe)
						{
							umkTimeHD[k]=umkTime[i]
							k=k+1
						}
						umkXY  = spline(refTime, refLux, xout=umkTimeHD)
						k=1
						for (i in upa:upe)
						{
							umkLux[i] = umkXY[[2]][[k]]
							k=k+1
						}

						# graphics for test

						if (isTest>0)
						{
							require(graphics)
							op = par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
							plot(refTime, refLux, main = paste("spline[fun](.) through", rpe-rpa+1, "points"))
							lines(spline(refTime, refLux))
							lines(umkLux, col = 3)
						}
					}  # end if upa>0 & rpa>0

					# create output file 'umyyyymmddw.iii', write header and data to it

					otf = file(outFilename,open="w")
					exOutfile=1
					infoFile = sprintf("%s%s\n","    > ", outFilename)
					cat(infoFile, file="")
					outl = sprintf ("\n%40s%s\n\n","DOBSON ", dobson)
					cat (outl, file=otf)
					outl = sprintf ("%35s%02d%s%02d%1s%04d\n\n","Messung vom  ",day,"-",mon,"-",year)
					cat (outl, file=otf)
					outl = sprintf ("%77s\n","Zeit           R-Wert      R-STD         Lux        Lux STD    Temp       HV")
					cat (outl, file=otf)
					outl = sprintf ("%84s\n","------------------------------------------------------------------------------------")
					cat (outl, file=otf)

					for (i in 1:ndat)
					{
						mTime  = dataUM[[2]][[2]][[i]]
						Rdial  = as.single(dataUM[[2]][[4]][[i]])
						StdR   = as.single(dataUM[[2]][[5]][[i]])
						Lux    = umkLux[i]
						StdLux = as.single(dataUM[[2]][[7]][[i]])
						Temper = as.single(dataUM[[2]][[8]][[i]])
						HV     = as.single(dataUM[[2]][[9]][[i]])
						outl = sprintf ("%9s%12.2f%12.3f%13.2f%11.2f%12.2f%11.2f\n", mTime,Rdial,StdR,Lux,StdLux,Temper,HV)
						cat (outl, file=otf)
					}  # next i
					close(otf)  # close output file 'umyyyymmddw.iii'

				}  # end if ex1 & ex2
				logl = sprintf ("%s\n", logl)
				cat (logl, file=log)

			}  # end for day=day1..day2
		}  # end for mon=mon1..mon2
		close(log)  # close logfile
	} # end for year=yearA..yearE

}  # end if iniOk

# end of program 'LuxValuesMerging.R'
