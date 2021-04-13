#############################################################################
#                                                                           #
#                                                                           #
#                            SUN POSITION CALCULATION                       #
#                                                                           #
#                                                                           #
#        Program Name        :  CalcSunTracks.R                             #
#                                                                           #
#                                                                           #
#        Creation Date       :  21.10.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               15.04.2020                                  #
#                               06.12.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD/WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'CalcSunTracks.R' calculates the azimuths, elevations and the      #
#        airmasses of the sun for one day and a given location.             #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'CalcSunTracks.ini'.                                    #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt calendar format to array instead of list                 #
#                                                                           #
#                                                                           #
#        - 15.04.2020 by Herbert Schill:                                    #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
TZ="Europe/Greenwich"

source("ReadInstTables.R")
source("DateZeit.R")

#  from:               import:
#
#  ReadInstTables.R    ReadIniFile, ReadStationsFile
#
#  DateZeit.R          AzimuthSunhei, ConvertDate


# Open the ini-file 'CalcSunTracks.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "CalcSunTracks.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Read the values for the station from the station list file

	StationPara = ReadStationsFile (iniPar[4],iniPar[2])
	StationName = StationPara[[2]][[1]]
	StaLong = StationPara[[3]][[1]]
	StaLat = StationPara[[3]][[2]]
	StaHeight = StationPara[[3]][[3]]
	O3Layer = StationPara[[3]][[5]]
	AtmLayer = StationPara[[3]][[6]]
	RadiusEarth = StationPara[[3]][[7]]
	iniOk = StationPara[[1]][[1]]
}

if (iniOk)  # Continue, if previous files were properly read
{
	# Treat date

	day  = as.integer(substr(iniPar[3],1,2))
	mon  = as.integer(substr(iniPar[3],4,5))
	year = as.integer(substr(iniPar[3],7,10))
	cDate = array()
	cDate[1] = day
	cDate[2] = mon
	cDate[3] = year
	cDate[4] = 0
	cDate[5] = 0
	cDate = ConvertDate(cDate, 1)
	jdn = cDate[4]
	jdnStr = sprintf("%03d",jdn)

	# Time-step and array length for suntrack calculation

	dT = as.integer(iniPar[7])/60
	STdim=1440/as.integer(iniPar[7])

	# Open output file and write header

	df = file(iniPar[5],open="w")
	outl = sprintf ("%s%s  %s%s%s%s%s", "Suntrack (Azimut, Sunhei)  ", StationName, iniPar[3], " (", jdnStr, ")", "\n")
	cat (outl, file=df)
	outl = sprintf ("%s%s", "Time[UTC] Azimut  Sunhei  Airmass", "\n")
	cat (outl, file=df)

	# Calculate suntrack for the day

	for (j in 1:STdim)
	{
		TimeHr = (j-1)*dT
		TimeHMS = TimeToString(TimeHr)
		sunpos = AzimuthSunhei (day, mon, year, TimeHr, StaLat, StaLong, 
		                        StaHeight, O3Layer, AtmLayer, RadiusEarth)

		outl = sprintf ("%s%8.2f%8.2f%8.3f%s", TimeHMS, sunpos$Azimuth, sunpos$Sunhei, sunpos$AirmassO3, "\n")
		cat (outl, file=df)
	}

	close(df)  # close output file

	# Termination message

	infoFile = sprintf("%s%s%s", "\n\n  Suntracks written to:\n\n  ", iniPar[5], "\n\n")
	cat (infoFile, file="")

}  # end if iniOk

#### end of program 'CalcSunTracks.R' ######################################
