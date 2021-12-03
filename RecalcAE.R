#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  RecalcAE.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  02.08.2009                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               02.11.2021                                  #
#                               28.08.2020                                  #
#                               16.07.2020                                  #
#                               20.03.2020                                  #
#                               13.12.2019                                  #
#                               11.11.2019                                  #
#                               08.11.2019                                  #
#                               02.10.2018                                  #
#                               31.08.2018                                  #
#                               27.07.2018                                  #
#                               17.07.2018                                  #
#                               07.05.2018                                  #
#                               24.02.2018                                  #
#                               06.12.2017                                  #
#                               21.10.2017                                  #
#                               25.04.2017                                  #
#                               22.04.2017                                  #
#                               27.03.2017                                  #
#                               09.03.2017                                  #
#                               02.02.2017                                  #
#                               10.12.2016                                  #
#                               21.10.2016                                  #
#                               10.08.2016                                  #
#                               23.02.2016                                  #
#                               25.11.2015                                  #
#                               16.10.2012                                  #
#                               26.10.2010                                  #
#                               03.11.2009                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   MeteoSwiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2018)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RecalcAE.R' recalculates Dobson total ozone, reading the raw      #
#        data from the 'Dyyyymmdd.iii' files or from the 'AEyyyymmdd.iii'   #
#        files; results are written to 'AEyyyymmdd.iii' files or to         #
#        'AXyyyymmdd.iii' files (the latter containing extended information)#
#        located in the same or another directory.                          #
#                                                                           #
#        Data from one or several Dobsons, located at the same or at dif-   #
#        ferent places, also changing within the period are calculated.     #
#                                                                           #
#        Day statistics values (mean, stadev, number of measurements) are   #
#        written to the dayfile "DAYOZON_yyyy.iii" ( data of one year or    #
#        a part of a year, iii=Dobson identifier) and halfday means are     #
#        written to 'HdOzon_yyyy.iii'.                                      #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'RecalcAE.ini'.                                                    #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 03.11.2009 by Herbert Schill:                                    #
#                                                                           #
#          Day statistics values (mean, stadev, number of measurements)     #
#          for all wavelengths added to 'cdata' list; the values are        #
#          written to a dayfile (see description above).                    #
#                                                                           #
#                                                                           #
#        - 26.10.2010 by Herbert Schill:                                    #
#                                                                           #
#          - Introduction of hald day means for use in Umkehr data files:   #
#            creation of a file 'HdOzon_yyyy.iii'.                          #
#                                                                           #
#          - Change Nparams to 34: introduction of filter tool: filtering   #
#            of ozone values by sun intensity or flag value or both, or     #
#            no filtering.                                                  #
#                                                                           #
#                                                                           #
#        - 16.10.2012 by Herbert Schill:                                    #
#                                                                           #
#          - set Nparams=36 according to newest version of ini-file         #
#            (iniPar36: 'proceeding modus' not used in this program)        #
#                                                                           #
#          - Allow treatment of multiple instruments by reading an          #
#            instrument string such as e.g. '062_101' or '051,062,101'      #
#            each instrument is proceeded in a separate loop.               #
#                                                                           #
#          - adapt header line for 'df'-file (day statistics)               #
#                                                                           #
#                                                                           #
#        - 25.11.2015 by Herbert Schill:                                    #
#                                                                           #
#          - rename 'data'-structure to 'dataAE'                            #
#                                                                           #
#                                                                           #
#        - 23.02.2016 by Herbert Schill:                                    #
#                                                                           #
#          - set Nparams=33 according to newest version of ini-file         #
#            (iniPar33: 'proceeding modus' not used in this program)        #
#            and adapt parameter indices                                    #
#                                                                           #
#          - add multiple instrument proceeding                             #
#                                                                           #
#          - add 'AX' data format for extended output                       #
#                                                                           #
#                                                                           #
#        - 10.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - rename source 'ReadDobsTables.R' by 'ReadInstTables.R'         #
#                                                                           #
#          - rename call 'ReplaceCalDobsYear' by 'ReplaceCalInstYear'       #
#                                                                           #
#          - add treatment for reading also LV raw data-format (call        #
#            of function 'ReadDobsonLV'), defined by input path             #
#                                                                           #
#                                                                           #
#        - 21.10.2016 by Herbert Schill:                                    #
#                                                                           #
#          - add import info at begin of code (comment only)                #
#                                                                           #
#                                                                           #
#        - 10.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          - simpler format of the ini-file 'RecalcAE.ini', which is        #
#            then rearranged to the old format for compatibility            #
#                                                                           #
#          - read the values for the station from the station list file     #
#                                                                           #
#        - 02.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - replace source name 'TreatDobsonAE' by 'TreatDobsonData'       #
#                                                                           #
#                                                                           #
#        - 09.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - rename program from 'RecalcOz.R' to 'RecalcAE.R'               #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#          - remove bug in 'multCalStr' treatment                           #
#                                                                           #
#                                                                           #
#        - 22.04.2017 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'multCalStr' treatment                           #
#                                                                           #
#                                                                           #
#        - 25.04.2017 by Herbert Schill:                                    #
#                                                                           #
#          - add location to header version message                         #
#                                                                           #
#                                                                           #
#        - 21.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat'                             #
#                                                                           #
#          - final message added                                            #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - import some functions from 'DateZeit.R'                        #
#                                                                           #
#          - some minor adaptions                                           #
#                                                                           #
#                                                                           #
#        - 24.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 07.05.2018 by Herbert Schill:                                    #
#                                                                           #
#          - reads location from the 'NcorCalxxx.iii'-file for a period     #
#            and adapt calculations and output accordingly                  #
#                                                                           #
#                                                                           #
#        - 17.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          - use ARO as default station at begin, if location=VAR           #
#                                                                           #
#                                                                           #
#        - 27.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          - more possibilities of SunDur-info to sun intensity conversion  #
#            and sunInt dependend flagging (in 'ConvertSunInfo')            #
#                                                                           #
#        - 31.08.2018 by Herbert Schill:                                    #
#                                                                           #
#          - read first set of dN-values as default                         #
#                                                                           #
#                                                                           #
#        - 02.10.2018 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug for proper station at begin, if location=VAR        #
#                                                                           #
#                                                                           #
#        - 08.11.2019 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for use of high-resoluted direct radiation data for      #
#            sun intensity conversion and sunInt dependend flagging         #
#                                                                           #
#          - remove unused parameter 'iniPar' in some function calls        #
#                                                                           #
#                                                                           #
#        - 11.11.2019 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug for initialization of 'LocMeteo' name               #
#                                                                           #
#                                                                           #
#        - 13.12.2019 by Herbert Schill:                                    #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
#                                                                           #
#          - use strsplit for proper instrument and calibration identifier  #
#            in instrument loop                                             #
#                                                                           #
#                                                                           #
#        - 20.03.2020 by Herbert Schill:                                    #
#                                                                           #
#          - extend program for calculations with different absorption      #
#            coefficients set                                               #
#                                                                           #
#                                                                           #
#        - 16.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - modify program for calculations with different absorption      #
#            coefficients set based on the daily effective stratosphere     #
#            temperature                                                    #
#                                                                           #
#        - 28.08.2020 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameters for calling 'ReadStraTempData' and            #
#            'ReadConstantsFile'                                            #
#                                                                           #
#                                                                           #
#        - 02.11.2021 by Herbert Schill:                                    #
#                                                                           #
#          - station parameters: use again ARO as default station, if       #
#            station indicator=VAR                                          #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

library(timeDate)

source("DateZeit.R")
source("ReadInstTables.R")
source("TreatDobsonData.R")
source("CalcDobson.R")

#  from:               import:
#
#  DateZeit.R          CompareDate, ConvertDate, LeapYear, ReplaceCalInstYear 
#
#  ReadInstTables.R    ConvertSunInfo, ReadAbsCoeffData, ReadConstantsFile, 
#                      ReadSunIntFile, ReadDNtable, ReadIniFile, ReadMeteoFile, 
#                      ReadRNtable, ReadStationsFile, ReadStraTempData
#
#  TreatDobsonData.R   ReadDobsonAE, ReadDobsonLV, WriteDayStatis, 
#                      WriteDobsonAE, WriteHalfDayStatis
#
#  CalcDobson.R        CalculX


statusOK <- 0

# Open the ini-file 'RecalcAE.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=30
iniParams = ReadIniFile(Nparams, "RecalcAE.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]
	iniPar[2] = toupper(iniPar[2])
	iniPar[13] = toupper(iniPar[13])
	iniPar[30] = toupper(iniPar[30])

	# Read the values for all stations from the station list file

	StationPara = ReadStationsFile (iniPar[20],"VAR")
	iniOk  = StationPara[[1]][[1]]

	if (iniOk)  # Continue, if station list file was properly read
	{
		# Rearrange ini-parameter vector (for compatibility)
		#
		#  iniPar    Value
		#
		#     1      Station indicator (ARO/DAV/VAR)
		#     2      Dobson Instrument Identifier(s) 'iii'
		#     3      Calibration(s)
		#     4      Period Begin (dd.mm.yyyy)
		#     5      Period End   (dd.mm.yyyy)
		#     6      use standard RN-Tables (1=yes, 0=no)
		#     7      Filename standard RN-Tables
		#     8      Filename Delta-N-Correction Table
		#     9      Absorption Coefficients set name
		#    10      UTC-offset (MEZ->UTC=-1, TST->UTC=-33)
		#    11      Output time reference (UTC/LOC)
		#    12      Outputfile Format: AE/AX
		#    13      Path Input Datafiles
		#    14      Path Output Datafiles
		#    15      Path Tabellfiles
		#    16      Filename Atmospheric Constants Tables
		#    17      Sunpos/Airmass calculation Komhyr 1980 (1=yes, 0=no)
		#    18      Info about series
		#    19      Station Name
		#    20      Station Longitude (deg)
		#    21      Station Latitude (deg)
		#    22      Station Altitude (km)
		#    23      Station standard pressure (hPa)
		#    24      O3 layer barycentric height over station (km)
		#    25      Atmosphere barycentric height over station (km)
		#    26      Radius Earth (km)
		#    27      Sea level standard pressure (hPa)
		#    28      Filter ozone data (0=no, 1..3=yes)
		#    29      Filter: minimal sun intensity
		#    30      Filter: maximal sun intensity
		#    31      Filter: minimal flag value
		#    32      Filter: maximal flag value
		#    33      Enter SunDur-info to sun intensity (0..4)
		#    34      Path Meteofiles (iii=location)
		#    35      Path directRad/sunInt files (DAV only)
		#    36      Filename dirRad/sunInt file (DAV only)
		#    37      Station indicator (ARO/DAV/VAR)

		for (n in 1:18)  iniPar[n] = iniPar[n+1]
		for (n in 37:28)  iniPar[n] = iniPar[n-7]
		LocIni = iniPar[1]
		LocMeteo0 = LocMeteo = iniPar[37]
		sdInfo = as.integer(iniPar[33])
		# DirRadLim=180  # direct radiation limit for sunshine indicator

	}  # end if iniOk
}  # end if iniOk

#  1st indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

if (iniOk)  # Continue, if previous files were properly read
{
	# check input data format: if the substring 'LV' appear in the name of
	# the input-path, the respective data format is selected, otherwise
	# 'AE' is the default format)

	if (length(grep("LV",iniPar[13],ignore.case=FALSE))>0) inputFmt="LV" else inputFmt="AE"

	# check number of instruments to be proceed

	multInstStr0=iniPar[2]
	multInstStr=strsplit(iniPar[2],",")
	multCalStr=strsplit(iniPar[3],",")
	multAbsCoefStr=strsplit(iniPar[9],",")
	nnInstr = length(multInstStr[[1]])

	#  2nd indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	
	####  Loop for instruments  ########################################################

	for (inn in 1:nnInstr)
	{
	  # inn=1
		# get proper instrument identifier, instrument calibration and absorption ceofficient set

		iniPar[2] = multInstStr[[1]][[inn]]

		if (length(multCalStr[[1]])>1)
		{
			iniPar[3] = multCalStr[[1]][[inn]]
		} else
		{
			iniPar[3] = multCalStr[[1]][[1]]
		}

		if (length(multAbsCoefStr[[1]])>1)
		{
			iniPar[9] = multAbsCoefStr[[1]][[inn]]
		} else
		{
			iniPar[9] = multAbsCoefStr[[1]][[1]]
		}

		# screen-info about instrument loop

		infoFile = sprintf("%s%s%s%s%s%s%s","\n\n  Loop ",inn,"  Instrument to process: ",iniPar[2]," (",iniPar[3],")\n\n")
		cat(infoFile, file="")

		# Check wether a specific RN-table should be used, or wether the
		# time-dependend RN-tables are used for the R/N conversion;
		# standard filename is 'O3DRNT_yyyy.iii' (yyyy=year of table,
		# iii=Dobson identifier)

		if  (iniPar[6]!="0")
		{
		# Opens the required RN-tables (e.g. 'O3DRNT_2006.101') and reads the
		# R to N conversion values for the 3 wavelengths A, C, D.

			iniPar[7] = ReplaceCalInstYear (iniPar[7], iniPar[3], iniPar[2], 9999)
			RNfilename  = iniPar[7]
			RNfilename0 = RNfilename
			RNdata = ReadRNtable(iniPar[2], RNfilename, iniPar[15])
			iniOk  = RNdata[[1]][[1]]
		}  # end if iniPar[6]!="0"

		# Check wether a temperature-dependent temporal absorption ceofficient series, 
		# based on the stratospheric temperature profile of Payerne, is desired 
		# (absorption ceofficient set name (iniPar[9]) ending with 'Teff'), temperature 
		# values being read from the respective text file defined in the atmospheric 
		# constants file (iniPar[16]))

		if (substr(iniPar[9],nchar(iniPar[9])-3,nchar(iniPar[9]))=="Teff")
		{
			StraTempData = ReadStraTempData(iniPar[15], iniPar[16])
			tempACdataSet=1
			iniPar[9] = substr(iniPar[9],1,nchar(iniPar[9])-4)
		} else
		{ tempACdataSet=0 }

		# station parameters: use ARO as default station, if station indicator=VAR

		if (LocIni == "VAR")
		{
			i=1
		} else
		{
			i=grep(iniPar[1],StationPara$StationData[1,],ignore.case=TRUE)
			if (i==0) i=1
		}
		iniPar[1] = StationPara$StationData[1,i]
		for (n in 1:9)  iniPar[n+18] = StationPara$StationData[n+1,i]
		if (LocMeteo0=="LOC") iniPar[37]=iniPar[1]
		infoFile = sprintf("%s%s%s","    Station is ",iniPar[19], "\n\n")
		cat(infoFile, file="")

		# Read the atmospheric constants from the constants file

		ATconst0 = ReadConstantsFile("Dobson",iniPar[15],iniPar[16],iniPar[9],iniPar[23],iniPar[27])
		iniOk  = ATconst0[[1]][[1]]

		if (iniOk)  # Continue, if desired files were properly read so far
		{
			# Opens the file which contain the Delta-N-Correction history and 
			# information about the RN-tables history and the location, filename 
			# is usually 'NcorCalxxx.iii' (xxx=calibration identifier, e.g. '18a' 
			# or '00', iii=Dobson identifier) and reads the dates, the name of the 
			# valid RN-table, the location and the dN-values for the 3 wavelengths
			# A, C, D. 

			DNdata = ReadDNtable(iniPar)
			iniOk  = DNdata[[1]][[1]]
		}  # end if iniOk

		#  3rd indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		
		if (iniOk)  # Continue, if desired files were properly read so far
		{
			# Set first dN period date and read values

			Calendar    = array()
			DNvect      = array()
			nPeriods    = DNdata[[1]][[1]]
			pd=1
			RNfilename0 = sprintf("%s.%s",DNdata[[3]][[pd]],iniPar[2])
			periodDate0 = DNdata[[2]][[pd]]  # format "dd.mm.yyyy"
			Loc0        = DNdata[[7]][[pd]]  # location indicator
			for (i in 1:3)  DNvect[i] = DNdata[[i+3]][[pd]]  # dN-values

			# Test: first measurement date older than first DN period date ?
			
			i = (CompareDate(iniPar[4], periodDate0))

			#  4th indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
			
			if (i>=0)
			{
				# Create new header version message "VersionMess"

				fd=as.character(Sys.time(),TZ="GMT")  # "yyyy-mm-dd hh:mm:ss"
				iniPar[18] = ReplaceCalInstYear (iniPar[18], iniPar[3], iniPar[2], 9999)
				Veme0 = sprintf("%s%s%s%s%s%s %s", "Version: ", substr(fd,9,10), ".", substr(fd,6,7), ".", substr(fd,1,4), substr(fd,12,19))
				Veme = sprintf("%s  %s  %s  %s%s", Veme0, iniPar[18], iniPar[19], "AbsCoeff: ",iniPar[9])
				Veme = sprintf("%s%7.4f%7.4f%7.4f", Veme, ATconst0$AbsCoeff[[1]], ATconst0$AbsCoeff[[2]], ATconst0$AbsCoeff[[3]])

				# Treat desired period

				dayA  = as.integer(substr(iniPar[4],1,2))
				monA  = as.integer(substr(iniPar[4],4,5))
				yearA = as.integer(substr(iniPar[4],7,10))
				dayE  = as.integer(substr(iniPar[5],1,2))
				monE  = as.integer(substr(iniPar[5],4,5))
				yearE = as.integer(substr(iniPar[5],7,10))
				first = 1

				####  Time Loop  ########################################################

				# Treat year; set proper month range

				for (year in yearA:yearE)
				{
					# year=yearA

					# If a temperature-dependent temporal absorption ceofficient series, based 
					# on the stratospheric temperature profile of Payerne, is desired ,the 
					# yearly dataset is extracted from the total dataset 'StraTempData'

					if (tempACdataSet>0)
					{
						stDOY <- StraTempData$DOY[StraTempData$Year==year]
						stTemp <- StraTempData$straTemp[StraTempData$Year==year]
					}

					# If DirRad- or SunDur-info to sun intensity conversion is required,
					# set proper pathes for input files

					if (sdInfo>0)
					{
						LocMeteo  = iniPar[37]
						PathMeteo = ReplaceCalInstYear (iniPar[34], iniPar[3], LocMeteo, year)
						PathMeteo = sprintf("%s%s", PathMeteo, "\\")
						PathDirRad = ReplaceCalInstYear (iniPar[35], iniPar[3], LocMeteo, year)
					}

					# Open daily mean output file "DAYOZON_yyyy.iii" for the year and write header

					dfilename = sprintf("%s%s.%s", "Dayozon_", year, iniPar[2])
					dpathname = ReplaceCalInstYear (iniPar[14], iniPar[3], iniPar[2], year)
					dpathname = sprintf("%s%s", dpathname, dfilename)

					ds = file(dpathname,open="w")
					outl = sprintf ("%s", " iii M yyyymmdd   MeanC  SDev NNN   MeanD  SDev NNN   MeanA  SDev NNN")
					outl = sprintf ("%s%s%s", outl, "  MeanAD  SDev NNN  MeanCD  SDev NNN", "\n")
					cat(outl, file=ds)

					# Open halfday mean output file "HDOZON_yyyy.iii" for the year and write header

					dfilename = sprintf("%s%s.%s", "HdOzon_", year, iniPar[2])
					dpathname = ReplaceCalInstYear (iniPar[14], iniPar[3], iniPar[2], year)
					dpathname = sprintf("%s%s", dpathname, dfilename)

					hd = file(dpathname,open="w")
					outl = sprintf ("%s%s", "yyyymmdd  OzDay  nD   OzAM  nA   OzPM  nP", "\n")
					cat(outl, file=hd)

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
						
						# Treat days in month  #####################################################

						for (day in day1:day2)
						{
							# day=day1

							measDate = sprintf("%02d%s%02d%s%04d", day, '.', mon, '.', year)

							# Compare date with period date to evaluate the proper dN-period

							periodDate = periodDate0
							i = CompareDate(periodDate, measDate)
							while ((i<0) & (pd<nPeriods))
							{
								pd=pd+1
								periodDate = DNdata[[2]][[pd]]
								i = CompareDate(periodDate, measDate)
								#i;pd;nPeriods;periodDate;measDate
							}
							if  ((i>0) & (pd>1) & (pd<=nPeriods))	pd = pd-1
							periodDate = DNdata[[2]][[pd]]
							#i;pd;nPeriods;periodDate0;periodDate;measDate

							# If necessary, set the proper dN-values for the day and 
							# proper location parameters, recalc proper AtmCorr etc.

							if (periodDate != periodDate0)
							{
								for (i in 1:3)  DNvect[i] = DNdata[[i+3]][[pd]]
								Loc = DNdata[[7]][[pd]]
								if (Loc != Loc0)
								{
									i=grep(Loc,StationPara$StationData[1,],ignore.case=TRUE)
									iniPar[1]=iniPar[37]=Loc
									for (n in 1:9)  iniPar[n+18] = StationPara$StationData[n+1,i]
									Veme = sprintf("%s  %s  %s  %s%s", Veme0, iniPar[18], iniPar[19], "AbsCoeff: ",iniPar[9])
									Veme = sprintf("%s%7.4f%7.4f%7.4f", Veme, ATconst0$AbsCoeff[[1]], ATconst0$AbsCoeff[[2]], ATconst0$AbsCoeff[[3]])
									infoFile = sprintf("%s%s%s","\n    Station is ",iniPar[19], "\n\n")
									cat(infoFile, file="")

									# Recalculate Atmospheric Correction Factors for A, C, D, AC, AD, CD:
									#
									# Acor=((Beta-Beta')/(Alfa-Alfa'))*1000*p/p0

									pfac = 1000*(as.double(iniPar[23]))/(as.double(iniPar[27]))
									for (i in 1:6)  ATconst0$AtmosCorr[[i]]=(ATconst0$RayleighScatt[[i]]/ATconst0$AbsCoeff[[i]])*pfac

									# If SunDur-info to sun intensity conversion is required, set
									# proper path for input files

									if (sdInfo>0)
									{
										LocMeteo  = iniPar[37]
										PathMeteo = ReplaceCalInstYear (iniPar[34], iniPar[3], LocMeteo, year)
										PathMeteo = sprintf("%s%s", PathMeteo, "\\")
									}
									Loc0 = Loc
								}  # end if Loc!=Loc0
								periodDate0 = periodDate
							}  # end if periodDate!=periodDate0

							# Read RN-table at the first pass, if not read yet; check wether 
							# RN-table is the same as before, read new RN-table otherwise

							if  (iniPar[6]=="0") 
							{
								RNfilename = sprintf("%s.%s",DNdata[[3]][[pd]],iniPar[2])
								if  ((first) | (RNfilename0!=RNfilename)) 
								{
									RNdata = ReadRNtable(iniPar[2], RNfilename, iniPar[15])
									iniOk  = RNdata[[1]][[1]]
									RNfilename0 = RNfilename
									first=0
								}
							}  # end if iniPar[6]=="0"

							if (iniOk)  # Continue, if desired files were properly read so far
							{
								# Read header data and measurement data of the day from file "AEyyyymmdd.iii"
								# or "Dyyyymmdd.iii"; if measurements are available for the day, recalculate 
								# calendar-dependent coefficients and recalculate the total ozone

								if (inputFmt=="AE")
								{
									dataAE = ReadDobsonAE (day, mon, year, iniPar[2], iniPar[3], iniPar[13])
								} else
								{
									dataAE = ReadDobsonLV (day, mon, year, iniPar[2], iniPar[3], iniPar[13])
								}

								if  (dataAE$HeaderData$NumberMeas>0)  # measurements exist of this day
								{
									Calendar[1]=day
									Calendar[2]=mon
									Calendar[3]=year
									Calendar[4]=0
									Calendar[5]=leap
									Calendar = ConvertDate (Calendar,1)
									doy = Calendar[4]

									# If a temperature-dependent temporal absorption ceofficient series, based 
									# on the stratospheric temperature of Payerne of the day, is desired ,the 
									# proper absorption ceofficients Alfa-Alfa' are calculated; if no effective 
									# stratospheric temperature is available, the standard values are set

									if (tempACdataSet>0)
									{
										stTempDay = stTemp[stDOY==doy]
										if (length(stTempDay)==0) stTempDay=ATconst$AbsCoeff[7]
										ATconst<-ATconst0
										for (w in 1:6)
										{
											atcx = ATconst$AbsCoeff[w]*(1+ATconst$AbsCoeff[8]*(stTempDay-ATconst$AbsCoeff[7])/100)
											ATconst$AbsCoeff[[w]]=atcx
										}

										# Update header version message "VersionMess", including stratospheric temperature

										Veme = sprintf("%s  %s  %s  %s%s", Veme0, iniPar[18], iniPar[19], "AbsCoeff: ",iniPar[9])
										Veme = sprintf("%s%7.4f%7.4f%7.4f", Veme, ATconst$AbsCoeff[[1]], ATconst$AbsCoeff[[2]], ATconst$AbsCoeff[[3]])
										Veme = sprintf("%s%s%6.1f", Veme, " StratTemp:", stTempDay)

										# Recalculate Atmospheric Correction Factors for A, C, D, AC, AD, CD:
										#
										# Acor=((Beta-Beta')/(Alfa-Alfa'))*1000*p/p0

										pfac = 1000*(as.double(iniPar[23]))/(as.double(iniPar[27]))
										for (i in 1:6)  ATconst$AtmosCorr[[i]]=(ATconst$RayleighScatt[[i]]/ATconst$AbsCoeff[[i]])*pfac
									} else
									{
										ATconst<-ATconst0
									}  # end if tempACdataSet>0

									# SunDur-info to sun intensity conversion and sunInt dependent flagging:
									#
									#  sdInfo  action performed
									#
									#     4    - record of 1-min direct radiation and sun intensity data (Davos only)
									#          - flags for C, D, A are set, if sunInt<sunIntMin
									#
									#     3    - record of 10-Min-SunDur for the day read, if available, and
									#            SunDur-info to sun intensity conversion done
									#          - flags for C, D, A are set, if sunInt<sunIntMin
									#
									#     1    - record of 10-Min-SunDur for the day read, if available, and
									#            SunDur-info to sun intensity conversion done
									#
									#     0    - no action
									#
									#    -1    - values of sunInt>40 (ancient flagging method) reduced by 40
									#
									#    -3    - values of sunInt>40 (ancient flagging method) reduced by 40
									#          - flags for C, D, A are set if not yet done

									if (sdInfo>0)
									{
										if ((sdInfo==4) & (LocMeteo=="DAV"))
										{
											FileDirRad = ReplaceDayMonthYear (iniPar[36], day, mon, year)
											drFilename = sprintf("%s%s%s", PathDirRad, "\\", FileDirRad)
											meteoData = ReadSunIntFile (drFilename)
										} else
										{
											meteoData = ReadMeteoFile (PathMeteo, LocMeteo, year, doy)
										}
									}
									if (abs(sdInfo)>0) dataAE = ConvertSunInfo (dataAE, meteoData, iniPar)

									# Recalculation of the total ozone data

									cdata = CalculX (iniPar, Calendar, dataAE, ATconst, RNdata, DNvect)

									# if valid measurements of this day exist, write recalculated total ozone
									# to a new file "AEyyyymmdd.iii", write day statistics to the day 
									# statistics file "DAYOZON_yyyy.iii" and write halfday statistics of
									# AD-wavelengths to the file "HDOZ_yyyy.iii"

									if  (cdata[[1]][[4]]>0)  # valid measurements exist of this day
									{
										cdata[[1]][[6]] = Veme
										statusOK = WriteDobsonAE (iniPar, cdata, RNfilename, DNvect)
										ds = WriteDayStatis (cdata, ds)
										if  (cdata[[3]][[2]][[4]]>0)  # valid AD measurements exist of this day
										{
											hd = WriteHalfDayStatis (cdata, hd)
										}
									}
								}  # if measurements exist of this day
							}  # if iniOk
						}  # for day=day1:day2  #########################################################

					}  # for mon=mon1:mon2

					close(ds)  # close day statistics file "DAYOZON_yyyy.iii" of the year
					close(hd)  # close halfday statistics file "HDOZON_yyyy.iii" of the year

				}  # for year=yearA:yearE
			}
			else  # first measurement date is older than first DN period date
			{
				infoFile = sprintf("%s%s%s%s%s","\n\n  First measurement date ",iniPar[4], 
				                   " is older than first DN period date ", periodDate, "\n\n")
				cat(infoFile, file="")
				statusOK=0
			}  # end if i>=0 (first measurement date : first DN period date)

		}  # end if iniOk

	}  # for inn=0:nnInstr-1 (instrument loop)

	infoFile = sprintf("%s%s%s%s%s%s%s","\n\n  Period ", iniPar[4], "-", iniPar[5],
	                   "  for instrument(s) ", multInstStr0, "  recalculated\n\n\n")
	cat(infoFile, file="")

}  # end if iniOk

#### end of program 'RecalcAE.R' ############################################
