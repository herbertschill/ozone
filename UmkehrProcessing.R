#############################################################################
#                                                                           #
#                                                                           #
#                            DOBSON UMKEHR POSTPROCESSING                   #
#                                                                           #
#                                                                           #
#        Module Name         :  UmkehrProcessing.R                          #
#                                                                           #
#                                                                           #
#        Creation Date       :  07.12.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               15.01.2020                                  #
#                               04.07.2018                                  #
#                               26.01.2018                                  #
#                               21.01.2018                                  #
#                               12.12.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'UmkehrProcessing.R' reads the raw Dobson Umkehr datafile          #
#        'umyyyymmddw.iii' (yyymmdd=date, w=wavelength (C/D/A), iii=        #
#        instrument identifier).                                            #
#                                                                           #
#        The raw data is separated in AM/PM, sunheights are calculated,     #
#        R/N-conversion is done, the total ozone for the half day is eva-   #
#        luated, cloud correction of the N-values is done, if necessary,    #
#        leading to a C-curve, or the lux values of a cloudless Umkehr are  #
#        added to the luxlist-file 'LuxCurves_iiiyyyymm.txt', if required,  #
#        and the new maxlux-curve in the file is calculated.                #
#                                                                           #
#        The results of a day are written to a 'Cxyyyymmdd.txt' file which  #
#        might be further processed by the LabView Umkehr postprocessing    #
#        programs 'NCorrection.LV' and 'ZUM.LV'. If required, half day      #
#        data are also written to 'ukyymmdd.hd' (hd=am/pm) in the           #
#        'Umkpre'-format.                                                   #
#                                                                           #
#        All necessary initial information is read from the ASCII-file      #
#        'UmkehrProcessing.ini'. A full calendar month or parts of it       #
#        are allowed as period. The days to proceed are listed in the       #
#        yearly cloudlist file 'CloudList_yyyy.iii'.                        #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 12.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt to altered sequence of ini-file                          #
#                                                                           #
#                                                                           #
#        - 21.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - possibility of additional 'Umkpre'-formatted output added      #
#                                                                           #
#          - more flexibility in treatment of input-names ("mm", "yyyy")    #
#            and their replacement by values added                          #
#                                                                           #
#                                                                           #
#        - 26.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - test on CL-code for AM and PM                                  #
#                                                                           #
#          - additional info output added                                   #
#                                                                           #
#          - allow filtering of measurements with StaDevR>LimitSDevR        #
#                                                                           #
#                                                                           #
#        - 04.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 15.01.2020 by Herbert Schill:                                    #
#                                                                           #
#          - import 'ReplaceCalInstYear' from 'DateZeit.R'                  #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
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
#  DateZeit.R          AzimuthSunhei, CalcTrueNoon, ReplaceCalInstYear
#
#  ReadInstTables.R    ReadCloudCorTable, ReadCloudListTable, ReadIniFile, 
#                      ReplaceDayMonthYear
#
#  TreatDobsonData.R   ReadDobsonUM

calib=""
iniOk=0
isTest=0

# Open the ini-file 'UmkehrProcessing.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=24
iniParams = ReadIniFile(Nparams, "UmkehrProcessing.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Get period

	if (nchar(iniPar[6])==7)  # period: month (format "mm.yyyy")
	{
		mon  = as.integer(substr(iniPar[6],1,2))
		year = as.integer(substr(iniPar[6],4,7))
		leap = LeapYear(year)
		dayA = 1
		dayE = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
		if (leap==1 & mon==2) dayE=29
	} else if (nchar(iniPar[6])==10)  # period: single day (format "dd.mm.yyyy")
	{
		dayA = as.integer(substr(iniPar[6],1,2))
		mon  = as.integer(substr(iniPar[6],4,5))
		year = as.integer(substr(iniPar[6],7,10))
		dayE = dayA
	} else  # period: dayA..dayE in month (format "d1.-d2.mm.yyyy")
	{
		dayA = as.integer(substr(iniPar[6],1,2))
		dayE = as.integer(substr(iniPar[6],5,6))
		mon  = as.integer(substr(iniPar[6],8,9))
		year = as.integer(substr(iniPar[6],11,14))
	}

	# Get other parameters

	StaIndex = iniPar[2]
	dobson = iniPar[3]
	headerOP = iniPar[4]
	wavelength = iniPar[5]
	LuxFileName = iniPar[7]
	CloudListName = iniPar[8]
	DobsTotoz  = iniPar[9]
	CalTotoz  = iniPar[10]
	RNfilename = ReplaceCalInstYear (iniPar[11], "", dobson, "")
	CloudCorName = iniPar[12]
	inPathname = iniPar[13]
	outPathname = iniPar[14]
	PathUmkpre = iniPar[15]
	PathTotoz0 = iniPar[16]
	CloudListPath = iniPar[17]
	LuxFilePath = iniPar[18]
	TablePath = iniPar[19]
	StaFilename = iniPar[20]
	AtmTableName = iniPar[21]
	nhead = as.integer(iniPar[22])
	LimitSDevR = as.double(iniPar[23])
	splineSmo = as.integer(iniPar[24])

	# Read the station parameters from the station list file

	StationPara = ReadStationsFile (StaFilename, StaIndex)
	iniOk  = StationPara[[3]][[1]]

	# Read R/N conversion table and cloud correction table

	RNdata = ReadRNtable(dobson, RNfilename ,TablePath)
	CCdata = ReadCloudCorTable(CloudCorName ,TablePath)

	# Read the entries for each halfday of the yearly cloud list table:
	#
	#   0  cloudless curve, use for Luxcurve
	#   1  cloudless curve, do not use for Luxcurve
	#   2  cloudy curve, perform cloud correction
	#   9  no or unusable Umkehr available for this half day

	CloudListName = ReplaceDayMonthYear (CloudListName, 0, mon, year)
	CloudListName = ReplaceCalInstYear (CloudListName, calib, dobson, year)
	CloudListPath = ReplaceCalInstYear (CloudListPath, calib, dobson, year)
	CLdata = ReadCloudListTable(CloudListName ,CloudListPath, mon)

	# Read the information of the Luxcurves used for the cloud correction

	LuxFileName = ReplaceDayMonthYear (LuxFileName, 0, mon, year)
	LuxFileName = ReplaceCalInstYear (LuxFileName, calib, dobson, year)
	LuxFilePath = ReplaceCalInstYear (LuxFilePath, calib, dobson, year)
	LuxFilePathName = sprintf("%s%s", LuxFilePath, LuxFileName)
	LuxInfo = list(LuxFilePathName, CLdata$ListAM, CLdata$ListPM)
	names(LuxInfo) = c("LuxFileName","ListAM","ListPM")

	# Set proper input/output pathes for the year

	inPathname = ReplaceCalInstYear (inPathname, calib, dobson, year)
	outPathname = ReplaceCalInstYear (outPathname, calib, dobson, year)
	if (PathUmkpre!="0") 	PathUmkpre = ReplaceCalInstYear (PathUmkpre, calib, dobson, year)

	# Treat days in month

	for (day in dayA:dayE)
	{

		if (isTest) day=dayA  # test only

		measDate = sprintf("%04d%02d%02d", year, mon, day)

		# Proceeding desired for this day ?

		if ((CLdata$ListAM[day]!=9) | (CLdata$ListPM[day]!=9))
		{
			# read Umkehr data from 'umyyyymmddw.iii' file

			dataUM = ReadDobsonUM (day, mon, year, dobson, wavelength, inPathname, nhead)

			if (dataUM$HeaderData$NumberMeas>9)  # data exist
			{
				dataCX = TreatDobsonUM (dataUM, CCdata, RNdata, StationPara, LuxInfo,
																DobsTotoz, CalTotoz, PathTotoz0, LimitSDevR, splineSmo)

				if ((dataCX$HeaderData$NMeasAM+dataCX$HeaderData$NMeasPM)>19)  # CX-data exist
				{
					WriteDobsonCX (outPathname, headerOP, dataCX)
					if (PathUmkpre!="0") WriteDobUmkpre (PathUmkpre, dataCX)
				} else  # not enough proceedable data
				{
					infoFile = sprintf("%s%04d.%02d.%02d%s","    ", year, mon, day, ": not enough CX data for processing\n")
					cat(infoFile, file="")
				}  # end if enough CX data exist or not

			} else  # no proceedable data exist
			{
				if (dataUM$HeaderData$NumberMeas>0)  # some data exist
				{
					infoFile = sprintf("%s%04d.%02d.%02d%s","    ", year, mon, day, ": not enough Umkehr data for processing\n")
					cat(infoFile, file="")
				}
			}  # end if data exist or not
		} else  # CLdata-code for both halfdays 9
		{
			infoFile = sprintf("%s%04d.%02d.%02d%s","    ", year, mon, day, ": no processing desired (CLdata-code = 9 9)\n")
			cat(infoFile, file="")
		}
	}  # end for day=day1..day2

}  # end if iniOk

# end of program 'UmkehrProcessing.R'