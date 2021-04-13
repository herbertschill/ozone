#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  RecalcDS.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  02.02.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               28.08.2020                                  #
#                               22.07.2020                                  #
#                               24.02.2018                                  #
#                               05.01.2018                                  #
#                               27.03.2017                                  #
#                               19.03.2017                                  #
#                               07.02.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RecalcDS.R' recalculates O3 and SO2 values from the raw data or   #
#        from the summary data read from the 'Bjjjyy.iii'-files (single     #
#        day file), or from the 'DSjjjyy.iii'-files (single day file).      #
#                                                                           #
#        Instrument constants will be read either from 'inst'-values from   #
#        B-file (for input data type=BRaw/BSum), from DS-file (for input    #
#        data type=DS), from 'ICFjjjyy.iii'-file, or as period constants    #
#        from 'ICFCalxxx.iii'-file.                                         #
#                                                                           #
#        Used instrument constants, recalculated O3 and SO2 values and      #
#        day statistics values (mean, stadev, number of measurements) are   #
#        written to new DS-files "DSjjjyy.bbb".                             #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'RecalcDS.ini'.                                         #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 07.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          Add timestamp and calibration info to DS-data                    #
#                                                                           #
#                                                                           #
#        - 19.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          allow use of original AbsCoeffs and ETCs from DS-file to         #
#          "recalculate" DS-file in 'CalcNewDS'                             #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#                                                                           #
#        - 05.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - adapt calendar format to array instead of list                 #
#                                                                           #
#          - use of 'LeapYear' for evaluation of leap                       #
#                                                                           #
#                                                                           #
#        - 24.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 22.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - modify program for calculations with different O3 absorption   #
#            coefficient sets, based or not based on the daily effective    #
#            stratosphere temperature                                       #
#                                                                           #
#                                                                           #
#        - 28.08.2020 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameters for calling 'ReadStraTempData'                #
#                                                                           #
#                                                                           #
#############################################################################


#############################################################################
#                                                                           #
#  tobedone: add option for straylight-correction                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("ReadInstTables.R")
source("TreatBrewerData.R")

#  from:               import:
#
#  DateZeit.R          CompareDate, ConvertDate, LeapYear
#
#  ReadInstTables.R    ReadIniFile, ReadStationsFile, ReadStraTempData,
#                      ReplaceCalInstYear
#
#  TreatBrewerData.R   CalcNewDS, ReadBrewerB, ReadBrewerDS, WriteBrewerDS,
#                      WriteDayozDS


# Reads the path-file 'RecalcDS.pth' with the adress of the ini-file
# 'RecalcDS.ini' and reads the initialization parameters from the latter
# (Nparams designs the number of parameters to read from ini-file)

ICFvalues=2.35  # SO2 standard absorption coefficient
Nparams=22
iniParams = ReadIniFile(Nparams, "RecalcDS.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	LocStr      = iniParams[[2]][[2]]
	BrewerStr   = iniParams[[2]][[3]]
	CalStr      = iniParams[[2]][[4]]
	PeriodA     = iniParams[[2]][[5]]
	PeriodE     = iniParams[[2]][[6]]
	InputType   = iniParams[[2]][[7]]
	OutputPath  = iniParams[[2]][[14]]
	DayozPath   = iniParams[[2]][[15]]
	StationList = iniParams[[2]][[16]]
	AbsCoefType = iniParams[[2]][[17]]
	RelDiff2BP  = as.double(iniParams[[2]][[18]])/100

	# Check if DS- or SL-data should be proceeded

		if (length(grep("SL",InputType,ignore.case=FALSE))>0) inData<-"sl" else inData<-"ds"


	# Check wether a temperature-dependent temporal absorption ceofficient series, 
	# based on the stratospheric temperature profile of Payerne, is desired 
	# (absorption ceofficient set name 'AbsCoefType' ending with 'Teff'), temperature 
	# values being read from the respective text file; recalculation of total ozone 
	# uses totoz values from the DS-file and made the necessary corrections

	if (substr(AbsCoefType,nchar(AbsCoefType)-3,nchar(AbsCoefType))=="Teff")
	{
		tempACdataSet=1
		StraTempData = ReadStraTempData(iniParams[[2]][[21]], iniParams[[2]][[22]])
		AbsCoefType = substr(AbsCoefType,1,nchar(AbsCoefType)-4)
		InputType="DS"
		iniParams[[2]][[8]]=="DS"
		TempRatio   = as.double(iniParams[[2]][[19]])/100
		StdTemper   = as.double(iniParams[[2]][[20]])+273.15
	} else
	{
		tempACdataSet=0
	}

	if ((iniParams[[2]][[8]]=="B") | (iniParams[[2]][[8]]=="DS"))
		ICFType=0 else if (iniParams[[2]][[8]]=="ICFjjjyy")
		ICFType=1 else if (iniParams[[2]][[8]]=="ICFCalxxx") 
		ICFType=2 else
		ICFType = as.integer(iniParams[[2]][[8]])

	if (InputType=="DS")
		InputPath=iniParams[[2]][[13]] else 
		InputPath=iniParams[[2]][[12]]

	len=nchar(InputPath)
	if (substr(InputPath,len,len+1)!="\\")  InputPath=sprintf("%s%s",InputPath, "\\")
	len=nchar(OutputPath)
	if (substr(OutputPath,len,len+1)!="\\")  OutputPath=sprintf("%s%s",OutputPath, "\\")

	if (ICFType>0)  # read instr const from 'ICFCalxxx.iii' or 'ICFjjjyy.iii'
	{
		ICFPath  = iniParams[[2]][[11]]
		len=nchar(ICFPath)
		if (substr(ICFPath,len,len+1)!="\\")  ICFPath=sprintf("%s%s",ICFPath, "\\")
		ICFPath = ReplaceCalInstYear (ICFPath, CalStr, BrewerStr, 9999)
		icfParams = array()
		ICFvalues = array()

		if (ICFType==1)  # read instr const from 'ICFjjjyy.iii'
		{
			IcfFilename = sprintf("%s%s",ICFPath, iniParams[[2]][[9]])
			if (file.exists(IcfFilename))
			{
				Nparams=23
				inifile = file(IcfFilename,open="r")
				for (j in 2:Nparams)
				{
					line = scan(inifile,what="character",nlines=1,quiet=TRUE)
					ICFvalues[j]=line[1]
				}
				close(inifile)
				iniOk = Nparams
			} else  # ICF-file file doesen't exist
			{
				infoFile = sprintf("%s%s%s","  File  ",IcfFilename, "  not found")
				print(infoFile)
				iniOk=0
			}
		} else  # read instr const table from 'ICFCalxxx.iii'
		{
			ICFCalName  = iniParams[[2]][[10]]
			ICFCalName = ReplaceCalInstYear (ICFCalName, CalStr, BrewerStr, 9999)
			IcfFilename = sprintf("%s%s",ICFPath, ICFCalName)
			if (file.exists(IcfFilename))
			{
				Nparams=23
				inifile = file(IcfFilename,open="r")
				for (i in 1:3) line = scan(inifile,what="character",nlines=1,quiet=TRUE)
				i=0
				while ((length(line)!=0))
				{
					k=0
					for (j in 1:Nparams)
					{
						i=i+1
						if (j==2) k=2
						icfParams[i]=line[j+k]
					}
					line = scan(inifile,what="character",nlines=1,quiet=TRUE)
				}
				nPeriods=i/Nparams
				dim(icfParams) <- c(Nparams,nPeriods) # reshape array
				iniOk=1
				close(inifile)

				pd=1
				periodDate  = icfParams[1,1]  # first ICF period date (format "dd.mm.yyyy")

				# Test: first measurement date older than first ICF period date ?
				
				if ((CompareDate(PeriodA, periodDate))<0)
				{
					infoFile = sprintf("%s%s%s%s","  First measurement date ",PeriodA, 
					                   " is older than first DN period date ", periodDate)
					print(infoFile)
					iniOk=0
				}
			} else  # ICF-file file doesen't exist
			{
				infoFile = sprintf("%s%s%s","  File  ",IcfFilename, "  not found")
				print(infoFile)
				iniOk=1
			}
		}  # end if ICFType:1
	}  # end if ICFType>0

	if (iniOk)  # Continue, if ICF-file was properly read
	{
		# Read the values for the station from the station list file

		StationPara = ReadStationsFile (StationList, LocStr)
		iniOk  = StationPara[[1]][[1]]
	}

	if (iniOk)  # Continue, if desired files were properly read so far
	{
		# Create timestamp "Version: dd.mm.yyyy hh:mm:ss"

		fd=as.character(Sys.time())  # "yyyy-mm-dd hh:mm:ss"
		Veme0 = sprintf("%s%s%s%s", "Version: ", substr(fd,9,10), ".", substr(fd,6,7))
		Veme0 = sprintf("%s%s%s%s  %s", Veme0, ".", substr(fd,1,4), substr(fd,11,19), AbsCoefType)
		if (tempACdataSet>0) Veme0 = sprintf("%s%s", Veme0, " Teff")

		# Set desired period

		dayA  = as.integer(substr(PeriodA,1,2))
		monA  = as.integer(substr(PeriodA,4,5))
		yearA = as.integer(substr(PeriodA,7,10))
		dayE  = as.integer(substr(PeriodE,1,2))
		monE  = as.integer(substr(PeriodE,4,5))
		yearE = as.integer(substr(PeriodE,7,10))
		first = 1
		cDate = array()

		####  Time Loop  ########################################################

		# Treat year; set proper month range

		for (year in yearA:yearE)
		{
			# year=yearA
			leap = LeapYear(year)
			mon1=1
			mon2=12
			if (year==yearA) mon1=monA
			if (year==yearE) mon2=monE

			# Open daily mean output file "DAYOZON_yyyy_Calxxx.iii" for the year and write header, if desired

			if (DayozPath!="0")
			{
				dfilename = sprintf("%s%s_%s.%s", "DayOzon_", year, CalStr, BrewerStr)
				dpathname = ReplaceCalInstYear (DayozPath, CalStr, BrewerStr, year)
				dpathname = sprintf("%s%s", dpathname, dfilename)

				df = file(dpathname,open="w")
				outl = sprintf ("%s%04d\n", "Daily means of Brewer direct sun measurements for ", year)
				outl = sprintf ("%s%s%s  %s    %s\n", outl, " Brewer ", BrewerStr, CalStr, Veme0)
				outl = sprintf ("%s%s\n", outl, " iii M yyyymmdd  MeanO3  SDev NNN MeanSO2  SDev NNN")
				cat (outl, file=df)
			}

			# If a temperature-dependent temporal absorption ceofficient series, based 
			# on the stratospheric temperature profile of Payerne, is desired ,the 
			# yearly dataset is extracted from the total dataset 'StraTempData'

			if (tempACdataSet>0)
			{
				stDOY <- StraTempData$DOY[StraTempData$Year==year]
				stTemp <- StraTempData$straTemp[StraTempData$Year==year]
			}

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

				# Treat days in month

				for (day in day1:day2)
				{
					# day=day1
					cDate[1] = day
					cDate[2] = mon
					cDate[3] = year
					cDate[4] = 0
					cDate[5] = leap
					cDate = ConvertDate(cDate, 1)
					doy = cDate[4]

					if (ICFType==2)
					{
						# Compare date with period date to evaluate the proper ICF-period

						measDate = sprintf("%02d%s%02d%s%04d", day, '.', mon, '.', year)
						i = CompareDate(periodDate, measDate)
						while ((i<0) & (pd<nPeriods))
						{
							pd=pd+1
							periodDate = icfParams[1,pd]
							i = CompareDate(periodDate, measDate)
							i;pd;periodDate;measDate
						}
						if ((i>0) & (pd>1) & (pd<=nPeriods)) pd=pd-1

						# Set the proper ICF-values for the day

						ICFvalues = array()
						ICFvalues = icfParams[,pd]
						periodDate = icfParams[1,pd]

					}  # end if ICFType==2

					# Read header data and measurement data of the day from file "Bjjjyy.iii"
					# or "DSjjjyy.iii"; if measurements are available for the day, recalculate 
					# the total ozone

					if (InputType=="DS")
					{
						DSdata = ReadBrewerDS(doy, day, mon, year, BrewerStr, CalStr, InputPath)
						if (DSdata[[1]][[1]]>0)
						{
							# If a temperature-dependent temporal absorption ceofficient series, based 
							# on the stratospheric temperature of Payerne of the day, is desired ,the 
							# proper absorption ceofficient is calculated; if no effective 
							# stratospheric temperature is available, the standard value is set

							A1org = DSdata[[1]][[10]] # original O3 absorption coefficient from DS-file
							if (tempACdataSet>0)
							{
								stTempDay = stTemp[stDOY==doy]
								if (length(stTempDay)==0) stTempDay=StdTemper
								A1cor = A1org*(1+RelDiff2BP)*(1+TempRatio*(stTempDay-StdTemper))
								Veme = sprintf("%s%s%6.1f", Veme0, "=", stTempDay)
							} else
							{
								Veme = Veme0
								if (ICFType==0) A1cor = A1org*(1+RelDiff2BP) else A1cor = A1org
							}
							nData = DSdata[[1]][[1]]
							RecalcData = CalcNewDS(BrewerStr, CalStr, cDate, InputType, DSdata, ICFType, 
							                       ICFvalues, StationPara, tempACdataSet, A1cor)
						} else
						{
							nData = 0
						}
					} else  # InputType="B"
					{
						Bdata = ReadBrewerB(doy, day, mon, year, BrewerStr, InputPath, inData)

#           ReadBrewerB <- function (jdn, day, mon, year, brewStr, bPath)
#						jdn=doy
#						brewStr<-BrewerStr
#						bPath<-InputPath
#						jdn;day;mon;year;brewStr;bPath

						if (length(Bdata[[2]][[1]])>=6)
						{
							if (inData=="ds")
							{
								A1cor=0
								RecalcData = CalcNewDS(BrewerStr, CalStr, cDate, InputType, Bdata, ICFType, 
																			 ICFvalues, StationPara, tempACdataSet, A1cor)
							} else
							{
								RecalcData = CalcNewSL(BrewerStr, CalStr, cDate, InputType, Bdata, ICFType, 
																			 ICFvalues, StationPara, tempACdataSet, A1cor)
							}
#
#             CalcNewDS <- function (BrewerStr, CalStr, cDate, rcType, bdata, ICFType, 
#                                    ICFvalues, StationPara, tempACdataSet, A1cor)
#							rcType=InputType 
#							bdata<-Bdata
#
							nData = length(Bdata[[3]][[1]])
							Veme = Veme0
						} else
						{
							nData = 0
						}
					}  # end if InputType:"DS"

					# create and write output file 'DSjjjyy.bbb', if data are available

					if (nData>0)
					{
						RecalcData[[1]][[7]] = CalStr
						RecalcData[[1]][[15]] = Veme
						names(RecalcData[[1]][[15]]) = c("VersionMessage")
						WriteBrewerDS(RecalcData, OutputPath, StationPara)
						if (DayozPath!="0") df = WriteDayozDS(df, RecalcData)
					}

				}  # for day=day1:day2

			}  # for mon=mon1:mon2

			# close dayoz file "DAYOZON_yyyy_Calxxx.iii" of the year

			if (DayozPath!="0") close(df)

		}  # for year=yearA:yearE

	}  # end if iniOk

}  # end if iniOk


#### end of program 'RecalcDS.R' ############################################
