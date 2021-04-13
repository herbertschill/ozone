#############################################################################
#                                                                           #
#                                                                           #
#                            STANDARD LAMP DATA RECALCULATION               #
#                                                                           #
#                                                                           #
#        Program Name        :  RecalcSL.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  24.12.2020                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD/WRC                   #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RecalcSL.R' recalculates Standard Lamp values from the raw data   #
#        from the 'Bjjjyy.iii'-files (single day file).                     #
#                                                                           #
#        Instrument constants will be read either from 'inst'-values from   #
#        B-file, from 'ICFjjjyy.iii'-file, or as period constants from      #
#        'ICFCalxxx.iii'-file.                                              #
#                                                                           #
#        Used instrument constants, recalculated Standard Lamp values and   #
#        day statistics values (mean, stadev, number of measurements) are   #
#        written to new SL-files "SLjjjyy.bbb", recalculated avg data are   #
#        written to "SLavg_yyyy_Calxxx.iii".                                #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'RecalcSL.ini'.                                         #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
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
#  ReadInstTables.R    ReadIniFile, ReadStationsFile, ReplaceCalInstYear
#
#  TreatBrewerData.R   CalcNewSL, ReadBrewerB, WriteBrewerSL, WriteAvgSL


# Reads the path-file 'RecalcSL.pth' with the adress of the ini-file
# 'RecalcSL.ini' and reads the initialization parameters from the latter
# (Nparams designs the number of parameters to read from ini-file)

ICFvalues=2.35  # SO2 standard absorption coefficient
Nparams=14
iniParams = ReadIniFile(Nparams, "RecalcSL.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	LocStr      = iniParams[[2]][[2]]
	BrewerStr   = iniParams[[2]][[3]]
	CalStr      = iniParams[[2]][[4]]
	PeriodA     = iniParams[[2]][[5]]
	PeriodE     = iniParams[[2]][[6]]
	InputPath   = iniParams[[2]][[11]]
	OutputPath  = iniParams[[2]][[12]]
	SLavgPath   = iniParams[[2]][[13]]
	StationList = iniParams[[2]][[14]]

	if (iniParams[[2]][[7]]=="B")
		ICFType=0 else if (iniParams[[2]][[7]]=="ICFjjjyy")
		ICFType=1 else if (iniParams[[2]][[7]]=="ICFCalxxx") 
		ICFType=2 else
		ICFType = as.integer(iniParams[[2]][[7]])

	len=nchar(InputPath)
	if (substr(InputPath,len,len+1)!="\\")  InputPath=sprintf("%s%s",InputPath, "\\")
	len=nchar(OutputPath)
	if (substr(OutputPath,len,len+1)!="\\")  OutputPath=sprintf("%s%s",OutputPath, "\\")

	if (ICFType>0)  # read instr const from 'ICFCalxxx.iii' or 'ICFjjjyy.iii'
	{
		ICFPath  = iniParams[[2]][[10]]
		len=nchar(ICFPath)
		if (substr(ICFPath,len,len+1)!="\\")  ICFPath=sprintf("%s%s",ICFPath, "\\")
		ICFPath = ReplaceCalInstYear (ICFPath, CalStr, BrewerStr, 9999)
		icfParams = array()
		ICFvalues = array()

		if (ICFType==1)  # read instr const from 'ICFjjjyy.iii'
		{
			IcfFilename = sprintf("%s%s",ICFPath, iniParams[[2]][[8]])
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
			ICFCalName  = iniParams[[2]][[9]]
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
		Veme = sprintf("%s%s%s%s", "Version: ", substr(fd,9,10), ".", substr(fd,6,7))
		Veme = sprintf("%s%s%s%s", Veme, ".", substr(fd,1,4), substr(fd,11,19))

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

			# Open daily mean output file "SLavg_yyyy_Calxxx.iii" for the year and write header, if desired

			if (SLavgPath!="0")
			{
				dfilename = sprintf("%s%s_%s.%s", "SLavg_", year, CalStr, BrewerStr)
				dpathname = ReplaceCalInstYear (SLavgPath, CalStr, BrewerStr, year)
				dpathname = sprintf("%s%s", dpathname, dfilename)

				df = file(dpathname,open="w")
				outl = sprintf ("%s%04d\n", "Daily means of Brewer Standard Lamp tests for ", year)
				outl = sprintf ("%s%s%s  %s    %s\n", outl, " Brewer ", BrewerStr, CalStr, Veme)
				outl = sprintf ("%s%s\n", outl, "jjjyy TN TX NN    R1    R2    R3    R4    R5    R6 LampInt  StaDevs")
				cat (outl, file=df)
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

					# Read header data and measurement data of the day from file "Bjjjyy.iii"; 
					# if SL-tests are available for the day, recalculate the test values

					Bdata = ReadBrewerB(doy, day, mon, year, BrewerStr, InputPath, "sl")

#           ReadBrewerB <- function (jdn, day, mon, year, brewStr, bPath, "sl")
#						jdn=doy
#						brewStr<-BrewerStr
#						bPath<-InputPath
#						jdn;day;mon;year;brewStr;bPath

					if (length(Bdata[[2]][[1]])>=6)
					{
						SLdata = CalcNewSL(BrewerStr,CalStr,cDate,Bdata,ICFType,ICFvalues,StationPara)
#
#             CalcNewSL <- function (BrewerStr,CalStr,cDate,bdata,ICFType,ICFvalues,StationPara)
#             bdata<-Bdata
#
						nData = length(Bdata[[3]][[1]])
					} else
					{
						nData = 0
						SLdata = array()
					}

					# create and write output file 'SLjjjyy.bbb', if data are available; write
					# line to SLavg-file

					if (nData>0)
					{
						SLdata[[1]][[8]] = Veme
						names(SLdata[[1]][[8]]) = c("VersionMessage")
						WriteBrewerSL(SLdata, OutputPath, StationPara)
						if (SLavgPath!="0") df = WriteAvgSL(df, year, doy, nData, SLdata)
					} else
					{
						nData=0
						if (SLavgPath!="0") df = WriteAvgSL(df, year, doy, nData, SLdata)
					}

				}  # for day=day1:day2

			}  # for mon=mon1:mon2

			# close dayoz file "SLavg_yyyy_Calxxx.iii" of the year

			if (SLavgPath!="0") close(df)

		}  # for year=yearA:yearE

	}  # end if iniOk

}  # end if iniOk


#### end of program 'RecalcSL.R' ############################################
