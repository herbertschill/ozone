#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  BrewerFlagOz.R                              #
#                                                                           #
#                                                                           #
#        Creation Date       :  17.03.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               15.06.2020                                  #
#                               16.05.2019                                  #
#                               24.02.2018                                  #
#                               27.03.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.1.2    (2015)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'BrewerFlagOz.R' flags the total ozone measurements of one or more #
#        Brewers, reading the data from the 'DSjjjyy.iii' file, results are #
#        written to an equal named file located in the same or annother     #
#        directory.                                                         #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'BrewerFlagOz.ini'.                                     #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#          - change calls from 'BDcomparSub.R' to 'BrewerDobsonSub.R'       #
#                                                                           #
#                                                                           #
#        - 24.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 16.05.2019 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat'                             #
#                                                                           #
#                                                                           #
#        - 15.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          - use 'strsplit' to evaluate proper InstrStr and CalStr          #
#                                                                           #
#          - do some other simplifications                                  #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("BrewerDobsonSub.R")
source("DateZeit.R")
source("ReadInstTables.R")
source("TreatBrewerData.R")

#  from:               import:
#
#  BrewerDobsonSub.R   CalcFlagDiffs, WriteFlaglist
#
#  DateZeit.R          ConvertDate
#
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear
#
#  TreatBrewerData.R   ManuFlagDS, ReadBrewerDS, ReduceDS, SetFlagsDS, 
#                      WriteBrewerDS

statusOK <- 0

# Open the ini-file 'BrewerFlagOz.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=23
iniParams = ReadIniFile(Nparams, "BrewerFlagOz.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Check flagging type:
	#
	#  0 or reset  reset all flags
	#  1 or auto   set automatic flags, values are written to 
	#              the flaglist file (output)
	#  2 or manu   set/reset manual flags/unflags accordingly to 
	#              the flaglist file (input)

	if (iniPar[5]=="reset")
	FlagingType=0 else if (substr(iniPar[5],1,4)=="auto")
	FlagingType=1 else if (substr(iniPar[5],1,4)=="manu") 
	FlagingType=2 else
	FlagingType = as.integer(iniPar[5])

	# Read instruments to be proceed and filter constants 

	multInstStr=strsplit(iniPar[7], ",")
	multCalStr=strsplit(iniPar[8], ",")
	nnInstr = length(multInstStr[[1]])
	WaveSeq = iniPar[14]
	if (WaveSeq=="O3") {w=1} else {w=2}
	RDiffDRvalLim = array ()

	if (FlagingType==1)
	{
		FlagSeq = iniPar[15]
		DiffRef = iniPar[16]
		RDiffDayozStr = iniPar[17]
		RDiffDRvalStr = iniPar[18]
		RDiffPolyNStr = iniPar[19]
		RDiffConseStr = iniPar[20]
		DeltaTconsec = as.double(iniPar[21])/60
		minSun = as.integer(iniPar[22])
		maxAirmass = as.double(iniPar[23])

		RDiffDayozLim = array ()
		RDiffPolyNLim = array ()
		RDiffConseLim = array ()
		k=strsplit(RDiffDayozStr, ",")
		for (i in 1:length(k[[1]])) RDiffDayozLim[i]=as.double(k[[1]][i])
		k=strsplit(RDiffDRvalStr, ",")
		for (i in 1:length(k[[1]])) RDiffDRvalLim[i]=as.double(k[[1]][i])
		k=strsplit(RDiffPolyNStr, ",")
		for (i in 1:length(k[[1]])) RDiffPolyNLim[i]=as.double(k[[1]][i])
		k=strsplit(RDiffConseStr, ",")
		for (i in 1:length(k[[1]])) RDiffConseLim[i]=as.double(k[[1]][i])

		if (length(grep("dayoz", FlagSeq, ignore.case=TRUE))) AutoflagDayoz=TRUE else AutoflagDayoz=FALSE
		if (length(grep("drval", FlagSeq, ignore.case=TRUE))) AutoflagDRval=TRUE else AutoflagDRval=FALSE
		if (length(grep("consec", FlagSeq, ignore.case=TRUE))) AutoflagConsec=TRUE else AutoflagConsec=FALSE
		if (length(grep("poly", FlagSeq, ignore.case=TRUE))) AutoflagPoly=TRUE else AutoflagPoly=FALSE
		if (AutoflagPoly)
		{
			tres = regexpr("poly", FlagSeq, ignore.case=TRUE)[1]
			PolyOrder = as.integer(substr(FlagSeq,tres+4,tres+4))
		}
	} else  # end if FlagingType=1
	{
		AutoflagDRval=FALSE
		RDiffDRvalLim=0
	}

	# loop for instruments

	for (inn in 1:nnInstr)
	{
#		inn=1

		# get proper instrument identifier and instrument calibration

		InstStr = multInstStr[[1]][[inn]]
		CalStr = multCalStr[[1]][[inn]]
		infoFile = sprintf("%s%s%s%s%s","\n\n  Loop: ",inn, "  Instrument to process: ",InstStr, "\n\n")
		cat(infoFile, file="")

		# Create timestamp "Version: dd.mm.yyyy hh:mm:ss"

		fd = Sys.time()  # "yyyy-mm-dd hh:mm:ss"
		Veme = sprintf("%s%s%s%s", "Version: ", substr(fd,9,10), ".", substr(fd,6,7))
		Veme = sprintf("%s%s%s%s", Veme, ".", substr(fd,1,4), substr(fd,11,19))

		# Treat desired period

		# Calendar = array()
		dayA  = as.integer(substr(iniPar[3],1,2))
		monA  = as.integer(substr(iniPar[3],4,5))
		yearA = as.integer(substr(iniPar[3],7,10))
		dayE  = as.integer(substr(iniPar[4],1,2))
		monE  = as.integer(substr(iniPar[4],4,5))
		yearE = as.integer(substr(iniPar[4],7,10))
		first = 1

		####  Time Loop  ########################################################

		# Treat year; set proper month range

		for (year in yearA:yearE)
		{
#		year = yearA

			leap = LeapYear(year)
			mon1=1
			mon2=12
			if (year==yearA) mon1=monA
			if (year==yearE) mon2=monE

			# status of 'FlagingType':  0 reset all flags
			#                           1 set automatic flags and write values to flaglist file
			#                           2 set/reset manual flags/unflags accordingly to flaglist file

			# read flaglist input file, if desired; open flaglistfile for writing otherwise

			if (FlagingType!=0)
			{
				FlaglistFile = ReplaceCalInstYear (iniPar[11], CalStr, InstStr, year)
				FlaglistPath = ReplaceCalInstYear (iniPar[12], CalStr, InstStr, year)
				FlaglistName = sprintf("%s%s%s", FlaglistPath, "\\", FlaglistFile)

				if (FlagingType==2)  # FlagingType=2: open and read flaglistfile
				{
					if (file.exists(FlaglistName))
					{
							fl = file(FlaglistName,open="r")
							fline = scan(fl,what="character",nlines=3,quiet=TRUE)     # header
							flagLst = scan(fl,what="character",nlines=-1,quiet=TRUE)  # data lines
							dim(flagLst) = c(15,(length(flagLst)/15))
							f1ix =grep (InstStr,flagLst[4,],ignore.case=FALSE)  # indices for instrument
					} else
					{
						infoFile = sprintf("%s%s%s","  File  ",FlaglistFile, "  not found")
						print(infoFile)
					}
				} else  # FlagingType=1: open flaglistfile for writing (append)
				{
					fl = file(FlaglistName,open="a")
				}
			}  # end if FlagingType!=0

			 # Treat month; set proper days in month 'mdays'

			for (mon in mon1:mon2)
			{
#			mon=mon1
			
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
#					day=day1
					jdn=0
					cDate = array()
					cDate = list(day, mon, year, jdn, leap)
					cDate = ConvertDate(cDate, 1)
					jdn = cDate[[4]][[1]]
					jdnStr = sprintf("%03d",jdn)
					if (FlagingType==2) f2ix  <- f1ix[as.integer(flagLst[3,f1ix])==jdn]  # indices for jdn

					# Read header data and measurement data of the day from file "DSjjjyy.iii"
					# if measurements are available for the day, the complete dataset of one
					# day is reduced to the values needed for automated flagging

					#jdn;day;mon;year;InstStr;CalStr;iniPar[9]
					dataDS = ReadBrewerDS (jdn, day, mon, year, InstStr, CalStr, iniPar[9])
					NMdata = dataDS[[1]][[1]]

					if (NMdata>0)  # measurements exist of this day
					{
						if (FlagingType==2)
						{
							if (length(f2ix)>0) { dataDS = ManuFlagDS (dataDS, f2ix, flagLst) }
						} else
						{ 
							dataF1 = ReduceDS (WaveSeq, FlagingType, AutoflagDRval, RDiffDRvalLim, dataDS)
						}

						if (FlagingType!=2)
						{
							if (dataF1$Measurements$NMeasOut>0)
							{
								if (FlagingType==1)
								{
									if (AutoflagPoly)
									{
										dataF1 = CalcFlagDiffs (dataF1, RDiffPolyNLim[w], PolyOrder)
									}
									if ((AutoflagDayoz) &  (dataF1$Measurements$NMeasOut>0))
									{
										dataF1 = CalcFlagDiffs (dataF1, RDiffDayozStr[w], 0)
									}
									if ((AutoflagConsec) & (dataF1$Measurements$NMeasOut>2))
									{
										dataF1 = CalcFlagDiffs (dataF1, RDiffConseLim[w], -1)
									}
								}  # end if FlagingType=1

								# Set flags in dataAE resp. dataDS acordingly to the flagging process

								if ((dataF1$Measurements$NMeasOut<NMdata) | (FlagingType==0))
								{
									dataDSF = SetFlagsDS (WaveSeq, dataDS, dataF1, FlagingType, maxAirmass, minSun)
									if (FlagingType==1) fl = WriteFlaglist (day, mon, year, jdn, 0, wl, dataDSF, fl)
									dataDS = dataDSF$dataDS
								}
							} else  # if NMeasF1=0 (flag all measurements in original datset)
							{
									dataDSF = SetFlagsDS (WaveSeq, dataDS, dataF1, FlagingType, maxAirmass, minSun)
									if (FlagingType==1) fl = WriteFlaglist (day, mon, year, jdn, 0, wl, dataDSF, fl)
									dataDS = dataDSF$dataDS
							}  # end if NMeasF1:0
						}  # end if FlagingType!=2

						# Update header values and write data in lists

						dataDS[[1]][[7]] = CalStr
						dataDS[[1]][[15]] = Veme
						names(dataDS[[1]][[15]]) = c("VersionMessage")
						WriteBrewerDS (dataDS, iniPar[10], 0)

					}  # end if NMdata>0 (measurements exist of this day)
				}  # for day=day1:day2
			}  # for mon=mon1:mon2
			if (FlagingType!=0) close(fl)
		}  # for year=yearA:yearE
	}  # for inn=0:nnInstr-1 (instrument loop)

}  # end if iniOk

#### end of program 'BrewerFlagOz.R' ########################################
