#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  DobsonFlagOz.R                              #
#                                                                           #
#                                                                           #
#        Creation Date       :  19.03.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               27.05.2021                                  #
#                               15.06.2020                                  #
#                               20.03.2020                                  #
#                               24.02.2018                                  #
#                               22.02.2018                                  #
#                               19.12.2017                                  #
#                               21.10.2017                                  #
#                               14.08.2017                                  #
#                               27.03.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'DobsonFlagOz.R' flags the total ozone measurements of a Dobson,   #
#        reading the data from the 'AEyyyymmdd.iii' files, results are      #
#        written to an equal named file located in the same or annother     #
#        directory.                                                         #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'DobsonFlagOz.ini'.                                     #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#          - change calls from 'BDcomparSub.R' to 'BrewerDobsonSub.R'       #
#                                                                           #
#          - remove bug in 'multCalStr' treatment                           #
#                                                                           #
#                                                                           #
#        - 14.08.2017 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'Veme'-string (include location info)            #
#                                                                           #
#                                                                           #
#        - 21.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat'                             #
#                                                                           #
#                                                                           #
#        - 19.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for import of 'ConvertDate' from 'DateZeit.R'            #
#                                                                           #
#          - call of 'ReduceAE' adapted: measurements with an airmass       #
#            bigger than maxAirmass resp. a sun intensity lower than        #
#            minSun are also filtered                                       #
#                                                                           #
#                                                                           #
#        - 22.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - read separate names of auto and manu flaglist files from       #
#            modified ini-file                                              #
#                                                                           #
#                                                                           #
#        - 24.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 20.03.2020 by Herbert Schill:                                    #
#                                                                           #
#          - modify program for reading/writing of header line of the       #
#            AE-data file including name and values of the absorption       #
#            coefficients set                                               #
#                                                                           #
#                                                                           #
#        - 15.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          - use 'strsplit' to evaluate proper InstrStr and CalStr          #
#                                                                           #
#          - do some other simplifications                                  #
#                                                                           #
#                                                                           #
#        - 27.05.2021 by Herbert Schill:                                    #
#                                                                           #
#          - dataF1: remove bug in 'ReduceAE', when no valid measurement    #
#            of the day and wl is left                                      #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("BrewerDobsonSub.R")
source("ReadInstTables.R")
source("TreatDobsonData.R")
source("CalcDobson.R")

#  from:               import:
#
#  BrewerDobsonSub.R   CalcFlagDiffs, WriteFlaglist
#
#  DateZeit.R          ConvertDate
#
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear
#
#  TreatDobsonData.R   ManuFlagAE, ReadDobsonAE, ReduceAE, SetFlagsAE, 
#                      WriteDobsonAE

statusOK <- 0

# Open the ini-file 'DobsonFlagOz.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=25
iniParams = ReadIniFile(Nparams, "DobsonFlagOz.ini", 0)
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

	# Read filter constants and number of instruments to be proceed

	multInstStr = strsplit(iniPar[7], ",")
	multCalStr = strsplit(iniPar[8], ",")
	nnInstr = length(multInstStr[[1]])
	nWave = as.integer(iniPar[15])
	WaveSeq = iniPar[16]
	FlagSeq = iniPar[17]
	DiffRef = iniPar[18]
	RDiffDayozStr = iniPar[19]
	RDiffDRvalStr = iniPar[20]
	RDiffPolyNStr = iniPar[21]
	RDiffConseStr = iniPar[22]
	DeltaTconsec = as.double(iniPar[23])/60
	minSun = as.integer(iniPar[24])
	maxAirmass = as.double(iniPar[25])

	DNvect = array()
	iniPar2 = array()
	RDiffDayozLim = array ()
	RDiffDRvalLim = array ()
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
		occ=0
		for (i in 1:nchar(FlagSeq)-3) if (substr(FlagSeq,i,i+3)=="poly") occ=i
		if (occ>1) PolyOrder=as.integer(substr(FlagSeq,occ+4,occ+4))
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

		# adapt parameters for 'WriteDobsonAE'

		iniPar2[2]=InstStr
		iniPar2[3]=CalStr
		iniPar2[12]="AE"
		iniPar2[14]=iniPar[10]
		Veme = sprintf("%s%s%s", Veme, "  ", CalStr)

		# Treat desired period

		Calendar = array()
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

			if (year/4==floor(year/4)) {leap=1} else {leap=0}
			mon1=1
			mon2=12
			if (year==yearA) mon1=monA
			if (year==yearE) mon2=monE

			# status of 'FlagingType':  0 reset all flags
			#                           1 set automatic flags and write values to flaglist file
			#                           2 set/reset manual flags/unflags accordingly to flaglist file

			# read flaglist input file, if desired; open flaglist output file for writing otherwise

			if (FlagingType!=0)
			{
				if (FlagingType==1) {FlaglistFile = ReplaceCalInstYear (iniPar[11], CalStr, InstStr, year)} else
				                    {FlaglistFile = ReplaceCalInstYear (iniPar[12], CalStr, InstStr, year)}
				FlaglistPath = ReplaceCalInstYear (iniPar[13], CalStr, InstStr, year)
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
							if (length(f1ix)>0) f0wl=as.integer(substr(flagLst[5,],1,1))
					} else
					{
						infoFile = sprintf("%s%s%s","\n\n  File  ",FlaglistFile, "  not found !\n\n")
						cat(infoFile, file="")
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

					# Read header data and measurement data of the day from file "AEyyyymmdd.iii"
					# if measurements are available for the day, the complete dataset of a Dobson 
					# of one day is reduced to the values needed for automated flagging

					dataAE = ReadDobsonAE (day, mon, year, InstStr, CalStr, iniPar[9])
					NMdata = dataAE$HeaderData$NumberMeas

					if (NMdata>0)  # measurements exist of this day
					{
						for (w in 1:nWave)
						{
#							w=1
							wl=substr(WaveSeq,(w-1)*2+1,(w-1)*2+1)
							if (FlagingType==2)
							{
								if (wl=="C")
									wx=1 else if (wl=="D")
									wx=2 else if (wl=="A")
									wx=3

								if (length(f2ix)>0)
								{
									f3ix  <- f2ix[f0wl[f2ix]==w]
									if (length(f3ix)>0) { dataAE = ManuFlagAE (wl, dataAE, f3ix, flagLst) }
								}
							} else
							{
								dataF1 = ReduceAE (wl, FlagingType, AutoflagDRval, RDiffDRvalLim, 
							                     maxAirmass, minSun, dataAE)
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
								}  # end if NMeasF1>0

								# Set flags in dataAE acordingly to the flagging process

								if ((dataF1$Measurements$NMeasOut<NMdata) | (FlagingType==0))
								{
									dataAEF = SetFlagsAE (wl, dataAE, dataF1, FlagingType, maxAirmass, minSun)
									if (FlagingType==1) fl = WriteFlaglist (day, mon, year, jdn, 1, wl, dataAEF, fl)
									dataAE = dataAEF$dataAE
								}  # end if NMeasF1<NMdata or FlagingType=0
							}  # end if FlagingType!=2
						}  # for w=1:nWave

						# Update header values and write data in lists

						pv = strsplit(dataAE[[1]][[6]], " ")
						for (i in 2:4)  DNvect[i-1] = as.double(pv[[1]][[i]])
						RNTname = pv[[1]][[6]]
						if (length(pv[[1]])>10) staName = pv[[1]][[11]]
						dataAE[[1]][[6]] = sprintf ("%s  %s ", Veme, staName)
						if  (length(pv[[1]])==16)
						{
							for (i in 12:16)  dataAE[[1]][[6]] = sprintf ("%s %s", dataAE[[1]][[6]], pv[[1]][[i]])
						}

						HeaderValues <- dataAE[[1]]
						names(HeaderValues) = c("Dobson-Id", "TypeMess", "DateMess", "NumberMess", "UTC-Corr", "VersionMess")

						Measures = list(NMdata, dataAE[[2]][[2]], dataAE[[2]][[3]], dataAE[[2]][[4]], dataAE[[2]][[6]], dataAE[[2]][[7]], dataAE[[2]][[8]], 
														dataAE[[2]][[11]], dataAE[[2]][[9]], dataAE[[2]][[10]], dataAE[[2]][[12]], dataAE[[2]][[12]])
						names(Measures) = c("NumberMess", "Sun", "InstrTemp", "TimeStr", "R-Values", "StaDevs", "Flags", 
																"N'-Values", "Airmass","OzCalc", "Bemporad", "SZA")

						DayStat = list(0, 0, 0, 0, 0)
						names(DayStat) = c("OzonDay", "NumberDay", "StaDevDay", "OzonHD", "NumbHD")
						cdata <- list("Header data"=HeaderValues,"Measurements"=Measures, "DayStatistics"=DayStat)

						# write flagged total ozone to a new file "AEyyyymmdd.iii"

						statusOK = WriteDobsonAE (iniPar2, cdata, RNTname, DNvect)

					}  # end if NMdata>0 (measurements exist of this day)
				}  # for day=day1:day2
			}  # for mon=mon1:mon2
			if (FlagingType!=0) close(fl)
		}  # for year=yearA:yearE
	}  # for inn=0:nnInstr-1 (instrument loop)

}  # end if iniOk

#### end of program 'DobsonFlagOz.R' ########################################
