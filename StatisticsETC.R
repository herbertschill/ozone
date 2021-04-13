#############################################################################
#                                                                           #
#                                                                           #
#                            BREWER ETC-STATISTICS                          #
#                                                                           #
#                                                                           #
#        Program Name        :  StatisticsETC.R                             #
#                                                                           #
#                                                                           #
#        Creation Date       :  27.03.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               30.07.2018                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.1.2    (2015)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'StatisticsETC.R' reads from Brewer DS-files over a given period   #
#        the ETC- and the Absorption-Constants, and the daily ETD-values,   #
#        if available, and writes them to the output file.                  #
#                                                                           #
#        All necessary initial information is read from the ASCII-file      #
#        'StatisticsETC.ini'.                                               #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 30.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper Sys.setenv and sources (add "DateZeit.R")           #
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
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear 
#                      
#  TreatBrewerData.R   ReadBrewerDS


statusOK <- 0

# Open the ini-file 'StatisticsETC.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=10
iniParams = ReadIniFile(Nparams, "StatisticsETC.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Get desired period

	dayA  = as.integer(substr(iniPar[4],1,2))
	monA  = as.integer(substr(iniPar[4],4,5))
	yearA = as.integer(substr(iniPar[4],7,10))
	dayE  = as.integer(substr(iniPar[5],1,2))
	monE  = as.integer(substr(iniPar[5],4,5))
	yearE = as.integer(substr(iniPar[5],7,10))

	# Get ini-info and create proper filenames and pathes

	BrewerStr   = iniPar[2]
	BrewerCal   = iniPar[3]
	InputPath0  = iniPar[6]
	OutputFilename = iniPar[7]
	OutputPath  = iniPar[8]
	IcfFilename = iniPar[9]
	RecalcETD   = iniPar[10]

	len=nchar(InputPath0)
	if (substr(InputPath0,len,len+1)!="\\")  InputPath0=sprintf("%s%s",InputPath0, "\\")
	len=nchar(OutputPath)
	if (substr(OutputPath,len,len+1)!="\\")  OutputPath=sprintf("%s%s",OutputPath, "\\")

	OutputFilename = ReplaceCalInstYear (OutputFilename, BrewerCal, BrewerStr, 9999)
	OutputPath = ReplaceCalInstYear (OutputPath, BrewerCal, BrewerStr, yearA)
	IcfFilename = ReplaceCalInstYear (IcfFilename, BrewerCal, BrewerStr, 9999)

	# Opens the ICF-file and reads the instrument coefficients

	TX = array()
	if (file.exists(IcfFilename))
	{
		exICF=1
		Nparams=52
		inifile = file(IcfFilename,open="r")
		icfPar = array()
		for (i in 1:Nparams)
		{
			line = scan(inifile,what="character",nlines=1,quiet=TRUE)
			icfPar[i]=line[1]
		}
		close(inifile)

		# calculate temperature coeffs

		for (i in 1:6) TX[i+2] = as.double(icfPar[i])
		TX[1] = TX[3]-TX[6] -3.2*(TX[6]-TX[7])
		TX[2] = TX[4]-TX[6] -0.5*(TX[5]-TX[6]) -1.7*(TX[6]-TX[7])
	} else  # ICF-file file doesen't exist
	{
		exICF=0
		infoFile = sprintf("%s%s%s","  File  ",IcfFilename, "  not found")
		print(infoFile)
		for (i in 1:8) TX[i]=0
	}  # end if file.exists(IcfFilename)

	# replace in OutputFilename "yyy1-yyy2" by "yearA-yearE", if occuring; open file for write

	if (yearA==yearE)
	{
		subnew = sprintf("%s",substr(iniPar[4],7,10))
	} else
	{
		subnew = sprintf("%s%s%s",substr(iniPar[4],7,10), "-", substr(iniPar[5],7,10))
	}
	subold = "yyy1-yyy2"
	occ=0
	len=nchar(OutputFilename)
	len0=nchar(subold)
	for (i in 1:(len-len0+1)) if (substr(OutputFilename,i,i+len0-1)==subold) occ=i
	if  (occ>=1)  OutputFilename  = sprintf("%s%s%s", substr(OutputFilename,1,occ-1), subnew, substr(OutputFilename,occ+len0,len))

	OutputPathFile = sprintf("%s%s",OutputPath, OutputFilename)
	otf = file(OutputPathFile,open="w")

	# Create header version message "dd.mm.yyyy  hh:mm"

	fd=as.character(Sys.time())  # "yyyy-mm-dd hh:mm:ss"
	CalcInfo = sprintf("%s%s%s%s%s", substr(fd,9,10), ".", substr(fd,6,7), ".", substr(fd,1,4))
	CalcInfo = sprintf("%s%s%s", CalcInfo, "  ", substr(fd,12,16))

	# Write header

	outl = sprintf("Brewer instrument constants for Brewer %s   %s",BrewerStr, BrewerCal)
	outl = sprintf ("%s  %s;;;;;;;;Version:;%s\n from  %s\n", outl, subnew, CalcInfo, IcfFilename)
	cat (outl, file=otf)
	outl = sprintf("TempCoeffs; %9.5f", TX[1])
	for (i in 2:8) outl = sprintf("%s;%9.5f", outl, TX[i])
	cat (outl, file=otf)
	outl = sprintf("\n    ETC O3;   ETC SO2;   Absorption Coeff. (O3;OS;SO2)\n")
	cat (outl, file=otf)
	if (exICF==1)
	{
		IC = array()
		for (i in 1:5) IC[i] = as.double(icfPar[i+6])
		outl = sprintf("%10.3f;%10.3f;%10.4f;%10.4f;%10.4f\n", IC[4], IC[5], IC[1], IC[3], IC[2])
	} else
	{
		outl = sprintf("%10.8f;%10.8f;%10.8f;%s%10.8f;%10.8f\n", 0,0,0,0,0,0)
	}
	cat (outl, file=otf)
	outl = sprintf(" dd.mm.yyyy;    Ozon;   SO2;    My; Anz; ETC O3;ETC SO2;")
	outl = sprintf ("%s ETD O3;ETD SO2; AbsC O3; AbsC OS\n", outl)
	cat (outl, file=otf)

	# Treat year; set proper month range

	for (year in yearA:yearE)
	{
#		year=yearA
		InputPath = ReplaceCalInstYear (InputPath0, BrewerCal, BrewerStr, year)
		if (year/4==floor(year/4)) {leap=1} else {leap=0}
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

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
#				day=day1
				outl = sprintf(" %02d.%02d.%04d",day,mon,year)

				dataDS = ReadBrewerDS(0, day, mon, year, BrewerStr, BrewerCal, InputPath)
				if (dataDS[[1]][[1]]>0)
				{
					EtcOz3 = dataDS[[1]][[8]]
					EtcSO2 = dataDS[[1]][[9]]
					AbsOz3 = dataDS[[1]][[10]]
					AbsSO2 = dataDS[[1]][[11]]
					NbOzVal = dataDS[[3]][[1]]
					AvgAirm = dataDS[[3]][[4]]
					AvgOz3 = dataDS[[3]][[5]]
					AvgSO2 = dataDS[[3]][[6]]
					EtdOz3 = dataDS[[3]][[9]]
					EtdSO2 = dataDS[[3]][[10]]

					if (RecalcETD==1)
					{
					}
					if (NbOzVal>0)
					{
						outl = sprintf ("%s;%8.1f;%6.1f;%6.3f;%4i", outl, AvgOz3, AvgSO2, AvgAirm, NbOzVal)
						outl = sprintf ("%s;%7.1f;%7.1f;%7.1f;%7.1f", outl, EtcOz3, EtcSO2, EtdOz3, EtdSO2)
						outl = sprintf ("%s;%8.4f;%8.4f", outl, AbsOz3, AbsSO2)
					}
				}  # end if dataDS[[1]][[1]]>0

				outl = sprintf ("%s\n", outl)
				cat (outl, file=otf)

			}  # for day=day1:day2
		}  # for mon=mon1:mon2
	}  # for year=yearA:yearE

	close(otf)  # close output-datafile

}  # end if iniOk

# end of program 'StatisticsETC.R'
