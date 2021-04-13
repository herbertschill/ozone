#############################################################################
#                                                                           #
#                                                                           #
#                            DATA FILE CONVERSION                           #
#                                                                           #
#                                                                           #
#        Module Name         :  ConcatUmFiles.R                             #
#                                                                           #
#                                                                           #
#        Creation Date       :  26.11.2015                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               09.01.2019                                  #
#                               05.12.2017                                  #
#                               24.01.2017                                  #
#                               30.07.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.1.2    (2014)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ConcatUmFiles.R' reads a series of halfday umkehr data files of   #
#        the 'Nucout', 'Nucpri', 'Nudecode' or 'Umkpre' format for one or   #
#        several years and concatenates the data lines in one single file   #
#        'OutFilename'. All lines are copied.                               #
#                                                                           #
#        The parameters are read from the file 'ConcatUmFiles.ini'.         #
#                                                                           #
#        (Standard) cloud parameters in output file are replaced by values  #
#        from cloud parameter file 'CloudPara_yyyy.iii' (year, Dobson),     #
#        if desired [only UMKPRE file]                                      #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 30.07.2016 by Herbert Schill:                                    #
#                                                                           #
#          - replace calls 'ReplaceDobsYear' by 'ReplaceCalDobsYear'        #
#                                                                           #
#                                                                           #
#        - 24.01.2017 by Herbert Schill:                                    #
#                                                                           #
#          - replace source 'ReadInstTables' by 'ReadDobsTables'            #
#                                                                           #
#          - replace calls 'ReplaceCalDobsYear' by 'ReplaceCalInstYear'     #
#                                                                           #
#                                                                           #
#        - 05.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#                                                                           #
#        - 09.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          - allow replacements of (standard) cloud parameters in output    #
#            file by values from cloud parameter file [only UMKPRE file]    #
#                                                                           #
#                                                                           #
#        - 23.01.2020 by Herbert Schill:                                    #
#                                                                           #
#          - import 'ReplaceCalInstYear' from 'DateZeit.R'                  #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          ConvertDate, LeapYear, ReplaceCalInstYear
#  ReadInstTables.R    ReadCloudParaTable, ReadIniFile


statusOK=0

# Open the ini-file 'ConcatUmFiles.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=10
iniParams = ReadIniFile(Nparams, "ConcatUmFiles.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Initialize values

	DobsStr   = iniParams[[2]][[2]]
	YearStr   = iniParams[[2]][[3]]
	DataType  = iniParams[[2]][[4]]
	InOutPath = iniParams[[2]][[5]]
	OutFile   = iniParams[[2]][[6]]
	header    = iniParams[[2]][[7]]
	SetCloudPar  = iniParams[[2]][[8]]
	CloudParPath = iniParams[[2]][[9]]
	CloudParFile = iniParams[[2]][[10]]
	Dobson    = as.integer(DobsStr)
	Year      = as.integer(YearStr)
	SYear     = substr(YearStr,3,4)

	# Set number of lines per Umkehr according to data type

	first=1
	na=1; ne=1
	if (DataType=="NUCPRI")   {na=2; ne=9}
	if (DataType=="NUDECODE") {na=1; ne=2}

	# create output-filename and open file for write

	InOutPath = ReplaceCalInstYear (InOutPath, DataType, DobsStr, Year)
	OutFile   = ReplaceCalInstYear (OutFile, DataType, DobsStr, Year)
	opathname = sprintf("%s%s", InOutPath, OutFile)
	df = file(opathname,open="w")

	# create filename and read cloud parameter file, if desired (only UMKPRE)

	if ((DataType=="UMKPRE") && (SetCloudPar==1))
	{
		CloudParPath = ReplaceCalInstYear (CloudParPath, DataType, DobsStr, Year)
		CloudParFile = ReplaceCalInstYear (CloudParFile, DataType, DobsStr, Year)
		CPdata = ReadCloudParaTable(CloudParFile ,CloudParPath)
		if (CPdata[1]==0) SetCloudPar=0
	}

	cDate = array()
	leap=LeapYear (Year)  # test on leap year
	cDate[3] = Year

	# Treat months; set proper days in month 'mdays'

	for (mon in 1:12)
	{
		mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
		if (leap==1 && mon==2) mdays=29
		cDate[2] = mon

		# Treat days resp halfdays in month

		for (day in 1:mdays)
		{
			cDate[1]=day
			cDate = ConvertDate (cDate, 1)
			jdn=cDate[4]

			for (i in 1:2)
			{
			# create input-filename and open 'hhhyymmdd.hd'-file (hhh=header) if existing

				hd=ifelse(i==1, ".am", ".pm")
				ipathname = sprintf("%s%s%s%02i%02i%s", InOutPath, header, SYear, mon, day, hd)
				ex=file.exists(ipathname)
				if (ex)  # proceed lines
				{
					cf = file(ipathname,open="r")
					if (na>1)
					{
						hline = readLines(cf, n=na-1)
					}
					for (n in na:ne)
					{
						hline = readLines(cf, n=1)
						if ((DataType=="UMKPRE") & (SetCloudPar==1))
						{
							um2  = CPdata[[i+1]][[jdn]]
							if (um2<6)  # only curves with cloud parameter [1..5] are concatenated
							{
								um1  = (substr(hline,1,7))
								um3  = (substr(hline,9,80))
								outl = sprintf ("%s%1i%s%s", um1, um2, um3, "\n")
								cat (outl, file=df)
							}
						} else
						{
							outl = sprintf ("%s%s", hline, "\n")
							cat (outl, file=df)
						}
					}  # for n=na..ne

					# special treatment for NUCPRI resp. NUDECODE format

					if (DataType=="NUCPRI")
					{
						outl = sprintf ("%s", "  \n")
						cat (outl, file=df)
					}
					if (DataType=="NUDECODE") na=2

					close(cf)
				}  # end if 'ex'
			}  # for i=1..2
		}  # for day=1..mdays
	}  # for mon=1..12

	close(df)

}  # end if 'iniOk'

# end of program 'ConcatUmFiles.R'