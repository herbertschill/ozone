#############################################################################
#                                                                           #
#                                                                           #
#                            METEOROLOGIGAL OBSERVATIONS                    #
#                                                                           #
#                                                                           #
#        Module Name         :  GetOzoneFiles.R                             #
#                                                                           #
#                                                                           #
#        Creation Date       :  31.01.2019                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD-WRC                   #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'GetOzoneFiles.R' copies a number of files from the source-        #
#        directories to the destination-directories.                        #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\LKO\\Programs\\R")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          ConvertDate, LeapYear, ReplaceDayMonthYear
#
#  ReadInstTables.R    ReadIniFile


# Open the ini-file 'GetOzoneFiles.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "GetOzoneFiles.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector; treat parameters

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:Nparams)  iniPar[n] = iniParams[[2]][[n]]

	lstFilename = iniPar[4]
	firstL = as.integer(iniPar[5])
	lastL  = as.integer(iniPar[6])
	logFilename = iniPar[7]
	nFiles = lastL-firstL+1

	dayA  = as.integer(substr(iniPar[2],1,2))
	monA  = as.integer(substr(iniPar[2],4,5))
	yearA = as.integer(substr(iniPar[2],7,10))
	dayE  = as.integer(substr(iniPar[3],1,2))
	monE  = as.integer(substr(iniPar[3],4,5))
	yearE = as.integer(substr(iniPar[3],7,10))

	# Open list-file and read information

	if (file.exists(lstFilename))
	{
		lf = file(lstFilename,open="r")
		hline = scan(lf,what="character",nlines=firstL-1,quiet=TRUE)
		hline = scan(lf,what="character",nlines=nFiles,sep="",quiet=TRUE)
		close(lf)

		# Open log-file, if desired

		if (logFilename!="0") lg = file(logFilename,open="a")

		# Treat desired period; set proper days in month 'mdays'

		for (year in yearA:yearE)
		{
			# year=yearA
			leap = LeapYear(year)
			if (leap)
				{jdne=366} else
				{jdne=365}
			yr=year-2000
			mon1=1
			mon2=12
			if (year==yearA) mon1=monA
			if (year==yearE) mon2=monE

			# Treat month; set proper days in month 'mdays'

			for (mon in mon1:mon2)
			{
				# mon=mon1
				if (leap)
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
					inDate = array()
					inDate[1]= day
					inDate[2]= mon
					inDate[3]= year
					inDate = ConvertDate(inDate, 1)
					jdn = as.integer(inDate[4])
					otl = sprintf("  %02d.%02d.%02d  %03d ", day, mon, yr, jdn)

					# create source- and destination-filenames and proceed copy

					for (k in 1:nFiles)
					{
						inFilename0 = hline[(k-1)*3+1]
						sourcePath0 = hline[(k-1)*3+2]
						sourcePath = ReplaceDayMonthYear (sourcePath0, day, mon, year)

						# special treatment for meteo-files of Arosa and Tscghuggen: read from
						# ftp-server, and for VQBA28-files: take date of next day

						if (grepl("meteo",sourcePath,ignore.case = TRUE))
						{
							dataType="meteo"
							inpathname = sprintf("%s%s%s", sourcePath, "/", inFilename)
						} else
						{
							if (grepl("sma_002",sourcePath,ignore.case = TRUE))
							{
								dataType="VQBA28"
								inDate1 = array()
								inDate1 = inDate
								jdn1=jdn+1
								if (jdn1>jdne)
								{
									jdn1=1
									year1=year+1
								}else
								{ year1=year }
								inDate[4]=jdn1
								inDate[3]= year1
								inDate1 = ConvertDate(inDate1, -1)
								inFilename = ReplaceDayMonthYear (inFilename0, inDate1[1], inDate1[2], year1)
							} else
							{
								inFilename = ReplaceDayMonthYear (inFilename0, day, mon, year)
							}  # end if grepl:"sma_002"
							dataType="ozone"
							inpathname = sprintf("%s%s%s", sourcePath, "\\", inFilename)
						}  # end if grepl:"meteo"

						if ((file.exists(inpathname)) || (dataType=="meteo"))
						{
							destinPath0 = hline[(k-1)*3+3]
							destinPath = ReplaceDayMonthYear (destinPath0, day, mon, year)
							outpathname = sprintf("%s%s%s", destinPath, "\\", inFilename)
							if (dataType=="meteo")
							{
								download.file(inpathname, outpathname)
								if (file.exists(outpathname)) {cstat=TRUE} else {cstat=FALSE}
							} else
							{
								cstat = file.copy(inpathname, outpathname, overwrite=TRUE, copy.date=TRUE)
							}  # end if dataType:"meteo"

							if (cstat)
							{
								otl = sprintf("%s%s", otl, "   ok")
								infoFile = sprintf("%s%s%s", "  File ", inFilename, " copied\n")
								cat(infoFile, file="")
							} else
							{
								otl = sprintf("%s%s", otl, "    0")
								infoFile = sprintf("%s%s%s", "  Filecopy of ", inFilename, " failed\n")
								cat(infoFile, file="")
							}  # end if cstat
						} else
						{
							otl = sprintf("%s%s", otl, "    0")
							infoFile = sprintf("%s%s%s", "  File ", inpathname, " not found\n")
							cat(infoFile, file="")
						}  # end if file.exists(inpathname)

					}  # next k
					otl = sprintf("%s%s", otl, "\n")
					cat(otl, file=lg)

				}  # next day
			}  # next mon
		}  # next year

	close(lg)
	} else
	{
		infoFile = sprintf("%s%s%s", "  Lisr-file ", lstFilename, " not found\n")
		cat(infoFile, file="")
	}  # end if file.exists(lstFilename)

}  # end if iniOk

# end of program 'GetOzoneFiles.R'