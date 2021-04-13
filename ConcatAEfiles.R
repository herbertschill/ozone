#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Module Name         :  ConcatAEfiles.R                             #
#                                                                           #
#                                                                           #
#        Creation Date       :  12/01/2011                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 2.5.0    (2009)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ConcatAEfiles.R' reads a series of total ozone data files of      #
#        the 'AE' format for one or several years and concatenates the      #
#        data lines in one single file 'AEyyyy.iii' (yyyy=first year,       #
#        iii=instrument identifier). The header line of each file is        #
#        omitted.                                                           #
#                                                                           #
#        The parameters are read from the file 'ConcatAEfiles.ini'.         #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


# setwd("C:\\LKO\\Programs\\R")

source("ReadDobsTables.R")

statusOK <- 0

# Reads the path-file 'ConcatAEfiles.pth' with the adress of the
# ini-file 'ConcatAEfiles.ini' and reads the initialization parameters
# from the latter (Nparams designs the number of parameters to read from
# ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "ConcatAEfiles")
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Initialize values

	DobsStr   = iniParams[[2]][[2]]
	FirstYear = as.integer(iniParams[[2]][[3]])
	LastYear  = as.integer(iniParams[[2]][[4]])
	InPath    = iniParams[[2]][[5]]
	OutPath   = iniParams[[2]][[6]]
	CalInfo   = iniParams[[2]][[7]]

	# create output-filename and open file for write

	opathname = sprintf("%s%s%04i%s%s", OutPath, "AE", FirstYear, ".", DobsStr)
	df = file(opathname,open="w")

	# Create and write header line

	fd=as.character(Sys.time())  # "yyyy-mm-dd hh:mm:ss"
	outl = sprintf("%s%s%s%04i%s%04i", " Totoz LKO Arosa  Dobson ", DobsStr, " N  ", FirstYear, "-", LastYear)
	outl = sprintf("%s%s%s%s%s%s", outl, "  ", CalInfo, "  Time=UTC  Version: ", substr(fd,9,10), ".")
	outl = sprintf("%s%s%s%s%s%s%s", outl, substr(fd,6,7), ".", substr(fd,1,4), " ", substr(fd,12,19), "\n")
	cat (outl, file=df)

	# e.g. " Totoz LKO Arosa  Dobson 101 N  2009-2010    Version: 30.12.2010 16:41:03  Cal10b  Time=UTC"

	# proceed years

	for (year in FirstYear:LastYear)
	{
		InPath = ReplaceDobsYear (iniParams[[2]][[5]], iniParams[[2]][[2]], year)

			# test on leap year

			leap=0
			if  (year/4==floor(year/4))  leap=1
			if  (year/100==floor(year/100))  leap=0
			if  (year/400==floor(year/400))  leap=1

		# Treat months; set proper days in month 'mdays'

		for (mon in 1:12)
		{
			if (leap==1) 
				mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)
			else
				mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)

			# Treat days in month

			for (day in 1:mdays)
			{

				# create input-filename and open 'AEyyyymmdd.iii'-file if existing

				ipathname = sprintf("%s%s%04i%02i%02i%s%s", InPath, "AE", year, mon, day, ".", DobsStr)
				ex=file.exists(ipathname)
				if (ex)
				{
					cf = file(ipathname,open="r")

					# read header line; get number of measurements; proceed data lines 1..nmeas

					hline = readLines(cf, n=1)
					nmeas=as.integer(substr(hline,17,20))

					for (k in 1:nmeas)
					{
					hline = readLines(cf, n=1)
					outl = sprintf ("%s%s", hline, "\n")
					cat (outl, file=df)
					}
					close(cf)

					}  # end if 'ex'
			}  # for day=1..mdays
		}  # for mon=1..12
	}  # for year=FirstYear..LastYear

	close(df)

}  # end if 'iniOk'

# end of program 'ConcatAEfiles.R'