#############################################################################
#                                                                           #
#                                                                           #
#                            BREWER ETC-STATISTICS                          #
#                                                                           #
#                                                                           #
#        Program Name        :  FindKeyword.R                               #
#                                                                           #
#                                                                           #
#        Creation Date       :  13.07.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               19.08.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.1.2    (2015)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'FindKeyword.R' searchs in date-named files (e.g. 'Bjjjyy.iii',    #
#        'Dyyyymmdd.iii') over a given period the desired keyword and       #
#        lists them to the output file.                                     #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'FindKeyword.ini'.                                      #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 19.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - rename source 'ReadDobsTables.R' by 'ReadInstTables.R'         #
#                                                                           #
#          - rename calls 'ReplaceCalDobsYear' by 'ReplaceCalInstYear'      #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\LKO\\Programs\\R")

source("ReadInstTables.R")
source("DateZeit.R")

#  from:               import:
#
#  ReadInstTables.R    ReadIniFile, ReplaceCalInstYear 
#                      
#  DateZeit.R          ConvertDate



statusOK <- 0

# Reads the path-file 'FindKeyword.pth' with the adress of the ini-file
# 'FindKeyword.ini' and reads the initialization parameters from the latter
# (Nparams designs the number of parameters to read from ini-file)

Nparams=9
iniParams = ReadIniFile(Nparams, "FindKeyword")
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Get desired period

	dayA  = as.integer(substr(iniPar[5],1,2))
	monA  = as.integer(substr(iniPar[5],4,5))
	yearA = as.integer(substr(iniPar[5],7,10))
	dayE  = as.integer(substr(iniPar[6],1,2))
	monE  = as.integer(substr(iniPar[6],4,5))
	yearE = as.integer(substr(iniPar[6],7,10))

	# Get ini-info and create proper filenames and pathes

	FileType   = iniPar[2]
	InstrStr  = iniPar[3]
	Keyword    = iniPar[4]
	InputPath0 = iniPar[7]
	OutputPath = iniPar[9]
	OutputFilename = iniPar[8]
	InstrType=substr(InstrStr,1,1)
	InstrStr=substr(InstrStr,2,4)
	InputPath0 = ReplaceCalInstYear (InputPath0, "", InstrStr, yearA)
	OutputPath = ReplaceCalInstYear (OutputPath, "", InstrStr, yearA)

	len=nchar(InputPath0)
	if (substr(InputPath0,len,len+1)!="\\")  InputPath0=sprintf("%s%s",InputPath0, "\\")
	len=nchar(OutputPath)
	if (substr(OutputPath,len,len+1)!="\\")  OutputPath=sprintf("%s%s",OutputPath, "\\")

	OutputPathFile = sprintf("%s%s",OutputPath, OutputFilename)
	otf = file(OutputPathFile,open="w")
	infoFile = sprintf("%s%s%s%s%s%s%s%s\n\n"," Keyword '", Keyword, "' in ", InstrType, InstrStr, " ", FileType, "-files")
	cat (infoFile, file=otf)

	# Treat years; set proper month range

#	for (year in yearA:yearE)
#	{
  year=yearA
		if (year/4==floor(year/4)) {leap=1} else {leap=0}
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

		# Treat month; set proper days in month 'mdays'

#		for (mon in mon1:mon2)
#		{
		 mon=mon1
			if (leap==1) 
				{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
				{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

			day1=1
			day2=mdays
			if ((year==yearA) & (mon==monA)) day1=dayA
			if ((year==yearE) & (mon==monE)) day2=dayE

			# Treat days in month

#			for (day in day1:day2)
#			{
			day=day1
				# Create input-filename and check on file existence, open it

				date0=sprintf("%02d.%02d.%04d",day,mon,year)
				if (InstrType=="D")
				{
					dd = substr(date0,1,2)
					mm = substr(date0,4,5)
					yy = substr(date0,7,10)
					InputFilename = sprintf("%s%s%s%s%s.%s",InputPath0, FileType, yy, mm, dd, InstrStr)
				} else
				{
					jjj=0
					leap=0
					xDate = list(day, mon, year, jjj, leap)
					xDate = ConvertDate (xDate, 1)
					jjj=xDate[[4]]
					yyStr=substr(date0,9,10)
					InputFilename = sprintf("%s%s%03d%s.%s",InputPath0, FileType, jjj, yyStr, InstrStr)
				}

				if (file.exists(InputFilename))
				{
					ipf = file(InputFilename,open="r")
					iniOk = 1
				} else  # Input-file file doesen't exist
				{
					infoFile = sprintf("%s%s%s","  File  ",InputFilename, "  not found")
					print(infoFile)
					iniOk=0
				}

				if (iniOk)  # Continue, if desired files were properly read so far
				{
					# Read lines of input-file, get matches with keyword

					inpStr=readLines(ipf,n=-1,ok=TRUE)
					n=grep (Keyword,inpStr,ignore.case=FALSE)
					found=length(n)
					if (InstrType=="D")
					{
						if (found>0)
						{ infoFile = sprintf("  %s%4d%s%6d\n", date0, found, " occurences, first at line ", n[1]) } else
						{ infoFile = sprintf("  %s%s\n", date0, "  no occurences") }
					} else
					{
						if (found>0)
						{ 
							infoFile = sprintf("  %s%s%03d%s%4d%s%6d\n", date0, " (", jjj, ")", found, " occurences, first at line ", n[1]) 
						} else
						{ infoFile = sprintf("  %s%s%03d%s\n", date0, " (", jjj, ")  no occurences") }
					}
					cat (infoFile, file=otf)
					close(ipf)  # close input-datafile
				}  # end if iniOk

#			}  # for day=day1:day2
#		}  # for mon=mon1:mon2
#	}  # for year=yearA:yearE
	close(otf)  # close outputfile

}  # end if iniOk

# end of program 'FindKeyword.R'

