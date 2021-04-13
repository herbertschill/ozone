#############################################################################
#                                                                           #
#                                                                           #
#                            METEOROLOGIGAL OBSERVATIONS                    #
#                                                                           #
#                                                                           #
#        Module Name         :  SplitVQBA.R                                 #
#                                                                           #
#                                                                           #
#        Creation Date       :  30.01.2019                                  #
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
#        'SplitVQBA.R' reads a series of daily VQBA-files(data files con-   #
#        taining meteorological data of a number of SMN-stations sent by    #
#        MeteoSwiss to PMOD-WRC), extracts the desired station and writes   #
#        the desired data columns to daily-separated data files.            #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")

source("DateZeit.R")        # import ConvertDate, LeapYear
source("ReadInstTables.R")  # import ReadIniFile

# Open the ini-file 'SplitVQBA.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

maxVal=9999
Nparams=9
iniParams = ReadIniFile(Nparams, "SplitVQBA.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector; treat parameters

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:Nparams)  iniPar[n] = iniParams[[2]][[n]]

	Loc = iniPar[2]
	inPathnameHead = iniPar[5]
	inNameCoda = iniPar[6]
	nHead = as.integer(iniPar[7])
	outPath = iniPar[9]
	strPos = array()
	pos = array()
	strPos = strsplit(iniPar[8],";")
	nDataOut = length(strPos[[1]])
	for (k in 1:nDataOut) pos[k]=as.integer(strPos[[1]][[k]])

	dayA  = as.integer(substr(iniPar[3],1,2))
	monA  = as.integer(substr(iniPar[3],4,5))
	year  = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	first = 1

	# Treat desired period; set proper days in month 'mdays'

	leap = LeapYear(year)

	for (mon in monA:monE)
	{
		if (leap==1) 
			{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
			{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

		day1=1
		day2=mdays
		if (mon==monA) day1=dayA
		if (mon==monE) day2=dayE
		
		# Treat days in month

		for (day in day1:day2)
		{
			# create input-filename and open daily input-file if existing;
			# read dataset

			ipathname = sprintf("%s%04i%02i%02i%s", inPathnameHead, year, mon, day, inNameCoda)
			if (file.exists(ipathname))
			{
				vq = file(ipathname,open="r")
				hline = scan(vq,what="character",nlines=nHead,quiet=TRUE)
				hline = scan(vq,what="character",nlines=0,sep="",quiet=TRUE)
				close(vq)

				# Check wether desired station 'Loc' has data in input-file

				n=grep (Loc,hline,ignore.case=FALSE)
				if (length(n)>0)
				{

					# Create output-file 'Dyyyyjjj.Loc' for the day and open it.
					#
					# Please note: the date of the input-file contents is NOT the
					# same as the date of the input-file itself, but usually the
					# previous day !

					inDate = array()
					inDate[1]= as.integer(substr(hline[n[1]+1],7,8))
					inDate[2]= as.integer(substr(hline[n[1]+1],5,6))
					inDate[3]= as.integer(substr(hline[n[1]+1],1,4))
					inDate = ConvertDate(inDate, 1)
					jdn = as.integer(inDate[4])

					outPathName = sprintf("%s%s%04d%03d.%s", outPath, "D", inDate[3], jdn, Loc)
					of = file(outPathName,open="w")

					# proceed measurement lines of the desired station

					for (i in 1:length(n))
					{
						hr  = as.integer(substr(hline[n[i]+1],9,10))
						mn = as.integer(substr(hline[n[i]+1],11,12))
						outl = sprintf (" %02d %02d", hr, mn)
						for (k in 1:nDataOut)
						{
							mval = as.double(hline[n[i]+pos[k]+1])
							if (mval>maxVal)
							{
								mval=0.0
								infoFile = sprintf("%s%04d.%03d.%s%s%02d:%02d%s", "  Invalid value in D", inDate[3], jdn, Loc, " at ", hr, mn, "  > set to 0.0\n")
								cat(infoFile, file="")
							}
							outl = sprintf ("%s%7.1f", outl, mval)
						}
						outl = sprintf ("%s%s", outl, "\n")
						cat (outl, file=of)
					}  # next i
					close(of)
					infoFile = sprintf("%s%04d%03d.%s%s", "  File  D", inDate[3], jdn, Loc, "  ok\n")
					cat(infoFile, file="")
				} else  # no data of station 'Loc' in input-file
				{
					infoFile = sprintf("%s%s%s", "  No data of station ", Loc, " in input-file\n")
					cat(infoFile, file="")
				}  # end if length(n):0
			} else  # input file doesen't exist
			{
				infoFile = sprintf("%s%s%s","  File  ",ipathname, "  not found\n")
				cat(infoFile, file="")
				iniOk=0
			}  # end if file.exists(ipathname)
		}  # next day
	}  # next mon

}  # end if iniOk

# end of program 'SplitVQBA.R'