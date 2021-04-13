#############################################################################
#                                                                           #
#                                                                           #
#                            METEOROLOGIGAL OBSERVATIONS                    #
#                                                                           #
#                                                                           #
#        Module Name         :  SplitClimap.R                               #
#                                                                           #
#                                                                           #
#        Creation Date       :  27.04.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               19.12.2017                                  #
#                               27.03.2017                                  #
#                               30.08.2016                                  #
#                               01.06.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'SplitClimap.R' reads a Climap-generated datafile, splits its      #
#        contents and writes them to daily-separated data files.            #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 01.06.2016 by Herbert Schill:                                    #
#                                                                           #
#          - print output-info on sreen                                     #
#                                                                           #
#                                                                           #
#        - 30.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - replace source 'ReadDobsTables' by 'ReadInstTables'            #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#                                                                           #
#        - 19.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for import of 'ConvertDate' from 'DateZeit.R'            #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")

source("DateZeit.R")        # import ConvertDate
source("ReadInstTables.R")  # import ReadIniFile

# Open the ini-file 'SplitClimap.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "SplitClimap.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:Nparams)  iniPar[n] = iniParams[[2]][[n]]

	Loc = iniPar[2]
	inPathname = iniPar[3]
	nHead = as.integer(iniPar[4])
	nData = as.integer(iniPar[5])
	outPath = iniPar[6]
	outNameTemplate = iniPar[7]

	# Open inputfile for read, overread header and read first data line

	if (file.exists(inPathname))
	{
		cf = file(inPathname,open="r")
		hline = ""
		inDate = array()
		inDate[1]=0
		inDate[2]=0
		for (k in 1:(nHead+1))
		{
			hline = scan(cf, what="character", nlines=1, sep="", quiet=TRUE)
		}

		# Proceed data lines

		while (length(hline)==(nData+6))
		{
			mm = as.integer(hline[3])
			dd = as.integer(hline[4])
			if ((dd==inDate[1]) & (mm==inDate[2]))  # same day as before
			{
				inDate[1]= dd
				inDate[2]= mm
			} 
			else  # next day
			{
				if (inDate[1]>0)  # close output file of previous day
				{
					close(of)
					infoFile = sprintf("%s%04d%03d.%s%s", "File  D", yy, jdn, Loc, "  ok")
					print(infoFile)
				}

				yy = as.integer(hline[2])
				inDate[1]= dd
				inDate[2]= mm
				inDate[3]= yy
				inDate = ConvertDate(inDate, 1)
				jdn = as.integer(inDate[4])

				# Create output-file 'Dyyyyjjj.Loc' for new day and open it

				outPathName = sprintf("%s%s%04d%03d.%s", iniPar[6], "D", yy, jdn, Loc)
				of = file(outPathName,open="w")
			}

			hr = as.integer(hline[5])
			mn = as.integer(hline[6])
			outl = sprintf (" %02d %02d", hr, mn)
			for (k in 1:nData)
			{
				mval = as.double(hline[k+6])
				outl = sprintf ("%s%7.1f", outl, mval)
			}
			outl = sprintf ("%s%s", outl, "\n")
			cat (outl, file=of)
			hline = scan(cf, what="character", nlines=1, sep="", quiet=TRUE)
		}  # end while

		close(cf)  # close input file
		close(of)  # close output file 'Dyyyyjjj.Loc' of last day
		infoFile = sprintf("%s%04d%03d.%s%s", "File  D", yy, jdn, Loc, "  ok")
		print(infoFile)

	}
	else  # input file doesen't exist
	{
		infoFile = sprintf("%s%s%s","  File  ",inPathname, "  not found")
		print(infoFile)
		iniOk=0
	}
}  # end if iniOk

# end of program 'SplitClimap.R'