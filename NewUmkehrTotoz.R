#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  NewUmkehrTotoz.R                            #
#                                                                           #
#                                                                           #
#        Creation Date       :  27.10.2010                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               24.03.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 2.5.0    (2009)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'NewUmkehrTotoz.R' writes the halfday mean values of total         #
#        ozone values of Dobson measurements from the input file            #
#        "HDOZON_yyyy.iii" (df) to the output file 'NewTot_yyyy.iii'        #
#        and sets the quality flag:                                         #
#                                                                           #
#        - if measurements for both halfdays exist, the quality flags       #
#          are set to 0 for both halfdays                                   #
#        - if no measurements are available for one halfday, the daily      #
#          mean (which is the OzonHD for the other halfday) is used for     #
#          OzonHD[hd], and quality flag is set to 1                         #
#        - if no measurements at all are available for the day, the half-   #
#          day ozone mean is interpolated from the surrounding days and     #
#          the quality flags are set to 2 for both halfdays                 #
#        - if the gap is more than 10 days, halfday ozone values are set    #
#          to 999 and the quality flags are set to 3 for both halfdays;     #
#          the same is done for missing days at the begin and the end of    #
#          the year (if 'completeYr'=1)                                     #
#                                                                           #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'NewUmkehrTotoz.ini'.                         #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 24.03.2016 by Herbert Schill:                                    #
#                                                                           #
#          Adapt to new 'ReplaceCalDobsYear'                                #
#                                                                           #
#############################################################################


# setwd("C:\\LKO\\Programs\\R")

source("ReadDobsTables.R")
source("NewUmkehrTotSub.R")

statusOK <- 0

# Reads the path-file 'NewUmkehrTotoz.pth' with the adress of the ini-file
# 'NewUmkehrTotoz.ini' and reads the initialization parameters from the
# latter (Nparams designs the number of parameters to read from ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "NewUmkehrTotoz")
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]
	completeYr = as.integer(iniPar[7])

	# Treat desired period

	yearA = as.integer(iniPar[3])
	yearE = as.integer(iniPar[4])

	for (year in yearA:yearE)
	{
		# Open halfday mean input file "HDOZON_yyyy.iii" for the year, if existing

		dfilename = sprintf("%s%s.%s", "HdOzon_", year, iniPar[2])
		dpathname = ReplaceCalDobsYear (iniPar[5],"", iniPar[2], year)
		dpathname = sprintf("%s%s", dpathname, dfilename)
		ex=file.exists(dpathname)

		if (ex)
		{
			infoFile = sprintf("%s%5i%s%s%s","  Proceed", year, ":  File  ",dfilename, "  found")
			print(infoFile)
			hd = file(dpathname,open="r")

			# Open halfday mean output file "NEWTOT_yyyy.iii" for the year

			nfilename = sprintf("%s%s.%s", "NewTot_", year, iniPar[2])
			npathname = ReplaceCalDobsYear (iniPar[5],"", iniPar[2], year)
			npathname = sprintf("%s%s", npathname, nfilename)
			nt = file(npathname,open="w")

			# test on leap year

			leap=0
			if  (year/4==floor(year/4))  leap=1
			if  (year/100==floor(year/100))  leap=0
			if  (year/400==floor(year/400))  leap=1

			hline = ""
			hline = scan(hd,what="character",nlines=1,quiet=TRUE)  # read header
			hline = scan(hd,what="character",nlines=1,quiet=TRUE)
			cdata1 = DecodeHdLine (hline, leap)

			# fill empty days at begin of year with ' ddmmyyh 999 3', if desired

			djdn1 = as.integer(cdata1[4])
			if  ((completeYr>0) & (djdn1>1))  nt = CompleteYear (nt, djdn1, year, leap, -1)

			nt = WriteNewtot (cdata1, nt)
			hline = ""
			hline = scan(hd,what="character",nlines=1,quiet=TRUE)
			dateStrX = hline[1]
			while (nchar(dateStrX)>7)
			{
				cdata2 = DecodeHdLine (hline, leap)
				djdn1 = as.integer(cdata1[4])
				djdn2 = as.integer(cdata2[4])
				djdn = djdn2-djdn1
				if (djdn>1)
				{
					djdn=djdn-1
					for (n in 1:djdn)
					{
						cdataN = InterpolateDays(n, cdata1, cdata2)
						nt = WriteNewtot (cdataN, nt)
					}
				}
				nt = WriteNewtot (cdata2, nt)
				cdata1=cdata2
				hline = ""
				hline = scan(hd,what="character",nlines=1,quiet=TRUE)
				dateStrX = hline[1]
				dateStrX
			}  # while end

			# fill empty days at end of year with ' ddmmyyh 999 3', if desired

			if (completeYr>0)
			{
				djdn1 = as.integer(cdata1[4])
				if (leap>0)
				{
					if  (djdn1<366)  nt = CompleteYear (nt, djdn1, year, leap, 1)
				}
				else
				{
					if  (djdn1<365)  nt = CompleteYear (nt, djdn1, year, leap, 1)
				}
			}

			# close yearly input/output files
			
			close (hd)
			close (nt)
		}
		else
		{
			infoFile = sprintf("%s%5i%s%s%s","  Proceed", year, ":  File  ",dfilename, "  not found")
			print(infoFile)
		}  # end if (ex)
	}  # next year
}  # end if iniOk

# end of program 'NewUmkehrTotoz.R'
