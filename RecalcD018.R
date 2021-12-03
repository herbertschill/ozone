#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  RecalcD018.R                                #
#                                                                           #
#                                                                           #
#        Creation Date       :  01.10.2021                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               26.11.2021                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD/WRC                   #
#        Modifications by    :     "      "         "                       #
#                                                                           #
#        Developing System   :  R 4.1.1    (2021)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RecalcD018.R' recalculates Dobson D018 total ozone, reading the   #
#        raw data from the 'Djjjyyyy.iii' files, results are written to     #
#        'Djjjyyyy.iii' files or the all day file located in the same or    #
#        another directory.                                                 #
#                                                                           #
#        Day statistics values (mean, stadev, number of measurements) are   #
#        written to the dayfile "DAYOZON_yyyy.iii" ( data of one year or a  #
#        part of a year, iii=Dobson identifier).                            #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'RecalcD018.ini'.                                                  #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 26.11.2021 by Herbert Schill:                                    #
#                                                                           #
#          - Add parameter 'doy' in call list of 'WriteDobsonAD' for        #
#            adding doy on output lines                                     #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

library(timeDate)

source("DateZeit.R")
source("ReadInstTables.R")
source("TreatDobsonData.R")
source("CalcDobson.R")

#  from:               import:
#
#  DateZeit.R          CompareDate, ConvertDate, LeapYear, ReplaceCalInstYear 
#
#  ReadInstTables.R    ReadAbsCoeffData, ReadConstantsFile, ReadDNtable, 
#                      ReadIniFile, ReadRNtable
#
#  TreatDobsonData.R   ReadDobsonDD, WriteDayStatis, WriteDobsonAD, WriteDobsonDD
#
#  CalcDobson.R        CalculDD


statusOK <- 0

# Open the ini-file 'RecalcD018.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=34
iniParams = ReadIniFile(Nparams, "RecalcD018.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 2:npar)  iniPar[n-1] = iniParams[[2]][[n]]
	iniPar[1] = toupper(iniPar[1])
	iniPar[12] = toupper(iniPar[12])
	iniPar[28] = toupper(iniPar[28])
	
	#  iniPar    Value
	#
	#     1      Station indicator (NAI)
	#     2      Dobson Instrument Identifier 'iii'
	#     3      Calibration
	#     4      Period Begin (dd.mm.yyyy or jjj.yyyy)
	#     5      Period End   (dd.mm.yyyy or jjj.yyyy)
	#     6      use standard RN-Tables (1=yes, 0=no)
	#     7      Filename standard RN-Tables
	#     8      Filename Delta-N-Correction Table
	#     9      Absorption Coefficients set name
	#    10      UTC-offset (MEZ->UTC=-1, TST->UTC=-33)
	#    11      Output time reference (UTC/LOC)
	#    12      Outputfile Format: AE/DD
	#    13      Path Input Datafiles
	#    14      Path Output Datafiles
	#    15      Path Tabellfiles
	#    16      Filename Atmospheric Constants Tables
	#    17      Filename of all day output file
	#    18      Info about series
	#    19      Station Name
	#    20      Station Longitude (deg)
	#    21      Station Latitude (deg)
	#    22      Station Altitude (km)
	#    23      Station standard pressure (hPa)
	#    24      O3 layer barycentric height over station (km)
	#    25      Atmosphere barycentric height over station (km)
	#    26      Radius Earth (km)
	#    27      Sea level standard pressure (hPa)
  #    28      Measurement Sequence
	#    29      Filter ozone data (0=no, 1..3=yes)
	#    30      Filter ozone data (0=no, 1..3=yes)
	#    31      Filter: minimal flag value
	#    32      Filter: maximal flag value
	#    33      Logfile; if no logfile desired:0
	
	# Check date format, convert to "dd.mm.yyy" if necessary
	
	cDate = array()
	cDate = CheckConvertDate(iniPar[4])
	dayA  = cDate[1]
	monA  = cDate[2]
	yearA = cDate[3]
	cDate = array()
	cDate = CheckConvertDate(iniPar[5])
	dayE  = cDate[1]
	monE  = cDate[2]
	yearE = cDate[3]
	if (nchar(iniPar[4])!=10)
	{
	  iniPar[4] = sprintf("%02d.%02d.%04d", dayA, monA, yearA)
	  iniPar[5] = sprintf("%02d.%02d.%04d", dayE, monE, yearE)
	}
	  
}  # end if iniOk

#  1st indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

if (iniOk)  # Continue, if previous files were properly read
{

	# Check wether a specific RN-table should be used, or wether the
	# time-dependend RN-tables are used for the R/N conversion;
	# standard filename is 'O3DRNT_yyyy.iii' (yyyy=year of table,
	# iii=Dobson identifier)

	if  (iniPar[6]!="0")
	{
	# Opens the required RN-tables (e.g. 'O3DRNT_2019.018') and reads the
	# R to N conversion values for the 3 wavelengths A, C, D.

		iniPar[7] = ReplaceCalInstYear (iniPar[7], iniPar[3], iniPar[2], 9999)
		RNfilename  = iniPar[7]
		RNfilename0 = RNfilename
		RNdata = ReadRNtable(iniPar[2], RNfilename, iniPar[15])
		iniOk  = RNdata[[1]][[1]]
	}  # end if iniPar[6]!="0"

	infoFile = sprintf("%s%s%s","    Station is ",iniPar[19], "\n\n")
	cat(infoFile, file="")

	# Read the atmospheric constants from the constants file

	ATconst = ReadConstantsFile("Dobson",iniPar[15],iniPar[16],iniPar[9],iniPar[23],iniPar[27])
	iniOk  = ATconst[[1]][[1]]

	if (iniOk)  # Continue, if desired files were properly read so far
	{
		# Opens the file which contain the Delta-N-Correction history and 
		# information about the RN-tables history and the location, filename 
		# is usually 'NcorCalxxx.iii' (xxx=calibration identifier, e.g. '18a' 
		# or '00', iii=Dobson identifier) and reads the dates, the name of the 
		# valid RN-table, the location and the dN-values for the 3 wavelengths
		# A, C, D. 

		DNdata = ReadDNtable(iniPar)
		iniOk  = DNdata[[1]][[1]]
	}  # end if iniOk

	#  2nd indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	
	if (iniOk)  # Continue, if desired files were properly read so far
	{
		# Set first dN period date and read values

		DNvect      = array()
		nPeriods    = DNdata[[1]][[1]]
		pd=1
		RNfilename0 = sprintf("%s.%s",DNdata[[3]][[pd]],iniPar[2])
		periodDate0 = DNdata[[2]][[pd]]  # format "dd.mm.yyyy"
		Loc0        = DNdata[[7]][[pd]]  # location indicator
		for (i in 1:3)  DNvect[i] = DNdata[[i+3]][[pd]]  # dN-values

		# Test: first measurement date older than first DN period date ?
		
		i = (CompareDate(iniPar[4], periodDate0))

		#  3rd indent  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		
		if (i>=0)
		{
			# Set proper info about series

  		iniPar[18] = ReplaceCalInstYear (iniPar[18], iniPar[3], iniPar[2], 9999)
  		ty = switch(iniPar[30], "DS"=1, "ZC"=2, "00"=3)
  		
  		# If desired output format is "AD", create and open the all day file
  		# "SingOzon_yearA-yearE_Calxxx.iii" ('ad') for the year and write header
  		
  		if (iniPar[12]=="AD")
  		{
  		  if (yearA==yearE)
  		    { adfilename = sprintf("%s%04d_%s.%s", "SingOzon_", yearA, iniPar[3], iniPar[2]) } else
  		    { adfilename = sprintf("%s%04d-%04d_%s.%s", "SingOzon_", yearA, yearE, iniPar[3], iniPar[2]) }
  		  adpathname = ReplaceCalInstYear (iniPar[14], iniPar[3], iniPar[2], yearA)
    		adpathname = sprintf("%s%s", adpathname, adfilename)
    		
    		ad = file(adpathname,open="w")
    		if (iniPar[28]=="ADADA")
    		{
    		  outl = sprintf ("%s", "iii dd.mm.yyyy doy MM hh:mm:ss  OzA1 hh:mm:ss  OzD1 hh:mm:ss  OzA2 hh:mm:ss")  
    		  outl = sprintf ("%s%s", outl, "  OzD2 hh:mm:ss  OzA3 hh:mm:ss Airms   SZA  OzAD\n")
    		} else
    		{
    		  outl = sprintf ("%s", "iii dd.mm.yyyy doy MM hh:mm:ss   OzC hh:mm:ss   OzD hh:mm:ss   OzA hh:mm:ss")  
    		  outl = sprintf ("%s%s", outl, " Airms   SZA  OzAD\n")
    		}
    		cat(outl, file=ad)
  		}  # end if iniPar[12]=="AD"
  		
  		# If logfile desired, create and open log file

  		if (iniPar[33]!="0")
  		{
  		  lf = file(iniPar[33],open="w")
  		  infoFile = sprintf("%s%s%s%s%s%s%s","\n  Recalculate Period ", iniPar[4], "-", iniPar[5],
  		                     "  for instrument ", iniPar[2], "\n\n")
  		  cat(infoFile, file=lf)
#  		  close(lf)
  		}

  		####  Time Loop: treat desired period  ##################################

			# Treat years; set proper month range

  		first = 1

  		for (year in yearA:yearE)
			{
				# year=yearA

				# Open daily mean output file "DAYOZON_yyyy.iii" for the year and write header

			  inPath = ReplaceCalInstYear (iniPar[13], iniPar[3], iniPar[2], year)
			  dfilename = sprintf("%s%s.%s", "Dayozon_", year, iniPar[2])
				dpathname = ReplaceCalInstYear (iniPar[14], iniPar[3], iniPar[2], year)
				dpathname = sprintf("%s%s", dpathname, dfilename)

				ds = file(dpathname,open="w")
				if (iniPar[30]=="00")
				{outl = sprintf ("%s", " iii MM yyyymmdd  MeanDS  SDev NNN  MeanZC  SDev NNN  MeanAll SDev NNN\n")} else
				{outl = sprintf ("%s%s%s", " iii MM yyyymmdd  Mean", iniPar[30], "SDev NNN\n")}
				cat(outl, file=ds)

				leap = LeapYear(year)
				mon1=1
				mon2=12
				if (year==yearA) mon1=monA
				if (year==yearE) mon2=monE

				# Treat months; set proper days in month 'mdays'

				for (mon in mon1:mon2)
				{
					# mon=mon1
					if (leap==1) 
						{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
						{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

					day1=1
					day2=mdays
					if ((year==yearA) & (mon==monA)) day1=dayA
					if ((year==yearE) & (mon==monE)) day2=dayE
					
					# Treat days in month  ##############################################

					for (day in day1:day2)
					{
						# day=day1

						measDate = sprintf("%02d%s%02d%s%04d", day, '.', mon, '.', year)

						# Compare date with period date to evaluate the proper dN-period

						periodDate = periodDate0
						i = CompareDate(periodDate, measDate)
						while ((i<0) & (pd<nPeriods))
						{
							pd=pd+1
							periodDate = DNdata[[2]][[pd]]
							i = CompareDate(periodDate, measDate)
							#i;pd;nPeriods;periodDate;measDate
						}
						if  ((i>0) & (pd>1) & (pd<=nPeriods))	pd = pd-1
						periodDate = DNdata[[2]][[pd]]

						# If necessary, set the proper dN-values for the day and 
						# proper location parameters, recalc proper AtmCorr etc.

						if (periodDate != periodDate0)
						{
							for (i in 1:3)  DNvect[i] = DNdata[[i+3]][[pd]]
							Loc = DNdata[[7]][[pd]]
							periodDate0 = periodDate
						}

						# Read RN-table at the first pass, if not read yet; check wether 
						# RN-table is the same as before, read new RN-table otherwise

						if  (iniPar[6]=="0") 
						{
							RNfilename = sprintf("%s.%s",DNdata[[3]][[pd]],iniPar[2])
							if  ((first) | (RNfilename0!=RNfilename)) 
							{
								RNdata = ReadRNtable(iniPar[2], RNfilename, iniPar[15])
								iniOk  = RNdata[[1]][[1]]
								RNfilename0 = RNfilename
								first=0
							}
						}  # end if iniPar[6]=="0"

						#  4th ident  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

						if (iniOk)  # Continue, if desired files were properly read so far
						{
							# Read header data and measurement data of the day from file "Dyyyymmdd.iii";
						  # if measurements are available for the day, recalculate calendar-dependent 
							# coefficients and recalculate the total ozone

						  Calendar=array()
						  Calendar[1]=day
							Calendar[2]=mon
							Calendar[3]=year
							Calendar[4]=0
							Calendar[5]=leap
							Calendar = ConvertDate (Calendar,1)
							doy = Calendar[4]
						  
						  dataDD = ReadDobsonDD (doy, day, mon, year, iniPar[2], inPath, 1, lf)
						  if  (dataDD[[1]][[4]]>0)  # measurements exist of this day
						  {
						    cdata = CalculDD (iniPar, Calendar, dataDD, ATconst, RNdata, DNvect)
						  }

							# if valid measurements of this day exist, write recalculated total ozone
							# to a new file "Djjjyyyy.iii" or to the all day file 'ad' (iniPar[17])

						  if  (dataDD[[1]][[4]]>0)   # measurements exist of this day
						  {
						    if  (cdata[[1]][[4]]>0)  # valid measurements exist of this day
						    {
						      if (iniPar[12]=="DD")
  							  { statusOK = WriteDobsonDD (iniPar, cdata, ATconst) } else
  							  { ad = WriteDobsonAD (doy, cdata, ad) }
  
  							  # Write day statistics to the file "DAYOZON_yyyy.iii"
  							  
  							  outl= sprintf (" %s %s %04d%02d%02d", iniPar[2], iniPar[30], year, mon, day)
  							  if (iniPar[30]=="00")
  							  {
  							    for (w in 1:3)
  							    {
  							      outl = sprintf ("%s%8.1f%6.1f%4d", outl, cdata[[4]][[1]][[w]], 
  							                      cdata[[4]][[3]][[w]], cdata[[4]][[2]][[w]])
  							    }
  							    outl = sprintf ("%s\n", outl)
  							  } else
  							  {
  							    outl= sprintf ("%s%8.1f%6.1f%4d\n", outl,cdata[[4]][[1]][[ty]], 
  							                   cdata[[4]][[3]][[ty]], cdata[[4]][[2]][[ty]])
  							  }
  							  cat(outl, file=ds)
						    }  # if dataDD[[1]][[4]]>0
							}  # if cdata[[1]][[4]]>0
						}  # if iniOk
					}  # for day=day1:day2  #############################################

				}  # for mon=mon1:mon2

				close(ds)  # close day statistics file "DAYOZON_yyyy.iii" of the year

			}  # for year=yearA:yearE

  		if (iniPar[12]=="AD") close(ad)  # close all day measurements file 
  		infoFile = sprintf("%s%s%s%s%s%s%s","\n\n  Period ", iniPar[4], "-", iniPar[5],
  		                   "  for instrument ", iniPar[2], "  recalculated\n")
  		cat(infoFile, file=lf)
  		if (iniPar[33]!="0")  close(lf)   # close logfile 
		}
		else  # first measurement date is older than first DN period date
		{
			infoFile = sprintf("%s%s%s%s%s","\n\n  First measurement date ",iniPar[4], 
			                   " is older than first DN period date ", periodDate, "\n\n")
			cat(infoFile, file="")
			statusOK=0
		}  # end if i>=0 (first measurement date : first DN period date)

	}  # end if iniOk

	infoFile = sprintf("%s%s%s%s%s%s%s","\n\n  Period ", iniPar[4], "-", iniPar[5],
	                   "  for instrument ", iniPar[2], "  recalculated\n\n\n")
	cat(infoFile, file="")

}  # end if iniOk

#### end of program 'RecalcD018.R' ###########################################
