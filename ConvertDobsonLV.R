#############################################################################
#                                                                           #
#                                                                           #
#                            DATA FILE CONVERSION                           #
#                                                                           #
#                                                                           #
#        Module Name         :  ConvertDobsonLV.R                           #
#                                                                           #
#                                                                           #
#        Creation Date       :  30.12.2013                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               15.01.2020                                  #
#                               20.12.2018                                  #
#                               15.09.2017                                  #
#                               29.03.2017                                  #
#                               06.12.2016                                  #
#                               21.07.2016                                  #
#                               07.12.2015                                  #
#                               03.12.2015                                  #
#                               29.10.2015                                  #
#                               24.04.2015                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ConvertDobsonLV.R' reads the Dobson Umkehr datafiles of format    #
#        'uuddmmyy.iii' (uu=UU/VV/Uw, w=C/D/A, iii=Dobson identifier or     #
#        TXT), 'Dyyyymmdd.iii' or 'yyyymmdd_Dobson_data.iii', reformatts    #
#        its contents and writes them to the data file 'umyyyymmddw.iii',   #
#        which is the unified input format for LabView postprocessing       #
#        programs.                                                          #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'ConvertDobsonLV.ini'.                                  #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 24.04.2015 by Herbert Schill:                                    #
#                                                                           #
#          - allow reading of temperature from different columns            #
#            (parameter 8 from ini-file)                                    #
#                                                                           #
#          - create outputfile only, if 'um' data available                 #
#                                                                           #
#                                                                           #
#        - 29.10.2015 by Herbert Schill:                                    #
#                                                                           #
#          - read inputfile format ['LV' or 'D'] from ini-file              #
#                                                                           #
#          - new output-filename 'umyyyymmddw.iii' (w=wl C/D/A)             #
#                                                                           #
#                                                                           #
#        - 03.12.2015 by Herbert Schill:                                    #
#                                                                           #
#          - extend for reading 'D'-format inputfile                        #
#                                                                           #
#                                                                           #
#        - 07.12.2015 by Herbert Schill:                                    #
#                                                                           #
#          - change Lux-value output format to higher precision             #
#                                                                           #
#                                                                           #
#        - 21.07.2016 by Herbert Schill:                                    #
#                                                                           #
#          - replace call 'ReplaceDobsYear' by 'ReplaceCalDobsYear'         #
#                                                                           #
#                                                                           #
#        - 06.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          - replace source 'ReadDobsTables' by 'ReadInstTables'            #
#                                                                           #
#          - replace call 'ReplaceCalDobsYear' by 'ReplaceCalInstYear'      #
#                                                                           #
#          - read number of header lines 'hd' from ini-file                 #
#                                                                           #
#                                                                           #
#        - 29.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#                                                                           #
#        - 15.09.2017 by Herbert Schill:                                    #
#                                                                           #
#          - extend for processing 'uuddmmyy.iii'-files (uu=UU/VV/Uw,       #
#            w=C/D/A, iii=Dobson identifier or TXT)                         #
#                                                                           #
#                                                                           #
#        - 20.12.2018 by Herbert Schill:                                    #
#                                                                           #
#          - modify screen-output                                           #
#                                                                           #
#                                                                           #
#        - 15.01.2020 by Herbert Schill:                                    #
#                                                                           #
#          - import 'ReplaceCalInstYear' from 'DateZeit.R'                  #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          ReplaceCalInstYear
#
#  ReadInstTables.R    ReadIniFile

calib=""
statusOK=0
isTest=0

# Open the ini-file 'ConvertDobsonLV.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=10
iniParams = ReadIniFile(Nparams, "ConvertDobsonLV.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	dobson = iniPar[2]
	dayA  = as.integer(substr(iniPar[3],1,2))
	monA  = as.integer(substr(iniPar[3],4,5))
	yearA = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	yearE = as.integer(substr(iniPar[4],7,10))
	wavelength = iniPar[5]
	inputFmt = iniPar[8]
	hd = as.integer(iniPar[9])  # number of header lines
	posTT = as.integer(iniPar[10])

	nx = ifelse(inputFmt=="D", 4, 5)
	tag= ifelse(inputFmt=="D", "um", "Umkehr")
	pos= ifelse(inputFmt=="D", 3, 7)
	oldFmt=0
	separ="	"

	if ((inputFmt=="UU") || (inputFmt=="VV") || (inputFmt=="UW"))
	{ 
		separ=""
		oldFmt=1
		hd = 0
		posTT = 2
		StdR = 0.0
		StdLux = 0.0
		HV = 0.0
	}

	if ((inputFmt=="YY0"))
	{ 
		oldFmt=2
		hd = 5
		pos= 4
#		posTT = 5
	}

	if (isTest)  # test only
	{
		day=dayA
		mon=monA
		year=yearA
	}

	# Treat year; set proper month range

	for (year in yearA:yearE)
	{
		# Set proper input/output pathes for the year; write info on screen

		inPathname = ReplaceCalInstYear (iniPar[6], calib, dobson, year)
		outPathname = ReplaceCalInstYear (iniPar[7], calib, dobson, year)
		infol = sprintf("   %s   %s\n\n", "Inputpath:", inPathname)
		cat(infol, file="")
		infol = sprintf("   %s  %s\n\n", "Outputpath:", outPathname)
		cat(infol, file="")
		infol = sprintf("   %s\n\n", "Outputfiles:")
		cat(infol, file="")

		# Set proper month range

		leap=0
		if  (year/4==floor(year/4))  leap=1
		if  (year/100==floor(year/100))  leap=0
		if  (year/400==floor(year/400))  leap=1
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

		# Treat month; set proper days in month 'mdays'

		for (mon in mon1:mon2)
		{

			mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
			if (leap==1 && mon==2) mdays=29

			day1=1
			day2=mdays
			if ((year==yearA) && (mon==monA)) day1=dayA
			if ((year==yearE) && (mon==monE)) day2=dayE

			# Treat days in month

			for (day in day1:day2)
			{

				measDate = sprintf("%04d%02d%02d", year, mon, day)

				if (oldFmt==1)  inDate = sprintf("%02d%02d%02d", day, mon, year-2000)

				# create filenames and open input/output files

				if (inputFmt=="YY")  inFilename = sprintf("%s%s%s%s", inPathname, measDate, "_Dobson_data.", dobson)
				if (inputFmt=="YY0") inFilename = sprintf("%s%s%s%s", inPathname, measDate, "_Dobson_data.", dobson)
				if (inputFmt=="D")   inFilename = sprintf("%s%s%s%s%s", inPathname, "D", measDate, ".", dobson)
				if (inputFmt=="UU")  inFilename = sprintf("%s%s%s%s", inPathname, "UU", inDate, ".TXT")
				if (inputFmt=="VV")  inFilename = sprintf("%s%s%s%s", inPathname, "VV", inDate, ".TXT")
				if (inputFmt=="UW")  inFilename = sprintf("%s%s%s%s%s%s", inPathname, "U", wavelength, measDate, ".", dobson)
				ex=file.exists(inFilename)

				if (ex)
				{
					first=1
					exOutfile=0
					inf = file(inFilename,open="r")

					mv = array()
					mv = scan(inf,what="character",sep=separ,skip=hd,nlines=1,quiet=TRUE)

					# proceed the lines of 'UWyyyymmdd.iii'/'UUddmmyyyy.TXT'/'VVddmmyyyy.TXT'/
					#
					# Lux	Temp	Time	Rd	instr
					#
					# mv[1]  Lux
					# mv[2]  Temperature
					# mv[3]  Time [hhmmss]
					# mv[4]  R.dial

					# proceed the lines of 'Dyyyymmdd.iii'
					#
					# Mode	Time	wl	Q1	Q2	HV	Rd	std(Rd)	lux	std(lux)	dlaP	dlaQ	phi	Nmeas	tacc	std(dlaP)	skew(dlaP)	kurtosis(dlaP)	min(dlaP)	max(dlaP)	Med(FFT)	Temp	Rel Hum
					#
					# mv[1]  Mode
					# mv[2]  Time [s since 0h00 UTC]
					# mv[3]  wlgth
					# mv[4]  Q1
					# mv[5]  Q2
					# mv[6]  HV
					# mv[7]  R.dial
					# mv[8]  Std.R
					# mv[9]  Lux
					# mv[10] Std.Lux
					# mv[..] ...
					# mv[22] Temperature
					# mv[23] Rel.Hum

					# proceed the lines of 'yyyymmdd_Dobson_data.iii'
					#
					# Mode	Time	R.dial	Std.R	Lux	Std.Lux	wlgth	Temperature	HV	tacc	Nmeas	dla.ampl	int.noise	Trot	RHrot
					#
					# mv[1]  Mode
					# mv[2]  Time
					# mv[3]  R.dial
					# mv[4]  Std.R
					# mv[5]  Lux
					# mv[6]  Std.Lux
					# mv[7]  wlgth
					# mv[8]  Temperature [until 07.2014]
					# mv[9]  HV
					# mv[14] Temperature [since 11.2014]

					while (length(mv)!=0)
					{
						if (((mv[1]==tag) && (mv[pos]==wavelength)) || ((oldFmt==2) && (mv[pos]==wavelength)) || (oldFmt==1))
						{
							if (first)
							{
								# when first 'Umkehr' line occurs, create output file 'umyyyymmddw.iii', write header to it

								outFilename = sprintf("%s%s%s%s%s", "um", measDate, wavelength, ".", dobson)
								infol = sprintf("     %s\n", outFilename)
								outFilename = sprintf("%s%s", outPathname, outFilename)
								otf = file(outFilename,open="w")
								exOutfile=1
								cat(infol, file="")
								outl = sprintf ("\n%40s%s\n\n","DOBSON ", dobson)
								cat (outl, file=otf)
								outl = sprintf ("%35s%02d%s%02d%1s%04d\n\n","Messung vom  ",day,"-",mon,"-",year)
								cat (outl, file=otf)
								outl = sprintf ("%77s\n","Zeit           R-Wert      R-STD         Lux        Lux STD    Temp       HV")
								cat (outl, file=otf)
								outl = sprintf ("%84s\n","------------------------------------------------------------------------------------")
								cat (outl, file=otf)
								first=0
							}  # end if first

							if (inputFmt=="D")
							{
								tsec = as.single(mv[2])  # convert time[s since 0h00 UTC] to "hh:mm:ss"
								hr=floor(tsec/3600)
								min=floor((tsec-hr*3600)/60)
								sec=floor(tsec-hr*3600-min*60)
								TimeStr = sprintf("%02d%s%02d%s%02d", hr, ":", min, ":", sec)
								Rdial = as.single(mv[7])
								StdR = as.single(mv[8])
								Lux = as.single(mv[9])
								StdLux = as.single(mv[10])
								Temperature = as.single(mv[22])
								HV = as.single(mv[6])
							} else if (inputFmt=="YY")
							{
								TimeStr = mv[2]
								Rdial = as.single(mv[3])
								StdR = as.single(mv[4])
								Lux = as.single(mv[5])
								StdLux = as.single(mv[6])
								Temperature = as.single(mv[posTT])
								HV = as.single(mv[9])
							} else if (inputFmt=="YY0")
							{
								TimeStr = mv[1]
								if (nchar(TimeStr)==7) TimeStr = sprintf("%s%s", "0", TimeStr)
								Rdial = as.single(mv[2])
								StdR = as.single(mv[3])
								Temperature = as.single(mv[5])
								HV = as.single(mv[6])
								Lux = as.single(mv[8])
								StdLux = as.single(mv[9])
							} else if (oldFmt==1)
							{
								TimeStr = sprintf("%s%s%s%s%s", substr(mv[3],1,2), ":", substr(mv[3],3,4), ":", substr(mv[3],5,6))
								Rdial = as.single(mv[4])/10
								Lux = as.single(mv[1])/10
								Temperature = as.single(mv[2])
							}
							outl = sprintf ("%9s%12.2f%12.3f%13.2f%11.2f%12.2f%11.2f\n", TimeStr,Rdial,StdR,Lux,StdLux,Temperature,HV)
							cat (outl, file=otf)
						}
						mv = array()
						mv[1]="x"
						mv[pos]="x"
						mv = scan(inf,what="character",sep=separ,nlines=1,quiet=TRUE)

					}  # end while

					close(inf)  # close input file 'yyyymmdd_Dobson_data.iii'
					if (exOutfile) close(otf)  # close output file 'umyyyymmdd.txt'

				}  # end if file.exists(inFilename)
			}  # end for day=day1..day2
		}  # end for mon=mon1..mon2
	} # end for year=yearA..yearE

}  # end if iniOk

#### end of program 'ConvertDobsonLV.R' ########################################
