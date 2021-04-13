#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE CALCULATION                        #
#                                                                           #
#                                                                           #
#        Module Name         :  KomhyrYear.R                                #
#                                                                           #
#                                                                           #
#        Creation Date       :  02/09/2010                                  #
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
#        'KomhyrYear.R' calculates the detailed sun position variables      #
#        according to Komhyr for one year and writes them to the file       #
#        'KomhyrYearR_yyyy.txt'.                                            #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


# setwd("C:\\LKO\\Programs\\R")

source("ReadDobsTables.R")

statusOK <- 0

# Reads the path-file 'Recalcoz.pth' with the adress of the ini-file
# 'Recalcoz.ini' and reads the initialization parameters from the latter
# (Nparams designs the number of parameters to read from ini-file)

Nparams=29
iniParams = ReadIniFile(Nparams)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Initialisation
	#
	#   Pi = 3.14159265358979
	#
	#   ufac = pi/180.0, deg-rad conversion factor

	ufac = 0.017453293

	# Get station parameters, height of the ozone layer and of the atmosphere
	# and the earth radius [km]
 
	StationLongitude = as.double(iniPar[[22]])
	StationLatitude  = as.double(iniPar[[23]])
	StaHeight        = as.double(iniPar[[24]])
	O3Layer          = as.double(iniPar[[26]])
	AtmLayer         = as.double(iniPar[[27]])
	RadiusEarth      = as.double(iniPar[[28]])

	StationLongitude;StationLatitude;StaHeight;O3Layer;AtmLayer;RadiusEarth

	LatRad = StationLatitude*ufac

	# Proper setting of year

	InYear = as.integer(substr(iniPar[3],7,10))
	JDN=0
	TimeHr=12

	if (InYear/4==floor(InYear/4))
		leap=1
	else
		leap=0

	# Open output file 'KomhyrYearR_yyyy.txt' and write header
	
	dfilename = sprintf("%s%s%s", "KomhyrYearR_", InYear, ".txt")
	dpathname = sprintf("%s%s", iniPar[17], dfilename)

	df = file(dpathname,open="w")
	outl = sprintf ("%s", "jdn       Date     AA BB         CC      UU      EE  VV    GG")
	outl = sprintf ("%s%s%s", outl, "    HH  WW    DD     TT SunlDeg SunlRad    SZA", "\n")
	cat (outl, file=df)
	
	# Treat month; set proper days in month 'mdays'

	for (mon in 1:12)
	{
		InMon = mon
		MonYrStr = sprintf ("%s%02i%s%4i", ".", InMon, ".", InYear)

		if (leap==1) 
			mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)
		else
			mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)

		# Treat days in month

		for (InDay in 1:mdays)
		{
			JDN = JDN+1
			
			DateStr = sprintf ("%02i%s", InDay, MonYrStr)

			# Calculation of time elapsed in days 'DD', since 00.01.1900 12:00 UTC
			# and number of Julian centuries 'TT' from 1900.0 (Julian date 2415020.0)
			# (Komhyr p. 105; "INT" of Komhyr has to be replaced by "trunc" commands
			# for a proper calculation of negative numbers)

			AA = (InMon - 14) / 12
			BB = trunc(AA)
			CC = (1461 * (InYear + 4800 + BB)) / 4
			UU = floor(CC)
			EE = 367 * (InMon - 2 - 12 * BB) / 12
			VV = trunc(EE)
			GG = (InYear + 4900 + BB) / 100
			HH = 3 * trunc(GG) / 4
			WW = -trunc(HH)
			DD = InDay - 2447095.5 + UU + VV + WW + TimeHr / 24
			TT = DD / 36525
			
				AA;BB;CC;UU;EE;VV;GG;HH;WW;DD;TT

			# Calculation of mean longitude of sun 'SunLDeg/SunlRad', equation of time 'ET' [secs],
			# tangens of ecliptic obliquity 'tanEcl', apparent right ascension of sun 'RA' [deg],
			# apparent declination of sun 'Decl' [deg] and great hour angle 'GHA' [deg] (Komhyr p. 104)

		  SunlDeg = 279.697 + 36000.769 * TT
		  while (SunlDeg > 360)  SunlDeg = SunlDeg-360

		  SunlRad = SunlDeg*ufac
		  
		  T2 = TT * TT
		  set1 = -(93 + 14.23 * TT - 0.0144 * T2) * sin(SunlRad)
		  cet1 = -(432.5 - 3.71 * TT - 0.2063 * T2) * cos(SunlRad)
		  set2 = (596.9 - 0.81 * TT - 0.0096 * T2) * sin(SunlRad*2)
		  cet2 = (1.4 + 0.28 * TT) * cos(SunlRad*2)
		  set3 = (3.8 + 0.6 * TT) * sin(SunlRad*3)
		  cet3 = (19.5 - 0.21 * TT - 0.0103 * T2) * cos(SunlRad*3)
		  set4 = -(12.8 - 0.03 * TT) * sin(SunlRad*4)
		  
		  ET = set1 + cet1 + set2 + cet2 + set3 + cet3 + set4
		  tanEcl = 0.43382 - 0.00027 * TT
		  ecl = atan(tanEcl) / ufac
		  RA = SunlDeg - ET / 240
		  DeclRad = (atan(tanEcl * sin(RA * ufac)))
		  DeclDeg = DeclRad / ufac
		  GHA = TimeHr * 15 + ET / 240 + 180

				SunlDeg;SunlRad;ET;ecl;RA;DeclRad;DeclDeg;GHA
			
			# Calculation of local time correction [hr], solar time correction 'TSTCor' [hr],
			# true solar time 'TST' [hr] and hour angle 'HARrad' [rad]

		  LocalTimeCorr = (StationLongitude)/15
		  TSTcor = LocalTimeCorr + ET / 3600
		  TST = TimeHr + TSTcor
		  HARdeg = (TST * 15 - 180)
		  HARrad = HARdeg * ufac
		  
			 LocalTimeCorr;TSTcor;TST;HARdeg;HARrad
			
			# Calculation of solar zenith angle SZA (Komhyr pp 121) and Sunheight [deg]

		  cosSZA = cos(HARrad) * cos(DeclRad) * cos(LatRad) + sin(DeclRad) * sin(LatRad)
		  SZArad = acos(cosSZA)
		  SZAdeg = SZArad/ufac
		  Sunheight = 90 - SZAdeg

			 SZArad;SZAdeg;Sunheight

			# Calculation of airmass and Bemporad (m/mu)

		  O3Factor = (RadiusEarth + StaHeight) / (RadiusEarth + O3Layer)
		  O3Factor = O3Factor*O3Factor
		  AtmFactor = (RadiusEarth + StaHeight) / (RadiusEarth + AtmLayer)
		  AtmFactor = AtmFactor*AtmFactor
		  AirmassMu = 1 / sqrt(1 + O3Factor * (cosSZA * cosSZA - 1))
		  AirmassM   = 1 / sqrt(1 + AtmFactor * (cosSZA * cosSZA - 1))
		  Bemporad  = AirmassM/(AirmassMu)

			 AirmassMu;AirmassM;Bemporad
		
			# Write data on output file
			#
			# jdn Date AA BB CC UU EE VV GG HH WW DD TT SunlDeg SunlRad SZA
			
			outl = sprintf ("%03i%s%s", JDN, " ", DateStr)
			outl = sprintf ("%s%7.3f%3.0f%11.2f%8.0f%8.3f%4.0f%6.2f%6.2f", outl, AA, BB, CC, UU, EE, VV, GG, HH)
			outl = sprintf ("%s%4.0f%6.0f%7.4f%9.4f%7.4f%7.3f", outl, WW, DD, TT, SunlDeg, SunlRad, SZAdeg)
			outl = sprintf ("%s%s", outl, "\n")
			cat (outl, file=df)

		}  # for day=1:mdays
	}  # for mon=1:12

	close(df)  # close output file 'KomhyrYearR_yyyy.txt'
	
}  # end if iniOk
