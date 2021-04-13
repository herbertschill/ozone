#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE CALCULATION                        #
#                                                                           #
#                                                                           #
#        Module Name         :  CalcDobson.R                                #
#                                                                           #
#                                                                           #
#        Creation Date       :  29.07.2009                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               06.12.2017                                  #
#                               27.12.2016                                  #
#                               21.10.2016                                  #
#                               24.08.2016                                  #
#                               16.08.2016                                  #
#                               07.03.2016                                  #
#                               23.02.2016                                  #
#                               26.11.2015                                  #
#                               16.10.2012                                  #
#                               04.08.2011                                  #
#                               26.10.2010                                  #
#                               02.09.2010                                  #
#                               02.11.2009                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'CalcDobson.R' contains functions for the calculation of the       #
#        total ozone values of Dobson measurements from the 'idata' list    #
#        format for one day.                                                #
#                                                                           #
#                                                                           #
#        List of functions:                                                 #
#                                                                           #
#          - CalculX                                                        #
#          - SunposKomhyr                                                   #
#          - zenith_angle    Author: Florian Pantillon (paf)                #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 02.11.2009 by Herbert Schill:                                    #
#                                                                           #
#          Implement day statistics (mean, stadev, number of measurements)  #
#          in 'CalculX' and add values to 'cdata' list                      #
#                                                                           #
#                                                                           #
#        - 02.09.2010 by Herbert Schill:                                    #
#                                                                           #
#          Remove of bug in elapsed time in 'SunposKomhyr'                  #
#                                                                           #
#                                                                           #
#        - 26.10.2010 by Herbert Schill:                                    #
#                                                                           #
#          - modify structure 'sunList' (include TSTcor)                    #
#                                                                           #
#          - extensions for calculation of max. sunhei (time, height)       #
#                                                                           #
#          - extensions for calculating halfday means (value, nmeas)        #
#                                                                           #
#                                                                           #
#        - 04.08.2011 by Herbert Schill:                                    #
#                                                                           #
#          Add filter function for sun intensity or/and flags for           #
#          halfday and daily mean                                           #
#                                                                           #
#                                                                           #
#        - 16.10.2012 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug for asign proper 'Flag' idata vector                #
#                                                                           #
#          - set lower RVal-boundary from 3.0 to 19.9 for ozone             #
#            calculation                                                    #
#                                                                           #
#          - set hard boundaries for ozone: 100<Ozone<600 DU for            #
#            day statistics (same as in 'WriteDobsonAE')                    #
#                                                                           #
#          - remove bug for proper filtering by flags for halfday and       #
#            daily means                                                    #
#                                                                           #
#                                                                           #
#        - 26.11.2015 by Herbert Schill:                                    #
#                                                                           #
#          Remove of parameter 'ATconst' in 'SunposKomhyr' [not used]       #
#                                                                           #
#                                                                           #
#        - 23.02.2016 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameter indices to newest version of ini-file          #
#                                                                           #
#          - add multiple instrument proceeding                             #
#                                                                           #
#          - adapt 'CalculX' for extended output information                #
#                                                                           #
#                                                                           #
#        - 07.03.2016 by Herbert Schill:                                    #
#                                                                           #
#          - allow flexible parameter list for atmospheric constants        #
#            in 'SunposKomhyr'; adapt calls in 'CalculX'                    #
#                                                                           #
#                                                                           #
#        - 16.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - set flag=4 for single wavelength ozone values outside          #
#            hardcoded boundaries in 'CalculX'                              #
#                                                                           #
#                                                                           #
#        - 24.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - rename 'data' to 'idata' in 'CalculX'                          #
#                                                                           #
#                                                                           #
#        - 21.10.2016 by Herbert Schill:                                    #
#                                                                           #
#          - add function 'zenith_angle' of Florian Pantillon (paf)         #
#                                                                           #
#                                                                           #
#        - 27.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          - rename 'SunposKomhyr' to 'SunposKomhyr0'; adapt calls          #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - call 'SunposKomhyr' from program file 'DateZeit.R' via         #
#            'SunposKomhyrCall'                                             #
#                                                                           #
#############################################################################


CalculX <- function (iniPar, Calendar, idata, ATconst, RNdata, DNvect)

#############################################################################
#                                                                           #
#        Function 'CalculX' calculates the total ozone values of Dobson     #
#        measurements, the day statistics (mean, stadev, number of          #
#        measurements) and the halfday statistics (mean, quality flag)      #
#        from the 'idata' list format for one day.                          #
#                                                                           #
#        Function call: CalcDobson (iniPar, idata, ATconst, RNdata, DNvect) #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        iniPar    List of input parameters from 'Recalcoz.ini'             #
#        Calendar  Array containing day, month, year, jdn, leap             #
#        idata     List of raw and calculated measurement data of one day   #
#        ATconst   List of atmospheric constants                            #
#        RNdata    List of R/N conversion tables                            #
#        DNvect    Vector of dN-correction values for waveleghts A, C, D    #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        cdata    List of raw and (re)calculated measurement data and       #
#                 of the halfday (only AD wavelength pair) and day          #
#                 statistics (mean, stadev, number of measurements) of      #
#                 one day; filter function for sun intensity or/and flags   #
#                 for halfday and daily mean might have been applied        #
#                                                                           #
#############################################################################
{

atmPos    = 20  # 1st position of atmospheric constants in 'iniPar'-record

UtcCorr   = as.double(iniPar[10])  # Time offset to UTC (for MEZ UtcCorr=-1)
Komhyr    = as.integer(iniPar[17])
Filtering = as.integer(iniPar[28])

# Default filter values for sun and flag boundaries

SunMin=10
SunMax=80
FlagMin=0
FlagMax=9

# if filtering of data is required, read filter values as follows:
#
#     Filtering value     effect
#
#              0          no filtering (use default values)
#              1          filtering by [SunMin, SunMax] only
#              2          filtering by [FlagMin, FlagMax] only
#              3          filtering by both sun and flag boundaries

if ((Filtering==1) | (Filtering==3))
{
	SunMin = as.integer(iniPar[29])
	SunMax = as.integer(iniPar[30])
}
if ((Filtering==2) | (Filtering==3))
{
	FlagMin = as.integer(iniPar[31])
	FlagMax = as.integer(iniPar[32])
}

# Reshapes the idata vectors

nmeas = idata[[2]][[1]]  # number of measurements of the day

Sun       = array()
InstrTemp = array()
TimeStr   = array()
MeasTime  = array()
NVal      = array()
RVal      = array()
StaDev    = array()
Flag      = array()
Airmass   = array()
Ca        = array()
SZA       = array()
OzCalc    = array()
OzonDay   = array()
OzonHD    = array()
NumbDay   = array()
NumbHD    = array()
SDevDay   = array()
#NValMu    = array()
#Sunhei    = array()
#TST       = array()
#TSTcorr   = array()

MeasTime = idata[[2]][[5]]
RVal     = idata[[2]][[6]]
StaDev   = idata[[2]][[7]]
Flag     = idata[[2]][[8]]

dim(MeasTime) = c(3,nmeas)
dim(RVal)     = c(3,nmeas)
dim(Flag)     = c(3,nmeas)

# Calculates time and value of maximal sunheight of the true noon 
# of the day, using the Komhyr algorithms

StationLongitude = as.double(iniPar[[20]])
TimeHMax = 12-StationLongitude/15
Sunpos = SunposKomhyrCall (Calendar, TimeHMax, iniPar, atmPos)
TimeHMax = 12-Sunpos[[1]]
Sunpos = SunposKomhyrCall (Calendar, TimeHMax, iniPar, atmPos)
SunheiMax = Sunpos[[3]]

# Calculates R/N conversion, airmass 'My', sunhight 'High',
# solar zenith angle 'SZA' and atmospheric correction 'Ca'
# for all measurements and for each of the wavelengths C, D, A

# Reset day counters for mean, stadev, number of measurements

for (w in 1:6)
{
	OzonDay[w]= 0.0
	SDevDay[w]= 0.0
	NumbDay[w]= 0
}

# Reset halfday counters for mean and number of measurements

for (w in 1:2)
{
	OzonHD[w]= 0.0
	NumbHD[w]= 0
}

for (n in 1:nmeas)  # proceed measurement
{
	Sun = idata[[2]][[2]][[n]]

	for  (w in 1:3)  # proceed wavelength (C:w=1, D:w=2, A:w=3)
	{
		k = (n-1)*3+w   # continous vector numbering
		j = (w %% 3)+1  # reverse order for reading RN-table and dN-values

		if  (RVal[w,n]>19.9)
		{
			TimeUTC = MeasTime[w,n] + UtcCorr  # UTC-correction, if necessary

			# R to N conversion and dN correction
			
			if  (RVal[w,n]>289)  RVal[w,n]=289

			IR = floor((RVal[w,n])/10.0)
			n1 = RNdata[[j+1]][[IR+1]]
			n2 = RNdata[[j+1]][[IR+2]]
			NVal0   = n1+(n2-n1)*((RVal[w,n]-(IR*10)))/10.0
			NVal[k] = NVal0 + DNvect[j]
			#RVal[w,n];IR;n1;n2;NVal0;DNvect[j];NVal[k]

 			# Calculation of sunheight, SZA, airmass (mu) and bemporad (m/mu):
			#
			# if 'Komhyr'=1, sunheight, airmass and bemporad are calculated according
			# the formulae of "Walter D. Komhyr, WMO Report No. 6, June 1980",
			# otherwise, sunheight, airmass and Bemporad are calculated by using the 
			# formulae from the R-functions developped 2008 by Florian Pantillon
			# based on the Brewer software algorithms, but adding Bemporad correction

			if  (Komhyr)
			{
				Sunpos = SunposKomhyrCall (Calendar, TimeUTC, iniPar, atmPos)

				#TSTcorr[k] = Sunpos[[1]]  # true solar time correction [hr]
				#Sunhei[k]  = Sunpos[[3]]  # Elevation [deg]
				#TST[k]     = Sunpos[[6]]  # true solar time [hr]
				SZA[k]     = Sunpos[[2]]  # Solar Zenith Angle [deg]
				Airmass[k] = Sunpos[[4]]  # Airmass Mu
				Ca[k]      = Sunpos[[5]]  # Bemporad=M/Mu
				#NValMu[k]   = NVal[k]/Airmass[k]
			}
			else  # use function 'zenith_angle' of F. Pantillon for SZA and Airmass
			{
				jdn           = Calendar[4]
				year          = Calendar[3]
				longitude     = as.double(iniPar[[20]])
				latitude      = as.double(iniPar[[21]])
				StaHeight     = as.double(iniPar[[22]])
				O3Layer       = as.double(iniPar[[24]])
				AtmLayer      = as.double(iniPar[[25]])
				RadiusEarth   = as.double(iniPar[[26]])
				TimeMin       = TimeUTC*60
				if (year<2000)
					syear = year-1900
				else
					syear = year-2000
				
				SZA[k]     = zenith_angle (TimeMin, jdn, syear, latitude, -longitude)
				Airmass[k] = 1/cos(asin(RadiusEarth/(RadiusEarth+O3Layer)*sin(SZA[k]*pi/180)))
				m          = 1/cos(asin(RadiusEarth/(RadiusEarth+AtmLayer)*sin(SZA[k]*pi/180)))
				Ca[k]      = m/Airmass[k]   # Bemporad correction
			}  # end if Komhyr
		} else
		{
			NVal[k]=0
			Airmass[k]=0
			SZA[k]=0
			Ca[k]=0
		}  # if RVal[w,n]:19.9
	}  # for w=1..3

	# Calculation the total ozone of a single wavelength measurement C, D, A
	# or of a wavelength-combination measurement AD, CD, AC

	for  (w in 1:6)
	{
		i1 = switch (w, 1,2,3,3,1,3)
		i2 = switch (w, 1,2,3,2,2,1)
		j  = switch (w, 2,3,1,5,6,4)  # reverse order for using Alfa and Acor

		N1=NVal[(n-1)*3+i1]
		Ca1=Ca[(n-1)*3+i1]
		Ca2=Ca[(n-1)*3+i2]
		if (w<4) { N2=0 } else { N2=NVal[(n-1)*3+i2] }

		Mu1=Airmass[(n-1)*3+i1]
		Mu2=Airmass[(n-1)*3+i2]
		Alfa=ATconst[[2]][[j]]
		Acor=ATconst[[4]][[j]]

		if  ((Mu1>=1.0)&(Mu2>=1.0)&(Alfa>0.1))
		{
			OzoneA = (10.0*N1/Mu1-10.0*N2/Mu2)/Alfa-Acor*(Ca1/2.0+Ca2/2.0)
			OzCalc[(n-1)*6+w] = OzoneA

			# is measurement within hard boundaries for ozone: 100<OzoneA<600 DU ?

			if  ((OzoneA>100.0) & (OzoneA<600.0))
				MeasValid=1
			else
				MeasValid=0

			# is measurement within sun intensity or/and flag limits ?

			if  ((Filtering==1) | (Filtering==3))
			{
				if  ((Sun<SunMin) | (Sun>SunMax))  MeasValid=0
			}
			if  ((Filtering==2) | (Filtering==3))
			{
				if  (Flag[(n-1)*3+i1]>=Flag[(n-1)*3+i2])
					FlagW=Flag[(n-1)*3+i1]
				else
					FlagW=Flag[(n-1)*3+i2]
				if  ((FlagW<FlagMin) | (FlagW>FlagMax))  MeasValid=0
			}

			if  (MeasValid)
			{
				NumbDay[w] = NumbDay[w]+1  # summation for day statistics
				OzonDay[w] = OzonDay[w]+OzoneA
				SDevDay[w] = SDevDay[w]+(OzoneA*OzoneA)

				if  (w==4)  # summation for halfday statistics for w=AD only
				{
					TimeUTC = (MeasTime[2,n] + MeasTime[3,n])/2 + UtcCorr  # UTC-correction, if necessary
					if  (TimeUTC<=TimeHMax)  # summation for AM
					{
						NumbHD[1] = NumbHD[1]+1
						OzonHD[1] = OzonHD[1]+OzoneA
					}
					else  # summation for PM
					{
						NumbHD[2] = NumbHD[2]+1
						OzonHD[2] = OzonHD[2]+OzoneA
					}
				}  # end if w=4
			}  # if MeasValid=1
		} else OzCalc[(n-1)*6+w]=0.0

	}  # for w=1..6

	# if single wave ozone is outside hard boundaries [100:600] DU, flag[wl] is set to 4

	for  (w in 1:3)
	{
		if (OzCalc[(n-1)*6+w]<100) Flag[w,n]=4
	}

}  # for n=1..nmeas

# Reshape arrays

dim(Airmass) = c(3,nmeas)
dim(Ca)      = c(3,nmeas)
dim(NVal)    = c(3,nmeas)
dim(SZA)     = c(3,nmeas)
dim(OzCalc)  = c(6,nmeas)

# Day statistics (mean, stadev, number of measurements)

for  (w in 1:6)
{
	if  (NumbDay[w]>0)
	{
		OzonDay[w] = OzonDay[w]/NumbDay[w]
		if  (NumbDay[w]>1)
			{
			SDevDay[w] = (1.0/(NumbDay[w]-1))*(SDevDay[w]-(OzonDay[w]*OzonDay[w]*NumbDay[w]))
			SDevDay[w] = sqrt(SDevDay[w])
			if  (SDevDay[w]>99.99)  SDevDay[w]=99.99
			}
		else
			SDevDay[w] = 0.0
	}
}  # for w=1..6

# Halfday statistics for wl=AD (AM=1, PM=2)

for  (n in 1:2)
{
	if  (NumbHD[n]>0)  OzonHD[n] = OzonHD[n]/NumbHD[n]
}

# Write data in lists

HeaderValues = idata[[1]]
names(HeaderValues) = c("Dobson-Id", "TypeMess", "DateMess", "NumberMess", "UTC-Corr", "VersionMess")

Measures = list(nmeas, idata[[2]][[2]], idata[[2]][[3]], idata[[2]][[4]], idata[[2]][[6]], idata[[2]][[7]], Flag, NVal, Airmass,
                OzCalc, Ca, SZA)
names(Measures) = c("NumberMess", "Sun", "InstrTemp", "TimeStr", "R-Values", "StaDevs", "Flags", "N'-Values", "Airmass", 
                    "OzCalc", "Bemporad", "SZA")

DayStat = list(OzonDay, NumbDay, SDevDay, OzonHD, NumbHD)
names(DayStat) = c("OzonDay", "NumberDay", "StaDevDay", "OzonHD", "NumbHD")

return(list("Header data"=HeaderValues,"Measurements"=Measures, "DayStatistics"=DayStat))

}  # end of function 'CalculX'


SunposKomhyrCall <- function (Calendar, TimeHr, iniPar, atmPos)

#############################################################################
#                                                                           #
#        Function 'SunposKomhyrCall' expands the parameters for the proper  #
#        call of function 'SunposKomhyr', which calculates the position of  #
#        the sun (SZA, Sunheigh, Airmass) for a specific time, date and     #
#        location according the formulae of "Walter D. Komhyr, WMO Report   #
#        No. 6, June 1980"                                                  #
#                                                                           #
#        Function call: SunposKomhyrCall (Calendar,TimeHr,iniPar,atmPos)    #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Calendar  Array containing day, month, year, jdn, leap             #
#        TimeHr    time in UTC of format 'h.h' (hour, fraction of it)       #
#        iniPar    List of input parameters from 'RecalcAE.ini'             #
#        atmPos    1st position of atmospheric constants in 'iniPar'-record #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        List containing TSTcorr, SZA, Sunheight, Airmass, Bemporad         #
#                                                                           #
#          TSTcorr    True Solar Time Correction [hrs]                      #
#          SZA        Solar Zenith Angle [deg]                              #
#          Sunheight  90-SZA                                                #
#          Airmass    mu                                                    #
#          Bemporad   m/mu                                                  #
#                                                                           #
#############################################################################
{

	# Proper setting of calendar values and station parameters

	InDay  = Calendar[1]
	InMon  = Calendar[2]
	InYear = Calendar[3]

	StaLong     = as.double(iniPar[[atmPos]])
	StaLat      = as.double(iniPar[[atmPos+1]])
	StaHeight   = as.double(iniPar[[atmPos+2]])
	O3Layer     = as.double(iniPar[[atmPos+4]])
	AtmLayer    = as.double(iniPar[[atmPos+5]])
	RadiusEarth = as.double(iniPar[[atmPos+6]])

	sunList = SunposKomhyr (InDay, InMon, InYear, TimeHr, StaLong, StaLat, 
                          StaHeight, O3Layer, AtmLayer, RadiusEarth)

	return (sunList)

}  # end of function 'SunposKomhyrCall'


SunposKomhyr0 <- function (Calendar, TimeHr, iniPar, atmPos)

#############################################################################
#                                                                           #
#        Function 'SunposKomhyr0' calculates the position of the sun        #
#        (SZA, Sunheigh, Airmass) for a specific time, date and location    #
#        according the formulae of "Walter D. Komhyr, WMO Report No. 6,     #
#        June 1980"                                                         #
#                                                                           #
#        Function call:  SunposKomhyr0 (Calendar, TimeHr, iniPar, atmPos)   #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Calendar  Array containing day, month, year, jdn, leap             #
#        TimeHr    time in UTC of format 'h.h' (hour, fraction of it)       #
#        iniPar    List of input parameters from 'Recalcoz.ini'             #
#        atmPos    1st position of atmospheric constants in 'iniPar'-record #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        List containing TSTcorr, SZA, Sunheight, Airmass, Bemporad         #
#                                                                           #
#          TSTcorr    True Solar Time Correction [hrs]                      #
#          SZA        Solar Zenith Angle [deg]                              #
#          Sunheight  90-SZA                                                #
#          Airmass    mu                                                    #
#          Bemporad   m/mu                                                  #
#                                                                           #
#                                                                           #
#############################################################################
{

	# Initialisation
	#
	#   Pi = 3.14159265358979
	#
	#   ufac = pi/180.0, deg-rad conversion factor

	ufac = 0.017453293

	# Proper setting of calendar values

	InDay  = Calendar[1]
	InMon  = Calendar[2]
	InYear = Calendar[3]

	# Get station parameters, height of the ozone layer and of the atmosphere
	# and the earth radius [km]
 
	StationLongitude = as.double(iniPar[[atmPos]])
	StationLatitude  = as.double(iniPar[[atmPos+1]])
	StaHeight        = as.double(iniPar[[atmPos+2]])
	O3Layer          = as.double(iniPar[[atmPos+4]])
	AtmLayer         = as.double(iniPar[[atmPos+5]])
	RadiusEarth      = as.double(iniPar[[atmPos+6]])

	LatRad = StationLatitude*ufac

	# Calculation of time elapsed in days 'DD', since 00.01.1900 12:00 UTC
	# and number of Julian centuries 'TT' from 1900.0 (Julian date 2415020.0)
	# (Komhyr p. 105; "INT" of Komhyr has to be replaced by "trunc" resp.
	# "floor" commands for a proper calculation of negative numbers)

	AA = (InMon - 14) / 12
	BB = trunc(AA)
	CC = (1461 * (InYear + 4800 + BB)) / 4
	UU = floor(CC)                          # modif. 02.09.2010
	EE = 367 * (InMon - 2 - 12 * BB) / 12
	VV = trunc(EE)
	GG = (InYear + 4900 + BB) / 100
	HH = 3 * trunc(GG) / 4
	WW = -trunc(HH)
	DD = InDay - 2447095.5 + UU + VV + WW + TimeHr / 24
	TT = DD / 36525

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

	# Calculation of local time correction [hr], solar time correction 'TSTCor' [hrs],
	# true solar time 'TST' [hrs] and hour angle 'HARrad' [rad]

  LocalTimeCorr = (StationLongitude)/15
  TSTcor = LocalTimeCorr + ET / 3600
  TST = TimeHr + TSTcor
  HARdeg = (TST * 15 - 180)
  HARrad = HARdeg * ufac
  
	# Calculation of solar zenith angle SZA (Komhyr pp 121) and Sunheight [deg]

  cosSZA = cos(HARrad) * cos(DeclRad) * cos(LatRad) + sin(DeclRad) * sin(LatRad)
  SZArad = acos(cosSZA)
  SZAdeg = SZArad/ufac
  Sunheight = 90 - SZAdeg

	# Calculation of airmass and Bemporad (m/mu)

  O3Factor = (RadiusEarth + StaHeight) / (RadiusEarth + O3Layer)
  O3Factor = O3Factor*O3Factor
  AtmFactor = (RadiusEarth + StaHeight) / (RadiusEarth + AtmLayer)
  AtmFactor = AtmFactor*AtmFactor
  AirmassMu = 1 / sqrt(1 + O3Factor * (cosSZA * cosSZA - 1))
  AirmassM   = 1 / sqrt(1 + AtmFactor * (cosSZA * cosSZA - 1))
  Bemporad  = AirmassM/(AirmassMu)

	# Inverse function: MU->sh (for test purposes)
	#
	#mu2=AirmassMu*AirmassMu
	#sza = acos(sqrt((1/(mu2)-1)/O3Factor+1))
	#sh = 90 - sza/(pi/180.0)
	#mu;sh

	# Write data in lists

	sunList = list(TSTcor, SZAdeg, Sunheight, AirmassMu, Bemporad, TST)
	names(sunList) = c("TSTcorr", "SZA", "Sunhei", "Airmass", "Bemporad", "TrueSolarTime")
	return (sunList)

}  # end of function 'SunposKomhyr0'


zenith_angle <- function(t0,day,year,latitude,longitude)

##########################################################################
#                                                                        #
# FUNCTION ZENITH_ANGLE(T0,DAY,YEAR,LATITUDE,LONGITUDE)                  #
#                                                                        #
# INPUT: TIME [MIN], DAY OF YEAR, YEAR [YY], LATITUDE [°], LONGITUDE [°] #
# OUTPUT: ZENITH ANGLE [°] COMPUTED WITH BREWER SUN POSITION ALGORITHM   #
#                                                                        #
# CREATION: PAF! 26.6.08                                                 #
#                                                                        #
##########################################################################

{

	# LEAP YEAR CORRECTION

	if (year/4==floor(year/4)) t <- -1 else t <- 0

	# YEARS SINCE 1965

	if (year<65) {
		t <- (t+day+floor(year/4)+(year+35)*365+9)/365.2422
	} else {
		t <- (t+day+floor(year/4)+(year-65)*365-16)/365.2422
	}

	# COORDINATES OF THE EARTH

	p0 <- pi/180
	i <- (279.4574+360*t+t0/1460.97)*p0
	e <- 4.2*sin(3*i)-2*cos(2*i)+596.5*sin(2*i)-12.8*sin(4*i)+19.3*cos(3*i)
	e <- e-(102.5+0.142*t)*sin(i)+(0.033*t-429.8)*cos(i)

	# DECLINATION AND RIGHT ASCENSION

	ra <- (t0+e/60+720-longitude*4)*p0/4
	a <- atan(0.4336*sin(i-e*p0/240))

	# ZENITH ANGLE

	e <- cos(ra)*cos(a)*cos(latitude*p0)+sin(latitude*p0)*sin(a)

	ep <- 0.999999999
	if (e>=1) e <- ep else if (e<=-1) e <- -ep else if (e==0) e <- 0.0000001

	e <- acos(e)

	if (e<0) e <- e+pi

	return (e/p0)

}  # end of function 'zenith_angle'
