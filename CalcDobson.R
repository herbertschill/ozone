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
#                               26.11.2021                                  #
#                               27.09.2021                                  #
#                               22.09.2021                                  #
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
#        Modifications by    :  SCHILL Herbert   PMOD/WRC                   #
#                                                                           #
#        Developing System   :  R 4.1.1    (2021)                           #
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
#                                                                           #
#        - 22.09.2021 by Herbert Schill:                                    #
#                                                                           #
#          - add function 'CalculDD'                                        #
#                                                                           #
#                                                                           #
#        - 27.09.2021 by Herbert Schill:                                    #
#                                                                           #
#          - remove function 'SunposKomhyr0'                                #
#                                                                           #
#                                                                           #
#        - 26.11.2021 by Herbert Schill:                                    #
#                                                                           #
#          - add hard boundaries for ozone values in 'CalculDD'             #
#                                                                           #
#############################################################################


CalculDD <- function (iniPar, Calendar, idata, ATconst, RNdata, DNvect)
  
#############################################################################
#                                                                           #
#        Function 'CalculDD' calculates the total ozone values of Dobson    #
#        measurements in the 'O3Dobson'-format of Martin Stanek and in      #
#        the D018 'ADADA' measurement sequence, the day statistics (mean,   #
#        stadev, number of measurements) from the 'idata' list format for   #
#        one day.                                                           #
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
#                 of the day statistics (mean, stadev, number of            #
#                 measurements) of one day; filter function for flags       #
#                 for daily means might have been applied                   #
#                                                                           #
#############################################################################
{
  
atmPos    = 20  # 1st position of atmospheric constants in 'iniPar'-record

UtcCorr   = as.double(iniPar[10])  # Time offset to UTC (for MEZ UtcCorr=-1)
Filtering = as.integer(iniPar[29])

# Default filter values for flag boundaries

FlagMin=0
FlagMax=9

# if filtering of data is required, read filter values as follows:
#
#     Filtering value     effect
#
#              0          no filtering
#              1          filtering by measurement type [DS/ZS/ZC] only
#              2          filtering by [FlagMin, FlagMax] only
#              3          filtering by both type and flag boundaries

if ((Filtering==2) | (Filtering==3))
{
  FlagMin = as.integer(iniPar[31])
  FlagMax = as.integer(iniPar[32])
}

# Reshapes the idata vectors

nmeas = idata[[1]][[4]]  # number of measurements of the day

AirmassAD = array()
Flags     = array()
MeasType  = array()
MeasWave  = array()
NVal      = array()
OzCalc    = array()
OzCalcAD  = array()
RVal      = array()
SZAad     = array()
TimeStr   = array()
TimeStrAD = array()
Wave      = array()
wlXX      = array()
ZPoly     = array()

MeasType = idata[[3]][[2]]
Flags    = idata[[3]][[3]]
wlSeq    = idata[[3]][[4]]
MeasWave = idata[[3]][[5]]
TimeStr  = idata[[3]][[6]]
RVal     = idata[[3]][[7]]
StaDev   = idata[[3]][[8]]
wlXX     = idata[[3]][[11]]
ZPoly    = idata[[2]][[3]]

# Reset day counters for mean, stadev, number of measurements

OzonDay = array()
SDevDay = array()
NumbDay = array()
OzonDay[1:3]=0.0
SDevDay[1:3]=0.0
NumbDay[1:3]=0

for (n in 1:nmeas)  # proceed measurement
{
  Airmass = array()
  Bemp    = array()
  TimeUTC = array()
  SZA     = array()
  
  for  (s in 1:5)  # proceed wavelength sequence (A:w=1, C:w=2, D:w=3)
  {
    k = (n-1)*5+s   # continous vector numbering
    Wave[k]=MeasWave[n,s]
    w = switch(Wave[k], "A"=1, "C"=2, "D"=3)
    
    if  (RVal[n,s]>19.9)
    {
      mHour = as.integer(substr(TimeStr[n,s],1,2))
      mMin  = as.integer(substr(TimeStr[n,s],4,5))
      mSec  = as.integer(substr(TimeStr[n,s],7,8))
      TimeUTC[s] = as.double(mHour+mMin/60+mSec/3600) + UtcCorr  # UTC-correction, if necessary
      
      # R to N conversion and dN correction
      
      if  (RVal[n,s]>289)  RVal[n,s]=289
      
      IR = floor((RVal[n,s])/10.0)
      n1 = RNdata[[w+1]][[IR+1]]
      n2 = RNdata[[w+1]][[IR+2]]
      NVal0   = n1+(n2-n1)*((RVal[n,s]-(IR*10)))/10.0
      NVal[k] = NVal0 + DNvect[w]

      # Calculation of sunheight, SZA, Airmass (mu) and Bemporad (m/mu)

      Sunpos = SunposKomhyrCall (Calendar, TimeUTC[s], iniPar, atmPos)

      SZA[s]     = Sunpos[[2]]  # Solar Zenith Angle [deg]
      Airmass[s] = Sunpos[[4]]  # Airmass Mu
      Bemp[s]    = Sunpos[[5]]  # Bemporad=M/Mu

      # Calculation of the total ozone of a single wavelength measurement, and
      # check if value is within hard boundaries for ozone: 0<OzCalc[n]<600 DU ?


      if  ((Airmass[s]>=1.0)&(ATconst[[2]][[w]]>0.1))
      {
        OzCalc[k] = (10.0*NVal[k]/Airmass[s])/ATconst[[2]][[w]]-ATconst[[4]][[w]]*Bemp[s]
        if  (OzCalc[k]>600.0) OzCalc[k]=599.9
        if  (OzCalc[k]<0.0)   OzCalc[k]=0.0

      } else 
      { OzCalc[k]=0.0 }
    } else  # if RVal[n,s]>19.9
    {
      NVal[k]=0
      Airmass[s]=0
      SZA[s]=0
      Bemp[s]=0
      OzCalc[k]=0.0
      Wave[k]=""
    }  # if RVal[n,s]:19.9
  }  # for s=1..5
  
  # Calculation of the total ozone of the wavelength-combination AD
  # from the single wavelength sequence 'ADADA'

  N1  = mean(NVal[(k-4):k][Wave[(k-4):k]=="A"])
  N2  = mean(NVal[(k-4):k][Wave[(k-4):k]=="D"])
  Mu1 = mean(Airmass[Wave[(k-4):k]=="A"])
  Mu2 = mean(Airmass[Wave[(k-4):k]=="D"])
  Bemp1 = mean(Bemp[Wave[(k-4):k]=="A"])
  Bemp2 = mean(Bemp[Wave[(k-4):k]=="D"])
  Alfa  = ATconst[[2]][[5]]
  Acor  = ATconst[[4]][[5]]
  
  if  ((Mu1>=1.0)&(Mu2>=1.0)&(Alfa>0.1))
  {
    Obs = as.integer(substr(Flags[n],3,3))
    if (Obs==0)
    {
      OzCalcAD[n] = (10.0*N1/Mu1-10.0*N2/Mu2)/Alfa-Acor*(Bemp1/2.0+Bemp2/2.0)
    } else
    {
      X = N1-N2
      Y = (Mu1+Mu2)/2
      XAD = ZPoly[1] + ZPoly[2]*Y + ZPoly[3]*X + ZPoly[4]*Y*Y + ZPoly[5]*X*X + ZPoly[6]*Y*X +
            ZPoly[7]*Y*Y*X + ZPoly[8]*Y*X*X + ZPoly[9]*Y*Y*Y + ZPoly[10]*X*X*X

      zp=21+(Obs-3)*4
      CloudCorr = ZPoly[zp] + ZPoly[zp+1] * XAD + ZPoly[zp+2] * Y + ZPoly[zp+3] * XAD * Y
      OzCalcAD[n] = XAD - CloudCorr

#      CloudCorr = ZCAD1[0] + ZCAD1[1] * XAD + ZCAD1[2] * Y + ZCAD1[3] * XAD * Y
#      zp=21+(Obs-3)*4
#      ZCAD1[0...3] ZPoly[21..24] Flag=3 - uniform stratified layer of small opacity
#      ZCAD2[0...3] ZPoly[25..28] Flag=4 - uniform or moderately variable layer of medium opacity      
#      ZCAD3[0...3] ZPoly[29..32] Flag=5 - uniform or moderately variable layer of large opacity
#      ZCAD4[0...3] ZPoly[33..36] Flag=6 - highly variable opacity, with or without precipitation
#      ZCAD5[0...3] ZPoly[37..40] Flag=7 - fog
    }
    AirmassAD[n] = mean(Airmass)
    SZAad[n] = mean(SZA)
    timeAD = mean(TimeUTC)
    mHour = floor(timeAD)
    mMin  = floor((timeAD-mHour)*60+0.01)
    mSec  = round(timeAD-mHour-mMin/60)
    TimeStrAD[n] = sprintf ("%02d:%02d:%02d", mHour, mMin, mSec)
    wlXX[n] = "AD"
    
    # is measurement within hard boundaries for ozone: 100<OzCalcAD[n]<600 DU ?
    
    if  ((OzCalcAD[n]>100.0) & (OzCalcAD[n]<600.0))
      {MeasValid=1} else
      {
        MeasValid=0
        if  (OzCalcAD[n]>600.0) OzCalcAD[n]=599.9
        if  (OzCalcAD[n]<100.0) OzCalcAD[n]=100.0
      }
    
    # correspond measurement to measurement type, or/and is within flag limits ?
    
    if  ((Filtering==1) | (Filtering==3))
    {
      if  (MeasType[n]!=iniPar[30])  MeasValid=0
    }
    if  ((Filtering==2) | (Filtering==3))
    {
      FlagWW = as.integer(substr(Flags[n],1,1))
      if  ((FlagWW<FlagMin) | (FlagWW>FlagMax))  MeasValid=0
    }
    if  (MeasValid)
    {
      ty = switch(MeasType[n], "DS"=1, "ZC"=2)
      NumbDay[ty] = NumbDay[ty]+1  # summation for day statistics
      OzonDay[ty] = OzonDay[ty]+OzCalcAD[n]
      SDevDay[ty] = SDevDay[ty]+(OzCalcAD[n]*OzCalcAD[n])
      NumbDay[3] = NumbDay[3]+1  # summation for day statistics
      OzonDay[3] = OzonDay[3]+OzCalcAD[n]
      SDevDay[3] = SDevDay[3]+(OzCalcAD[n]*OzCalcAD[n])
    } else
    { Flags[n] = sprintf ("%s%s", "9", substr(Flags[n],2,3)) }

  } else { OzCalcAD[n]=0.0 }

}  # for n=1..nmeas
  
# Reshape arrays
  
dim(NVal)    = c(5,nmeas)
dim(OzCalc)  = c(5,nmeas)
NVal=t(NVal)
OzCalc=t(OzCalc)

# Day statistics (mean, stadev, number of measurements of AD pairs)

for (w in 1:3)
{
  if (NumbDay[w]>0)
  {
    OzonDay[w] = OzonDay[w]/NumbDay[w]
    if (NumbDay[w]>1)
    {
      SDevDay[w] = sqrt((1.0/(NumbDay[w]-1))*(SDevDay[w]-(OzonDay[w]*OzonDay[w]*NumbDay[w])))
      if  (SDevDay[w]>99.99)  SDevDay[w]=99.99
    } else
    { SDevDay[w]=0.0 }
  }
}  

# Write data in lists

HeaderValues = idata[[1]]

Tables = list(DNvect, RNdata, idata[[2]][[3]], idata[[2]][[4]])
names(Tables) = c("dNset", "RNtab", "Zpoly", "EmpCor")

Measures = list(nmeas, idata[[3]][[2]], Flags, idata[[3]][[4]], idata[[3]][[5]], idata[[3]][[6]], idata[[3]][[7]], idata[[3]][[8]], 
                NVal, OzCalc, wlXX, TimeStrAD, AirmassAD, SZAad, OzCalcAD, idata[[3]][[16]])
names(Measures) = c("NumberMeas", "mType", "Flags", "wlSeq", "wlX", "timeStr", "RVal", "dR", 
                    "NVal", "XVal", "wlXX", "timeStrXX", "Airmass", "SZA", "XVal2", "CommStr")

DayStat = list(OzonDay, NumbDay, SDevDay)
names(DayStat) = c("OzoneDay", "NumberDay", "StaDevDay")

return(list("HeaderData"=HeaderValues, "Tables"=Tables, "Measurements"=Measures, "DayStatistics"=DayStat))
  
}  # end of function 'CalculDD'
    
    
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
