#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Module Name         :  TreatDobsonData.R                           #
#                                                                           #
#                                                                           #
#        Creation Date       :  02.02.2017                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               29.07.2020                                  #
#                               15.06.2020                                  #
#                               08.11.2019                                  #
#                               30.01.2019                                  #
#                               09.01.2019                                  #
#                               06.09.2018                                  #
#                               07.05.2018                                  #
#                               04.05.2018                                  #
#                               09.03.2018                                  #
#                               14.02.2018                                  #
#                               27.01.2018                                  #
#                               24.01.2018                                  #
#                               21.01.2018                                  #
#                               20.01.2018                                  #
#                               28.12.2017                                  #
#                               19.12.2017                                  #
#                               12.12.2017                                  #
#                               06.12.2017                                  #
#                               21.10.2017                                  #
#                               26.03.2017                                  #
#                               23.02.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.4.2    (2017)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'TreatDobsonData.R' contains functions for the reading, writing    #
#        and processing of total ozone values of Dobson measurements from   #
#        and to different data formats:                                     #
#                                                                           #
#          - 'AEyyyymmdd.iii'-files  (single day file)                      #
#                                                                           #
#          - 'AXyyyymmdd.iii'-files  (extended output information)          #
#                                                                           #
#          - 'DAYOZON_yyyy.iii' (daily values output file (1-line format))  # 
#          - 'DAYOZyyN.iii'     (daily values output file (4-line format))  #
#                                                                           #
#          - 'HDOZON_yyyy.iii'  (halfday values (1-line format))            #
#                                                                           #
#                                                                           #
#        List of functions:                                                 #
#                                                                           #
#          - GetUmkTotoz                                                    #
#          - ManuFlagAE                                                     #
#          - ReadDobsonAE                                                   #
#          - ReadDobsonLV                                                   #
#          - ReadDobsonUM                                                   #
#          - ReadLuxfile                                                    #
#          - ReduceAE                                                       #
#          - SetFlagsAE                                                     #
#          - TreatDobsonUM                                                  #
#          - UpdateLuxfile                                                  #
#          - WriteLuxfile                                                   #
#          - WriteDobsonAE                                                  #
#          - WriteDobsonCX                                                  #
#          - WriteDobUmkpre                                                 #
#          - WriteDayStatis                                                 #
#          - WriteDayoz                                                     #
#          - WriteHalfDayStatis                                             #
#                                                                           #
#                                                                           #
#         Former name of file was 'TreatDobsonAE.R' (created 29.07.2009)    #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 23.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - remove bugs in 'ManuFlagAE' (proper indexing of flags)         #
#                                                                           #
#                                                                           #
#        - 26.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt 'ReadDobsonLV' to recent D-file data contents            #
#                                                                           #
#                                                                           #
#        - 21.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat' in all procedures           #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - add procedures 'GetUmkTotoz', 'ReadLuxfile', 'UpdateLuxfile'   #
#            'WriteLuxfile', 'TreatDobsonUM' and 'WriteDobsonCX'            #
#                                                                           #
#          - remove bug in 'ReadDobsonLV': proper last line control and     #
#            more detailed error messages                                   #
#                                                                           #
#                                                                           #
#        - 12.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'UpdateLuxfile' (cdate)                          #
#          - allow 'Cx' header choice in 'WriteDobsonCX'                    #
#                                                                           #
#                                                                           #
#        - 19.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'ReduceAE': measurements with an airmass bigger than           #
#            maxAirmass resp. a sun intensity lower than minSun are also    #
#            filtered                                                       #
#                                                                           #
#                                                                           #
#        - 28.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': if 'AX'-output format is required, the        #
#            R- and Ozone-values are written with higher precicion          #
#                                                                           #
#                                                                           #
#        - 20.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'TreatDobsonUM': proper test on minimal and maximal sunhei in  #
#            range [0°..MinSunhei°], MinSunhei hardcoded                    #
#                                                                           #
#                                                                           #
#        - 21.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - Procedure 'WriteDobUmkpre' added                               #
#                                                                           #
#                                                                           #
#        - 24.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'TreatDobsonUM': remove bug in proper test on minimal and      #
#            maximal sunhei in range [0°..20°]                              #
#                                                                           #
#                                                                           #
#        - 27.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'TreatDobsonUM': allow filtering of measurements with          #
#            StaDevR>LimitSDevR                                             #
#                                                                           #
#          - 'TreatDobsonUM': reorganising error message structure          #
#                                                                           #
#          - 'ReadDobsonUM': include HV-values in data-list                 #
#                                                                           #
#                                                                           #
#        - 14.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': R-values are written with 2-digit precicion   #
#                                                                           #
#                                                                           #
#        - 09.03.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': check on maximal SDev-value of 99.99          #
#                                                                           #
#                                                                           #
#        - 04.05.2018 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'ReadDobsonLV': proper error message setting     #
#                                                                           #
#                                                                           #
#        - 07.05.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'ReadDobsonAE': read all ozone values (5 or 6) from input-file #
#                                                                           #
#                                                                           #
#        - 06.09.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': headerline format depending of dN-values      #
#                                                                           #
#                                                                           #
#        - 09.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': remove bug for proper airmass-values my:      #
#            if R<=19.9 then my='/' instead of my=0.000                     #
#                                                                           #
#                                                                           #
#        - 30.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          - 'ReadDobsonAE': minor format change in info output             #
#                                                                           #
#        - 08.11.2019 by Herbert Schill:                                    #
#                                                                           #
#          - remove unused parameter 'iniPar' in some functions             #
#                                                                           #
#                                                                           #
#        - 15.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': simplify variable use in output statements    #
#                                                                           #
#                                                                           #
#        - 29.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - 'WriteDobsonAE': if Fmt="AX" write also ozone values outside   #
#            the hardcoded boundaries [100..600]                            #
#                                                                           #
#############################################################################


GetUmkTotoz <- function (day, mon, year, span, TrueNoonHr, DobsTotoz, 
                         CalTotoz, PathTotoz0)

#############################################################################
#                                                                           #
#       'GetUmkTotoz' gets the total ozone half day means of valid (i.e.    #
#       unflagged) AD-measurements of the choosen day and Dobson from the   #
#       'AEyyyymmdd.iii'-file; if no valid total ozone for this day is      #
#       available, the closest days before and after the date (max. +/-span #
#       days) are searched and the daily means are interpolated. 'OZqual'   #
#       is set accordingly for the half days:                               #
#                                                                           #
#         OZqual  status of total ozone                                     #
#                                                                           #
#            0    halfday means at day of Umkehr found                      #
#            1    daily mean at day of Umkehr found                         #
#            2    daily means at days before and after day of Umkehr found  #
#            3    no total ozone found within the period                    #
#                                                                           #
#       Output:                                                             #
#                                                                           #
#         list with OZqual, OZmeanAM, OZmeanPM                              #
#                                                                           #
#############################################################################

{

	nnTotoz1=nnTotoz2=0
	OZaval=0
	OZqual="33"

	dataAE1 = ReadDobsonAE (day, mon, year, DobsTotoz, CalTotoz, PathTotoz0)
	nnTotoz1 = dataAE1$HeaderData$NumberMeas
	if (nnTotoz1>0)
	{
		if (min(dataAE1$Measurements$Flags[2,]+dataAE1$Measurements$Flags[3,])>0) nnTotoz1=0
	}

	if (nnTotoz1==0)  # no measurements at the Umkehr day: search before and after
	{
		OZmean = array()
		OZmean[1]=OZmean[2]=0
		OZnn=0
		for (d in 1:2)
		{
			lo=0
			nnTotoz=0
			tday=day
			tmon=mon
			tyr=year
			while ((nnTotoz==0) && (lo<=(span)))
			{
				lo=lo+1
				if (d==1) 
				{ 
					tdate <- seq(c(ISOdate(tyr,tmon,tday)), by = "-1 day", length.out = 2)
				} else
				{
					tdate <- seq(c(ISOdate(tyr,tmon,tday)), by = "1 day", length.out = 2)
				}
				tday = as.integer(substr(tdate[2],9,10))
				tmon = as.integer(substr(tdate[2],6,7))
				tyr  = as.integer(substr(tdate[2],1,4))
				d;lo;tdate;tday;tmon;tyr
				if (d==1) 
				{ 
					dataAE1 = ReadDobsonAE (tday, tmon, tyr, DobsTotoz, CalTotoz, PathTotoz0)
					nnTotoz1 = dataAE1$HeaderData$NumberMeas
					if (nnTotoz1>0)
					{
						if (min(dataAE1$Measurements$Flags[2,]+dataAE1$Measurements$Flags[3,])>0) nnTotoz1=0
					}
					nnTotoz = nnTotoz1
				} else
				{ 
					dataAE2 = ReadDobsonAE (tday, tmon, tyr, DobsTotoz, CalTotoz, PathTotoz0)
					nnTotoz2 = dataAE2$HeaderData$NumberMeas
					if (nnTotoz2>0)
					{
						if (min(dataAE2$Measurements$Flags[2,]+dataAE2$Measurements$Flags[3,])>0) nnTotoz2=0
					}
					nnTotoz = nnTotoz2
				}
			}  # end while

			if ((d==1) & (nnTotoz1>0))
			{
				OZaval=1
				ddiff1=lo
			}
			if ((d==2) & (nnTotoz2>0))
			{
				OZaval=OZaval+1
				ddiff2=lo
			}
			nd=OZaval
			#if ((nd==1) & (lo==1)) break
		}  # end for d=1:2
	} else
	{
		OZaval=0
		nd=1
	}  # end if nnTotoz:0

	if (nd>0)  # valid totoz found at or before and/or after Umkehr day
	{
		if (nnTotoz1>0) {dataAE<-dataAE1} else {dataAE<-dataAE2}
		for (n in 1:nd)
		{
			# Get all unflagged AD-measurements for AM and PM; create halfday or 
			# day means

			RAtime <- dataAE$Measurements$MeasTime[3,]
			RADflag <- dataAE$Measurements$Flags[2,]+dataAE$Measurements$Flags[3,]

			if (OZaval==0)  # totoz found at Umkehr day
			{
				RADflagAM <- RADflag[RAtime<=TrueNoonHr]
				OZcorAM <- dataAE$Measurements$OzOrg[4,][RAtime<=TrueNoonHr][RADflagAM==0]
				OZmeanAM=as.integer(mean(OZcorAM)*10+0.5)
				RADflagPM <- RADflag[RAtime>TrueNoonHr]
				OZcorPM <- dataAE$Measurements$OzOrg[4,][RAtime>TrueNoonHr][RADflagPM==0]
				OZmeanPM=as.integer(mean(OZcorPM)*10+0.5)
				if ((length(OZcorAM)>0) & (length(OZcorPM)>0))  # valid halfday totoz found at Umkehr day
				{
					OZqual="00"
				} else if (length(OZcorAM)>0)
				{
					OZqual="01" 
					OZmeanPM=OZmeanAM
				} else if (length(OZcorPM)>0)
				{
					OZqual="10" 
					OZmeanAM=OZmeanPM
				}
			} else # OZaval>0 (totoz found before or after Umkehr day)
			{
				OZcor <- dataAE$Measurements$OzOrg[4,][RADflag==0]
				if (length(OZcor)>0)  # valid totoz found before or after Umkehr day
				{
					OZmean[n] = as.integer(mean(OZcor)*10+0.5)
					OZnn = OZnn+1
				}
				if ((n==nd) & (OZnn>0))
				{
					OZqual="22"
					if (OZnn==2)
					{
						OZmean = OZmean[1]+ddiff1/(ddiff1+ddiff2)*(OZmean[2]-OZmean[1])
					} else
					{
						OZmean = OZmean[1]+OZmean[2]
					}
					OZmeanPM=OZmeanAM = as.integer(OZmean+0.5)
				}
			} # end if OZaval:0
			if ((n!=nd) & (nnTotoz2>0)) dataAE<-dataAE2
		}  # end for n=1:nd
	}  # end if nd>0

	if (OZqual=="33") OZmeanAM=OZmeanPM=9999
	return(list("OZqual"=OZqual,"OZintAM"=OZmeanAM,"OZintPM"=OZmeanPM))

}  # end of function 'GetUmkTotoz'


ManuFlagAE <- function (wl, dataIn, f3ix, flagLst)

#############################################################################
#                                                                           #
#        Function 'ManuFlagAE' sets flags accordingly to 'FlagListDay' to   #
#        the AE-data in dataset 'dataIn'                                    #
#                                                                           #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        ManuFlagAE (wl, dataIn, f3ix, flagLst)                             #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        wl       Wavelength (C, D or A)                                    #
#        dataIn   Unflagged (or partially unflagged) AE dataset             #
#        flagLst  Complete list of measurements to flag/unflag              #
#        f3ix     Indices of flaglist for instrument, day and wl            #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return   Properly flagged (or unflagged) AE dataset.               #
#                                                                           #
#############################################################################
{

	if (wl=="C")
		w=1 else if (wl=="D")
		w=2 else if (wl=="A")
		w=3
	if (w==3) {j=12} else {j=w+12}
	hms <- flagLst[6,f3ix]
	hms <- sprintf("%s%s%s",substr(hms,1,2),substr(hms,4,5),substr(hms,7,8))

	st = dataIn$Measurements$TimeStr[w,]
	for (i in 1:length(hms))
	{
		n=grep (hms[i],st,ignore.case=FALSE)
		if (length(n)>0)
		{
			k=f3ix[i]
			dataIn$Measurements$Flags[w,n] = as.integer(flagLst[j,k])
		}
	}  # end for i=1:length(hms)

	return(dataIn)

}  # end of function 'ManuFlagAE'


ReadDobsonAE <- function (day, mon, year, dobson, calib, aepath)

#############################################################################
#                                                                           #
#        'ReadDobsonAE' reads the total ozone values of Dobson              #
#        measurements from the 'AE' data format:                            #
#                                                                           #
#          - 'AEyyyymmdd.iii'-files  (single day file)                      #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#            ReadDobsonAE (day, mon, year, dobson, calib, aepath)           #
#                                                                           #
#        Input:  Date:   day [dd], month [mm], year [yyyy], dobson          #
#                        identifier [iii], calibration [Calxxx],            #
#                        path of input data file [string]                   #
#                                                                           #
#        Output: List (List of Header values, List of measurements)         #
#                                                                           #
#############################################################################

{

# Replace "iii" and "yyyy" in filepath 'aepath' by dobson and year, if occurring
 
aepath = ReplaceCalInstYear (aepath, calib, dobson, year)

# Create filename, test on existence and open 'AEyyyymmdd.iii'-file

filename = sprintf("%s%4.4d%2.2d%2.2d.%s", "AE", year, mon, day, dobson)
fpathname = sprintf("%s%s", aepath, filename)
#ex=file.exists(fpathname)

if (file.exists(fpathname))
{
	aefile = file(fpathname,open="r")

	# Initialization

	SunInt   = array()
	InstrT   = array()
	strTime  = array()
	MessTime = array()
	Rval     = array()
	SDval    = array()
	flag     = array()
	Airmass  = array()
	OzOrg    = array()
	Nval     = array()
	AirmasM  = array()

	# Read header line

	hline = ""
	hline = scan(aefile,what="character",nlines=1,quiet=TRUE)
	hline
	dobId = hline[1]
	mtype=hline[2]
	mdate=hline[3]
	nmess = as.integer(hline[4])
	utcCorr = as.integer(hline[5])
	infoMess = hline[6]
	for (i in 7:(length(hline))) infoMess = sprintf("%s%s%s",infoMess," ",hline[i])

	# Read data lines (25 or 26 items per line)

	line = scan(aefile,what="character",nlines=1,quiet=TRUE)
	if (length(line)==26) od=6 else od=5

	for (i in 1:nmess)
	{
		SunInt[i] = as.integer(line[4])
		InstrT[i] = as.integer(line[5])
		for (w in 1:3)
		{
			if (line[w*3+3]!="/")
			{
				strTime[(i-1)*3+w] = line[w*3+3]
				mHour = as.integer(substr(line[w*3+3],1,2))
				mMin  = as.integer(substr(line[w*3+3],3,4))
				mSec  = as.integer(substr(line[w*3+3],5,6))
				MessTime[(i-1)*3+w] = as.double(mHour+mMin/60+mSec/3600)
			}
			else
			{
				strTime[(i-1)*3+w] = "000000"
				MessTime[(i-1)*3+w] = 0.0
			}
			Nval[(i-1)*3+w]=0.0
			AirmasM[(i-1)*3+w]=0.0
		}  # for w=1..3

		if (line[7]!="/")  Rval[i*3-2] = as.double(line[7])     else  Rval[i*3-2]=0.0
		if (line[10]!="/") Rval[i*3-1] = as.double(line[10])    else  Rval[i*3-1]=0.0
		if (line[13]!="/") Rval[i*3-0] = as.double(line[13])    else  Rval[i*3-0]=0.0
		if (line[8]!="/")  SDval[i*3-2] = as.double(line[8])    else  SDval[i*3-2]=0.0
		if (line[11]!="/") SDval[i*3-1] = as.double(line[11])   else  SDval[i*3-1]=0.0
		if (line[14]!="/") SDval[i*3-0] = as.double(line[14])   else  SDval[i*3-0]=0.0
		if (line[15]!="/") flag[i*3-2] = as.integer(line[15])   else  flag[i*3-2]=0
		if (line[16]!="/") flag[i*3-1] = as.integer(line[16])   else  flag[i*3-1]=0
		if (line[17]!="/") flag[i*3-0] = as.integer(line[17])   else  flag[i*3-0]=0
		if (line[18]!="/") Airmass[i*3-2] = as.double(line[18]) else  Airmass[i*3-2]=0.0
		if (line[19]!="/") Airmass[i*3-1] = as.double(line[19]) else  Airmass[i*3-1]=0.0
		if (line[20]!="/") Airmass[i*3-0] = as.double(line[20]) else  Airmass[i*3-0]=0.0

		for (k in 1:od)
		{
			if (line[20+k]!="/") OzOrg[od*(i-1)+k] = as.double(line[20+k]) else OzOrg[od*(i-1)+k]=0.0
		}
		line = scan(aefile,what="character",nlines=1,quiet=TRUE)
	}  # for i=1..nmess

	close(aefile)

	# Reshape arrays

	dim(strTime) = c(3,nmess)
	dim(MessTime) = c(3,nmess)
	dim(Rval) = c(3,nmess)
	dim(SDval) = c(3,nmess)
	dim(flag) = c(3,nmess)
	dim(Airmass) = c(3,nmess)
	dim(OzOrg) = c(od,nmess)
	dim(Nval) = c(3,nmess)
	dim(AirmasM) = c(3,nmess)

	infoFile = sprintf("%s%s%s%4d%s","    File  ",filename, "  ok:", nmess, " ds-triples\n")
	cat(infoFile, file="")

	# Write data in lists

	HeaderValues = list(dobId, mtype, mdate, nmess, utcCorr, infoMess)
	names(HeaderValues) = c("DobsonId", "TypeMeas", "DateMeas", "NumberMeas", "UTCcorr", "VersionMeas")

	Measures = list(nmess, SunInt, InstrT, strTime, MessTime, Rval, SDval, flag, Airmass, OzOrg, Nval, AirmasM)
	names(Measures) = c("NumberMeas", "Sun", "InstrTemp", "TimeStr", "MeasTime", "Rvalues", 
                      "StaDevs", "Flags", "Airmass", "OzOrg", "Nvalues", "AirmassAtm")

	return(list("HeaderData"=HeaderValues,"Measurements"=Measures))

}
else  # file 'AEyyyymmdd.iii' doesen't exist
{
	# write zeros in header values
	
	dobId="000"
	mtype="0"
	mdate="00000000"
	nmess=0
	utcCorr=0
	infoMess="Version: none"
	
	infoFile = sprintf("%s%s%s","    File  ",filename, "  not found\n")
	cat(infoFile, file="")

	# Write header data in list

	HeaderValues = list(dobId, mtype, mdate, nmess, utcCorr, infoMess)
	names(HeaderValues) = c("DobsonId", "TypeMeas", "DateMeas", "NumberMeas", "UTCcorr", "VersionMeas")

	return(list("HeaderData"=HeaderValues))

}  # end: if file 'AEyyyymmdd.iii' exists or not

}  # end of function 'ReadDobsonAE'


ReadDobsonLV <- function (day, mon, year, dobson, calib, lvpath)

#############################################################################
#                                                                           #
#        'ReadDobsonLV' reads the raw total ozone values of Dobson          #
#        measurements from the 'LV' data format:                            #
#                                                                           #
#          - 'Dyyyymmdd.iii'-files  (single day file)                       #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#            ReadDobsonLV (day, mon, year, dobson, calib, lvpath)           #
#                                                                           #
#        Input:  Date:   day [dd], month [mm], year [yyyy], dobson          #
#                        identifier [iii], calibration [Calxxx],            #
#                        path of input data file [string]                   #
#                                                                           #
#        Output: List (List of Header values, List of measurements)         #
#                                                                           #
#############################################################################

{

	status=0

	# Replace "iii" and "yyyy" in filepath 'lvpath' by dobson and year, if occurring
 
	lvpath = ReplaceCalInstYear (lvpath, calib, dobson, year)

	# Create filename, test on existence and open 'Dyyyymmdd.iii'-file

	filename = sprintf("%s%4.4d%2.2d%2.2d.%s", "D", year, mon, day, dobson)
	fpathname = sprintf("%s%s", lvpath, filename)

	if (file.exists(fpathname))
	{
		lvfile = file(fpathname,open="r")

		# Initialization

		SunInt   = array()
		InstrT   = array()
		strTime  = array()
		MessTime = array()
		Rval     = array()
		SDval    = array()
		flag     = array()
		Airmass  = array()
		OzOrg    = array()
		Nval     = array()
		AirmasM  = array()

		utcCorr = 0
		nmess = 0
		mtype = "N"
		dobId = dobson

		# Read header lines

		hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
		if (length(hline)>7)
		{
			hday  = as.integer(hline[2])
			hmon  = as.integer(hline[3])
			hyear = as.integer(hline[4])
			mdate = sprintf("%04d%02d%02d",hyear,hmon,hday)
			
			hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
			if (length(hline)==5)
			{
				dNC = as.double(hline[3])
				dND = as.double(hline[4])
				dNA = as.double(hline[5])
				infoMess = sprintf("%s%6.2f%6.2f%6.2f%s%s","dN:",dNC,dND,dNA,"  RN: ",hline[2])
				infoMess = sprintf("%s%s%02d.%02d.%04d%s",infoMess,"    Version: ",hday,hmon,hyear," 00:00:00  CalOpa")
				status=0
			} else
			{ status=1 }
		} else
		{ status=1 }  # insufficient data to decode
	} else
	{ status=3 }  # input file not existing

	if (status==0)
	{
		# Search C-D-A triples of 'ds' lines and get raw values from it

		i=0
		eof=0

		repeat  # overread hash-lines
		{
			hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
			if (length(hline)<1)
			{
				eof=1
				status=1
				break
			}
			if (substr(hline[1],1,1)!="#") {break}
		}
		
		while (eof==0)
		{
			ct=0
			while (ct<3)
			{
				if (ct!=1)
				{
					repeat  # search wl=C
					{
						hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
						hline
						#if (nchar(hline[2])<4) {eof=1; break}
						if (length(hline)<2) {eof=1; break}
						if ((hline[1]=="ds") & (hline[3]=="C"))
						{
							ct=1
							lineC=hline
							break
						}
					}  # end search wl=C
				}  # end if ct<>1
				if (eof==1) break
				repeat  # search wl=D
				{
					hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
					hline
					#if (nchar(hline[2])<4) {eof=1; break}
					if (length(hline)<2) {eof=1; break}
					if (hline[1]=="ds") break
				}  # end search wl=D
				if (eof==1) break
				if (hline[3]=="D")
				{
					ct=2
					lineD=hline
					repeat  # search wl=A
					{
						hline = scan(lvfile,what="character",nlines=1,quiet=TRUE)
						hline
						#if (nchar(hline[2])<4) {eof=1; break}
						if (length(hline)<2) {eof=1; break}
						if (hline[1]=="ds") break
					}  # end search wl=A
					if (eof==1) break
					if (hline[3]=="A")
					{
						ct=3
						lineA=hline
					}  # end if wl=A
				}  # end if wl=D
				if (eof==1) break
				if (hline[3]=="C")
				{
					ct=1
					lineC=hline
				}  # end if wl=A
			}  # wend ct<3
			#ct;lineC;lineD;lineA

			# decoding the 3 'ds'-lines

			if (ct==3)
			{
				i=i+1
				TT=0
				SunInt[i] = 33
				for (v in 1:5) OzOrg[(i-1)*5+v]=0.0
				for (w in 1:3)
				{
					if (w==1)
						hline=lineC else if (w==2)
						hline=lineD else if (w==3)
						hline=lineA

					j=(i-1)*3+w
					w;j
					MessTime[j] = as.double(hline[2])/3600
					mHour = as.integer(MessTime[j])
					mMin  = as.integer((MessTime[j]-mHour)*60)
					mSec  = as.integer((MessTime[j]-mHour)*3600-mMin*60+0.5)
					if (mSec==60)
					{
						mSec=0
						mMin=mMin+1
						if (mMin==60)
						{
							mMin=0
							mHour=mHour+1
						}
					}
					strTime[j] = sprintf("%02d%02d%02d",mHour,mMin,mSec)
					MessTime[j];mHour;mMin;mSec;strTime[j]
					Rval[j] = as.double(hline[7])
					SDval[j] = as.double(hline[8])
					TT=TT+as.double(hline[22])
					flag[j]=0
					Nval[j]=0.0
					Airmass[j]=0.0
					AirmasM[j]=0.0
					if (w==3) InstrT[i] = as.integer(TT/3+0.5)
					Rval[j];SDval[j];flag[j];Nval[j];Airmass[j];AirmasM[j]
				}  # end for w=1:3
			}  # end if ct=3

		}  # wend eof=0

		close(lvfile)

		# Reshape arrays

		if (i>0)
		{
			nmess = j/3
			dim(strTime) = c(3,nmess)
			dim(MessTime) = c(3,nmess)
			dim(Rval) = c(3,nmess)
			dim(SDval) = c(3,nmess)
			dim(flag) = c(3,nmess)
			dim(Airmass) = c(3,nmess)
			dim(OzOrg) = c(5,nmess)
			dim(Nval) = c(3,nmess)
			dim(AirmasM) = c(3,nmess)

			infoFile = sprintf("%s%s%s%d%s","    File  ",filename, "  ok: ", nmess, " ds-triples\n")
			cat(infoFile, file="")
		} else  # no 'ds' measurements found
		{ 
			nmess=0
			status=2
		}

		# Write data in lists

		HeaderValues = list(dobId, mtype, mdate, nmess, utcCorr, infoMess)
		names(HeaderValues) = c("DobsonId", "TypeMeas", "DateMeas", "NumberMeas", "UTCcorr", "VersionMeas")

		Measures = list(nmess, SunInt, InstrT, strTime, MessTime, Rval, SDval, flag, Airmass, OzOrg, Nval, AirmasM)
		names(Measures) = c("NumberMeas", "Sun", "InstrTemp", "TimeStr", "MeasTime", "Rvalues", 
		                    "StaDevs", "Flags", "Airmass", "OzOrg", "Nvalues", "AirmassAtm")

		return(list("HeaderData"=HeaderValues,"Measurements"=Measures))

	}  # end if status=0
	
	if (status>0)  # file 'Dyyyymmdd.iii' doesen't exist, or insufficient data
	{
		# write zeros in header values
		
		dobId="000"
		mtype="0"
		mdate="00000000"
		nmess=0
		utcCorr=0
		infoMess="Version: none"
		
		infoFile = switch(status,
		       sprintf("%s%s%s","    File  ",filename, "  incomplete\n"),
		       sprintf("%s%s%s","    No ds found in ",filename, "\n"),
		       sprintf("%s%s%s","    File  ",filename, "  not found\n"))
		cat(infoFile, file="")

		# Write header data in list

		HeaderValues = list(dobId, mtype, mdate, nmess, utcCorr, infoMess)
		names(HeaderValues) = c("DobsonId", "TypeMeas", "DateMeas", "NumberMeas", "UTCcorr", "VersionMeas")

		return(list("HeaderData"=HeaderValues))

	}  # end: if file 'Dyyyymmdd.iii' exists or not

}  # end of function 'ReadDobsonLV'


ReadDobsonUM <- function (day, mon, year, dobson, wave, umpath, nhead)

#############################################################################
#                                                                           #
#        'ReadDobsonUM' reads the Umkehr raw values of Dobson               #
#        measurements from the 'UMyyyymmddw.iii' data format:               #
#                                                                           #
#          - 'UMyyyymmddw.iii'-files  (single day file)                     #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'UmkehrProcessing.ini'.                       #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#            ReadDobsonUM (day, mon, year, dobson, wave, umpath, nhead)     #
#                                                                           #
#        Input:  Date (day [dd], month [mm], year [yyyy]), dobson           #
#                identifier [iii], wavelength [C/D/A], path of input data   #
#                file [string], number of header lines in input file        #
#                                                                           #
#        Output: List (List of Header values, List of measurements)         #
#                                                                           #
#############################################################################

{

# Create filename, test on existence and open 'UMyyyymmddw.iii'-file

mdate = sprintf("%2.2d.%2.2d.%4.4d", day, mon, year)
filename = sprintf("%s%4.4d%2.2d%2.2d%s.%s", "um", year, mon, day, wave, dobson)
fpathname = sprintf("%s%s", umpath, filename)
ex=file.exists(fpathname)

if (ex)
{
	umfile = file(fpathname,open="r")

	# Initialization

	strTime  = array()
	MessTime = array()
	Rval     = array()
	SDval    = array()
	Lux      = array()
	SDlux    = array()
	InstrT   = array()
	HV       = array()

	# Overread header lines

	hline = ""
	hline = scan(umfile,what="character",nlines=nhead,quiet=TRUE)

	# Read and decode data sets (7 items per line)

	inSets = scan(umfile,what="character",nlines=-1,quiet=TRUE)
	nmess = length(inSets)/7

	for (i in 1:nmess)
	{
		strTime[i] = inSets[(i-1)*7+1]
		mHour = as.integer(substr(strTime[i],1,2))
		mMin  = as.integer(substr(strTime[i],4,5))
		mSec  = as.integer(substr(strTime[i],7,8))
		MessTime[i] = as.double(mHour+mMin/60+mSec/3600)
		Rval[i]   = as.double(inSets[(i-1)*7+2])
		SDval[i]  = as.double(inSets[(i-1)*7+3])
		Lux[i]    = as.double(inSets[(i-1)*7+4])
		SDlux[i]  = as.double(inSets[(i-1)*7+5])
		InstrT[i] = as.double(inSets[(i-1)*7+6])
		HV[i]     = as.double(inSets[(i-1)*7+7])
	}  # for i=1..nmess

	close(umfile)
	infoFile = sprintf("%s%s%s","    File  ",filename, "  ok\n")
	cat(infoFile, file="")

	# Write data in lists

	HeaderValues = list(dobson, mdate, wave, nmess)
	names(HeaderValues) = c("Dobson", "DateMeas", "Wavelength", "NumberMeas")

	Measures = list(nmess, strTime, MessTime, Rval, SDval, Lux, SDlux, InstrT, HV)
	names(Measures) = c("NumberMeas", "TimeStr", "MeasTime", "Rvalues", "SDevR",
                      "Lux", "SDevLux", "InstrTemp", "HV")

	return(list("HeaderData"=HeaderValues,"Measurements"=Measures))

}
else  # file 'UMyyyymmdd.iiiw' doesen't exist
{
	# write zeros in header values

	wave=0
	nmess=0
	infoFile = sprintf("%s%s%s","    File  ",filename, "  not found\n")
	cat(infoFile, file="")

	# Write header data in list

	HeaderValues = list(dobson, mdate, wave, nmess)
	names(HeaderValues) = c("Dobson", "DateMeas", "Wavelength", "NumberMeas")

	return(list("HeaderData"=HeaderValues))

}  # end: if file 'AEyyyymmdd.iii' exists or not

}  # end of function 'ReadDobsonUM'


ReadLuxfile <- function (LuxInfo)

#############################################################################
#                                                                           #
#       'ReadLuxfile' reads the values from the lux file: header data,      #
#       standard lux heights and values, lux values, maxlux values.         #
#                                                                           #
#       Output:                                                             #
#                                                                           #
#         list with header data, standard lux heights and values, lux       #
#         values, maxlux values                                             #
#                                                                           #
#############################################################################

{

	lx = file(LuxInfo$LuxFileName,open="r")

	# Read and decode header lines (former contents of 'LuxLstii.txt')

	LuxLen =75
	LuxJDN = array()
	LuxDD  = array()
	LuxMM  = array()
	LuxYY  = array()
	LuxHD  = array()
	LuxInt = array()
	LuxToz = array()

	hline = scan(lx,what="character",nlines=1,quiet=TRUE)
	nLux = as.integer(hline[1])
	hline = scan(lx,what="character",nlines=nLux,quiet=TRUE)
	for (i in 1:nLux)
	{
		j=(i-1)*7+1
		LuxJDN[i] = as.integer(hline[j])
		LuxDD[i]  = as.integer(hline[j+1])
		LuxMM[i]  = as.integer(hline[j+2])
		LuxYY[i]  = as.integer(hline[j+3])
		LuxHD[i]  = as.integer(hline[j+4])
		LuxInt[i] = as.double(hline[j+5])
		LuxToz[i] = as.integer(hline[j+6])
	}
	LuxJDN;LuxDD;LuxMM;LuxYY;LuxHD;LuxInt;LuxToz
	hline = scan(lx,what="character",nlines=5,quiet=TRUE)

	# Read and decode luxdata lines (former contents of 'LuxCurii.txt')

	hline = scan(lx,what="character",nlines=LuxLen,quiet=TRUE)
	Luxhei = array()
	LuxStd = array()
	LuxVal = array()
	MaxLux = array()
	for (i in 1:LuxLen)
	{
		Luxhei[i]= as.double(hline[(i-1)*(nLux+3)+1])
		LuxStd[i]= as.double(hline[(i-1)*(nLux+3)+2])
		MaxLux[i]= abs(as.double(hline[(i-1)*(nLux+3)+3]))
		j=(i-1)*nLux
		for (k in 1:nLux)
		{
			LuxVal[j+k]= as.double(hline[(i-1)*(nLux+3)+k+3])
		}
	}
	close(lx)
	dim(LuxVal) = c(nLux,LuxLen)
	#Luxhei;MaxLux;LuxVal

	return(list("nLux"=nLux,"LuxJDN"=LuxJDN,"LuxDD"=LuxDD,"LuxMM"=LuxMM,
	            "LuxYY"=LuxYY,"LuxHD"=LuxHD,"LuxInt"=LuxInt,"LuxToz"=LuxToz,
	            "Luxhei"=Luxhei,"LuxStd"=LuxStd,"MaxLux"=MaxLux,"LuxVal"=LuxVal))

}  # end of function 'ReadLuxfile'


ReduceAE <- function  (wl, FlagingType, AutoflagDRval, RDiffDRvalLim, 
                       maxAirmass, minSun, dataAE)
#############################################################################
#                                                                           #
#        'ReduceAE' reduces the complete AE dataset of a Dobson of one day  #
#        to the values needed for automated flagging: only the desired      #
#        wavelength (C, D or A), measurements already flagged are omitted   #
#        (FlagingType=1) or the flags are set back to zero (FlagingType=0); #
#        if AutoflagDRval=true, measurements are filtered accordingly to    #
#        the dR limits; measurements with an airmass bigger than maxAirmass #
#        resp. a sun intensity lower than minSun are also filtered.         #
#                                                                           #
#        Output: List (List of Header values, List of unfiltered            #
#                measurements)                                              #
#                                                                           #
#############################################################################

{

	if (wl=="C")
		w=1 else if (wl=="D")
		w=2 else if (wl=="A")
		w=3

	Index    = array()
	MessTime = array()
	OzVal    = array()

	dRlim = RDiffDRvalLim[w]
	n1 = dataAE$HeaderData$NumberMeas
	f  = 0
	for (n in 1:n1)
	{
		flag = dataAE$Measurements$Flags[w,n]
		if ((flag==0) | (FlagingType==0))
		{
			Oz = dataAE$Measurements$OzOrg[w,n]
			dR = dataAE$Measurements$StaDevs[w,n]
			Mu = dataAE$Measurements$Airmass[w,n]
			Si = dataAE$Measurements$Sun[n]
			if (Oz>0)
			{
				if ((Mu<=maxAirmass) & (Si>=minSun))
				{
					if ((AutoflagDRval==FALSE) | ((AutoflagDRval==TRUE) & (dR<=dRlim)))
					{
						f=f+1
						Index[f]=n
						MessTime[f] = dataAE$Measurements$MeasTime[w,n]
						OzVal[f] = Oz
					}
				}
			}
		}
	}  # next n
	DayOz = mean(OzVal)
	wl;w;n1;f;DayOz;Index;MessTime;OzVal

	# Write data in lists

	Measures = list(wl, w, n1, f, DayOz, Index, MessTime, OzVal)
	names(Measures) = c("wl", "wave", "NMeasIn", "NMeasOut", "DayOz", 
                      "Index", "MeasTime", "OzVal")

	return(list("Measurements"=Measures))
#	dataF1=list("Measurements"=Measures)

}  # end of function 'ReduceAE'


SetFlagsAE <- function (wl, dataIn, dataRed, flaggingT, maxAirmass, minSun)

#############################################################################
#                                                                           #
#        Function 'SetFlagsAE' compares an unflagged (or partially unflag-  #
#        ged) AE dataset 'dataIn' with a reduced dataset 'dataRed'; all     #
#        records which apear in 'dataIn' but not in 'dataRed' are flagged   #
#        as follows:                                                        #
#                                                                           #
#        - high airmass (Airmass >  maxAirmass)     -> flag=2               #
#                                                                           #
#        - mid airmass  (Airmass >= maxAirmass-0.5)                         #
#                       & SunInt <= minSun-3        -> flag=1               #
#                       & SunInt >  minSun-3        -> flag=2               #
#                                                                           #
#        - low airmass  (Airmass <  maxAirmass-0.5)                         #
#                       & SunInt <  minSun          -> flag=1               #
#                       & SunInt >= minSun          -> flag=4               #
#                                                                           #
#        - high dR, measurement error               -> flag=4               #
#                                                                           #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        SetFlagsAE (wl, dataIn, dataRed, flaggingT, maxAirmass, minSun)    #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        wl          Wavelength (C, D or A)                                 #
#        dataIn      Unflagged (or partially unflagged) AE dataset          #
#        dataRed     Unflagged reduced dataset of wavelength 'wl'           #
#        flaggingT   Type of flagging/unflagging (see below)                #
#        maxAirmass  Maximal airmas for filtering                           #
#        minSun      Minimal sun intensity [10..40] for filtering           #
#                                                                           #
#        status of 'flaggingT':                                             #
#                                                                           #
#          0  reset all flags                                               #
#          1  set automatic flags and write values to flaglist file         #
#          2  set/reset manual flags/unflags accordingly to flaglist file   #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return   Properly flagged (or unflagged) AE dataset, including     #
#                 the data for the flaglist.                                #
#                                                                           #
#############################################################################
{

	if (wl=="C")
		w=1 else if (wl=="D")
		w=2 else if (wl=="A")
		w=3
	nOrg=dataIn$Measurements$NumberMeas

	if (flaggingT!=0)
	{
		strTime  = array()
		MeasTime = array()
		InstrT   = array()
		Airmass  = array()
		Ozone    = array()
		Sun      = array()
		flag     = array()
		f=0
		m=0

		RedIx<-dataRed$Measurements$Index
		for (i in 1:nOrg)
		{
			if (length(RedIx[RedIx==i])==0)  # measurement is not in reduced dataset
			{
				SunInt=dataIn$Measurements$Sun[i]
				My=dataIn$Measurements$Airmass[w,i]
				if (My>maxAirmass)
					flagA=2 else
					if (My<(maxAirmass-0.5))  # low airmass
					{
						if (SunInt<minSun) flagA=1 else flagA=4
					} else  # middle airmass: Airmass>=maxAirmass-0.5
					{
						if (SunInt>(minSun-3)) flagA=2 else flagA=1
					}
				dataIn$Measurements$Flags[w,i]=flagA

				# add flagged measurement values to intermediary flaglist
				#
				# dd	mm	jdn	Instr	nf	hh:mm:ss	time	temp	my	Ozon	sd/sun	SO2	sd	A	C	D	Remark
				f=f+1
				st = dataIn$Measurements$TimeStr[w,i]
				strTime[f]  = sprintf("%s%s%s%s%s",substr(st,1,2),":",substr(st,3,4),":",substr(st,5,6))
				MeasTime[f] = dataIn$Measurements$MeasTime[w,i]/24
				InstrT[f]   = dataIn$Measurements$InstrTemp[i]
				Airmass[f]  = dataIn$Measurements$Airmass[w,i]
				Ozone[f]    = dataIn$Measurements$OzOrg[w,i]
				Sun[f]   = dataIn$Measurements$Sun[i]
				for (l in 1:3)
				{
					m=m+1
					flag[m] = dataIn$Measurements$Flags[l,i]
				}
				#strTime;MeasTime;InstrT;Airmass;Ozone;SunInt;flag

			}  # end if
		}  # end for i=1:nOrg

		# Write flaglist data in list

		dim(flag) = c(3,f)

		flagList = list(strTime, MeasTime, InstrT, Airmass, Ozone, Sun, flag)
		names(flagList) = c("TimeStr", "MeasTime", "InstrTemp", "Airmass", 
		                    "Ozone", "SunInt", "Flags")

	} else  # flaggingT=0: unflag all flags
	{
		dataIn$Measurements$Flags[w,]=0
		flagList = 0
	}

	dataFlagged = list(dataIn, flagList)
	names(dataFlagged) = c("dataAE", "flagList")
	return(dataFlagged)

}  # end of function 'SetFlagsAE'


TreatDobsonUM <- function (dataUM, CCdata, RNdata, StationPara, LuxInfo,
                           DobsTotoz, CalTotoz, PathTotoz0, LimitSDevR, 
                           splineSmo)

#############################################################################
#                                                                           #
#        'TreatDobsonUM' treats the Umkehr raw values of Dobson             #
#        measurements for one instrument and one day:                       #
#                                                                           #
#        - Filters measurements by R-StaDev limit                           #
#        - Convert times to sunheigths                                      #
#        - R to N conversion                                                #
#        - Test on minimal and maximal sunhei in range [0°..19.5°]          #
#        - Separation of AM and PM                                          #
#        - Extend Umkehr to 20°, if 19.5°<SunheiMax<20°                     #
#        - Cloud correction (if desired)                                    #
#        - Update of 'LuxCurveyyyymm.iii', if Umkehr is cloudless           #
#          and update is desired                                            #
#        - Calculate splined values for standard sunheights                 #
#        - Write results in a CX-list                                       #
#                                                                           #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          TreatDobsonUM (dataUM, CCdata, RNdata, StationPara, LuxInfo,     #
#                         DobsTotoz, CalTotoz, PathTotoz0, LimitSDevR,      #
#                         splineSmo)                                        #
#                                                                           #
#        Input:  Data list dataUM, cloud correction table CCdata,           #
#                R/N conversion table RNdata, station parameters,           #
#                LuxInfo: info about update 'LuxCur.iii', perform           #
#                cloud correction [0..3], path of Luxfile, identifier,      #
#                calibration and path of Dobson for total ozone,            #
#                Limit R-Stadev for filtering (no filtering if zero),       #
#                spline smoothing parameter [10..30]                        #
#                                                                           #
#        Output: List (List of Header values, List of measurements for      #
#                      AM and PM)                                           #
#                                                                           #
#############################################################################

{

	isTest=0  # set parameter for test purposes
	statusOk=0

	MinSunhei = 19.5  # minimal sunhei [°] to be reached during the day

	# Get date and status cAM/cPM of cloud correction for AM and PM
	#
	#   0  cloudless curve, use for Luxcurve
	#   1  cloudless curve, do not use for Luxcurve
	#   2  cloudy curve, perform cloud correction
	#   9  no or unusable Umkehr available for this half day

	dobson = dataUM$HeaderData$Dobson
	mDate = dataUM$HeaderData$DateMeas
	day  = as.integer(substr(mDate,1,2))
	mon  = as.integer(substr(mDate,4,5))
	year = as.integer(substr(mDate,7,10))
	cAM = LuxInfo$ListAM[day]
	cPM = LuxInfo$ListPM[day]
	nnUM=nnAM=nnPM=nnSplAM=nnSplPM=nnStdAM=nnStdPM=0

	# Open 'LuxCurve_yyyymm.iii', if update of it or cloud correction is desired

	if ((cAM<9) | (cPM<9))
	{
		if (file.exists(LuxInfo$LuxFileName))
		{
			Luxfile <- ReadLuxfile (LuxInfo)

			# Initialization for data conversion

			CValAMstd = array()
			CValPMstd = array()
			LuxheiStd <- c(0,1,2,3.5,5,7,10,13,16,20,25,30)
			dim(CCdata$CCvalues) = c(CCdata$nnSH,CCdata$nnCQ)
			nnUM = dataUM$HeaderData$NumberMeas
			StaLat = StationPara$StationData[2]
			StaLong = StationPara$StationData[1]
			StaHeight = StationPara$StationData[3]
			O3Layer = StationPara$StationData[5]
			AtmLayer = StationPara$StationData[6]
			RadiusEarth = StationPara$StationData[7]

			wl = dataUM$HeaderData$Wavelength
			if (wl=="C")
				w=1 else if (wl=="D")
				w=2 else if (wl=="A")
				w=3
			j = (w %% 3)+1  # reverse order for reading RN-table

			# Set proper cloud parameter

			if ((cAM<2) & (cPM<2)) CloudPar="00" else
			if ((cAM>1) & (cPM<2)) CloudPar="10" else
			if ((cAM<2) & (cPM>1)) CloudPar="01" else
			                       CloudPar="11" 

			NVal = array()
			TimeHr = array()
			Sunhei = array()
			FiltTimeStr = array()
			FiltMeasTime = array()
			FiltRvalues = array()
			FiltSDevR = array()
			FiltLux = array()

			# Filter measurements, if LimitSDevR>0.0

			if (LimitSDevR==0)
			{
				FiltTimeStr <- dataUM$Measurements$TimeStr
				FiltMeasTime <- dataUM$Measurements$MeasTime
				FiltRvalues <- dataUM$Measurements$Rvalues
				FiltSDevR <- dataUM$Measurements$SDevR
				FiltLux <- dataUM$Measurements$Lux
			} else
			{
				FiltTimeStr <- dataUM$Measurements$TimeStr[dataUM$Measurements$SDevR<LimitSDevR]
				FiltMeasTime <- dataUM$Measurements$MeasTime[dataUM$Measurements$SDevR<LimitSDevR]
				FiltRvalues <- dataUM$Measurements$Rvalues[dataUM$Measurements$SDevR<LimitSDevR]
				FiltSDevR <- dataUM$Measurements$SDevR[dataUM$Measurements$SDevR<LimitSDevR]
				FiltLux <- dataUM$Measurements$Lux[dataUM$Measurements$SDevR<LimitSDevR]
				nnUM = length(FiltRvalues)
			}

			if (nnUM>19)
			{
				# Convert measurement times to sunheight, R to N

				for (i in 1:nnUM)
				{
					TimeHr[i] = FiltMeasTime[i]
					sunpos = AzimuthSunhei (day, mon, year, TimeHr[i], StaLat, StaLong, 
																	StaHeight, O3Layer, AtmLayer, RadiusEarth)
					Sunhei[i] = sunpos$Sunhei

					RVal = FiltRvalues[i]
					if  (RVal>289)  RVal=289
					IR = floor((RVal)/10.0)
					n1 = RNdata[[j+1]][[IR+1]]
					n2 = RNdata[[j+1]][[IR+2]]
					NVal[i] = n1+(n2-n1)*((RVal-(IR*10)))/10.0
				}  # end for i=1:nnUM

				# Test on minimal and maximal sunhei: Curves not reaching the
				# range [0°..19.5°] are not treated

				if ((min(Sunhei)<=0) & (max(Sunhei)>=MinSunhei))
				{
					# Get true noon time for AM/PM separation and separate records;
					# curves with less than 10 point are not treated;
					# get splined MaxLux-values for Umkehr Luxhei-values for each
					# halfday; calculate cloud corrections and corrected N-values
					# (C-values)

					TrueNoon = CalcTrueNoon(day, mon, year, StaLong)
					TrueNoonHr = TrueNoon$TimeHR

					TimeAM <- TimeHr[TimeHr<=TrueNoonHr]
					nnAM = length(TimeAM)
					TimePM <- TimeHr[TimeHr>TrueNoonHr]
					nnPM = length(TimePM)
					nnUM = nnAM+nnPM

					# Treat AM Umkehr

					if ((cAM<9) & (nnAM>9))
					{
						hd=0
						Hr <- as.integer(TimeAM)
						Min <- as.integer((TimeAM-Hr)*60+0.5)
						TimeStrAM <- sprintf ("%02d:%02d", Hr, Min)
						NValAM <- NVal[1:nnAM]
						SunhAM <- Sunhei[1:nnAM]
						LuxAM  <- FiltLux[1:nnAM]

						# Extend Umkehr to 20°, if 19.5°<Sunhei[nnAM]<20° (this is
						# the case around december 21): set last AM sunhei to 20° 

						if ((SunhAM[nnAM]>MinSunhei) & (SunhAM[nnAM]<20)) SunhAM[nnAM] = 20

						# Test on complete Umkehr in range Sunhei=[0°..20°]

						if (((min(SunhAM)<=0) & max(SunhAM)>=20))
						{
							if (cAM>1)  # perform cloud correction
							{
								QuotAM = array()
								LuxAMspl <- spline(Luxfile$Luxhei, Luxfile$MaxLux, xout=SunhAM)
								QuotAM <- LuxAMspl$y/LuxAM
								CLuxAM = CloudCorInterpol (CCdata, SunhAM, QuotAM)
								CValAM <- NValAM-CLuxAM
							} else  # no cloud correction desired
							{
								CValAM <- NValAM
								if (cAM==0)  # update of Luxcurve-file desired
								{
									Luxfile <- UpdateLuxfile(Luxfile, SunhAM, LuxAM, mDate, hd)
								}
							}

							# Calculate standard sunhei values

							LuxheiRed <- Luxfile$Luxhei[Luxfile$Luxhei>=SunhAM[1]]
							LuxheiRed <- LuxheiRed[LuxheiRed<=SunhAM[nnAM]]
							CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=splineSmo)
							CValAMspl0 <- fitted(CValSmo.spl)
							CValAMspl <- spline(SunhAM, CValAMspl0, xout=LuxheiRed)
							nnSplAM = length(LuxheiRed)
							LuxheiMax = max(CValAMspl$x)
							LuxheiStdRedAM <- LuxheiStd[LuxheiStd<=LuxheiMax]
							nnStdAM = length(LuxheiStdRedAM)
							for (i in 1:nnStdAM) CValAMstd[i] <- CValAMspl$y[CValAMspl$x==LuxheiStdRedAM[i]]

							if (isTest>0)
							{
								plot(SunhAM, NValAM, main = paste("Umkehr AM: spline through", floor(nnAM), "points"))
								lines(SunhAM,CValAM, col = "black")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=6)
								CValAMspl06 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl06, col = "pink")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=8)
								CValAMspl08 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl08, col = "yellow")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=10)
								CValAMspl10 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl10, col = "gold")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=13)
								CValAMspl13 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl13, col = "red")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=16)
								CValAMspl16 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl16, col = "blue")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=20)
								CValAMspl20 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl20, col = "green")
								CValSmo.spl <- smooth.spline(SunhAM, CValAM, df=30)
								CValAMspl30 <- fitted(CValSmo.spl)
								lines(SunhAM, CValAMspl30, col = "purple")

								lines(LuxheiRed,CValAMspl$y, col = "green")
								lines(LuxheiStdRedAM,CValAMstd, col = "red")
							}  # end isTest>0
						} else
						{
							cAM=9
						}  # end if min(SunhAM)<=0 & max(SunhAM)>=20
					}  # end if cAM<9 & nnAM>9

					# Treat PM Umkehr

					if ((cPM<9) & (nnPM>9))
					{
						hd=1
						TimePM <- TimeHr[(nnAM+1):nnUM]
						Hr <- as.integer(TimePM)
						Min <- as.integer((TimePM-Hr)*60+0.5)
						TimeStrPM <- sprintf ("%02d:%02d", Hr, Min)
						NValPM <- NVal[(nnAM+1):nnUM]
						SunhPM <- Sunhei[(nnAM+1):nnUM]
						LuxPM  <- FiltLux[(nnAM+1):nnUM]

						# Extend Umkehr to 20°, if 19.5°<SunhPM[1]<20° (this is the case
						# around december 21): set first PM sunhei to 20° 

						if ((SunhPM[1]>MinSunhei) & (SunhPM[1]<20)) SunhPM[1] = 20

						# Test on complete Umkehr in range Sunhei=[0°..20°]

						if (((min(SunhPM)<=0) & max(SunhPM)>=20))
						{
							if (cPM>1)  # perform cloud correction
							{
								QuotPM = array()
								LuxPMspl <- spline(Luxfile$Luxhei, Luxfile$MaxLux, xout=SunhPM)
								QuotPM <- LuxPMspl$y/LuxPM
								CLuxPM = CloudCorInterpol (CCdata, SunhPM, QuotPM)
								CValPM <- NValPM-CLuxPM
							} else  # no cloud correction desired
							{
								CValPM <- NValPM
								if (cPM==0)  # update of Luxcurve-file desired
								{
									Luxfile <- UpdateLuxfile(Luxfile, SunhPM, LuxPM, mDate, hd)
								}
							}

							# Calculate standard sunhei values

							LuxheiRed <- Luxfile$Luxhei[Luxfile$Luxhei<=SunhPM[1]]
							LuxheiRed <- LuxheiRed[LuxheiRed>=SunhPM[nnPM]]
							CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=splineSmo)
							CValPMspl0 <- fitted(CValSmo.spl)
							CValPMspl <- spline(SunhPM, CValPMspl0, xout=LuxheiRed)
							nnSplPM = length(LuxheiRed)
							LuxheiMax = max(CValPMspl$x)
							LuxheiStdRedPM <- LuxheiStd[LuxheiStd<=LuxheiMax]
							nnStdPM = length(LuxheiStdRedPM)
							for (i in 1:nnStdPM) CValPMstd[i] <- CValPMspl$y[CValPMspl$x==LuxheiStdRedPM[i]]

							if (isTest>0)
							{
								plot(SunhPM, NValPM, main = paste("Umkehr PM: spline through", floor(nnPM), "points"))
								lines(SunhPM,CValPM, col = "black")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=6)
								CValPMspl06 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl06, col = "pink")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=8)
								CValPMspl08 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl08, col = "yellow")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=10)
								CValPMspl10 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl10, col = "gold")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=13)
								CValPMspl13 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl13, col = "red")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=16)
								CValPMspl16 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl16, col = "blue")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=20)
								CValPMspl20 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl20, col = "green")
								CValSmo.spl <- smooth.spline(SunhPM, CValPM, df=30)
								CValPMspl30 <- fitted(CValSmo.spl)
								lines(SunhPM, CValPMspl30, col = "purple")

								lines(LuxheiRed,CValPMspl$y, col = "green")
								lines(LuxheiStdRedPM,CValPMstd, col = "red")
							}  # end if isTest>0
						} else
						{
							cPM=9
						}  # end if min(SunhPM)<=0 & max(SunhPM)>=20
					}  # end if cPM<9 & nnPM>9

					if (cAM>8) nnAM=0
					if (cPM>8) nnPM=0
					nnUM = nnAM+nnPM

					if (nnUM>9)
					{
						# Get total ozone for half days; if no total ozone for this day is available,
						# search the closest days before and after the date (max. +/-10 days) and
						# interpolate the daily means;

						span=10
						TotozUmk = GetUmkTotoz (day, mon, year, span, TrueNoonHr, DobsTotoz, CalTotoz, PathTotoz0)

						# Write totoz also in updated Luxfile; rewrite Luxfile

						if ((cAM==0) | (cPM==0))
						{
							n=Luxfile$nLux
							if (Luxfile$LuxHD[n]==0) { Luxfile$LuxToz[n]=TotozUmk$OZintAM } else
																			 { Luxfile$LuxToz[n]=TotozUmk$OZintPM }

							WriteLuxfile (LuxInfo, Luxfile)
						}

						# Write data in lists, if available

						HeaderValues = list(dobson,mDate,wl,nnUM,nnAM,nnPM,nnSplAM,nnSplPM,nnStdAM,nnStdPM,
																TotozUmk$OZintAM,TotozUmk$OZintPM,TotozUmk$OZqual,CloudPar)
						names(HeaderValues) = c("Dobson","Date","Wavelength","NMeas","NMeasAM","NMeasPM","NsplAM",
																		"NsplPM","NstdAM","NstdPM","TotozAM","TotozPM","TotozQual","CloudPar")
						dobson;mDate;wl;nnUM;nnAM;nnPM;nnSplAM;nnSplPM;nnStdAM;nnStdPM;TotozUmk$OZintAM;TotozUmk$OZintPM;TotozUmk$OZqual

						if (nnAM>9)
						{
							MeasuresAM = list(TimeStrAM,SunhAM,NValAM,CValAM,LuxAM,CValAMspl$x,CValAMspl$y,
																LuxheiStdRedAM,CValAMstd)
							names(MeasuresAM) = c("TimeStrAM", "SunhAM", "NValAM", "CValAM", "LuxAM", 
																		"SunhAMspl", "CValAMspl", "SunhAMstd", "CValAMstd")
							TimeStrAM;SunhAM;NValAM;CValAM;LuxAM;CValAMspl$x;CValAMspl$y;LuxheiStdRedAM;CValAMstd
						} else { MeasuresAM = 0 }

						if (nnPM>9)
						{
							MeasuresPM = list(TimeStrPM,SunhPM,NValPM,CValPM,LuxPM,CValPMspl$x,CValPMspl$y,
																LuxheiStdRedPM,CValPMstd)
							names(MeasuresPM) = c("TimeStrPM", "SunhPM", "NValPM", "CValPM", "LuxPM", 
																		"SunhPMspl", "CValPMspl", "SunhPMstd", "CValPMstd")
							TimeStrPM;SunhPM;NValPM;CValPM;LuxPM;CValPMspl$x;CValPMspl$y;LuxheiStdRedPM;CValPMstd
						} else { MeasuresPM = 0 }

						dataCX = list("HeaderData"=HeaderValues,"MeasurementsAM"=MeasuresAM,"MeasurementsPM"=MeasuresPM)
						statusOk=1
					} else  # not enough data for processing
					{
						infoFile = sprintf("%s%s%s","    ",mDate, ": not enough data for processing\n")
					}

				} else  # sunhei not in range [0°..MinSunhei°]
				{
					infoFile = sprintf("%s%4.1f%s","          Sunhei not in range [0°..", MinSunhei, "°]: ")
					infoFile = sprintf("%s%5.2f%s%5.2f%s", infoFile, min(Sunhei), "°..", max(Sunhei), "°\n")
				}
			} else  # not enough data after filtering by SDevR
			{
				infoFile = sprintf("%s%s%s","    ",mDate, ": not enough data after filtering by SDevR for processing\n")
			}  # end if nnUM:9
		} else  # file 'Luxcurveyyyymm.iii' doesen't exist
		{
			infoFile = sprintf("%s%s%s","    File  ",LuxInfo$LuxFileName, "  not found\n")
		}  # end: if file 'LuxCurveyyyymm.iii' exists or not

	} else  # cAM=9 and cPM=9 -> no processing desired; return reduced header
	{
		infoFile = sprintf("%s%s%s","    ",mDate, ": no processing desired\n")
	}  # end: if cAM<9 or cPM<9

	if (statusOk==0)  # proper treatment of Umkehr data not possible
	{
		cat(infoFile, file="")
		HeaderValues = list(dobson,mDate,nnUM,nnAM,nnPM)
		names(HeaderValues) = c("Dobson","Date","NMeas","NMeasAM","NMeasPM")
		dataCX = list("HeaderData"=HeaderValues)
	}
	return(dataCX)

}  # end of function 'TreatDobsonUM'


UpdateLuxfile <- function (Luxfile, SunhHD, LuxcHD, mDate, hd)
#############################################################################
#                                                                           #
#       'UpdateLuxfile' adds a cloudless luxcurve to the luxfile; curves    #
#       older than 2 month are deleted (but at least one curve remains in   #
#       the file) [not yet implemented!]; then a new maxlux curve is        #
#       generated.                                                          #
#                                                                           #
#                                                                           #
#       Output:                                                             #
#                                                                           #
#         list with header data, lux heights, lux values, maxlux values     #
#                                                                           #
#############################################################################

{

	isTest=0  # set parameter for test purposes
	#test:
	#SunhHD<-SunhAM
	#LuxcHD<-LuxAM

	# Reverse vector order for PM Umkehrs

	if (hd==1)
	{
		SunhHD <- rev(SunhHD)
		LuxcHD <- rev(LuxcHD)
	}

	# Spline through cloudless luxcurve

	LuxHDspl <- spline(SunhHD, LuxcHD, xout=Luxfile$Luxhei)

	# Complete splined luxcurve in [-4..33°] from standard curve by adding
	# the difference of first/last real value and standard curve to the
	# missing values

	if (SunhHD[1]>Luxfile$Luxhei[1])
	{
		j = floor((SunhHD[1]-Luxfile$Luxhei[1])*2)+2
		dLux = LuxHDspl$y[j]-Luxfile$LuxStd[j]
		for (i in 1:(j-1)) LuxHDspl$y[i] = (Luxfile$LuxStd[i]+dLux)*(-1)
	}
	ne=length(SunhHD)
	nl=length(Luxfile$Luxhei)
	if (SunhHD[ne]<Luxfile$Luxhei[nl])
	{
		j = nl-floor((Luxfile$Luxhei[nl]-SunhHD[ne])*2)-1
		dLux = LuxHDspl$y[j]-Luxfile$LuxStd[j]
		for (i in (j+1):nl) LuxHDspl$y[i] = (Luxfile$LuxStd[i]+dLux)*(-1)
	}

	ll = length(LuxHDspl$y)
	LuxIntegral = sum(abs(LuxHDspl$y))/ll

	# Add curve to list,or replace an already existing curve of this date and halfday

	cdate = array()
	ListDate = array()
	replaceLux=0
	nn = Luxfile$nLux
	for (i in 1:nn)
	{
		ListDate[i] = sprintf("%02d.%02d.%04d", Luxfile$LuxDD[i],Luxfile$LuxMM[i],Luxfile$LuxYY[i])
		if ((mDate==ListDate[i]) & (hd==Luxfile$LuxHD[i]))
		{
			replaceLux=i
			for (k in 1:ll) Luxfile$LuxVal[i,k] = LuxHDspl$y[k]
		}
	}

	if (replaceLux==0)
	{
		nn = Luxfile$nLux+1
		LuxVec = array()
		for (k in 1:ll)
		{
			j=(k-1)*nn
			for (i in 1:nn)
			{
				if (i<nn) { LuxVec[i+j] = Luxfile$LuxVal[i,k] } else
				          { LuxVec[i+j] = LuxHDspl$y[k] }
			}
		}
		Luxfile$LuxVal <- LuxVec
		dim(Luxfile$LuxVal) = c(nn,ll)
	} else
	{ nn=replaceLux }

	Luxfile$nLux=nn
	Luxfile$LuxDD[nn] = as.integer(substr(mDate,1,2))
	Luxfile$LuxMM[nn] = as.integer(substr(mDate,4,5))
	Luxfile$LuxYY[nn] = as.integer(substr(mDate,7,10))
	cdate[1]=Luxfile$LuxDD[nn]
	cdate[2]=Luxfile$LuxMM[nn]
	cdate[3]=Luxfile$LuxYY[nn]
	cdate[4]=0
	cdate[5]=0
	cdate = ConvertDate(cdate,1)
	Luxfile$LuxJDN[nn] = cdate[4]
	Luxfile$LuxHD[nn] = hd
	Luxfile$LuxInt[nn]= LuxIntegral
	Luxfile$LuxToz[nn]= 9999

	# Generate a new maxlux curve: for each 'Luxhei' the maximal value of all curves
	# is used for the new maxlux curve, which will be smoothed then

	tMaxLux = array()
	for (i in 1:ll) tMaxLux[i] = max(abs(Luxfile$LuxVal[,i]))
	#sMaxLux.spl <- smooth.spline(Luxfile$Luxhei, tMaxLux)
	#Luxfile$MaxLux <- fitted(sMaxLux.spl)
	Luxfile$MaxLux <- fitted(smooth.spline(Luxfile$Luxhei, tMaxLux, df=30))
	return(Luxfile)

	if (isTest>0)
	{
		plot(SunhHD, LuxcHD, main = paste("Lux HD: spline through", length(LuxcHD), "points"), col = "grey")
		lines(Luxfile$Luxhei,tMaxLux, col = "green")
		Luxfile$MaxLux <- fitted(smooth.spline(Luxfile$Luxhei, tMaxLux))
		lines(Luxfile$Luxhei,Luxfile$MaxLux, col = "green")
		Luxfile$MaxLux <- fitted(smooth.spline(Luxfile$Luxhei, tMaxLux, df=60))
		lines(Luxfile$Luxhei,Luxfile$MaxLux, col = "gold")
		Luxfile$MaxLux <- fitted(smooth.spline(Luxfile$Luxhei, tMaxLux, df=30))
		lines(Luxfile$Luxhei,Luxfile$MaxLux, col = "pink")
	}

}  # end of function 'UpdateLuxfile'


WriteLuxfile  <- function(LuxInfo, lf)

#############################################################################
#                                                                           #
#       'WriteLuxfile' writes the values from lf ('Luxfile') to the lux     #
#       file: header data, standard lux heights and values, maxlux values,  #
#       lux values of each curve.                                           #
#                                                                           #
#       Output: none                                                        #
#                                                                           #
#############################################################################

{

	#lf <- Luxfile
	lx = file(LuxInfo$LuxFileName,open="w")
	nn=lf$nLux

	outl = sprintf ("%d%s", nn, "      [Anzahl Luxkurven]\n")
	cat(outl, file=lx)
	dline=""
	for (i in 1:nn)
	{
		outl = sprintf (" %03d %02d %02d %04d", lf$LuxJDN[i], lf$LuxDD[i], lf$LuxMM[i], lf$LuxYY[i])
		outl = sprintf ("%s%2d%8.2f %4d%s", outl, lf$LuxHD[i], lf$LuxInt[i], lf$LuxToz[i], "\n")
		cat(outl, file=lx)
		SY = lf$LuxYY[i]-2000
		if (SY<0) SY=SY+100
		dline = sprintf ("%s%02d%02d%02d", dline, lf$LuxDD[i], lf$LuxMM[i], SY)
		if (lf$LuxHD[i]==0) { dline = sprintf ("%s%s", dline, "am ") } else
		                    { dline = sprintf ("%s%s", dline, "pm ") }
	}
	outl = sprintf ("%s\n\n", "\nSunhei   LuxStd   MaxLux   Luxkurven")
	cat(outl, file=lx)
	outl = sprintf ("%s%s\n\n", "                         ", dline)
	cat(outl, file=lx)

	ll=length(lf$Luxhei)
	for (i in 1:ll)
	{
		outl = sprintf ("%6.1f%9.2f%9.2f", lf$Luxhei[i], lf$LuxStd[i], lf$MaxLux[i])
		for (k in 1:nn) outl = sprintf ("%s%9.2f", outl, lf$LuxVal[k,i])
		outl = sprintf ("%s\n", outl)
		cat(outl, file=lx)
	}
	close(lx)

}  # end of function 'WriteLuxfile'


WriteDobsonAE <- function (iniPar, cdata, RNfilename, DNvect)

#############################################################################
#                                                                           #
#        'WriteDobsonAE.R' writes the total ozone values of Dobson          #
#        measurements to the 'AE' or 'AX' data format:                      #
#                                                                           #
#          - 'AEyyyymmdd.iii'-files  (single day file)  or                  #
#                                                                           #
#          - 'AXyyyymmdd.iii'-files  (single day file)                      #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteDobsonAE (iniPar, data, RNfilename, DNvect)    #
#                                                                           #
#        Input:  iniPar: initial parameters                                 #
#                data: (list of Header values, list of measurements)        #
#                RNfilename: name of RN-tablefile used                      #
#                DNvect: vector of DN-values (A, C, D)                      #
#                                                                           #
#        Output: status value: ok=1, error=0                                #
#                                                                           #
#############################################################################

{

# Replace "iii" and "yyyy" in filepath 'aepath' by dobson and year, if occurring
 
dobson  = iniPar[2]       # "iii"
calib   = iniPar[3]       # "Calxxx"
aepath  = iniPar[14]
DateStr = cdata[[1]][[3]]  # "yyyymmdd"
year=substr(DateStr,1,4)

aepath = ReplaceCalInstYear (aepath, calib, dobson, year)

# Create filename and open new 'AEyyyymmdd.iii'- or 'AXyyyymmdd.iii'-file 

Fmt=iniPar[12]
filename = sprintf("%s%s.%s", Fmt, DateStr, dobson)
fpathname = sprintf("%s%s", aepath, filename)

aef = file(fpathname,open="w")

# Write header line

InstrNr      = cdata[[1]][[1]]
Type         = cdata[[1]][[2]]
NMeas        = cdata[[1]][[4]]
UTCcorr      = cdata[[1]][[5]]
TimeStampStr = cdata[[1]][[6]]

outl = sprintf ("%s%s%s%s%s%s%4i%4i%s", " ", InstrNr, " ", Type, "  ", DateStr, NMeas, UTCcorr, "  dN:")
if (max(abs(DNvect))>=10)
	{outl = sprintf ("%s%7.2f%7.2f%7.2f", outl, DNvect[[1]], DNvect[[2]], DNvect[[3]])} else
	{outl = sprintf ("%s%6.2f%6.2f%6.2f", outl, DNvect[[1]], DNvect[[2]], DNvect[[3]])}
outl = sprintf ("%s%s%s%s%s", outl, "  RN: ", RNfilename, "    ",TimeStampStr)
if  (Fmt=="AX")  outl = sprintf ("%s%s", outl, "  extended output includes also N, N' (N'=N+dN), N'/My, SZA and m values (wl=C,D,A)")
outl = sprintf ("%s\n", outl)

cat(outl, file=aef)

# Calc number of wavelengths (pairs) in ozone data (5 or 6)

nwp=length(cdata[[2]][[10]])/NMeas
RVal = array()

# Write data lines (25 items per line)

for (n in 1:NMeas)
{

	# Write instrument number, type, date, 'yyyymmdd', sun intensity and instrument temperature

	outl = sprintf ("%s%s%s%s%s%s%4i%4i", " ", InstrNr, " ", Type, "  ", DateStr, cdata[[2]][[2]][[n]], cdata[[2]][[3]][[n]])

	# Write measurement times 'hhmmss', R-values and StaDevs (wl=C,D,A), write slashes '/', if R<1.0 

	for (w in 1:3)
	{
		i=(n-1)*3+w  # proper index
		RVal[w] = cdata[[2]][[5]][[i]]
		if  (RVal[w]>1.0)
		{
			SDev    = cdata[[2]][[6]][[i]]
			if (SDev>99.99) SDev=99.9999
			outl= sprintf ("%s%s%s%7.2f%8.4f", outl, "  ", cdata[[2]][[4]][[i]], RVal[w], cdata[[2]][[6]][[i]])
		} else
		{
			outl = sprintf ("%s%s", outl, "       /      /       /")
		}
	}
	outl = sprintf ("%s%s", outl, " ")

	# Write flag values (wl=C,D,A)

	for  (w in 1:3)
	{
		i=(n-1)*3+w  # proper index
		outl = sprintf ("%s%2.0f", outl, cdata[[2]][[7]][[i]])
	}

	# Write airmass values 'My', (wl=C,D,A), write slash '/' for 'My', if RVal[w]<=19.9

	for  (w in 1:3)
	{
		if  (RVal[w]>19.9)  # modif. 09.01.2019
		{
			i=(n-1)*3+w  # proper index
			outl = sprintf ("%s%7.3f", outl, cdata[[2]][[9]][[i]])
		} else
			outl = sprintf ("%s%s", outl, "      /")
	}

	# Write total ozone values 'Ozon', (wl=C,D,A,AD,CD); if Fmt=="AE", write slash '/', 
	# if not 100.0<Ozon<600.0; if Fmt=="AX", write total ozone values in any case

	for  (w in 1:nwp)
	{
		i=(n-1)*nwp+w  # proper index
		if (Fmt=="AX")
		{
			outl = sprintf ("%s%8.2f", outl, cdata[[2]][[10]][[i]])
		} else  # Fmt=="AE"
		{
			if ((cdata[[2]][[10]][[i]]>100.0) & (cdata[[2]][[10]][[i]]<600.0))
			{
				outl = sprintf ("%s%7.1f", outl, cdata[[2]][[10]][[i]])
			} else
			{
				outl = sprintf ("%s%s", outl, "      /")
			}
		}
	}
	
		# Write also N, N' (N'=N+dN), N'/My, SZA and m values (wl=C,D,A), if extended output is required

	if  (Fmt=="AX")
	{
		outl = sprintf ("%s%s", outl, "  ")
		for  (w in 1:3)
		{
			j = (w %% 3)+1  # reverse order for dN-values
			if  (RVal[w]>1.0)
			{
				i=(n-1)*3+w  # proper index
				My   = cdata[[2]][[9]][[i]]
				NVal = cdata[[2]][[8]][[i]]
				N0Val= NVal-DNvect[[j]]
				NMy  = NVal/My
				AirM = cdata[[2]][[11]][[i]]*My
				SZA = cdata[[2]][[12]][[i]]

				outl = sprintf ("%s%7.2f%7.2f%7.2f%7.2f%7.3f", outl, N0Val, NVal, NMy, SZA, AirM)
			}
			else
				outl = sprintf ("%s%s", outl, "      /      /      /      /      /")
		}  # for w=1:3
	}  # if Fmt=="AX"


	outl = sprintf ("%s%s", outl, "\n")
	cat(outl, file=aef)

}  # for i=1..nmess

close(aef)

return(ok=1)


}  # end of function 'WriteDobsonAE'


WriteDobsonCX <- function (outPathname, headCX, dataCX)

#############################################################################
#                                                                           #
#        'WriteDobsonCX' writes the preprocessed Umkehr data of Dobson      #
#        measurements to the 'CX' data format:                              #
#                                                                           #
#          - 'Cxyyyymmdd.txt'-files  (single day file)                      #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteDobsonCX (outPathname, dataCX)                 #
#                                                                           #
#        Input:  outPathname: name of outputfile path                       #
#                dataCX: list of Header values, list of measurements        #
#                                                                           #
#        Output: status value: ok=1, error=0                                #
#                                                                           #
#############################################################################

{

	# Set header of filename accordingly to Dobson, if headCX="Cx":
	#
	#   D051  "CE"
	#   D062  "CF"
	#   D101  "CH"

	hed <- dataCX$HeaderData
	dobson = hed$Dobson
	if ((headCX=="Cx") | (headCX=="CX"))
	{
	if (dobson=="051") headCX="CE" else
	if (dobson=="062") headCX="CF" else headCX="CH"
	}

# Create filename and open new 'Cxyyyymmdd.txt'-file

dd = substr(hed$Date,1,2)
mm = substr(hed$Date,4,5)
yy = substr(hed$Date,9,10)
yyyy = substr(hed$Date,7,10)
fpathname = sprintf("%s%s%s%s%s%s", outPathname, headCX, yyyy, mm, dd, ".txt")

of = file(fpathname,open="w")

# Write header lines

outl = sprintf ("%s%s%s%s%s%s", "Umkehr D", dobson, " vom ", dd, mm, yy)
outl = sprintf ("%s%4d%4d%4d %s\n", outl, hed$NMeas, hed$NMeasAM, hed$NMeasPM, hed$CloudPar)
cat(outl, file=of)
outl = sprintf ("%s%5d%5d %s", "GesOzon*10:", hed$TotozAM, hed$TotozPM, hed$TotozQual)
outl = sprintf ("%s%3d%3d%3d%3d\n", outl, hed$NsplAM, hed$NsplPM, hed$NstdAM, hed$NstdPM)
cat(outl, file=of)

# Write data blocks for AM and PM (if available)

if (hed$NMeasAM>0)
{
	da <- dataCX$MeasurementsAM
	for (n in 1:hed$NMeasAM)
	{
		outl = sprintf ("   %s%8.2f%8.2f%8.2f%8.2f\n", da$TimeStrAM[n], da$SunhAM[n], da$NValAM[n], 
		                                               da$CValAM[n], da$LuxAM[n])
		cat(outl, file=of)
	}
	for (n in 1:hed$NsplAM)
	{
		outl = sprintf ("%8.2f%8.3f\n", da$SunhAMspl[n], da$CValAMspl[n])
		cat(outl, file=of)
	}
	for (n in 1:hed$NstdAM)
	{
		outl = sprintf ("%8.2f%8.3f\n", da$SunhAMstd[n], da$CValAMstd[n])
		cat(outl, file=of)
	}
}  # end if NMeasAM>0

if (hed$NMeasPM>0)
{
	da <- dataCX$MeasurementsPM
	for (n in hed$NMeasPM:1)
	{
		outl = sprintf ("   %s%8.2f%8.2f%8.2f%8.2f\n", da$TimeStrPM[n], da$SunhPM[n], da$NValPM[n], 
		                                               da$CValPM[n], da$LuxPM[n])
		cat(outl, file=of)
	}
	for (n in 1:hed$NsplPM)
	{
		outl = sprintf ("%8.2f%8.3f\n", da$SunhPMspl[n], da$CValPMspl[n])
		cat(outl, file=of)
	}
	for (n in 1:hed$NstdPM)
	{
		outl = sprintf ("%8.2f%8.3f\n", da$SunhPMstd[n], da$CValPMstd[n])
		cat(outl, file=of)
	}
}  # end if NMeasPM>0

close(of)

return(ok=1)

}  # end of function 'WriteDobsonCX'


WriteDobUmkpre <- function (outPathname, dataCX)

#############################################################################
#                                                                           #
#        'WriteDobUmkpre' writes the preprocessed Umkehr data of Dobson     #
#        measurements to the 'Umkpre' data format:                          #
#                                                                           #
#          - 'ukyymmdd.hd'-file  (half day file, hd=am/pm)                  #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteDobUmkpre (outPathname, dataCX)                #
#                                                                           #
#        Input:  outPathname: name of outputfile path                       #
#                dataCX: list of Header values, list of measurements        #
#                                                                           #
#        Output: status value: ok=1, error=0                                #
#                                                                           #
#############################################################################

{

# Create index and filename

hed <- dataCX$HeaderData
dob = hed$Dobson
if (dob=="051") iss="335" else
if (dob=="101") iss="235" else iss="135"
dd = substr(hed$Date,1,2)
mm = substr(hed$Date,4,5)
yy = substr(hed$Date,9,10)
fpathname0 = sprintf("%s%s%s%s%s%s", outPathname, "uk", yy, mm, dd, ".")
ext = list("am", "pm")

# Write data line:
#
# ddmmyyhwcqnnttt N[1] N[2] N[3] N[4] N[5] N[6] N[7] N[8] N[9]N[10]N[11]N[12]  iss
#
# ddmmyy  date  (day, month, year)
# h       half day  (AM=0, PM=1)
# w       cloud parameter  [0..5]  W=0: cloudless, w=5: perpetual, changing cloud cover
# c       standard distribution  [1..3]  ttt<315: c=2, ttt>406: c=3, else c=1
# q       quality total ozone  [0..3]  q=0: halfday mean, q=1: daily mean, 
#                                      q=2: interpolated, q=3: totoz guessed
# nn      number of Umkehr points  [10..12]
# ttt     total ozone  [Dobson Units DU]
# N[i]    Umkehr standard points  [1/10 N-Val]
# i       Dobson identifier  [D015/062=1, D101=2, D051=3]
# ss      station indentifier  [LKO Arosa=35]

for (h in 1:2)
{
	nn=as.integer(hed[h+8])
	if (nn>9)
	{
		fpathname = sprintf("%s%s", fpathname0, ext[h])
		of = file(fpathname,open="w")

		cl = substr(hed[14],h,h)
		tq = substr(hed[13],h,h)
		ttt = as.integer(hed[h+10])/10+0.5
		if (ttt<315) ci="2" else if (ttt>406) ci="3" else ci="1"
		outl = sprintf ("%s%s%s%1d%s%s%s%2d%3.0f", dd, mm, yy, h-1, cl, ci, tq, nn, ttt)
		for (i in 1:nn) outl = sprintf ("%s%5.0f", outl, dataCX[[h+1]][[9]][[i]]*10)
		if (nn<12) for (i in (nn+1):12) outl = sprintf ("%s%s", outl, "    0")
		outl = sprintf ("%s  %s\n", outl, iss)
		cat(outl, file=of)
		close(of)
	}   # end if nn>9
}  # for h=1..2

return(ok=1)

}  # end of function 'WriteDobUmkpre'


WriteDayStatis <- function (cdata, df)

#############################################################################
#                                                                           #
#        'WriteDayStatis' writes the daily values (mean, stadev, number     #
#        of measurements) of total ozone values of Dobson measurements to   #
#        the output file "DAYOZON_yyyy.iii" (df).                           #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteDayStatis (data, df)                           #
#                                                                           #
#        Input:  cdata: list of measurements                                #
#                df: open output file "DAYOZON_yyyy.iii"                    #
#                                                                           #
#        Output: df: open output file "DAYOZON_yyyy.iii"                    #
#                                                                           #
#############################################################################

{

# Write line header and statistical data

InstrNr = cdata[[1]][[1]]
DateStr = cdata[[1]][[3]]  # "yyyymmdd"
outl = sprintf ("%s%s%s%s", " ", InstrNr, " M ", DateStr)
for (w in 1:5)
{
		OzMean = cdata[[3]][[1]][[w]]
	if  ((OzMean<100) | (OzMean>600))
		OzMean = 0.0
	StaDev = cdata[[3]][[3]][[w]]
	NMeasr = cdata[[3]][[2]][[w]]
	outl   = sprintf ("%s%8.2f%6.2f%4i", outl, OzMean, StaDev, NMeasr)
}
outl = sprintf ("%s%s", outl, "\n")
cat(outl, file=df)

return(df)

}  # end of function 'WriteDayStatis'


WriteDayoz <- function (cdata, first, df)

#############################################################################
#                                                                           #
#        'WriteDayoz' writes the daily values (mean, stadev, number of      #
#        measurements) of total ozone values of Dobson measurements in      #
#        the 4-lines format to the output file "DAYOZyyN.iii" (df).         #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteDayoz (cdata, first, df)                       #
#                                                                           #
#        Input:  cdata: list of measurements                                #
#                df: open output file "DAYOZON_yyyy.iii"                    #
#                                                                           #
#        Output: df: open output file "DAYOZON_yyyy.iii"                    #
#                                                                           #
#############################################################################

{

# Write date to output line

DateStr = cdata[[1]][[3]]  # "yyyymmdd"
syrStr = sprintf ("%s%s", DateStr[3], DateStr[4])
monStr = sprintf ("%s%s", DateStr[5], DateStr[6])
dayStr = sprintf ("%s%s", DateStr[7], DateStr[8])
outl = sprintf ("%s%s%s%s%s", "  ", syrStr, "  ", monStr, "  ", dayStr)

# Add line header if it is the first output line

if (first)  outl = sprintf ("%s%s%s%s%s", outl, "  ", cdata[[1]][[6]])
InstrNr = cdata[[1]][[1]]
DateStr = cdata[[1]][[3]]  # "yyyymmdd"
outl = sprintf ("%s%s%s%s", " ", InstrNr, " M ", DateStr)
for (w in 1:5)
{
	OzMean = cdata[[3]][[1]][[w]]
	if  ((OzMean<100) | (OzMean>600))
		OzMean = 0.0
	StaDev = cdata[[3]][[3]][[w]]
	NMeasr = cdata[[3]][[2]][[w]]
	outl   = sprintf ("%s%8.2f%6.2f%3i", outl, OzMean, StaDev, NMeasr)
}
outl = sprintf ("%s%s", outl, "\n")
cat(outl, file=df)

return(df)

}  # end of function 'WriteDayoz'


WriteHalfDayStatis <- function (cdata, hd)

#############################################################################
#                                                                           #
#        'WriteHalfDayStatis' writes the halfday values (mean, number       #
#        of measurements) of total ozone values of Dobson measurements      #
#        (Wavelength=AD) to the output file "HDOZON_yyyy.iii" (hd).         #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'RecalcOz.ini'.                               #
#                                                                           #
#        Function call: WriteHalfDayStatis (cdata, hd)                      #
#                                                                           #
#        Input:  cdata: list of measurements                                #
#                hd: open output file "HDOZON_yyyy.iii"                     #
#                                                                           #
#        Output: hd: open output file "HDOZON_yyyy.iii"                     #
#                                                                           #
#############################################################################

{

# Write line with daily and halfday statistics for wl=AD (AM=1, PM=2) on 'hd'

DateStr = cdata[[1]][[3]]  # "yyyymmdd"
OzAM = cdata[[3]][[4]][[1]]
OzPM = cdata[[3]][[4]][[2]]
nnAM = cdata[[3]][[5]][[1]]
nnPM = cdata[[3]][[5]][[2]]
OzDD = cdata[[3]][[1]][[4]]
nnDD = cdata[[3]][[2]][[4]]

outl   = sprintf ("%s%7.1f%4i%7.1f%4i%7.1f%4i", DateStr, OzDD, nnDD, OzAM, nnAM, OzPM, nnPM)
outl = sprintf ("%s%s", outl, "\n")
cat(outl, file=hd)

return(hd)

}  # end of function 'WriteHalfDayStatis'


#### end of library 'TreatDobsonData.R' #####################################
