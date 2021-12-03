#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Function Name       :  ReadInstTables.R                            #
#                                                                           #
#                                                                           #
#        Creation Date       :  29.07.2009                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               02.09.2021                                  #
#                               20.11.2020                                  #
#                               06.11.2020                                  #
#                               03.09.2020                                  #
#                               16.07.2020                                  #
#                               17.06.2020                                  #
#                               03.06.2020                                  #
#                               23.04.2020                                  #
#                               19.03.2020                                  #
#                               31.01.2019                                  #
#                               09.01.2019                                  #
#                               27.07.2018                                  #
#                               30.04.2018                                  #
#                               21.01.2018                                  #
#                               12.12.2017                                  #
#                               06.12.2017                                  #
#                               03.11.2017                                  #
#                               21.10.2017                                  #
#                               25.04.2017                                  #
#                               26.03.2017                                  #
#                               10.12.2016                                  #
#                               17.11.2016                                  #
#                               11.08.2016                                  #
#                               09.05.2016                                  #
#                               23.02.2016                                  #
#                               03.11.2009                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2018)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ReadInstTables.R' contains functions to read the 'CoreName.ini'   #
#        file, the RN conversion tables and the Delta-N-tables etc.         #
#                                                                           #
#                                                                           #
#        List of functions (in alphabetic order):                           #
#                                                                           #
#          - ConvertSunInfo                                                 #
#          - CloudCorInterpol                                               #
#          - ReadAbsCoeffData                                               #
#          - ReadCloudCorTable                                              #
#          - ReadCloudListTable                                             #
#          - ReadCloudParaTable                                             #
#          - ReadConstantsFile                                              #
#          - ReadDNtable                                                    #
#          - ReadIniFile                                                    #
#          - ReadMeteoFile                                                  #
#          - ReadRNtable                                                    #
#          - ReadStationsFile                                               #
#          - ReadStratosData                                                #
#          - ReadSunIntFile                                                 #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 03.11.2009 by Herbert Schill:                                    #
#                                                                           #
#          add function 'ReplaceDobsYear' and modify the other functions    #
#          accordingly to use this function.                                #
#                                                                           #
#                                                                           #
#        - 23.02.2016 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameter indices to newest version of ini-file          #
#                                                                           #
#          - replace 'ReplaceDobsYear' by 'ReplaceCalDobsYear'              #
#                                                                           #
#                                                                           #
#        - 09.05.2016 by Herbert Schill:                                    #
#                                                                           #
#          remove bug in filename generating in 'ReadRNtable'               #
#                                                                           #
#                                                                           #
#        - 11.08.2016 by Herbert Schill:                                    #
#                                                                           #
#          - rename file 'ReadDobsTables.R' to 'ReadInstTables.R'           #
#                                                                           #
#          - rename 'ReplaceCalDobsYear' to 'ReplaceCalInstYear'            #
#                                                                           #
#          - remove bug in array-setting in 'ReadMeteoFile'                 #
#                                                                           #
#                                                                           #
#        - 17.11.2016 by Herbert Schill:                                    #
#                                                                           #
#          allow multiple occurences in 'ReplaceCalInstYear'                #
#                                                                           #
#                                                                           #
#        - 10.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          add function 'ReadStationsFile'                                  #
#                                                                           #
#                                                                           #
#        - 26.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          extend 'ReadIniFile' for direct reading of ini-file              #
#                                                                           #
#                                                                           #
#        - 25.04.2017 by Herbert Schill:                                    #
#                                                                           #
#          test on length of line for EOF in 'ReadDNtable'                  #
#          (needed for version R 3.3.2)                                     #
#                                                                           #
#                                                                           #
#        - 21.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          'print'-commands replaced by 'cat' in all procedures             #
#                                                                           #
#                                                                           #
#        - 03.11.2017 by Herbert Schill:                                    #
#                                                                           #
#          add functions 'CloudCorInterpol', 'ReadCloudCorTable'            #
#          and 'ReadCloudListTable'                                         #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          remove functions 'CompareDate', 'ConvertDate' (available in      #
#          program file 'DateZeit.R')                                       #
#                                                                           #
#                                                                           #
#        - 12.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          new format of CloudListTable in 'ReadCloudListTable'             #
#          (yearly instead monthly table)                                   #
#                                                                           #
#                                                                           #
#        - 21.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          add functions 'CloudCorInterpol', 'ReplaceDayMonthYear'          #
#                                                                           #
#                                                                           #
#        - 30.04.2018 by Herbert Schill:                                    #
#                                                                           #
#          read also location info from DN-Table in 'ReadDNtable'           #
#                                                                           #
#                                                                           #
#        - 27.07.2018 by Herbert Schill:                                    #
#                                                                           #
#          more possibilities of SunDur-info to sun intensity conversion    #
#          and sunInt dependend flagging in 'ConvertSunInfo'                #
#                                                                           #
#                                                                           #
#        - 21.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          add function 'ReadCloudParaTable'                                #
#                                                                           #
#                                                                           #
#        - 31.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          remove functions 'ReplaceCalInstYear', 'ReplaceDayMonthYear'     #
#          (available in program file 'DateZeit.R')                         #
#                                                                           #
#                                                                           #
#        - 19.03.2020 by Herbert Schill:                                    #
#                                                                           #
#          extend function 'ReadConstantsFile' for reading of different     #
#          absorption coefficients set from modified constants file         #
#                                                                           #
#                                                                           #
#        - 23.04.2020 by Herbert Schill:                                    #
#                                                                           #
#          add function 'ReadSunIntFile' for reading the Direct-Radi-       #
#          ation-datafile 'inFile' and writes the SunDur-data to the        #
#          array 'meteo' which is returned.                                 #
#                                                                           #
#                                                                           #
#        - 03.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          add function 'ReadAbsCoeffData' for reading the temperature-     #
#          dependent temporal absorption ceofficient series, based on the   #
#          stratospheric temperature profile of Payerne, and writes the     #
#          data to the matrix 'AbsCoeffList' which is returned.             #
#                                                                           #
#                                                                           #
#        - 17.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          modify 'ConvertSunInfo' to treat 1-min-sun-intenity data (from   #
#          direct radiation measurement at Davos).                          #
#                                                                           #
#        - 16.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - add function 'ReadStraTempData' for reading the effective      #
#            stratospheric temperature dataset of Payerne, and writes the   #
#            data to the matrix 'StraTempList' which is returned.           #
#                                                                           #
#          - modify function 'ReadConstantsFile' for reading of additional  #
#            parameters of the different absorption coefficients sets       #
#                                                                           #
#                                                                           #
#        - 03.09.2020 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameters of function 'ReadStraTempData'                #
#                                                                           #
#          - extend function 'ReadConstantsFile' for reading of different   #
#            Dobson Rayleigh scattering coefficient sets Beta-Beta', its    #
#            associated Rayleigh scattering correction set RcorBeta, and    #
#            Brewer absorption coefficients sets                            #
#                                                                           #
#                                                                           #
#        - 06.11.2020 by Herbert Schill:                                    #
#                                                                           #
#          - rename function 'ReadStraTempData' to 'ReadStratosData'        #
#                                                                           #
#          - extend function 'ReadStratosData' for reading the effective    #
#            barycentric O3-layer height of a station                       #
#                                                                           #
#                                                                           #
#        - 20.11.2020 by Herbert Schill:                                    #
#                                                                           #
#          - extend function 'ReadStratosData' for reading different        #
#            input formats: Teff, TeffHeff, ECMWF-Teff                      #
#                                                                           #
#        - 02.09.2021 by Herbert Schill:                                    #
#                                                                           #
#          - extend function 'ReadIniFile' for checking on "dd.mm.yyyy"-    #
#            strings as parameters; remove pth-check (no longer used)       #
#                                                                           #
#############################################################################


ConvertSunInfo <- function (dataAE, meteoData, iniPar)
#############################################################################
#                                                                           #
# Replaces the (default) sun intensity values in the daily AE dataset by    #
# a value calculated from the 10-min sum of sun duration from meteoData,    #
# using the following formula:                                              #
#                                                                           #
#        SunIntensity = 10+SunDur[min]*3                                    #
#                                                                           #
#        SunDur[min]    10   9   8   7   6   5   4   3   2   1   0          #
#        SunIntensity   40  37  34  31  28  25  22  19  16  13  10          #
#                                                                           #
# SunDur-info to sun intensity conversion and sunInt dependend flagging     #
# are performed as follows:                                                 #
#                                                                           #
# iniPar[33] action performed                                               #
#                                                                           #
#     4    - record of 1-min direct radiation and sun intensity data        #
#            (Davos only)                                                   #
#          - flags for C, D, A are set, if sunInt<sunIntMin                 #
#                                                                           #
#     3    - record of 10-Min-SunDur for the day read, if available, and    #
#            SunDur-info to sun intensity conversion done                   #
#          - flags for C, D, A are set, if sunInt<sunIntMin (iniPar[29])    #
#                                                                           #
#     1    - record of 10-Min-SunDur for the day read, if available, and    #
#            SunDur-info to sun intensity conversion done                   #
#                                                                           #
#     0    - no action                                                      #
#                                                                           #
#    -1    - values of sunInt>40 (ancient flagging method) reduced by 40    #
#                                                                           #
#    -3    - values of sunInt>40 (ancient flagging method) reduced by 40    #
#          - flags for C, D, A are set if not yet done                      #
#                                                                           #
#############################################################################
{

	SunIntMin=as.integer(iniPar[29])
	setFlag=as.integer(iniPar[33])
	LocMeteo=iniPar[37]

	kk = dataAE$Measurements$NumberMeas
	SunIntMin;setFlag;LocMeteo;kk
	if (setFlag>0)
	{
		n=meteoData$nMeas
		if (n>0)
		{
			for (k in 1:kk)
			{
				inTime = (dataAE$Measurements$MeasTime[2,k]+dataAE$Measurements$MeasTime[3,k])/2
				if ((setFlag==4) & (LocMeteo=="DAV"))
				{
					t1=length(meteoData$mTime[meteoData$mTime<=inTime])
					if (t1==0) {SunInt=10} else 
					if (t1==n){ SunInt = as.integer(meteoData$SunDur[t1]) } else
										{ SunInt = as.integer((meteoData$SunDur[t1]+meteoData$SunDur[t1+1])/2) }
				} else
				{
					ix = as.integer(inTime*6+1)+1
					SunInt = 10+meteoData$SunDur[ix]*3
				}
				dataAE$Measurements$Sun[k] = SunInt
				if ((setFlag>2) & (SunInt<SunIntMin)) { dataAE$Measurements$Flags[,k]=1 }
			}  # next k
		}
	} else  # end if setFlag>0
	{
		if (setFlag<0)
		{
			for (k in 1:kk)
			{
				SunInt = dataAE$Measurements$Sun[k]
				if (SunInt>40)
				{
					SunInt = SunInt-40
					dataAE$Measurements$Sun[k] = SunInt
					if (setFlag==-3)
					{
						sf = sum(dataAE$Measurements$Flags[,k])
						if (sf==0) dataAE$Measurements$Flags[,k]=1
					}
				}
			}  # next k
		}  # end if setFlag<0
	}  # end if setFlag:0

	return(dataAE)

}  # end of function 'ConvertSunInfo'


CloudCorInterpol <- function (CCdata, SunhHD, QuotHD)
#############################################################################
#                                                                           #
# Interpolates the proper values from the cloud-correction table            #
#                                                                           #
#############################################################################
{

	# CloudCor table interpolation

	#dim(CCdata$CCvalues) = c(CCdata$nnSH,CCdata$nnCQ)
	LuxCor = array()

	for (i in 1:length(QuotHD))
	{
		if (QuotHD[i]<1.0) QuotHD[i]=1.0
		if ((QuotHD[i]>1.1) && (QuotHD[i]<7.0))
		{
			q1 = floor((QuotHD[i]-1)*10)
			q2 = q1+1
			fq = (QuotHD[i]*10)-floor(QuotHD[i]*10)
		} else
		{
			fq=0
			if (QuotHD[i]<=1.1)
			{
				q1=q2=1
				fq = (QuotHD[i]*10)-floor(QuotHD[i]*10)
			} else
			{ q1=q2=CCdata$nnCQ }
		}

		if ((SunhHD[i]>-1) && (SunhHD[i]<33))
		{
			h1 = 33-floor(SunhHD[i])+1
			h2 = h1-1
			fh = 1-(SunhHD[i]-floor(SunhHD[i]))
		} else
		{
			fh=0
			if (SunhHD[i]<=-1) { h1=h2=CCdata$nnSH } else { h1=h2=1 }
		}
		i;SunhHD[i];QuotHD[i];h1;h2;fh;q1;q2;fq

		if (QuotHD[i]<=1.1)
		{
			cc1=cc3=0
		} else
		{
			cc1 <- CCdata$CCvalues[h1,q1]
			cc3 <- CCdata$CCvalues[h2,q1]
		}
		cc2 <- CCdata$CCvalues[h1,q2]
		cc4 <- CCdata$CCvalues[h2,q2]
		qh1 = cc3+fh*(cc1-cc3)
		qh2 = cc4+fh*(cc2-cc4)
		LuxCor[i] = qh1+fq*(qh2-qh1)
		cc1;cc2;cc3;cc4;qh1;qh2;LuxCor[i]

	}  # end for i

	show=0
	if (show>0)
	{
		plot(CCdata$CQvalues, CCdata$CCvalues[35,], main = paste("CLux vs. Quot: h*=", CCdata$SHvalues[35]))
		lines(CCdata$CQvalues, CCdata$CCvalues[30,], col = 6)
		lines(CCdata$CQvalues, CCdata$CCvalues[25,], col = 5)
		lines(CCdata$CQvalues, CCdata$CCvalues[20,], col = 4)
		lines(CCdata$CQvalues, CCdata$CCvalues[15,], col = 3)
		lines(CCdata$CQvalues, CCdata$CCvalues[10,], col = 2)
		lines(CCdata$CQvalues, CCdata$CCvalues[1,], col = 1)
	}

	return(LuxCor)

}  # end of function 'CloudCorInterpol'


ReadAbsCoeffData <- function (absCoefSet, tabPath, constFile)
#############################################################################
#                                                                           #
# Reads the temperature-dependend temporal absorption ceofficient series,   #
# based on the stratospheric temperature profile of Payerne, and writes     #
# the data to the matrix 'AbsCoeffList' which is returned.                  #
#                                                                           #
#############################################################################
{

	# Test on existence of 'inFile'

	nHead=1
	nMeas=0
	nPara=12
	djdn=719529.5  # difference of julian day number MatLab to R

	constFilename = sprintf("%s%s",tabPath,constFile)
	if (file.exists(constFilename))
	{

		# Open 'constFile' and search the name of the desired absorption coefficient
		# file

		cf = file(constFilename,open="r")
		absCoefStr = sprintf("%s%s%s","[",absCoefSet, "]")
		line = scan(cf,what="character",nlines=1,quiet=TRUE)
# 		while  ((keyw=line[1])!="[Alfa]")  line = scan(cf,what="character",nlines=1,quiet=TRUE)
		while  ((keyw=line[1])!=absCoefStr)  line = scan(cf,what="character",nlines=1,quiet=TRUE)
		absCoefFilename = sprintf("%s%s",tabPath,line[2])
		close(cf)

		# Open temporal coeff file and read the data

		if (file.exists(absCoefFilename))
		{
			af = file(absCoefFilename,open="r")
			mDate  = array()
			mTemp  = array()
			rDate  = array()
			pos = array()
			if (length(grep("Brem",absCoefSet,ignore.case=FALSE))==0)
				{for (i in 1:5) pos[i]=i+2} else
				{for (i in 1:5) pos[i]=i+7}

			hline = ""
			hline = scan(af,what="character",nlines=nHead,quiet=TRUE)

			# Read and decode data sets (nPara items per line)

			inSets = scan(af,what="character",nlines=-1,quiet=TRUE)
			nMeas = length(inSets)/nPara
			ACval = matrix(nrow=nMeas, ncol=5)
			close(af)

			for (i in 1:nMeas)
			{
				mDate[i] = as.double(inSets[(i-1)*nPara+1])
				mTemp[i] = as.double(inSets[(i-1)*nPara+2])
				for (j in 1:5)  ACval[i,j] = as.double(inSets[(i-1)*nPara+pos[j]])
			}  # for i=1..nmeas
			rDate = mDate-djdn  # convert jdn(MatLab) to jdn(R)

		} else  # 'absCoefFilename' doesen't exist
		{
			infoFile = sprintf("%s%s%s","\n  File  ",absCoefFilename, "  not found !\n")
			cat (infoFile, file="")
		}
	}
	else  # 'constFilename' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",constFilename, "  not found !\n")
		cat (infoFile, file="")
	}

	# Write data in lists

	AbsCoeffList = list(nMeas, rDate, mTemp, ACval)
	names(AbsCoeffList) = c("nMeas", "rDate", "mTemp", "AbsCoef")
	return (AbsCoeffList)

}  # end of function 'ReadAbsCoeffData'


ReadCloudCorTable <- function (CCfilename ,CCpath)
#############################################################################
#                                                                           #
# Opens and reads the table with the cloud correction standard values       #
#                                                                           #
#############################################################################
{

	CCfilename = sprintf("%s%s",CCpath, CCfilename)

	if (file.exists(CCfilename))
	{
		cf = file(CCfilename,open="r")

		CCval = array()
		CQval = array()
		SHval = array()

		hline = scan(cf, what="character", nlines=1, sep=",", quiet=TRUE)
		n = length(hline)
		for (k in 1:(n-1)) SHval[k]= as.double(hline[k+1])
		hline = scan(cf, what="character", nlines=-1, sep=",", quiet=TRUE)
		ll = length(hline)/n
		for (k in 1:ll)
		{
			j=(k-1)*n+1
			CQval[k] = as.double(hline[j])
			t=(k-1)*(n-1)
			for (h in 1:(n-1)) CCval[t+h]= as.double(hline[j+h])
		}
		close(cf)
		iniOk=1
		SHval = SHval-1

		# Write data in lists

		CCvalues = list(iniOk, n-1, ll, SHval, CQval, CCval)
		names(CCvalues) = c("iniOk", "nnSH", "nnCQ", "SHvalues", "CQvalues", "CCvalues")

	} else  # RN-table file 'O3DRNT_yyyy.iii' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",CCfilename, "  not found !\n")
		cat (infoFile, file="")
		iniOk=0
		CCvalues = list(iniOk)
		names(CCvalues) = c("iniOk")
	}

	return(CCvalues)

}  # end of function 'ReadCloudCorTable'


ReadCloudListTable <- function (CLfilename ,CLpath, mon)
#############################################################################
#                                                                           #
#   Read the entries for each halfday of the yearly cloud list table:       #
#                                                                           #
#     0  cloudless curve, use for Luxcurve                                  #
#     1  cloudless curve, do not use for Luxcurve                           #
#     2  cloudy curve, perform cloud correction                             #
#     9  no or unusable Umkehr available for this half day                  #
#                                                                           #
#############################################################################
{

	CLfilename = sprintf("%s%s",CLpath, CLfilename)

	if (file.exists(CLfilename))
	{
		cf = file(CLfilename,open="r")

		ListAM = array()
		ListPM = array()

		hline = scan(cf, what="character", nlines=3, quiet=TRUE)
		year = as.integer(hline[1])
		leap = LeapYear(year)
		nd = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
		if (leap==1 & mon==2) nd=29

		hline = scan(cf, what="character", nlines=31, quiet=TRUE)
		for (k in 1:nd)
		{
			ListAM[k] = as.integer(hline[(k-1)*26+mon*2])
			ListPM[k] = as.integer(hline[(k-1)*26+mon*2+1])
		}
		close(cf)

		# Write data in lists

		CLvalues = list(nd, ListAM, ListPM)
		names(CLvalues) = c("nd", "ListAM", "ListPM")

	} else  # 'CloudListTable' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",CLfilename, "  not found !\n")
		cat (infoFile, file="")
		nd=0
		CLvalues = list(nd)
		names(CLvalues) = c("nd")
	}

	return(CLvalues)

}  # end of function 'ReadCloudListTable'


ReadCloudParaTable <- function (CLfilename ,CLpath)
#############################################################################
#                                                                           #
#   Read the entries for each halfday of the yearly cloud parameter table:  #
#                                                                           #
#     0..5  cloud parameter for Umkehr curve                                #
#     9  no or unusable Umkehr available for this half day                  #
#                                                                           #
#############################################################################
{

	CLfilename = sprintf("%s%s",CLpath, CLfilename)

	if (file.exists(CLfilename))
	{
		cf = file(CLfilename,open="r")

		ListAM = array()
		ListPM = array()

		hline = scan(cf, what="character", nlines=3, quiet=TRUE)
		year = as.integer(hline[1])
		leap = LeapYear(year)
		k=0

		hline = scan(cf, what="character", nlines=31, quiet=TRUE)
		for (mon in 1:12)
		{
			nd = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)
			if (leap==1 & mon==2) nd=29
			for (d in 1:nd)
			{
				k=k+1
				ListAM[k] = as.integer(hline[(d-1)*26+mon*2])
				ListPM[k] = as.integer(hline[(d-1)*26+mon*2+1])
			}
		}
		close(cf)

		# Write data in lists

		CLvalues = list(k, ListAM, ListPM)
		names(CLvalues) = c("nd", "ListAM", "ListPM")

	} else  # 'CloudParaTable' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",CLfilename, "  not found !\n")
		cat (infoFile, file="")
		nd=0
		CLvalues = list(nd)
		names(CLvalues) = c("nd")
	}

	return(CLvalues)

}  # end of function 'ReadCloudParaTable'


ReadConstantsFile <- function (InstType, tabPath, constFile, absCoefSet, press, press0)
#############################################################################
#                                                                           #
# Read the atmospheric constants from the constants file for Dobsons resp.  #
# the absorption constants corrections for Brewers.                         #
#                                                                           #
#############################################################################
{

	ATfilename = sprintf("%s%s",tabPath,constFile)
	if (file.exists(ATfilename))
	{
		ATfile = file(ATfilename,open="r")
		absCoefStr = sprintf("%s%s%s","[",absCoefSet, "]")
		iniOk=1

		if (InstType=="Dobson")
		{
			AcorData = array()
			AlfaData = array()
			BetaData = array()
			RcorBeta = array()

			# search keyword '[Alfa]' which heads the absorption coefficients block and read
			# the desired ozone absorption coefficient set Alfa-Alfa' for wavelengths A, C, D,
			# the standard temperature value and the temperature change coefficient dTemp 
			# (percent change of Alfa per deg K); calculate combined wavelengths AC, AD, CD

			line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			while  ((keyw=line[1])!="[Alfa]")  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			while  ((keyw=line[1])!=absCoefStr)  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			for (i in 1:8) AlfaData[i] = as.double(line[i+1])
			AlfaData[4] = AlfaData[1]-AlfaData[2]
			AlfaData[5] = AlfaData[1]-AlfaData[3]
			AlfaData[6] = AlfaData[2]-AlfaData[3]

			# search keyword '[Beta]' which heads the Rayleigh scattering coefficients block and
			# contains the list of available sets;
			# read Rayleigh scattering coefficients  Beta-Beta' for wavelengths A, C, D and the 
			# Rayleigh scattering corrections 'RcorBeta' for A, C, D, AC, AD, CD towards BaPaWMO;
			# if the desired set is not in the list of available sets, use values of Bass-Paur WMO 1992

			while  ((keyw=line[1])!="[Beta]")  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			i=2
			ie=length(line)
			while (((length(grep(line[i],absCoefStr)))==0) & (i<ie)) i=i+1
			if ((i>1)& (i<ie)) { betaSet=line[i] } else { betaSet="BaPaWMO" }
			betaSet = sprintf("%s%s%s","[",betaSet, "]")

			while  ((keyw=line[1])!=betaSet)  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			for (i in 1:3) BetaData[i] = as.double(line[i+1])
			for (i in 1:6) RcorBeta[i] = as.double(line[i+4])
			BetaData[4] = BetaData[1]-BetaData[2]
			BetaData[5] = BetaData[1]-BetaData[3]
			BetaData[6] = BetaData[2]-BetaData[3]

			# Calculate Atmospheric Correction Factors for A, C, D, AC, AD, CD:
			#
			# Acor=((Beta-Beta')/(Alfa-Alfa'))*1000*p/p0

			pfac = 1000*(as.double(press))/(as.double(press0))
			for (i in 1:6)	AcorData[i]=(BetaData[i]/AlfaData[i])*pfac

			# Write data in lists

			ATconstants = list(iniOk, AlfaData, BetaData, AcorData, RcorBeta)
			names(ATconstants) = c("iniOK", "AbsCoeff", "RayleighScatt", "AtmosCorr", "RcorBeta")

		} else  # instrument type is Brewer
		{
			# search keyword '[BrewerAbsCoeff]' which heads the absorption coefficients block, then search 
			# the desired ozone absorption coefficient set, read the relative difference 'AbsCoeffRelDiff' to 
			#'BassPaurOp' for standard temperature -45°C and the temperature dependence 'AbsCoeffTempDiff' [%/K]

			line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			while  ((keyw=line[1])!="[BrewerAbsCoeff]")  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			StandardTemp = as.double(line[3])
			while  ((keyw=line[1])!=absCoefStr)  line = scan(ATfile,what="character",nlines=1,quiet=TRUE)
			AbsCoeffRelDiff = as.double(line[2])
			AbsCoeffTempDiff = as.double(line[3])

			# Write values in list

			ATconstants = list(iniOk, AbsCoeffRelDiff, AbsCoeffTempDiff, StandardTemp)
			names(ATconstants) = c("iniOK", "AbsCoeffRelDiff", "AbsCoeffTempDiff", "StandardTemp")
		}  # end instrument type is Dobson/Brewer

		close(ATfile)

	} else  # Constants file doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",ATfilename, "  not found !\n")
		cat (infoFile, file="")
		iniOk=0
		ATconstants = list(iniOk)
		names(ATconstants) = c("iniOK")

	}  # end if constants file exists or not

	return(ATconstants)

}  # end of function 'ReadConstantsFile'


ReadDNtable <- function (iniPar)
#############################################################################
#                                                                           #
# Opens the file which contain the Delta-N-Correction history and infor-    #
# mation about the RN-tables history, filename is usually 'NcorCalxxx.iii'  #
# (xxx=calibration identifier, e.g. '00' or '16a', iii=Dobson identifier)   #
# and reads the dates, the name of the valid RN-table, the location info    #
# and the dN-values for the 3 wavelengths A, C, D.                          #
#                                                                           #
#############################################################################
{

	# Replace "Calxxx" resp. "iii" in filename 'iniPar[8]' by calibration resp. 
	# dobson, if occurring
	 
	year = as.integer(substr(iniPar[4],7,10))
	DNfilename = ReplaceCalInstYear (iniPar[8], iniPar[3], iniPar[2], year)
	DNfilename = sprintf("%s%s",iniPar[15], DNfilename)

	if (file.exists(DNfilename))	# Read DN-table
	{
		DNTfile = file(DNfilename,open="r")

		DNdate = array()
		DNvalA = array()
		DNvalC = array()
		DNvalD = array()
		DNinfoRNT = array()
		DNinfoLoc = array()

		# overread header, read 1st line

		for (k in 1:3)  line = scan(DNTfile,what="character",nlines=1,quiet=TRUE)

		k=0
		while (length(line)>5)
		{
			k=k+1
			DNdate[k] = line[1]
			DNinfoRNT[k] = line[2]
			DNinfoLoc[k] = line[3]
			DNvalA[k] = as.double(line[4])
			DNvalC[k] = as.double(line[5])
			DNvalD[k] = as.double(line[6])
			line = scan(DNTfile,what="character",nlines=1,quiet=TRUE)
		}
		close(DNTfile)
		iniOk=k

		# Write data in lists

		DNvalues = list(iniOk, DNdate, DNinfoRNT, DNvalA, DNvalC, DNvalD, DNinfoLoc)
		names(DNvalues) = c("iniOK", "DNdates", "RNTinfo", "DNA-values", "DNC-values", "DND-values", "LocInfo")

	}
	else  # DN-table file doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",DNfilename, "  not found !\n")
		cat (infoFile, file="")
		iniOk=0
		DNvalues = list(iniOk)
		names(DNvalues) = c("iniOK")

	}  # end if DN-table file exists or not

return(DNvalues)

}  # end of function 'ReadDNtable'


ReadIniFile <- function (nini, CoreName, res)
#############################################################################
#                                                                           #
# Opens the ini-file 'CoreName.ini' from the same directory as the calling  #
# program and reads the initialization parameters                           #
#                                                                           #
# Check on "dd.mm.yyyy"-strings as parameters: if occurring, open           #
# "DataPeriod.ini"-file and read desired date(s), usually begin and end of  #
# a period to proceed.                                                      #
#                                                                           #
#############################################################################
{

	# Create 'CoreName.ini' filename, if necessary and test on existence

	if (length(grep(".ini",CoreName,ignore.case=FALSE))==0)
		{ fpathname = sprintf("%s%s", CoreName, ".ini") } else
		{ fpathname <- CoreName }

	if (file.exists(fpathname))
	{
		# Open 'CoreName.ini' and read ini parameters

	  dFilename = "DataPeriod.ini"
	  inifile = file(fpathname,open="r")
		iniPar = array()
		nd=0

		for (i in 1:nini)
		{
			line = scan(inifile,what="character",nlines=1,quiet=TRUE)
			if  (i==1)
				{ iniPar[i]=line[3] } else  # version from headerline
				{ iniPar[i]=line[1] }

			# Check on "dd.mm.yyyy"-string: if true, open "DataPeriod.ini"-file 
			# at first pass (nd==0), then read desired date(s)

			if (length(grep("dd.mm.yyyy",iniPar[i],ignore.case=FALSE))==1)
			{
				if (file.exists("DataPeriod.ini"))
				{
					if (nd==0)
					{
						dfile = file(dFilename,open="r")
						dline = scan(dfile,what="character",nlines=1,quiet=TRUE)
					}
					dline = scan(dfile,what="character",nlines=1,quiet=TRUE)
					iniPar[i]=dline[1]
					nd=nd+1
				} else
				{
					infoFile = sprintf("%s%s%s","\n  ", dFilename, "  not found !\n")
					cat (infoFile, file="")
					iniPar=0
					nini=0
					break
				}
			}  # end if "dd.mm.yyyy"

		}  # for i=1..nini

	 	infoFile = sprintf("%s%s%s","\n\n  Ini-data read from ",fpathname, "\n\n")
		cat (infoFile, file="")
		close(inifile)
		if (nd>0) close(dfile)

	} else  # file 'CoreName.ini' doesen't exist
	{
	 	infoFile = sprintf("%s%s%s","\n  Ini-file  ",filename, "  not found !\n")
		cat (infoFile, file="")
		iniPar=0
		nini=0
	}  # end if file 'CoreName.ini' exists or not

	# Write data in lists

	paramList = list(nini, iniPar)
	names(paramList) = c("NParams", "IniParameters")
	return (paramList)

}  # end of function 'ReadIniFile'


ReadMeteoFile <- function (PathMeteo, LocMeteo, year, jdn)
#############################################################################
#                                                                           #
# Reads the Meteo-datafile 'Dyyyyjjj.loc' and writes the SunDur-data to     #
# the array 'meteo' which is returned; missing 10-min data are filled       #
# with zero                                                                 #
#                                                                           #
#############################################################################
{

	# Create 'Dyyyyjjj.loc' filename and test on existence

	nMeas=0
	GloRad = array()
	SunDur = array()
	Rain   = array()

	if (LocMeteo=="ARO")
	{
		nPar=16
		posGR=10
		posSD=12
		posRR=14
		separ=","
	} else
	{
		nPar=5
		posGR=3
		posSD=4
		posRR=5
		separ=""
	}
	filename = sprintf("%s%04d%03d%s%s", "D", year, jdn, ".", LocMeteo)
	fpathname = sprintf("%s%s", PathMeteo, filename)
	ex=file.exists(fpathname)

	if (ex)
	{

		# Open 'Dyyyyjjj.loc' and read SunDur data

		met = file(fpathname,open="r")
		m=1
		SunSum=0
		RainSum=0
		mTime  = array()
		hline = ""
		hline = scan(met, what="character", nlines=1, sep=separ, quiet=TRUE)
		while (length(hline)==nPar)
		{
			mHour = as.integer(hline[1])
			mMin  = as.integer(hline[2])
			ix = 6*mHour+mMin/10+1
			while (ix>m)
			{
				mTime[m]=(m-1)/6
				GloRad[m]=0
				SunDur[m]=0
				Rain[m]=0
				m=m+1
			}
			mTime[m] = as.double(mHour+mMin/60)
			GloRad[m]= as.double(hline[posGR])
			if (m>1)
			{
				if (LocMeteo=="ARO")
				{
					Rain[m]  = as.double(hline[posRR])-RainSum
					RainSum  = as.double(hline[posRR])
					SunDur[m]= as.double(hline[posSD])-SunSum
					SunSum   = as.double(hline[posSD])
				} else
				{
					Rain[m]  = as.double(hline[posRR])
					SunDur[m]= as.double(hline[posSD])
				}
			}
			else
			{
				Rain[m]=0
				SunDur[m]=0
			}

			hline = scan(met, what="character", nlines=1, sep=separ, quiet=TRUE)
			m=m+1
		}  # end while length(hline)==nPar
		nMeas=m-1
		close(met)
	}
	else  # meteo-file 'Dyyyyjjj.loc' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",filename, "  not found !\n")
		cat (infoFile, file="")
		iniPar=0
	}  # end if path file 'CoreName.pth' exists or not

	# Write data in lists

	meteoList = list(nMeas, GloRad, SunDur, Rain)
	names(meteoList) = c("nMeas", "GlobalRad", "SunDur", "Rain")
	return (meteoList)

}  # end of function 'ReadMeteoFile'


ReadRNtable <- function (dobson, RNfilename ,rnPath)
#############################################################################
#                                                                           #
# Opens the required RN-tables (e.g. 'O3DRNT_2006.101') and reads the       #
# R to N conversion values for the 3 wavelengths A, C, D.                   #
#                                                                           #
#############################################################################
{

	if (dobson=="")
		{ RNfilename = sprintf("%s%s.%s",rnPath, RNfilename, dobson) } else
		{ RNfilename = sprintf("%s%s",rnPath, RNfilename) }

	if (file.exists(RNfilename))
	{
		RNTfile = file(RNfilename,open="r")
		
		# Read R/N conversion tables

		RNvalA = array()
		RNvalC = array()
		RNvalD = array()

		for (k in 1:31)
		{
			line = scan(RNTfile,what="character",nlines=1,quiet=TRUE)
			RNvalA[k] = as.double(line[1])
			RNvalC[k] = as.double(line[2])
			RNvalD[k] = as.double(line[3])
		}
		close(RNTfile)
		iniOk=1

		# Write data in lists

		RNvalues = list(iniOk, RNvalA, RNvalC, RNvalD)
		names(RNvalues) = c("iniOk", "RNA-values", "RNC-values", "RND-values")
	}
	else  # RN-table file 'O3DRNT_yyyy.iii' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",RNfilename, "  not found !\n")
		cat (infoFile, file="")
		iniOk=0
		RNvalues = list(iniOk)
		names(RNvalues) = c("iniOk")
	}

	return(RNvalues)

}  # end of function 'ReadRNtable'


ReadStationsFile <- function (StaFilename, StaInd)
#############################################################################
#                                                                           #
# Read the station parameters (StationName, Longitude, Latitude, Altitude,  #
# Pressure, O3LayerHeight, AtmLayerHeight, EarthRadius, SeaLevelPressure    #
# from the station list file (usually "StationList.txt").                   #
#                                                                           #
# If StaInd=VAR (or var), the parameters of all stations in the file are    #
# read into a 2-dimensional matrix and returned.                            #
#                                                                           #
#############################################################################
{

if (file.exists(StaFilename))
{
	sta = file(StaFilename,open="r")

	# Get number of stations in list and  block length

	StaInd=toupper(StaInd)
	StaData = array()
	nnSta=0
	for (i in 1:2)  line = scan(sta,what="character",nlines=1,quiet=TRUE)
	nnSta=as.integer(line[1])
	line = scan(sta,what="character",nlines=1,quiet=TRUE)
	nnBlock=as.integer(line[1])

	if (StaInd=="VAR")  # read parameters of all stations
	{
		StaName = array()
		for (s in 1:nnSta)
		{
			k=(s-1)*nnBlock+1
			line = scan(sta,what="character",nlines=1,quiet=TRUE) 
			StaData[(s-1)*nnBlock+1] = substr(line[1],2,4)
			line = scan(sta,what="character",nlines=1,quiet=TRUE) 
			StaData[(s-1)*nnBlock+2] = line[1]
			for (j in 3:nnBlock)
			{
				k=(s-1)*nnBlock+j
				line = scan(sta,what="character",nlines=1,quiet=TRUE) 
				StaData[k] = as.double(line[1])
			}
		}
		# Reshape array and write data in list

		dim(StaData) = c(nnBlock,nnSta)
		iniOk=nnSta
		StaInfo = list(iniOk, StaData)
		names(StaInfo) = c("iniOK", "StationData")

	} else  # search keyword '[StaInd]' which heads the station data block
	{
		StaHead = sprintf("%s%s%s","[",StaInd, "]")
		line = scan(sta,what="character",nlines=1,quiet=TRUE)
		while  (((keyw=line[1])!=StaHead) & (line[1]!="xxx"))
		{
			line = scan(sta,what="character",nlines=1,quiet=TRUE)
			if (length(line)<1) line[1]="xxx"
		}

		# read station parameters: StationName, Longitude, Latitude, Altitude,
		# Pressure, O3LayerHeight, AtmLayerHeight, EarthRadius, SeaLevelPressure

		if (line[1]!="xxx")
		{
			line = scan(sta,what="character",nlines=1,quiet=TRUE) 
			StaName = line[1]
			for (i in 1:(nnBlock-2))
			{
				line = scan(sta,what="character",nlines=1,quiet=TRUE) 
				StaData[i] = as.double(line[1])
			}
			iniOk=1

			# Write data in list

			StaInfo = list(iniOk, StaName, StaData)
			names(StaInfo) = c("iniOK", "StationName", "StationData")
		}
		else  # Station not in list file
		{
			infoFile = sprintf("%s%s%s","\n  Station ",StaInd, "  not found in list !\n")
			cat (infoFile, file="")
			iniOk=0
			StaInfo = list(iniOk)
			names(StaInfo) = c("iniOK")
		}
	}  # end if StaInd:VAR

	close(sta)

}
else  # Station list file doesen't exist
{
	infoFile = sprintf("%s%s%s","\n  File  ",StaFilename, "  not found !\n")
	cat (infoFile, file="")
	iniOk=0
	StaInfo = list(iniOk)
	names(StaInfo) = c("iniOK")

}  # end if constants file exists or not

return(StaInfo)

}  # end of function 'ReadStationsFile'


ReadStratosData <- function (tabPath, constFile)
#############################################################################
#                                                                           #
# Reads the stratospheric temperature and barycentric O3 layer height       #
# series of a station (e.g. Payerne), and writes the data to the matrix     #
# 'StraDataList' which is returned. Different input formats are allowed.    #
#                                                                           #
#############################################################################
{

	# Initialisation

	nHead=2
	nMeas=0
	nPara=4
	isECMWF=0

	constFilename = sprintf("%s%s",tabPath,constFile)
	if (file.exists(constFilename))
	{

		# Open 'constFile' and search the name of the desired stratospheric data
		# file, open it and read the data

		cf = file(constFilename,open="r")
		line = scan(cf,what="character",nlines=1,quiet=TRUE)
		while  ((keyw=line[1])!="[Teff]")  line = scan(cf,what="character",nlines=1,quiet=TRUE)
		stratosFilename = sprintf("%s%s",tabPath,line[2])
		close(cf)

		if (file.exists(stratosFilename))
		{
			if (length(grep("ECMWF",stratosFilename,ignore.case=FALSE))>0)
			{
				isECMWF=1
				nPara=3
			}
			if (length(grep("Heff",stratosFilename,ignore.case=FALSE))>0) nPara=5

			af = file(stratosFilename,open="r")
			tYear = array()
			tDOY  = array()
			sTemp = array()
			O3hei = array()
			hline = ""
			hline = scan(af,what="character",nlines=nHead,quiet=TRUE)

			# Read and decode data sets (nPara items per line)

			inSets = scan(af,what="character",nlines=-1,quiet=TRUE)
			nMeas = length(inSets)/nPara
			close(af)

			for (i in 1:nMeas)
			{
				if (isECMWF)
				{
					year = as.integer(substr(inSets[(i-1)*nPara+1],1,4))
					mon  = as.integer(substr(inSets[(i-1)*nPara+1],5,6))
					day  = as.integer(substr(inSets[(i-1)*nPara+1],7,8))
					jdn=0
					leap=0
					cDate = array()
					cDate = list(day, mon, year, jdn, leap)
					cDate = ConvertDate(cDate, 1)
					tYear[i] = year
					tDOY[i] = cDate[[4]][[1]]
					sTemp[i] = as.double(inSets[(i-1)*nPara+3])
					O3hei[i] = 22
				} else
				{
					tYear[i] = as.integer(inSets[(i-1)*nPara+1])
					tDOY[i]  = as.integer(inSets[(i-1)*nPara+2])
					sTemp[i] = as.double(inSets[(i-1)*nPara+4])
					O3hei[i] = as.double(inSets[(i-1)*nPara+5])
				}
			}  # for i=1..nmeas

		} else  # 'stratosFilename' doesen't exist
		{
			infoFile = sprintf("%s%s%s","\n  File  ",stratosFilename, "  not found !\n")
			cat (infoFile, file="")
		}
	}
	else  # 'constFilename' doesen't exist
	{
		infoFile = sprintf("%s%s%s","\n  File  ",constFilename, "  not found !\n")
		cat (infoFile, file="")
	}

	# Write data in lists

	StraTempList = list(nMeas, tYear, tDOY, sTemp, O3hei)
	names(StraTempList) = c("nMeas", "Year", "DOY", "straTemp", "O3height")
	return (StraTempList)

}  # end of function 'ReadStratosData'


ReadSunIntFile <- function (inFile)
#############################################################################
#                                                                           #
# Reads the Direct-Radiation-datafile 'inFile' and writes the SunDur-data   #
# to the array 'meteo' which is returned.                                   #
#                                                                           #
#############################################################################
{

	# Test on existence of 'inFile'

	nHead=2
	nMeas=0
	nPara=5
	mTime  = array()
	SDval = array()

	if (file.exists(inFile))
	{

		# Open 'inFile', overread header lines and read time and sunDur data

		met = file(inFile,open="r")

		hline = ""
		hline = scan(met,what="character",nlines=nHead,quiet=TRUE)

		# Read and decode data sets (7 items per line)

		inSets = scan(met,what="character",nlines=-1,quiet=TRUE)
		nMeas = length(inSets)/nPara
		for (i in 1:nMeas)
		{
			mTime[i] = as.double(inSets[(i-1)*nPara+1])
			SDval[i]  = as.double(inSets[(i-1)*nPara+5])
		}  # for i=1..nmeas

		close(met)
	}
	else  # 'inFile' doesen't exist
	{
		infoFile = sprintf("%s%s%s","    File  ",inFile, "  not found !\n")
		cat (infoFile, file="")
		mTime[1]=0
		SDval[1]=0
	}

	# Write data in lists

	meteoList = list(nMeas, mTime, SDval)
	names(meteoList) = c("nMeas", "mTime", "SunDur")
	return (meteoList)

}  # end of function 'ReadSunIntFile'


####  end of ReadInstTables.R  ###########################################