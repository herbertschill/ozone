#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE COMPARISON                         #
#                                                                           #
#                                                                           #
#        Program Name        :  BDcompar.R                                  #
#                                                                           #
#                                                                           #
#        Creation Date       :  14.04.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               22.10.2021                                  #
#                               10.11.2020                                  #
#                               03.09.2020                                  #
#                               23.03.2020                                  #
#                               19.03.2020                                  #
#                               17.12.2019                                  #
#                               26.02.2018                                  #
#                               06.12.2017                                  #
#                               26.10.2017                                  #
#                               27.03.2017                                  #
#                               02.02.2017                                  #
#                               12.01.2017                                  #
#                               27.12.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 4.1.1    (2021)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#       'BDCompar' allows the comparison of measurements between two Dob-   #
#       son spectrophotometers (total ozone or N-values), of two Brewer     #
#       spectrophotometers (total ozone or SO2 values) or of a Dobson       #
#       with a Brewer (total ozone only).                                   #
#                                                                           #
#       Data are read from 'AEyyyymmdd.iii' or 'AXyyyymmdd.iii' (Dobson     #
#       ASCII files) resp. from 'DSjjjyy.bbb' (Brewer ASCII files).         #
#       Comparison over any periods are possible.                           #
#                                                                           #
#       The following output data are written to ASCII files:               #
#                                                                           #
#       File  Filename        Contents                                      #
#                                                                           #
#       'ad'  ADcc...c.txt    Daily data (all values)                       #
#       'vd'  VDcc...c.txt    Daily data (Comparison values)                #
#       'vs'  VScc...c.txt    Single values (one file per period)           #
#                                                                           #
#       with: cc...c=yyy1-yyy2_tii1w1_Calxx1_tii2w2_Calxx2                  #
#                                                                           #
#             yyy1, yyy2      first & last year                             #
#             tii1, tii2      type & instrument identifiers                 #
#             Calxx1, Calxx2  calibration information                       #
#                                                                           #
#        e.g. "VS_2015_D062AD_Cal15d_D101AD_Cal15d.txt"                     #
#             "VD_2013-2015_B156_Cal13b_D101A_Cal15a.txt"                   #
#                                                                           #
#        All necessary initial information is read from the                 #
#        ASCII-file 'BDcompar.ini'.                                         #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 27.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          - add call of function CalcTrueNoon                              #
#                                                                           #
#          - add call of function 'WriteBDcompAD' (calculation of           #
#            statistics and table of all measurements of the day)           #
#                                                                           #
#                                                                           #
#        - 12.01.2017 by Herbert Schill:                                    #
#                                                                           #
#          - set calculation of 'TimeHMax' to proper place                  #
#                                                                           #
#                                                                           #
#        - 02.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - replace source name 'TreatDobsonAE' by 'TreatDobsonData'       #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt for direct reading of ini-file (without path-file)       #
#                                                                           #
#          - change calls from 'BDcomparSub.R' to 'BrewerDobsonSub.R'       #
#                                                                           #
#                                                                           #
#        - 26.10.2017 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat'                             #
#                                                                           #
#          - final message added                                            #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt call of 'CompareMeas'                                    #
#                                                                           #
#                                                                           #
#        - 26.02.2018 by Herbert Schill:                                    #
#                                                                           #
#          - set proper time zone environement variable                     #
#                                                                           #
#                                                                           #
#        - 17.12.2019 by Herbert Schill:                                    #
#                                                                           #
#          - set 'C:\PMOD\..' instead of 'C:\LKO\..' for setwd call         #
#                                                                           #
#                                                                           #
#        - 19.03.2020 by Herbert Schill:                                    #
#                                                                           #
#          - remove reading of AtmConst-values, as not used                 #
#                                                                           #
#                                                                           #
#        - 23.03.2020 by Herbert Schill:                                    #
#                                                                           #
#          - allow multiple wavelengths loop, if Dobson-Dobson comparison   #
#                                                                           #
#                                                                           #
#        - 03.09.2020 by Herbert Schill:                                    #
#                                                                           #
#          - extend program for calculations with different absorption      #
#            coefficients sets based on the daily effective stratosphere    #
#            temperature, if desired                                        #
#                                                                           #
#                                                                           #
#        - 10.11.2020 by Herbert Schill:                                    #
#                                                                           #
#          - extend program for calculations with airmass values based on   #
#            the daily effective barycentric O3-layer height, if desired    #
#                                                                           #
#                                                                           #
#        - 22.10.2021 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug by setting standard values for StdTempDobs and      #
#            StdTempBrew                                                    #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("BrewerDobsonSub.R")
source("DateZeit.R")
source("ReadInstTables.R")
source("TreatBrewerData.R")
source("TreatDobsonData.R")

#  from:               import:
#
#  BrewerDobsonSub     CompareMeas, FilterData, WriteBDcompFiles, 
#                      WriteBDcompVD, WriteBDcompVS,
#
#  DateZeit            CalcTrueNoon, ConvertDate
#
#  ReadInstTables      ReadConstantsFile, ReadIniFile, ReadRNtable,
#                      ReadStratosData
#
#  TreatBrewerData     ReadBrewerDS
#
#  TreatDobsonData     ReadDobsonAE


statusOK <- 0

# Open the ini-file 'BDcompar.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=46
sep=";"
RayCor1=0.0
RayCor2=0.0
RNfilename01="xxx1"
RNfilename02="xxx2"
iniParams = ReadIniFile(Nparams, "BDcompar.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk==Nparams)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	headStr = array()
	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Get desired period

	dayA  = as.integer(substr(iniPar[3],1,2))
	monA  = as.integer(substr(iniPar[3],4,5))
	yearA = as.integer(substr(iniPar[3],7,10))
	dayE  = as.integer(substr(iniPar[4],1,2))
	monE  = as.integer(substr(iniPar[4],4,5))
	yearE = as.integer(substr(iniPar[4],7,10))

	# Get type of comparison: Dobs-Dobs=1, Dobs-Brew=2, Brew-Dobs=3, Brew-Brew=4

	instr1=substr(iniPar[6],2,4)
	instr2=substr(iniPar[14],2,4)
	if ((substr(iniPar[6],1,1)=="D") && (substr(iniPar[14],1,1)=="D")) compType=1
	if ((substr(iniPar[6],1,1)=="D") && (substr(iniPar[14],1,1)=="B")) compType=2
	if ((substr(iniPar[6],1,1)=="B") && (substr(iniPar[14],1,1)=="D")) compType=3
	if ((substr(iniPar[6],1,1)=="B") && (substr(iniPar[14],1,1)=="B")) compType=4

	# Check wether a temperature-dependent temporal absorption ceofficient series, 
	# based on the stratospheric temperature profile of Payerne, is desired 
	# (absorption ceofficient set name (iniPar[8]) resp. iniPar[16] ending with 'Teff'), 
	# daily temperature values being read from the respective text file defined in the 
	# atmospheric constants file (iniPar[16]))
	#
	# Check also, wether airmass should be corrected depending of the real barycentric
	# height of the ozone layer (absorption ceofficient set name (iniPar[8]) resp. 
	# iniPar[16] ending with 'Heff'), O3 height values being read from the same text 
	# file as the daily temperatures, defined in the atmospheric constants file 
	# (iniPar[36])). This feature can only be activated in combination with 'Teff'

	heightO3dataSet=0
	tempACdataSet=0
	StdTempDobs=226.85
	StdTempBrew=228.15
	AbsCoeff10<-AbsCoeff11<-iniPar[8]
	AbsCoeff20<-AbsCoeff21<-iniPar[16]
	if ((length(grep("Teff",iniPar[8])))>0)
	{
		tempACdataSet=1
		if ((length(grep("Heff",iniPar[8])))>0)
		{
			heightO3dataSet=1
			iniPar[8] = substr(iniPar[8],1,nchar(iniPar[8])-8)
		} else
		{ iniPar[8] = substr(iniPar[8],1,nchar(iniPar[8])-4) }
		AbsCoeff11=iniPar[8]
	}
	if ((length(grep("Teff",iniPar[16])))>0)
	{
		tempACdataSet=1
		if ((length(grep("Heff",iniPar[16])))>0)
		{
			heightO3dataSet=1
			iniPar[16] = substr(iniPar[16],1,nchar(iniPar[16])-8)
		} else
		{ iniPar[16] = substr(iniPar[16],1,nchar(iniPar[16])-4) }
		AbsCoeff21=iniPar[16]
	}

	if (heightO3dataSet==1)  # calculate standard O3-layer factor
	{
		StationHei = as.double(iniPar[41])
		O3layerHei = as.double(iniPar[43])
		RadiusEarth = as.double(iniPar[45])
		O3FacStd = (RadiusEarth+StationHei)/(RadiusEarth+O3layerHei)
		O3FacStd = O3FacStd*O3FacStd
	}
	if (tempACdataSet==1) StratosData = ReadStratosData(iniPar[36], iniPar[37])

	# Read desired absorption coefficients from the constants file, if different from Bass-Paur

	if ((iniPar[8]=="BaPaWMO") | (iniPar[8]=="BassPaurOp"))
	{
		NewAC1=0
		if (compType>2) RayCor1=as.double(iniPar[10])  # instrument 1 is Brewer
	} else
	{
		NewAC1=1
		if (compType<3)  # instrument 1 is Dobson
		{
			AbsCoeff0 = ReadConstantsFile("Dobson",iniPar[36],iniPar[37],"BaPaWMO",iniPar[42],iniPar[46])
			AbsCoeff1 = ReadConstantsFile("Dobson",iniPar[36],iniPar[37],iniPar[8],iniPar[42],iniPar[46])
			TempRatioDobs1 =  AbsCoeff1$AbsCoeff[8]
			StdTempDobs = AbsCoeff1$AbsCoeff[7]
		} else  # instrument 1 is Brewer: calculate AbsCoeff ratio for standard stratospheric temperature
		{
			AbsCoeff1 = ReadConstantsFile("Brewer",iniPar[36],iniPar[37],iniPar[8],iniPar[42],iniPar[46])
			AbsCoeffStdRatio1 = 1/(1+AbsCoeff1$AbsCoeffRelDiff/100)
			TempRatioBrew1 =  AbsCoeff1$AbsCoeffTempDiff
			StdTempBrew = AbsCoeff1$StandardTemp
			RayCor1 = as.double(iniPar[10])
		}
	}  # if iniPar[8]:"BaPaWMO"|"BassPaurOp"

	if ((iniPar[16]=="BaPaWMO") | (iniPar[16]=="BassPaurOp"))
	{
		NewAC2=0
		if ((compType==2) | (compType==4)) RayCor2=as.double(iniPar[18])  # instrument 2 is Brewer
	} else
	{
		NewAC2=1
		if ((compType==1) | (compType==3))  # instrument 2 is Dobson
		{
			AbsCoeff0 = ReadConstantsFile("Dobson",iniPar[36],iniPar[37],"BaPaWMO",iniPar[42],iniPar[46])
			AbsCoeff2 = ReadConstantsFile("Dobson",iniPar[36],iniPar[37],iniPar[16],iniPar[42],iniPar[46])
			TempRatioDobs2 =  AbsCoeff2$AbsCoeff[8]
			StdTempDobs = AbsCoeff2$AbsCoeff[7]
		} else  # instrument 2 is Brewer: calculate AbsCoeff ratio for standard stratospheric temperature
		{
			AbsCoeff2 = ReadConstantsFile("Brewer",iniPar[36],iniPar[37],iniPar[16],iniPar[42],iniPar[46])
			AbsCoeffStdRatio2 = 1/(1+AbsCoeff2$AbsCoeffRelDiff/100)
			TempRatioBrew2 =  AbsCoeff2$AbsCoeffTempDiff
			StdTempBrew = AbsCoeff2$StandardTemp
			RayCor2 = as.double(iniPar[18])
		}
	}  # if iniPar[16]:"BaPaWMO"|"BassPaurOp"

	# Proceed wavelengths, if Dobson-Dobson comparison, just one loop otherwise 

	stdSeq<-c("A","C","D", "AC", "AD", "CD")
	seqWL1 = strsplit(iniPar[10], ",")
	seqWL2 = strsplit(iniPar[18], ",")
	if (length(seqWL1[[1]])==length(seqWL2[[1]])) nnWL=length(seqWL1[[1]]) else nnWL=1

	for (wl in 1:nnWL)
	{
		# wl=1
		iniPar[10] = seqWL1[[1]][[wl]]
		iniPar[18] = seqWL2[[1]][[wl]]
		Calendar = array()
		cdata = array()
		fdata1 = array()
		fdata2 = array()

		# Create and open temporary output files

		file.create(sprintf("%s%s", iniPar[35], "ad0.txt"))
		file.create(sprintf("%s%s", iniPar[35], "vd0.txt"))
		file.create(sprintf("%s%s", iniPar[35], "vs0.txt"))
		ad0 = file((sprintf("%s%s", iniPar[35], "ad0.txt")),open="w")
		vd0 = file((sprintf("%s%s", iniPar[35], "vd0.txt")),open="w")
		vs0 = file((sprintf("%s%s", iniPar[35], "vs0.txt")),open="w")

		totVS=0
		totVD=0
		totAD=0

		# Calculate AbsCoeff ratio for wavelength for standard stratospheric 
		# temperature and read Rayleigh correction RayCor for Dobson

		if ((NewAC1+NewAC2)>0)
		{
			if (compType<3)  # calculation for Dobson 1
			{
				vix = grep (iniPar[10],stdSeq)
				ix=vix[1]
				AbsCoeffStdRatio1 = AbsCoeff0$AbsCoeff[ix]/AbsCoeff1$AbsCoeff[ix]
				RayCor1 = AbsCoeff1$RcorBeta[ix]
			}
			if ((compType==1) | (compType==3))  # calculation for Dobson 2
			{
				vix = grep (iniPar[18],stdSeq)
				ix=vix[1]
				AbsCoeffStdRatio2 = AbsCoeff0$AbsCoeff[ix]/AbsCoeff2$AbsCoeff[ix]
				RayCor2 = AbsCoeff2$RcorBeta[ix]
			}
		}

		# Treat years; set proper month range

		for (year in yearA:yearE)
		{
	#  year=yearA
			if (year/4==floor(year/4)) {leap=1} else {leap=0}
			mon1=1
			mon2=12
			if (year==yearA) mon1=monA
			if (year==yearE) mon2=monE

			# If a temperature-dependent temporal absorption ceofficient series, based 
			# on the stratospheric temperature profile of Payerne, is desired ,the 
			# yearly dataset is extracted from the total dataset 'StratosData'.
			# The same is done for the daily barycentric height of the ozone layer

			if (tempACdataSet>0)
			{
				stDOY <- StratosData$DOY[StratosData$Year==year]
				stTemp <- StratosData$straTemp[StratosData$Year==year]
				stO3Hei <- StratosData$O3height[StratosData$Year==year]
			}

			# Treat month; set proper days in month 'mdays'

			for (mon in mon1:mon2)
			{
	#		 mon=mon1
				if (leap==1) 
					{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
					{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

				day1=1
				day2=mdays
				if ((year==yearA) & (mon==monA)) day1=dayA
				if ((year==yearE) & (mon==monE)) day2=dayE

				# Treat days in month

				for (day in day1:day2)
				{
	#			day=day1
					Calendar[1]=day
					Calendar[2]=mon
					Calendar[3]=year
					Calendar = ConvertDate(Calendar,1)
					doy=Calendar[4]
					nMeas1=0
					nMeas2=0
					nfdata1=0
					nfdata2=0
					cdata[[1]][[1]]=0
					fdata1[[1]][[1]]=0
					fdata2[[1]][[1]]=0

					# Calculates time of the true noon of the day, using the Komhyr algorithms

					StaLong = as.double(iniPar[39])  # station longitude
					TrueNoon = CalcTrueNoon(day, mon, year, StaLong)
					TimeHMax = TrueNoon[[1]]

					# If a temperature-dependent temporal absorption ceofficient series, based 
					# on the stratospheric temperature of Payerne of the day, is desired ,the 
					# effective stratospheric temperature of the day is read; if no effective 
					# stratospheric temperature is available, the standard values are set.
					# The same is done for the daily barycentric height of the ozone layer

					if (tempACdataSet>0)
					{
						stratosTempDayDobs<-stratosTempDayBrew<-stTemp[stDOY==doy]
						if (length(stratosTempDayDobs)==0)
						{
							stratosTempDayDobs=StdTempDobs
							stratosTempDayBrew=StdTempBrew
						}
					}
					if (heightO3dataSet>0)  # calculate O3-layer factor for the day
					{
						stratosO3HeiDay<-stO3Hei[stDOY==doy]
						if (length(stratosO3HeiDay)>0)
						{
							O3FacDay = (RadiusEarth+StationHei)/(RadiusEarth+stratosO3HeiDay)
							O3FacDay = O3FacDay*O3FacDay
						} else
						{
							O3FacDay = O3FacStd
							stratosO3HeiDay = as.double(iniPar[43])
						}
					}

					# Read header data and measurement data of the day from file "AEyyyymmdd.iii"
					# (Dobson) resp. from file "DSjjjyy.iii" (Brewer); if measurements of both
					# instruments are available for the day, function 'FilterData' filters them
					# by using various filters: flag, min/max airmass, max staDev, min/max neutral 
					# density filter (Brewer), min/max sun intensity (Dobson) and returns the 
					# reduced data sets 'fdata1', 'fdata2'
					#
					# Correct ozone values in 'fdataX' by desired absorption coefficient ratio 
					# (with or without effective temperature correction) and by modified airmasses
					# due to the effective O3 layer height of the day

					# Type of comparison: Dobs-Dobs=1, Dobs-Brew=2, Brew-Dobs=3, Brew-Brew=4

					if (compType<3)  # data1-set is Dobson
					{
						data1 = ReadDobsonAE (day, mon, year, instr1, iniPar[7], iniPar[12])

						nMeas1 = data1[[1]][[4]]  # NumberMeas
						if (nMeas1>0)
						{
							# Read RN-table for Dobson1 at the first pass; check wether 
							# RN-table is the same as before, read new RN-table otherwise

							headStr=strsplit(data1[[1]][[6]]," ")
							infStr1=
							RNfilename1  = headStr[[1]][[6]]
							if (RNfilename1!=RNfilename01) 
							{
								RNdata1 = ReadRNtable(instr1, RNfilename1, iniPar[36])
								iniOk  = RNdata1[[1]][[1]]
								RNfilename01 = RNfilename1
							}

							fdata1 = FilterData (1, iniPar, data1, RNdata1)
							nfdata1 = fdata1$Measurements$NumberMeas
							if ((nfdata1>0) & (NewAC1>0))
							{
								if (tempACdataSet>0)  # correct O3-values with absorption coefficients of the day
								{
									AbsCoeffRatio = AbsCoeffStdRatio1/(1+TempRatioDobs1*(stratosTempDayDobs-StdTempDobs)/100)
									fdata1$Measurements$CompVal = fdata1$Measurements$CompVal*AbsCoeffRatio+RayCor1
								} else
								{
									fdata1$Measurements$CompVal = fdata1$Measurements$CompVal*AbsCoeffStdRatio1+RayCor1
								}
							}  # if nfdata1>0 & NewAC1>0
						}  # if nMeas1>0

					} else  # compType>=3  data1-set is Brewer
					{
						data1 = ReadBrewerDS (doy, day, mon, year, instr1, iniPar[7], iniPar[12])
						nMeas1 = data1[[1]][[1]]  # NumberMeas
						if (nMeas1>0)
						{
							fdata1=FilterData (1, iniPar, data1, RNdata1)
							nfdata1 = fdata1$Measurements$NumberMeas
							if ((nfdata1>0) & (NewAC1>0))
							{
								if (tempACdataSet>0)
								{
									AbsCoeffRatio = AbsCoeffStdRatio1/(1+TempRatioBrew1*(stratosTempDayBrew-StdTempBrew)/100)
									fdata1$Measurements$CompVal = fdata1$Measurements$CompVal*AbsCoeffRatio
								} else
								{
									fdata1$Measurements$CompVal = fdata1$Measurements$CompVal*AbsCoeffStdRatio1
								}
							}  # if nfdata1>0 & NewAC1>0
						}  # if nMeas1>0
					}  # if compType:3

					if (nfdata1>0)
					{
						if (heightO3dataSet>0)  # correct airmasses and O3-values with O3-layer height of the day
						{
#							mu1sq = fdata1$Measurements$Airmass*fdata1$Measurements$Airmass
#							muK = (1-mu1sq)/(mu1sq*O3FacStd)
#							mu2 = 1/sqrt(1+O3FacDay*muK)
#             quotMu = fdata1$Measurements$Airmass/mu2
							mu1sq = fdata1$Measurements$Airmass^2
							mu2 = 1/sqrt(1+O3FacDay*((1-mu1sq)/(mu1sq*O3FacStd)))
							fdata1$Measurements$CompVal = fdata1$Measurements$CompVal*fdata1$Measurements$Airmass/mu2
							fdata1$Measurements$Airmass = mu2
						}
						if ((compType==1) | (compType==3))  # data2-set is Dobson
						{
							data2 = ReadDobsonAE (day, mon, year, instr2, iniPar[15], iniPar[20])

							nMeas2 = data2[[1]][[4]]  # NumberMeas
							if (nMeas2>0)
							{
								# Read RN-table for Dobson2 at the first pass; check wether 
								# RN-table is the same as before, read new RN-table otherwise

								headStr=strsplit(data2[[1]][[6]]," ")
								RNfilename2  = headStr[[1]][[6]]
								if  (RNfilename2!=RNfilename02) 
								{
									RNdata2 = ReadRNtable(instr2, RNfilename2, iniPar[36])
									iniOk  = RNdata2[[1]][[1]]
									RNfilename02 = RNfilename2
								}
								fdata2 = FilterData (2, iniPar, data2, RNdata2)
								nfdata2 = fdata2$Measurements$NumberMeas
								if ((nfdata2>0) & (NewAC2>0))
								{
									if (tempACdataSet>0)
									{
										AbsCoeffRatio = AbsCoeffStdRatio2/(1+TempRatioDobs2*(stratosTempDayDobs-StdTempDobs)/100)
										fdata2$Measurements$CompVal = fdata2$Measurements$CompVal*AbsCoeffRatio+RayCor2
									} else
									{
										fdata2$Measurements$CompVal = fdata2$Measurements$CompVal*AbsCoeffStdRatio2+RayCor2
									}
								}  # if nfdata2>0 & NewAC2>0
							}  # if nMeas2>0
						}
							else  # compType==2/4  data2-set is Brewer
						{  
							data2 = ReadBrewerDS (doy, day, mon, year, instr2, iniPar[15], iniPar[20])
							nMeas2 = data2[[1]][[1]]  # NumberMeas
							if (nMeas2>0)
							{
								fdata2=FilterData (2, iniPar, data2, RNdata1)
								nfdata2 = fdata2$Measurements$NumberMeas
								if ((nfdata2>0) & (NewAC2>0))
								{
									if (tempACdataSet>0)
									{
										AbsCoeffRatio = AbsCoeffStdRatio2/(1+TempRatioBrew2*(stratosTempDayBrew-StdTempBrew)/100)
										fdata2$Measurements$CompVal = fdata2$Measurements$CompVal*AbsCoeffRatio
									} else
									{
										fdata2$Measurements$CompVal = fdata2$Measurements$CompVal*AbsCoeffStdRatio2
									}
								}  # if fdata2$Measurements$NumberMeas>0 & NewAC2>0
							}  # if nMeas2>0
						}  # if compType
						if ((nfdata2>0) & (heightO3dataSet>0))  # correct airmasses and O3-values with O3-layer height of the day
						{
							mu1sq = fdata2$Measurements$Airmass^2
							mu2 = 1/sqrt(1+O3FacDay*((1-mu1sq)/(mu1sq*O3FacStd)))
							fdata2$Measurements$CompVal = fdata2$Measurements$CompVal*fdata2$Measurements$Airmass/mu2
							fdata2$Measurements$Airmass = mu2
						}
					} else  # nfdata1==0
					{ nfdata2=0 }

					# valid measurements of the two instruments exist of this day

					if ((nfdata1>0) & (nfdata2>0))
					{
						cdata = CompareMeas (iniPar, fdata1, fdata2, TimeHMax)

						# valid quasi-simultaneous measurements exist of this day

						if (cdata[[1]][[1]]>0)
						{
							totVS = totVS+cdata[[1]][[1]]
							vs0 = WriteBDcompVS (Calendar, cdata, vs0, sep)
						}# if cdata[[1]][[1]]:0
					}  # if nMeas1>0 & nMeas2>0

					# write daily means of all and quasi-simultaneous measurements 
					# to their respective lists

					totVD = totVD+1
					vd0 = WriteBDcompVD (Calendar, cdata, vd0, sep)
					totAD = totAD+1
					ad0 = WriteBDcompAD (Calendar, TimeHMax, fdata1, fdata2, ad0, sep)

				}  # for day=day1:day2
			}  # for mon=mon1:mon2
		}  # for year=yearA:yearE

		# Close temporary output files

		close(ad0)  # close AD-file
		close(vd0)  # close VD-file
		close(vs0)  # close VS-file

		if (totVS>0)
		{
			# Write title infos in list and write definitive data files

			titleInfo = list(compType, yearA, yearE, monA, monE, totAD, totVD, totVS, sep, RayCor1, RayCor2)
			names(titleInfo) = c("compType", "FYr", "LYr", "FMon", "LMon", "totAD", "totVD", "totVS", "Separ", "RayCor1", "RayCor2")
			list("TitleInfo"=titleInfo)

			iniPar[8]=AbsCoeff10
			iniPar[16]=AbsCoeff20
			outOk = WriteBDcompFiles (iniPar, titleInfo, ad0, vd0, vs0)
			iniPar[8]=AbsCoeff11
			iniPar[16]=AbsCoeff21

			# Write final message to screen

			iStr1 = switch(compType, iniPar[10],iniPar[10],iniPar[9],iniPar[9])
			iStr2 = switch(compType, iniPar[18],iniPar[17],iniPar[18],iniPar[17])

			infoFile = sprintf("%s%s%s%s%s%s %s %s %s%s%s %s %s %s%s","\n\n  Period ", iniPar[3], "-", iniPar[4], 
												 "  compared for instruments:\n\n  ", iniPar[6], iStr1, iniPar[7], AbsCoeff10, " - ", 
												 iniPar[14], iStr2, iniPar[15], AbsCoeff20, "\n\n\n")
			cat(infoFile, file="")

		} else
		{
			infoFile = sprintf("%s","\n\n  No valid quasi-simultaneous measurements for the period found !\n\n")
			cat(infoFile, file="")
		}# if totVS:0

		# Delete temporary output files

		file.remove(sprintf("%s%s", iniPar[35], "ad0.txt"))
		file.remove(sprintf("%s%s", iniPar[35], "vd0.txt"))
		file.remove(sprintf("%s%s", iniPar[35], "vs0.txt"))

	}  # end for wl=1:nnWL
}  # end if iniOk

#### end of program 'BDcompar.R' ############################################
