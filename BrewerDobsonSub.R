#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE COMPARISON                         #
#                                                                           #
#                                                                           #
#        Program Name        :  BrewerDobsonSub.R                           #
#                                                                           #
#                                                                           #
#        Creation Date       :  21.04.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               23.07.2021                                  #
#                               31.08.2020                                  #
#                               14.08.2020                                  #
#                               11.06.2020                                  #
#                               27.06.2019                                  #
#                               15.05.2018                                  #
#                               12.04.2018                                  #
#                               06.12.2017                                  #
#                               27.03.2017                                  #
#                               23.02.2017                                  #
#                               07.02.2017                                  #
#                               27.12.2016                                  #
#                               05.10.2016                                  #
#                               09.05.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'BrewerDobsonSub.R' contains functions for the comparison of two   #
#        instruments (Brewer/Dobson), used by program 'BDcompar.R'.         #
#                                                                           #
#                                                                           #
#        List of functions:                                                 #
#                                                                           #
#          - CalcFlagDiffs                                                  #
#          - CompareMeas                                                    #
#          - FilterData                                                     #
#          - WriteBDcompFiles                                               #
#          - WriteBDcompVD                                                  #
#          - WriteBDcompVS                                                  #
#          - WriteFlaglist                                                  #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 09.05.2016 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'WriteBDcompVD' (error when no comparison data)  #
#                                                                           #
#                                                                           #
#        - 05.10.2016 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'WriteBDcompFiles' (proper dT, dMy values)       #
#                                                                           #
#                                                                           #
#        - 27.12.2016 by Herbert Schill:                                    #
#                                                                           #
#          - call of function CalcTrueNoon in 'CompareMeas'                 #
#                                                                           #
#          - add function 'WriteBDcompAD'                                   #
#                                                                           #
#                                                                           #
#        - 07.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - add function 'WriteFlaglist'                                   #
#                                                                           #
#                                                                           #
#        - 23.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - set wl-index for Dobson in 'WriteFlaglist' as [1..3]           #
#            instead [11..13]                                               #
#                                                                           #
#                                                                           #
#        - 27.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - rename library from 'BDcomparSub.R' to 'BrewerDobsonSub.R'     #
#                                                                           #
#                                                                           #
#        - 06.12.2017 by Herbert Schill:                                    #
#                                                                           #
#          - adapt parameter list of 'CompareMeas'                          #
#                                                                           #
#          - adapt calendar format to array instead of list                 #
#                                                                           #
#                                                                           #
#        - 12.04.2018 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'WriteBDcompVS', when year=2000                  #
#                                                                           #
#                                                                           #
#        - 15.05.2018 by Herbert Schill:                                    #
#                                                                           #
#          - allow 5 or 6 wavelengths for Dobson ozone in 'FilterData'      #
#                                                                           #
#                                                                           #
#        - 27.06.2019 by Herbert Schill:                                    #
#                                                                           #
#          - adapt filenames and headers for Oz/N/N*/SO2-datasets in        #
#            'WriteBDcompFiles' and 'WriteBDcompVS'                         #
#                                                                           #
#                                                                           #
#        - 11.06.2020 by Herbert Schill:                                    #
#                                                                           #
#          - use either airmass (Mu) or ozone slant path (OSP) as a         #
#            filter value in 'FilterData' (if MuMin/MuMax<99: Mu, OSP       #
#            otherwise                                                      #
#                                                                           #
#                                                                           #
#        - 14.08.2020 by Herbert Schill:                                    #
#                                                                           #
#          - Brewer ozone data are corrected by the Rayleigh-correction     #
#            in 'FilterData'                                                #
#                                                                           #
#                                                                           #
#        - 31.08.2020 by Herbert Schill:                                    #
#                                                                           #
#          - Adapt 'FilterData', 'CompareMeas', 'WriteBDcompFiles' to new   #
#            indices of 'iniPar'                                            #
#                                                                           #
#          - Adapt 'WriteBDcompFiles' to additional title resp. output-     #
#            filenames including name of absorption coefficients set        #
#                                                                           #
#                                                                           #
#        - 23.07.2021 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'WriteBDcompVS', when sec=60                     #
#                                                                           #
#                                                                           #
#############################################################################


CalcFlagDiffs <- function (dataFin, RDiffLimit, Order)

#############################################################################
#                                                                           #
#        Function 'CalcFlagDiffs' calculates the relative differences of    #
#        a datset to its polynominal value, or to the mean value of the     #
#        dataset, or checks on differences of consecutive values.           #
#        The worst value is eliminated from the dataset; this procedure     #
#        is repeated until all outliers are eliminated (or no data are      #
#        left ...)                                                          #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        CalcFlagDiffs (dataFin, RDiffLimit, Order)                         #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        dataFin     List of data to analyse                                #
#        RDiffLimit  Maximal relative difference of a value from the        #
#                    reference value                                        #
#        Order       Order of polynome (Order>0), or mean value as refe-    #
#                    rence (Order=0) or test on consecutive values          #
#                    (Order<0)                                              #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return      List of reduced data.                                  #
#                                                                           #
#############################################################################
{

	Test=0

	OzRDiff = array()
	MTime = dataFin$Measurements$MeasTime
	OzVal = dataFin$Measurements$OzVal
	Index = dataFin$Measurements$Index
	len0 = length(Index)
	if (Order>=0)  # CalcPoly or CalcDayoz
	{
		MaxDiff = RDiffLimit+1
		loop=0
		while ((length(Index)>Order) & (MaxDiff>RDiffLimit))
		{
			if (Order>=0)  # CalcPoly
			{
				df = data.frame(MTime,OzVal)
				fm <- lm(OzVal ~ poly(MTime, Order), data = df)
				OzRef = predict(fm, data.frame(MTime))
			} else  # CalcDayoz
			{
				OzRef = mean(OzVal)
			}
			OzRDiff=abs(100*(OzVal-OzRef)/OzRef)
			MaxDiff=max(OzRDiff)
			if (MaxDiff>RDiffLimit)
			{
				loop = loop+1
				MTime <- MTime[OzRDiff!=MaxDiff]
				OzVal <- OzVal[OzRDiff!=MaxDiff]
				Index <- Index[OzRDiff!=MaxDiff]
			}
		}  # end while
		#length(Index);MaxDiff;RDiffLimit;Index;OzRDiff
	} else  # CalcConsec
	{
		if (len0>2)
		{
			if (len0<5) Order=len0-1 else Order=4
			df = data.frame(MTime,OzVal)
			fm <- lm(OzVal ~ poly(MTime, Order), data = df)
			OzRef = predict(fm, data.frame(MTime))
			len1=len0
			######################################
			if (Test>0)
			{
				j=0
				OzConIndex = array()
				OzConRDiffVect = array()
				OzConRDiffVect[1]=0
				for (i in 2:len0)
				{
					OzConRDiffVect[i]=abs(100*(OzVal[i]-OzVal[i-1])/OzVal[i-1])
					if ((OzConRDiffVect[i]>RDiffLimit) & ((MTime[i]-MTime[i-1])<DeltaTconsec))
					{
						if (abs(OzVal[i]-OzRef[i])<abs(OzVal[i-1]-OzRef[i-1])) ix=i-1 else ix=i
						j=j+1
						OzConIndex[j]=ix
					}
				}
				RDiffLimit;DeltaTconsec;OzConIndex;OzConRDiffVect;MTime;OzVal
			}  # end if Test>0
			######################################

			i=2
			while (i<=len1)
			{
				OzConRDiff=abs(100*(OzVal[i]-OzVal[i-1])/OzVal[i-1])
				#test1=((OzConRDiff>RDiffLimit) & ((MTime[i]-MTime[i-1])<DeltaTconsec))
				#test1;OzConRDiff;OzVal[i-1];OzVal[i];OzRef[i-1];OzRef[i]
				if ((OzConRDiff>RDiffLimit) & ((MTime[i]-MTime[i-1])<DeltaTconsec))
				{
					if (abs(OzVal[i]-OzRef[i])<abs(OzVal[i-1]-OzRef[i-1])) ix=i-1 else ix=i
					TimeX=MTime[ix]
					Index <- Index[MTime!=TimeX]
					OzRef <- OzRef[MTime!=TimeX]
					OzVal <- OzVal[MTime!=TimeX]
					MTime <- MTime[MTime!=TimeX]
					len1=len1-1
					#len1;Index;MTime;OzVal
				} else i=i+1# end if
			}  # wend
		}  # end if len0>2
	}  # end CalcConsec
	len1=length(Index)
	DayOz = mean(OzVal)
	len1;MTime;OzVal;OzRDiff;Index;DayOz

	# Write data in lists

	Measures = list(len0, len1, DayOz, Index, MTime, OzVal)
	names(Measures) = c("NMeasIn", "NMeasOut", "DayOz", 
                      "Index", "MeasTime", "OzVal")

	return(list("Measurements"=Measures))
	# dataF2=list("Measurements"=Measures)

	if (Test>0)
	{
		# An example of polynomial regression with plot

		minOz0=as.integer(min(dataF1$Measurements$OzVal))-5
		maxOz0=as.integer(max(dataF1$Measurements$OzVal))+5
		plot(df, xlab = "Time (hrs)", ylab = "Ozone (DU)", las = 1, xlim = c(7, 15), ylim=c(minOz0, maxOz0))
		d <- seq(7, 15, length.out = 300)
		fm <- lm(OzVal ~ poly(MTime, Order), data = df)
		# d <- MTime
		#	xp=predict(fm, data.frame(MTime))
		#	lines(d, xp, col = Order)
		lines(d, predict(fm, data.frame(MTime=d)), col = Order)
	}

}  # end of function 'CalcFlagDiffs'


CompareMeas <- function (iniPar, idata1, idata2, TimeHMax)

#############################################################################
#                                                                           #
#        Function 'CompareMeas' compares two datasets of Brewer and/or      #
#        Dobson measurements; quasi-simultaneous measurements are taken     #
#        and analysed.                                                      #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        CompareMeas (iniPar, idata1, idata2, TimeHMax)                     #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        iniPar    List of input parameters from 'BDcompar.ini'             #
#        idata1    List of data of instrument 1 (reference) to compare      #
#        idata2    List of data of instrument 2 to compare                  #
#        TimeHMax  Time of true noon [hrs]                                  #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        cdata    List of compared data.                                    #
#                                                                           #
#############################################################################
{

	mx = array()
	my = array()
	timRef  = array()
	timCom  = array()
	dTarry  = array()
	iParry  = array()
	halfDay = array()
	myRef   = array()
	myCom   = array()
	valRef  = array()
	valCom  = array()
	absDif  = array()
	relDif  = array()

	dTmax  = as.double(iniPar[32])/60
	dMYmax = as.double(iniPar[33])

	i1 = idata1[[1]][[1]]
	i2 = idata2[[1]][[1]]
	ma=0
	mp=0
	mr=1
	m=0

	# Create matrix with time differences

	for (k in 1:i2)
	{
		tRef = idata2[[1]][[2]][[k]]
		for (r in 1:i1)
		{
			m=m+1
			dTarry[m] = tRef-idata1[[1]][[2]][[r]]
			iParry[m] = 0
		}
	}
	deltaT =matrix(dTarry,i1,i2)
	isPair=matrix(iParry,i1,i2)

	# Find pairs of minimal time difference, which meet limits (dTmax, dMyMax)

	m=0
	mr=1
	for (k in 1:i1)
	{

		dT0=99
		for (r in mr:i2)
		{
			dT = abs(deltaT[k,r])
			if (dT < dT0)
			{
				mr = r
				dT0 = dT
			} else {break}
		}  # next r

		if (abs(deltaT[k,mr])<=dTmax)
		{
			yRef = idata2[[1]][[3]][[mr]]
			yCom = idata1[[1]][[3]][[k]]
			if (abs(yRef-yCom)<=dMYmax)
			{
				isPair[k,mr]=1
				m=m+1
				mx[m]=k
				my[m]=mr
			}
		}

		if (k>1)
		{
			if (isPair[k-1,mr]==1)
			{
				if (abs(deltaT[k,mr])<abs(deltaT[k-1,mr]))
				{
					isPair[k-1,mr]=0
					m=m-1
					mx[m]=k
					my[m]=mr
				} else
				{
					m=m-1
					mr=mr+1
				}
			}
		}  # end if k>1
	if (mr>i2) break
	}  # next k

	# Find pairs with time differences, which still meet limits (dTmax, dMyMax)

	if (m>2)
	{
		n=mx[m]
		for (k in 2:n)
		{
			if (k>m) break
			if (((mx[k]-mx[k-1])>1) & ((my[k]-my[k-1])>1))
			{
				x1=mx[k-1]+1
				x2=mx[k]-1
				for (i in x1:x2)
				{
					y1=my[k-1]+1
					y2=my[k]-1
					for (j in y1:y2)
					{
						if (abs(deltaT[i,j])<=dTmax)
						{
							yRef = idata2[[1]][[3]][[j]]
							yCom = idata1[[1]][[3]][[i]]
							if (abs(yRef-yCom)<=dMYmax)
							{
								for (l in m:k)  # shift vector
								{
									mx[l+1]=mx[l]
									my[l+1]=my[l]
								}
								m=m+1
								mx[k]=i
								my[k]=j
								isPair[i,j]=1
								break
							}  # end if deltaMy<dMYmax
						}  # end if deltaT<dTmax
					}  # next j
				}  # next i
			}  # end if mx/my
		}  # next k
	}  # end if m>2

	# Definitive comparison pairs found

	k=0
	if (m>0)  
	{
		for (i in 1:m)
		{
			mk=mx[i]
			mr=my[i]
			if (isPair[mk,mr]==1)
			{
				k=k+1
				timRef[k] = idata1[[1]][[2]][[mk]]
				myRef[k]  = idata1[[1]][[3]][[mk]]
				timCom[k] = idata2[[1]][[2]][[mr]]
				myCom[k]  = idata2[[1]][[3]][[mr]]
				valRef[k] = idata1[[1]][[4]][[mk]]
				valCom[k] = idata2[[1]][[4]][[mr]]
				absDif[k] = valRef[k]-valCom[k]
				relDif[k] = absDif[k]*100/valRef[k]
				#timRef[k];TimeHMax;valRef[k];valCom[k];absDif[k];relDif[k]  # test
				if (timRef[k]<=TimeHMax)
				{
					halfDay[k]="AM"
					ma=ma+1
				} else 
				{
					halfDay[k]="PM"
					mp=mp+1
				}
			}  # end if isPair[mk,mr]=1
		}  # next i
	}  # end if m>0

# Write data in lists

CompMeasures = list(k, ma, mp, halfDay, timRef, timCom, myRef, myCom, valRef, valCom, absDif, relDif)
names(CompMeasures) = c("nDay", "nAM", "nPM", "halfDay", "Time1", "Time2", "Airmass1", "Airmass2", 
                        "RefVal", "CompVal", "AbsDiff", "RelDiff")

return(list("CompMeasurements"=CompMeasures))

}  # end of function 'CompareMeas'


FilterData <- function (ix, iniPar, idata, RNdata)


#############################################################################
#                                                                           #
#        Function 'FilterData' filters a set of total ozone measurements    #
#        of a Brewer or a Dobson by using various filters: flag, min/max    #
#        airmass/OSP, max staDev, min/max neutral density filter (Brewer),  #
#        min/max sun intensity (Dobson) and returns the reduced data set.   #
#                                                                           #
#        Function call: FilterData (ix, iniPar, idata, RNdata)              #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        ix       Instrument index  [1/2]                                   #
#        iniPar   List of input parameters from 'BDcompar.ini'              #
#        idata    List of raw and calculated measurement data               #
#        RNdata   List of R/N conversion tables (Dobson only)               #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        fdata    List of measurement data used for the comparison.         #
#                                                                           #
#############################################################################
{

	# Define filter values for flag, airmass/ozone slant path, staDev, ndf 
	# and sun boundaries
	#
	# ii    [Filters]
	# 23    [Skip flagged measurements: Y/N]
	# 24    [Minimal airmass/ozone slant path value  [1.000..9.999] 
	#        resp. [100..1500], standard=1.000 resp. 100]
	# 25    [Maximal airmass/ozone slant path value  [1.000..9.999] 
	#        resp. [100..1500], standard=5.000 resp. 1500]
	# 26    [Maximal staDev Brewer  [0.0..9.9], standard=[2.5]
	# 27    [Maximal staDev Dobson  [0.0..9.9], standard=[0.5]
	# 28    [Minimal neutral density filter Brewer [1..5], standard=1]
	# 29    [Maximal neutral density filter Brewer [1..5], standard=5]
	# 30    [Minimal sun intensity Dobson  [10..40], standard=28]
	# 31    [Maximal sun intensity Dobson  [10..40], standard=40]
	# 32    [Maximal difference in time in min. [m.m], standard=5.0 min
	# 33    [Maximal difference in airmass [a.aa], standard=0.05

	isOSP=0
	OzMin=100
	OzMax=600
	SkipFlag = iniPar[23]
	MuMin  = as.double(iniPar[24])
	MuMax  = as.double(iniPar[25])
	dTmax  = as.double(iniPar[32])
	dMuMax = as.double(iniPar[33])
	if ((MuMin>99) & (MuMax>99)) isOSP=1

	dataType = iniPar[(ix-1)*8+9]
	waveleng = iniPar[(ix-1)*8+10]  # Dobson: Wavelength, Brewer: Rayleigh-correction
	if (substr(iniPar[(ix-1)*8+6],1,1)=="D")  # Dobson
	{
		isDobson=TRUE
		SDevMax=as.double(iniPar[27])
		SunMin=as.integer(iniPar[30])  # sun intensity
		SunMax=as.integer(iniPar[31])  # sun intensity
	} else  # Brewer
	{
		isDobson=FALSE
		RayleighCorr=as.double(waveleng)
		SDevMax=as.double(iniPar[26])
		NdfMin=as.integer(iniPar[28])  # neutral density filter
		NdfMax=as.integer(iniPar[29])  # neutral density filter
	}

	MeasTime  = array()
	Flag      = array()
	Airmass   = array()
	Ozone     = array()

	CTimeStr   = array()
	CMeasTime  = array()
	CAirmass   = array()
	CompVal    = array()

	if (isDobson)  # Filter Dobson measurements
	{
		Sun    = array()
		NVal   = array()
		RVal   = array()
		StaDev = array()
		CNVal  = array()

		# Get proper dN-values, if desired

		if ((dataType=="N") | (dataType=="N*"))
		{
			dNval = array()
			vStr = array()
			vStr=strsplit(idata[[1]][[6]]," ")
			dNval[1] = as.double(headStr[[1]][[3]])
			dNval[2] = as.double(headStr[[1]][[4]])
			dNval[3] = as.double(headStr[[1]][[2]])
		}

		# Reshapes the data vectors

		Sun      = idata[[2]][[2]]
		MeasTime = idata[[2]][[5]]
		RVal     = idata[[2]][[6]]
		StaDev   = idata[[2]][[7]]
		Flag     = idata[[2]][[8]]
		Airmass  = idata[[2]][[9]]
		Ozone    = idata[[2]][[10]]
		nwl=nrow(Ozone)

		nmeas = idata[[2]][[1]]  # number of measurements of the day

		dim(MeasTime) = c(3,nmeas)
		dim(RVal)     = c(3,nmeas)
		dim(StaDev)   = c(3,nmeas)
		dim(Flag)     = c(3,nmeas)
		dim(Airmass)  = c(3,nmeas)
		dim(Ozone)    = c(nwl,nmeas)

		# select proper wavelengths indices

		w=(grep(waveleng,list("C", "D", "A", "AD", "CD", "AC"))[1])
		w1 = switch (w, 1,2,3,3,1,3)
		w2 = switch (w, 1,2,3,2,2,1)

		# proceed measurements

		m=0
		for (n in 1:nmeas)
		{
			# Test on: Ozone, flag, staDev, airmass/OSP, sun intensity
			# Create new dataset with correct measurements only

			if ((Ozone[w,n]>=OzMin) & (Ozone[w,n]<=OzMax))
			{
				if ((SkipFlag=="N") | ((SkipFlag=="Y") & (Flag[w1,n]==0) & (Flag[w2,n]==0)))
				{
					if ((StaDev[w1,n]<=SDevMax) & (StaDev[w2,n]<=SDevMax))
					{
						Mu=(Airmass[w1,n]+Airmass[w2,n])/2
						if (isOSP) Mu=Mu*Ozone[w,n]
						if ((Mu>=MuMin) & (Mu<MuMax))
						{
							if ((Sun[n]>=SunMin) & (Sun[n]<=SunMax))
							{
								m=m+1
								CMeasTime[m] = (MeasTime[w1,n]+MeasTime[w2,n])/2
								CAirmass[m] = (Airmass[w1,n]+Airmass[w2,n])/2

								# R to N conversion (if dataType=N) and dN correction (if dataType=N*)

								if (((dataType=="N") | (dataType=="N*")) & (w<4))
								{
									j = (w %% 3)+1  # reverse order for reading RN-table
									if (RVal[w,n]>289) RVal[w,n]=289
									IR = floor((RVal[w,n])/10.0)
									n1 = RNdata[[j+1]][[IR+1]]
									n2 = RNdata[[j+1]][[IR+2]]
									CompVal[m] = n1+(n2-n1)*((RVal[w,n]-(IR*10)))/10.0
									if (dataType=="N*") CompVal[m] = CompVal[m] + dNval[w]
								} else  # if dataType<>N/N*
								{ CompVal[m] = Ozone[w,n]}
								
							}  # if Sun
						}  # if Mu
					}  # if StaDev
				}  # if SkipFlag
			}  # if Ozone
		}  # for n=1..nmeas

	} else  # is Brewer: filter Brewer measurements
	{
		# proceed measurements; correct ozone data by RayleighCorr

		nmeas = idata$HeaderData$NMeas  # number of measurements of the day
		Ozone = idata$Measurements$Oz3Org + RayleighCorr
		m=0
		for (n in 1:nmeas)
		{
			# Test on: Ozone, flag, staDev, airmass/OSP, sun intensity
			# Create new dataset with correct measurements only

			if ((Ozone[n]>=OzMin) & (Ozone[n]<=OzMax))
			{
				if ((SkipFlag=="N") | ((SkipFlag=="Y") & (idata$Measurements$Flag[n]==0)))
				{
					if (dataType=="O3")
					{
						cVal   = Ozone[n]
						errVal = idata$Measurements$ErrOz3[n]
					} else
					{
						cVal   = idata$Measurements$SO2org[n]
						errVal = idata$Measurements$ErrSO2[n]
					}
					if (errVal<=SDevMax)
					{
						Mu = idata$Measurements$Airmass[n]
						if (isOSP) Mu=Mu*Ozone[n]
						if  ((Mu>=MuMin) & (Mu<MuMax))
						{
							if ((idata$Measurements$NDF[n]>=NdfMin) & (idata$Measurements$NDF[n]<=NdfMax))
							{
								m=m+1
								CMeasTime[m] = idata$Measurements$MeasTime[n]
								CAirmass[m]  = idata$Measurements$Airmass[n]
								CompVal[m]   = cVal
							}  # if NDF
						}  # if Airmass
					}  # if StaDev
				}  # if SkipFlag
			}  # if Ozone
		}  # for n=1..nmeas

	}  # is Dobson/Brewer

	# Write data in lists

	Measures = list(m, CMeasTime, CAirmass, CompVal)
	names(Measures) = c("NumberMeas", "MeasTime", "Airmass", "CompVal")

	return(list(Measurements=Measures))

}  # end of function 'FilterData'


WriteBDcompFiles <- function (iniPar, titleInfo, ad0, vd0, vs0)

#############################################################################
#                                                                           #
#        Function 'WriteBDcompFiles' writes the titles to the different     #
#        output files of the Brewer/Dobson comparison and copies the data   #
#        lines from temporary files to definitive files; close all files    #
#                                                                           #
#        Function call: WriteBDcompFiles (iniPar, titleInfo, ad0, vd0, vs0) #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        iniPar     List of input parameters from 'BDcompar.ini'            #
#        titleInfo  List of title data                                      #
#        ad0        Output-file for daily means of all data                 #
#        ad0        Output-file for daily means of compared data            #
#        ad0        Output-file for single values of compared data          #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        outOk      not used                                                #
#                                                                           #
#############################################################################
{

	compType = titleInfo[[1]]
	yearA = titleInfo[[2]]
	yearE = titleInfo[[3]]
	monA  = titleInfo[[4]]
	monE  = titleInfo[[5]]
	totAD = titleInfo[[6]]
	totVD = titleInfo[[7]]
	totVS = titleInfo[[8]]
	sep   = titleInfo[[9]]
	RayCor1 = titleInfo[[10]]
	RayCor2 = titleInfo[[11]]

	# Reopen temporary output files

		ad0 = file((sprintf("%s%s", iniPar[35], "ad0.txt")),open="r")
		vd0 = file((sprintf("%s%s", iniPar[35], "vd0.txt")),open="r")
		vs0 = file((sprintf("%s%s", iniPar[35], "vs0.txt")),open="r")

	# Create header version message "Calc: dd.mm.yyyy  hh:mm"

	fd=as.character(Sys.time())  # "yyyy-mm-dd hh:mm:ss"
	CalcInfo = sprintf("%s%s.%s.%s", "Calc: ", substr(fd,9,10), substr(fd,6,7), substr(fd,1,4))
	CalcInfo = sprintf("%s  %s", CalcInfo, substr(fd,12,16))

	# Create strings for different typs of output data

	if (iniPar[9]=="N")
	{
		daty1 = "N0"
		dataType1 = " Nval1"
	} else if (iniPar[9]=="N*")
	{
		daty1 = "NN"
		dataType1 = "N*val1"
	} else if (iniPar[9]=="SO2")
	{
		daty1 = "SO"
		dataType1 = " SO2_1"
	} else
	{
		daty1 = ""
		dataType1 = " Ozon1"
	}

	if (iniPar[17]=="N")
	{
		daty2 = "N0"
		dataType2 = " Nval2"
	} else if (iniPar[17]=="N*")
	{
		daty2 = "NN"
		dataType2 = "N*val2"
	} else if (iniPar[17]=="SO2")
	{
		daty2 = "SO"
		dataType2 = " SO2_2"
	} else
	{
		daty2 = ""
		dataType2 = " Ozon2"
	}

	# Create output filenames  e.g. "VS_2015_D062AD_Cal15d_IUPSerdy_D101AD_Cal15d_IUPSerTUPS101.txt"

	if (compType<3)
		{name1 = sprintf("%s%s%s_%s_%s", iniPar[6], daty1, iniPar[10], iniPar[7], iniPar[8])}  else
		{name1 = sprintf("%s%s_%s_%s", iniPar[6], daty1, iniPar[7], iniPar[8])}

	if ((compType==1) || (compType==3))
		{name2 = sprintf("%s%s%s_%s_%s", iniPar[14], daty2, iniPar[18], iniPar[15], iniPar[16])}  else
		{name2 = sprintf("%s%s_%s_%s", iniPar[14], daty2, iniPar[15], iniPar[16])}

	if (yearA==yearE)
		{nameRoot = sprintf("%4d_%s_%s%s", yearE, name1, name2, ".txt")}  else
		{nameRoot = sprintf("%4d-%4d_%s_%s%s", yearA, yearE, name1, name2, ".txt")}

	# Open output files and write headers

	l=nchar(iniPar[35])
	if (substr(iniPar[35],l,l)!="\\") iniPar[35]=sprintf("%s%s", iniPar[35], "\\")
	adFilename = sprintf("%s%s", "AD_", nameRoot)
	adPathname = sprintf("%s%s", iniPar[35], adFilename)
	vdFilename = sprintf("%s%s", "VD_", nameRoot)
	vdPathname = sprintf("%s%s", iniPar[35], vdFilename)
	vsFilename = sprintf("%s%s", "VS_", nameRoot)
	vsPathname = sprintf("%s%s", iniPar[35], vsFilename)

	ad = file(adPathname,open="w")
	vd = file(vdPathname,open="w")
	vs = file(vsPathname,open="w")

	title1AD = sprintf ("%10d%s%s%s %s%s%s%6.2f%s%6.2f", totAD, sep, " Daily- resp. Halfday Means", sep, CalcInfo, sep, " Rayleigh-Corr:", RayCor1, sep, RayCor2)
	title1VD = sprintf ("%10d%s%s%s %s%s%s%6.2f%s%6.2f", totVD, sep, " Daily- resp. Halfday Means", sep, CalcInfo, sep, " Rayleigh-Corr:", RayCor1, sep, RayCor2)
	title1VS = sprintf ("%8d%s%s%s %s%s%s%6.2f%s%6.2f", totVS, sep, " Single Measurements", sep, CalcInfo, sep, " Rayleigh-Corr:", RayCor1, sep, RayCor2)

	title2VD = sprintf ("%s%s", "      Date", sep)
	title2VD = sprintf ("%s%s%s%s%s%s%s%s%s", title2VD, " Comparison Day                    ", sep, sep, sep, sep, sep, sep, sep)
	title2VD = sprintf ("%s%s%s%s%s%s%s%s%s", title2VD, " Comparison AM                     ", sep, sep, sep, sep, sep, sep, sep)
	title2VD = sprintf ("%s%s", title2VD, " Comparison PM")

	title2AD = sprintf ("%s%s", "      Date", sep)
	title2AD = sprintf ("%s%s%s%s%s%s%s%s", title2AD, " All Measurements Day         ", sep, sep, sep, sep, sep, sep)
	title2AD = sprintf ("%s%s%s%s%s%s%s%s", title2AD, " All Measurements AM          ", sep, sep, sep, sep, sep, sep)
	title2AD = sprintf ("%s%s", title2AD, " All Measurements PM")

	title3VD = sprintf ("%s%s%s%s%s%s%s", sep, dataType1, sep, dataType2, sep, "  N", sep)
	title3VD = sprintf ("%s%s%s%s%s%s%s%s", title3VD, " Diff", sep, " Quot", sep, " Zero", sep, " Slope")
	title3VD = sprintf ("%s%s%s%s", "dd.mm.yyyy", title3VD, title3VD, title3VD)

	title3AD = sprintf ("%s%s%s%s%s%s%s%s%s%s%s%s", sep, dataType1, sep, "  N1", sep, " SDv1", sep, dataType2, sep, "  N2", sep, " SDv2")
	title3AD = sprintf ("%s%s%s%s", "dd.mm.yyyy", title3AD, title3AD, title3AD)

	title2VS = sprintf ("%s%s%s%s%s%s%s%s%s%s", "dd.mm.yy", sep, " ht", sep, "  T1(UTC)", sep, "    My1", sep, dataType1, sep)
	title2VS = sprintf ("%s%s%s%s%s%s%s%s%s%s", title2VS, dataType2, sep, "  Diff", sep, "  Quot", sep, "    My2", sep, "  T2(UTC)")

	dateStr = sprintf("%02d%s%04d%s%02d%s%04d", monA, ".", yearA, "-", monE, ".", yearE)
	deltaStrV = sprintf("%s%s%s%s", " (Delta t = ", iniPar[32], " min., Delta My = ", iniPar[33])
	deltaStrV = sprintf("%s%s%s%s%s%s", deltaStrV, ", My-Range= ", iniPar[24], " ... ", iniPar[25], ")")
	deltaStrA = sprintf("%s%s%s%s%s", " (My-Range= ", iniPar[24], " ... ", iniPar[25], ")")

	if (compType<3)
		{name1 = sprintf("%s%s (%s %s)", iniPar[6], iniPar[10], iniPar[7], iniPar[8])} else
		{name1 = sprintf("%s (%s %s)", iniPar[6], iniPar[7], iniPar[8])}
	if ((compType==1) ||(compType==3))
		{name2 = sprintf("%s%s (%s %s)", iniPar[14], iniPar[18], iniPar[15], iniPar[16])} else
		{name2 = sprintf("%s (%s %s)", iniPar[14], iniPar[15], iniPar[16])}

	title0 = sprintf ("%s%s - %s  %s%s", "Total Ozone Intercomparison  ", name1, name2, dateStr, sep)

	outl = sprintf ("%s%s%s%s%s%s%s%s%s", title0, deltaStrA, "\n", title1AD, "\n", title2AD, "\n", title3AD, "\n")
	cat (outl, file=ad)
	outl = sprintf ("%s%s%s%s%s%s%s%s%s", title0, deltaStrV, "\n", title1VD, "\n", title2VD, "\n", title3VD, "\n")
	cat (outl, file=vd)
	outl = sprintf ("%s%s%s%s%s%s%s", title0, deltaStrV, "\n", title1VS, "\n", title2VS, "\n")
	cat (outl, file=vs)

	# Copy data lines from temporary files to definitive files; close all files
	
	for (i in 1:totVS)
	{
		outl = sprintf ("%s%s", (readLines(vs0,1)), "\n")
		cat (outl, file=vs)
	}
	for (i in 1:totVD)
	{
		outl = sprintf ("%s%s", (readLines(vd0,1)), "\n")
		cat (outl, file=vd)
	}
	for (i in 1:totAD)
	{
		outl = sprintf ("%s%s", (readLines(ad0,1)), "\n")
		cat (outl, file=ad)
	}

	close(ad0)
	close(vd0)
	close(vs0)
	close(ad)
	close(vd)
	close(vs)

}  # end of function 'WriteBDcompFiles'


WriteBDcompAD <- function (Cal, TimeHMax, fdata1, fdata2, ad, sep)

#############################################################################
#                                                                           #
#        Function 'WriteBDcompAD' writes the daily means and the halfday    #
#        means of all measurements of two Dobsons, two Brewers or a Dobson  #
#        and a Brewer for one day to the (temporary) file 'ad'.             #
#                                                                           #
#        Function call: WriteBDcompAD (Cal, fdata1, fdata2, ad, sep)        #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Cal       Calendar [day, month, year, jdn, leap]                   #
#        TimeHMax  Time of true noon [hrs]                                  #
#        fdata1    List of all data of instrument 1                         #
#        fdata2    List of all data of instrument 2                         #
#        ad        (Temporary) output-file                                  #
#        sep       Separator in data lines (e.g. sep=";")                   #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        ad       Updated (temporary) output-file                           #
#                                                                           #
#############################################################################
{

	# Write line header and statistical data

	xTime = array()
	NMes = array()
	Ozon = array()
	SDev = array()
	outl = sprintf ("%02d.%02d.%04d%s", Cal[1], Cal[2], Cal[3], sep)
	fdata=fdata1

	for (i in 1:2)  # treat instruments
	{
		# separate data block in AM and PM

		nnM = array()
		nnM[1] = fdata[[1]][[1]]
		if (nnM[1]>0)
		{
			xTime = fdata$Measurements$MeasTime
			nnM[2]= length(xTime[xTime<=TimeHMax])
			nnM[3]=nnM[1]-nnM[2]
		} else
		{
			nnM[2:3]=0
		}

		for (h in 1:3)
		{
			m = i+(h-1)*2
			if (nnM[h]>0)
			{
				na = switch(h, 1, 1, nnM[2]+1)
				ne = switch(h, nnM[1], nnM[2], nnM[1])
				NMes[m] = nnM[h]
				Ozon[m] = mean(fdata$Measurements$CompVal[na:ne])
				if (nnM[h]>1)
				{
					SDev[m] = sd(fdata$Measurements$CompVal[na:ne])
				} else
				{
					SDev[m]=0
				}
			} else
			{
				NMes[m] = 0
				Ozon[m] = 0
				SDev[m] = 0
			}
		}  # next h
		fdata=fdata2
	}  # next i

	if ((NMes[1]>0) | (NMes[2]>0))
	{
		for (i in 1:6) outl = sprintf ("%s%6.1f%s%4d%s%5.2f%s", outl, Ozon[i], sep, NMes[i], sep, SDev[i], sep)
	}
	outl = sprintf ("%s%s", outl, "\n")
	cat (outl, file=ad)
	return(ad)

}  # end of function 'WriteBDcompAD'


WriteBDcompVD <- function (Cal, cdata, vd, sep)

#############################################################################
#                                                                           #
#        Function 'WriteBDcompVD' writes the daily means and the halfday    #
#        means of the quasi-simultaneous measurements of two Dobsons, two   #
#        Brewers or a Dobson and a Brewer for one day to the (temporary)    #
#        file 'vd'.                                                         #
#                                                                           #
#        Function call: WriteBDcompVD (Cal, cdata, vd, sep)                 #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Cal      Calendar [day, month, year, jdn, leap]                    #
#        cdata    List of quasi-simultaneous data                           #
#        vs       (Temporary) output-file                                   #
#        sep      Separator in data lines (e.g. sep=";")                    #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        vd       Updated (temporary) output-file                           #
#                                                                           #
#############################################################################
{


	# Write line header and statistical data

	xMy = array()
	yRD = array()
	outl = sprintf ("%02d.%02d.%04d%s", Cal[1], Cal[2], Cal[3], sep)
	nDY = cdata[[1]][[1]]

	if (nDY>0)
	{
		nAM = cdata[[1]][[2]]
		nPM = cdata[[1]][[3]]

		for (h in 1:3)
		{
			nn = cdata[[1]][[h]]
			if (nn>0)
			{
				na = switch(h, 1, 1, nAM+1)
				ne = switch(h, nDY, nAM, nDY)
				oz1=mean(cdata$CompMeasurements$RefVal[na:ne])
				oz2=mean(cdata$CompMeasurements$CompVal[na:ne])
				quot=mean(cdata$CompMeasurements$RelDiff[na:ne])
				adif=mean(cdata$CompMeasurements$AbsDiff[na:ne])
				if (nn>1)
				{
					xMy = cdata$CompMeasurements$Airmass1[na:ne]
					yRD = cdata$CompMeasurements$RelDiff[na:ne]
					linReg=lm(yRD ~ xMy)
					zero=linReg[[1]][[1]]
					slope=linReg[[1]][[2]]
				} else
				{
					zero=0
					slope=0
				}
				outl = sprintf ("%s%6.1f%s%5.1f%s%3d%s%5.1f%s", outl, oz1, sep, oz2, sep, nn, sep, adif, sep)
				outl = sprintf ("%s%5.2f%s%5.1f%s%6.2f%s", outl, quot, sep, zero, sep, slope, sep)
			} else
			{
				outl = sprintf ("%s%s", outl, "      ;     ;   ;     ;     ;     ;      ;")
			}
		}  # next h
	}  # end if nDY>0

	outl = sprintf ("%s%s", outl, "\n")
	cat (outl, file=vd)
	return(vd)

}  # end of function 'WriteBDcompVD'


WriteBDcompVS <- function (Cal, cdata, vs, sep)

#############################################################################
#                                                                           #
#        Function 'WriteBDcompVS' writes the set of quasi-simultaneous      #
#        measurements of two Dobsons, two Brewers or a Dobson and a Brewer  #
#        for one day to the (temporary) file 'vs'.                          #
#                                                                           #
#        Function call: WriteBDcompVS (Cal, cdata, vs, sep)                 #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        Cal      Calendar [day, month, year, jdn, leap]                    #
#        cdata    List of quasi-simultaneous data                           #
#        vs       (Temporary) output-file                                   #
#        sep      Separator in data lines (e.g. sep=";")                    #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        vs       Updated (temporary) output-file                           #
#                                                                           #
#############################################################################
{


	# Write line header and statistical data; format/example:

	#dd.mm.yy; ht;  T1(GMT);    My1; Ozon1; Ozon2;  Diff;  Quot;    My2;  T2(GMT)
	#12.10.15; AM;  8:15:04;  2.448; 278.2; 248.6;  29.6; 10.64;  2.454;  8:14:32

	nMeas = cdata[[1]][[1]]
	if (Cal[3]>=2000) {yr=Cal[3]-2000} else {yr=Cal[3]-1900}
	DateStr = sprintf ("%02d.%02d.%02d%s%s", Cal[1], Cal[2], yr, sep, " ")

	for (m in 1:nMeas)
	{
		mTime=cdata[[1]][[5]][[m]]
		hr=as.integer(mTime)
		mn=as.integer((mTime-hr)*60)
		sec=as.integer((mTime-hr-mn/60)*3600+0.5)
		if (sec==60)
		{
			sec=0
			mn=mn+1
			if (mn==60)
			{
				mn=0
				hr=hr+1
			}
		}
		t1Str = sprintf ("%3d:%02d:%02d%s", hr, mn, sec, sep)

		mTime=cdata[[1]][[6]][[m]]
		hr=as.integer(mTime)
		mn=as.integer((mTime-hr)*60)
		sec=as.integer((mTime-hr-mn/60)*3600+0.5)
		if (sec==60)
		{
			sec=0
			mn=mn+1
			if (mn==60)
			{
				mn=0
				hr=hr+1
			}
		}
		t2Str = sprintf ("%3d:%02d:%02d", hr, mn, sec)

		hd =cdata[[1]][[4]][[m]]
		my1=cdata[[1]][[7]][[m]]
		my2=cdata[[1]][[8]][[m]]
		oz1=cdata[[1]][[9]][[m]]
		oz2=cdata[[1]][[10]][[m]]
		adif=cdata[[1]][[11]][[m]]
		quot=cdata[[1]][[12]][[m]]

		outl = sprintf ("%s%s%s%s%7.3f%s", DateStr, hd, sep, t1Str, my1, sep)
		outl = sprintf ("%s%6.2f%s%6.2f%s%6.2f%s", outl, oz1, sep, oz2, sep, adif, sep)
		outl = sprintf ("%s%6.2f%s%7.3f%s%s%s", outl, quot, sep, my2, sep, t2Str, "\n")
		cat (outl, file=vs)
	}

	return(vs)

}  # end of function 'WriteBDcompVS'


WriteFlaglist <- function (day, mon, year, jdn, isDobson, wl, dataIn, fl)

#############################################################################
#                                                                           #
#        Function 'WriteFlaglist' appends information about flagged         #
#        measurements of a Brewer or Dobson to a flaglist file.             #
#                                                                           #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return   updated flaglist file 'fl'                                #
#                                                                           #
#############################################################################
{

	li   = dataIn$flagList
	nflag = length(li$TimeStr)

	if (isDobson)
	{
		instr = dataIn$dataAE$HeaderData$DobsonId
		if (wl=="C")
		w=1 else if (wl=="D")
		w=2 else if (wl=="A")
		w=3

		# write flagged measurement values to flaglist 'fl'
		#
		# dd	mm	jdn	Instr	nf	hh:mm:ss	time	temp	my	Ozon	sd/sun	SO2	sd	A	C	D	Remark

		hed = sprintf ("%02i\t%02i\t%03i\t%s\t%s\t", day, mon, jdn, instr, w)
		for (n in 1:nflag)
		{
			outl = sprintf ("%s%s\t%4.2f\t%2i\t", hed, li$TimeStr[n], li$MeasTime[n], li$InstrTemp[n])
			outl = sprintf ("%s%5.3f\t%5.1f\t%2i\t", outl, li$Airmass[n], li$Ozone[n], li$SunInt[n])
			outl = sprintf ("%s\t\t%1i\t%1i\t%1i\t", outl, li$Flags[3,n], li$Flags[1,n], li$Flags[2,n])
			outl = sprintf ("%s%s\n", outl, "auto-flagged")
			cat (outl, file=fl)
		}  # next n
	} else #  if is Brewer
	{
		instr = dataIn$dataDS$HeaderData$Brewer

		# write flagged measurement values to flaglist 'fl'
		#
		# dd	mm	jdn	Instr	nf	hh:mm:ss	time	temp	my	Ozon	sd/sun	SO2	sd	A	C	D	Remark

		hed = sprintf ("%02i\t%02i\t%03i\t%s\t", day, mon, jdn, instr)
		for (n in 1:nflag)
		{
			outl = sprintf ("%s%1i\t%s\t%4.2f\t%2i\t", hed, li$NDF[n], li$TimeStr[n], li$MeasTime[n], li$InstrTemp[n])
			outl = sprintf ("%s%5.3f\t%5.1f\t%4.1f\t", outl, li$Airmass[n], li$Ozone[n], li$sdOz3[n])
			outl = sprintf ("%s%5.1f\t%4.1f\t%1i\t\t\t%s\n", outl, li$SO2[n], li$sdSO2[n], li$Flags[n], "auto-flagged")
			cat (outl, file=fl)
		}  # next n
	}  # end if is Dobson/Brewer

	return(fl)

}  # end of function 'WriteFlaglist'


#### end of library 'BrewerDobsonSub.R' ######################################
