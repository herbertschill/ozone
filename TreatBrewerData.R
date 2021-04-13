#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Module Name         :  TreatBrewerData.R                           #
#                                                                           #
#                                                                           #
#        Creation Date       :  21.04.2016                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               06.01.2021                                  #
#                               28.12.2020                                  #
#                               22.07.2020                                  #
#                               19.07.2020                                  #
#                               18.01.2019                                  #
#                               05.01.2018                                  #
#                               23.03.2017                                  #
#                               19.03.2017                                  #
#                               17.03.2017                                  #
#                               09.02.2017                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD/WRC                   #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'TreatBrewerData.R' contains functions for the reading, writing,   #
#        recalculating and flagging of total ozone (direct sun) and         #
#        Standard Lamp tests values of  Brewer measurements from/to         #
#        different data formats.                                            #
#                                                                           #
#                                                                           #
#        List of functions:                                                 #
#                                                                           #
#          - CalcNewDS                                                      #
#          - CalcNewSL                                                      #
#          - DataReduction                                                  #
#          - ManuFlagDS                                                     #
#          - ReadBrewerB                                                    #
#          - ReadBrewerDS                                                   #
#          - ReduceDS                                                       #
#          - SetFlagsDS                                                     #
#          - WriteBrewerDS                                                  #
#          - WriteBrewerSL                                                  #
#          - WriteDayozDS                                                   #
#          - WriteAvgSL                                                     #
#          - zenith_angle                                                   #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 08.02.2017 by Herbert Schill:                                    #
#                                                                           #
#          - addition of a number of new functions for recalculating        #
#            DS-measurements                                                #
#                                                                           #
#                                                                           #
#        - 17.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - addition of function 'ManuFlagDS' for flagging/unflagging      #
#            DS-measurements of a day accordingly to 'FlagLst'              #
#                                                                           #
#          - remove bug in 'SetFlagsDS' for proper unflagging of all        #
#            DS-measurements of a day                                       #
#                                                                           #
#                                                                           #
#        - 19.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - allow use of original AbsCoeffs and ETCs from DS-file to       #
#            "recalculate" DS-file in 'CalcNewDS'                           #
#                                                                           #
#          - addition of function 'WriteDayozDS' for writing data on        #
#            a daily statistics file                                        #
#                                                                           #
#                                                                           #
#        - 23.03.2017 by Herbert Schill:                                    #
#                                                                           #
#          - extend 'ReadBrewerDS' for reading DS-files of PMOD origin.     #
#                                                                           #
#                                                                           #
#        - 05.01.2018 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'ReadBrewerDS' (reading StaLong)                 #
#                                                                           #
#          - remove bug in 'WriteBrewerDS' (data format of ETC's)           #
#                                                                           #
#          - adapt calendar format to array instead of list                 #
#                                                                           #
#                                                                           #
#        - 15.05.2018 by Herbert Schill:                                    #
#                                                                           #
#          - 'print'-commands replaced by 'cat'                             #
#                                                                           #
#                                                                           #
#        - 18.01.2019 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'ReadBrewerB' (double info line)                 #
#                                                                           #
#                                                                           #
#        - 19.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - adaption in 'ReadBrewerDS' (Header line 5)                     #
#                                                                           #
#                                                                           #
#        - 22.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - modify procedure 'CalcNewDS' for calculations with different   #
#            O3 absorption coefficient sets, based or not based on the      #
#            daily effective stratosphere temperature                       #
#                                                                           #
#                                                                           #
#        - 18.12.2020 by Herbert Schill:                                    #
#                                                                           #
#          - remove bug in 'ReadBrewerDS': skip measurements with irregular #
#            raw data sections                                              #
#                                                                           #
#          - 'ReadBrewerB': check on valid filter pos                       #
#                                                                           #
#          - 'DataReduction': check on hard-coded boundaries for O3 and SO2 #
#                                                                           #
#                                                                           #
#        - 28.12.2020 by Herbert Schill:                                    #
#                                                                           #
#          - add procedures for recalculation of Standard Lamp tests from   #
#            raw data: 'CalcNewSL', 'WriteBrewerSL', 'WriteAvgSL'           #
#                                                                           #
#          - modify 'DataReduction', 'ReadBrewerB' for treatment of         #
#            Standard Lamp tests from raw data                              #
#                                                                           #
#                                                                           #
#        - 06.01.2021 by Herbert Schill:                                    #
#                                                                           #
#          - 'CalcNewDS', refine calculations for undefined ETC(SO2)        #
#                                                                           #
#############################################################################


CalcNewDS <- function (BrewerStr, CalStr, cDate, rcType, bdata, ICFType, 
                       ICFvalues, StationPara, tempACdataSet, A1cor)

#############################################################################
#                                                                           #
#        'CalcNewDS' recalculates O3 and SO2 values from the raw data or    #
#        from the summary data read from the 'Bjjjyy.iii'-file (single      #
#        day file), or from the 'DSjjjyy.iii'-file (single day file).       #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          CalcNewDS (BrewerStr, CalStr, cDate, rcType, bdata, ICFType,     #
#                     ICFvalues, StationPara, tempACdataSet, A1cor)         #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#          BrewerStr  Brewer identifier as string (e.g. "040")              #
#          CalStr     Brewer calibration as string (e.g. "Cal16a")          #
#          cDate      date array, format={day, mon, year, jdn, leap}        #
#                                                                           #
#          rcType     recalculation type:                                   #
#                                                                           #
#           BRaw      recalculation from raw counts of B-file               #
#           BSum      recalculation from summary data of B-file             #
#           DS        recalculation from existing O3-resp. SO2-values       #
#                     of the DS-file                                        #
#                                                                           #
#          bdata      structured Brewer direct sun raw and summary data     #
#                     (if rcType="BRaw" or "Bsum") resp. calculated         #
#                     direct sun data (if rcType="DS").                     #
#                                                                           #
#          ICFType    Instrument constants will be read from one of the     #
#                     following sources:                                    #
#                                                                           #
#               0     constants from 'inst'-values from B-file (for         #
#                     rcType=BRaw/BSum) resp. DS-file (for rcType=DS)       #
#               1     constants from 'ICFjjjyy.iii'-file                    #
#               2     period constants from 'ICFCalxxx.iii'-file            #
#                                                                           #
#          ICFvalues  contents the instrument constants from the            #
#                     'ICFjjjyy.iii'-file (ICFType=1)resp. from the         #
#                     'ICFCalxxx.iii'-file (ICFType=2) resp. the            #
#                     SO2 standard absorption coefficient (ICFType=0)       #
#                                                                           #
#          StationPara    station parameters                                #
#                                                                           #
#          tempACdataSet  Flag for calculating O3-values with absorption    #
#                         coefficients (A1cor) based on the effective       #
#                         stratospheric temperature (Teff)of the day        #
#                                                                           #
#          A1cor          Teff-corrected absorption coefficient             #
#                                                                           #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#          List (data header, single values, day statistics)                #
#                                                                           #
#                                                                           #
#        Some parts of this function were created by:                       #
#                                                                           #
#        Florian Pantillon (paf) at MeteoSuisse Payerne, 26.06.2008         #
#                                                                           #
#############################################################################

{
	# Choose source for instrument constants

	if (ICFType==0)
	{
		if (rcType!="DS")  # use instrument constants from B-file
		{
			PZ <- bdata[[1]][[1]]   # standard station pressure [hPa]
			TC <- bdata[[1]][[4]]   # temperature coefficients
			A1 <- bdata[[1]][[5]]   # absorption coefficient O3
			A2 <- bdata[[1]][[6]]   # absorption coefficient SO2
			A3 <- bdata[[1]][[7]]   # absorption coefficient O3/SO2
			B1 <- bdata[[1]][[8]]   # extraterrestrial constant O3
			B2 <- bdata[[1]][[9]]   # extraterrestrial constant SO2
			T1 <- bdata[[1]][[10]]  # dead time [sec]
			AF <- bdata[[1]][[11]]  # filter attenuation values
			#PZ;TC;A1;A2;A3;B1;B2;T1;AF
		} else
		{ A2 <- ICFvalues }
	} else  # use instrument constants from 'ICFjjjyy.iii'- resp. 'ICFCalxxx.iii'-file
	{
		PZ <- StationPara[[3]][[4]]       # standard station pressure [hPa]
		A1 <- as.double(ICFvalues[[8]])   # absorption coefficient O3
		A2 <- as.double(ICFvalues[[9]])   # absorption coefficient SO2
		A3 <- as.double(ICFvalues[[10]])  # absorption coefficient O3/SO2
		B1 <- as.double(ICFvalues[[11]])  # extraterrestrial constant O3
		B2 <- as.double(ICFvalues[[12]])  # extraterrestrial constant SO2
		T1 <- as.double(ICFvalues[[13]])  # dead time [sec]
		AF = array()
		TC = array()
		for (i in 1:5) (TC[i] <- as.double(ICFvalues[[i+1]]))   # temperature coefficients
		for (i in 1:6) (AF[i] <- as.double(ICFvalues[[i+16]]))  # filter attenuation values
		#PZ;TC;A1;A2;A3;B1;B2;T1;AF
	}

	# Choose source of data and type of recalculation

	if (rcType!="DS")
	{
		Flag = array()
		t0 = array()
		strTime <- bdata[[3]][[1]]       # time-strings [UTC]
		AirM    <- bdata[[3]][[3]]       # airmass
		NDF     <- bdata[[3]][[4]]       # neutral density filter pos
		Tmp     <- bdata[[3]][[5]]       # instrument temperature
		NMeas = length (bdata[[3]][[1]]) # total number of measurements
		for (i in 1:NMeas)
		{
			t0[i] = StringToTime(strTime[i])  # UTC time [hrs]
			Flag[i]=0
		}

		if (rcType=="BRaw")  # recalculation from raw counts
		{
			ZA    <- bdata[[2]][[2]]  # solar zenith angle
			p     <- bdata[[2]][[3]]  # neutral density filter pos
			TE    <- bdata[[2]][[4]]  # instrument temperature
			index <- bdata[[2]][[5]]  # raw data index
			F     <- bdata[[2]][[6]]  # raw photon counts

			recalc <- DataReduction(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,p,TE,index,F,StationPara,0)

#     DataReduction <- function(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,p,TE,index,F,StationPara,0) 

			# recalculated:

			MS9     <- recalc[[1]][[1]] # double ratio Oz3
			MS8     <- recalc[[1]][[2]] # double ratio SO2
			Oz3     <- recalc[[1]][[3]] # Oz3
			SO2     <- recalc[[1]][[5]] # SO2
			SdevOz3 <- recalc[[1]][[4]] # staDev Oz3
			SdevSO2 <- recalc[[1]][[6]] # staDev SO2

			# check wether all indices of the raw data were used for the data reduction,
			# otherwise adapt AirM, Flag, NDF, strTime, t0, Tmp vectors
			# (modif. 08.10.2020 by Herbert Schill)

			nnOrg=length(AirM)
			nnNew=length(Oz3)
			if (nnOrg!=nnNew)
			{
				AirM = array()
				Flag = array()
				NDF = array()
				strTime = array()
				tt0 = array()
				Tmp = array()
				k=0
				for (i in 1:nnOrg)
				{
					if (length(grep(i,index))>0)
					{
						k=k+1
						strTime[k] = bdata[[3]][[1]][[i]]
						AirM[k] = bdata[[3]][[3]][[i]]
						NDF[k] = bdata[[3]][[4]][[i]]
						Tmp[k] = bdata[[3]][[5]][[i]]
						Flag[k]=0
						tt0[k] = t0[i]
					}  # end if
				}  # next i
				t0 = array()
				t0 <- tt0
				NMeas = k
			}  # end if nnOrg!=nnNew

		} else  # rcType=="BSum": recalculation from original summary data
		{
			MS8     <- bdata[[3]][[10]]  # double ratio MS8
			MS9     <- bdata[[3]][[11]]  # double ratio MS9
			SdevOz3 <- bdata[[3]][[13]]  # original staDev O3
			SdevSO2 <- bdata[[3]][[15]]  # original staDev SO2

			Oz3 <- ((MS9-B1)/(A1*AirM))/10               # MS11
			SO2 <- ((MS8-B2)/(A2*A3*AirM)-Oz3*10/A2)/10  # MS10
		}
	} else  # rcType="DS": recalculation from the 'DSjjjyy.iii'-file data
	{
		NMeas   <- bdata[[1]][[1]]  # total number of measurements
		A1org   <- bdata[[1]][[10]] # original absorption coefficient O3
		A2org   <- A2               # absorption coefficient SO2
		A3org   <- bdata[[1]][[11]] # original absorption coefficient O3/SO2
		B1org   <- bdata[[1]][[8]]  # original extraterrestrial constant O3
		B2org   <- bdata[[1]][[9]]  # original extraterrestrial constant SO2
		strTime <- bdata[[2]][[1]]  # time-strings [UTC]
		t0      <- bdata[[2]][[2]]  # UTC time [hrs]
		NDF     <- bdata[[2]][[3]]  # neutral density filter positions
		Tmp     <- bdata[[2]][[4]]  # instrument temperatures
		AirM    <- bdata[[2]][[5]]  # airmasses
		Oz3org  <- bdata[[2]][[6]]  # original Oz3 values
		SdevOz3 <- bdata[[2]][[7]]  # original staDev O3
		SO2org  <- bdata[[2]][[8]]  # original SO2 values
		SdevSO2 <- bdata[[2]][[9]]  # original staDev SO2
		Flag    <- bdata[[2]][[10]] # flag

		# If a temperature-dependent temporal absorption ceofficient series, based 
		# on the stratospheric temperature of Payerne of the day, or an O3-absorption
		# ceofficient different from BassPaurOp, is desired ,the proper absorption 
		# ceofficient 'A1cor' is used for the O3-recalculation, otherwise original
		# AbsCoeffs and ETCs from the input file are used

		if (ICFType==0) 
		{
			A1 = A1cor
			A2 = A2org
			A3 = A3org
			B1 = B1org
			B2 = B2org
		}

		# backwards calculation of double ratios MS8 and MS9 from 
		# original Oz3 and SO2, then recalculation of Oz3 and SO2
		# with new (or old, if ICFType=0, tempACdataSet=0) 
		# AbsCoeffs and ETCs

		MS9 <- 10*Oz3org*A1org*AirM+B1org
		Oz3 <- ((MS9-B1)/(A1*AirM))/10                   # MS11
		if (B2<9000)
		{
			MS8 <- 10*(SO2org*A2org+Oz3org)*A3org*AirM+B2org
			SO2 <- ((MS8-B2)/(A2*A3*AirM)-Oz3*10/A2)/10      # MS10
		} else  # ETC(SO2) not defined
		{
			SO2 = array()
			for (i in 1:length(SO2org)) SO2[i] <- 9.9
			SdevSO2 <- SO2
			MS8 <- SO2*1000
		}
		#A1;A1org;A2;A2org;A3;A3org;B1;B1org;B2;B2org
		#Oz3org;Oz3;SO2org;SO2

	}  # end if rcType:"DS"

	# Daily summary statistics: filtering depends of data source:
	#
	# if data source = "DS", filtering is done by flag,
	# if data source = "B", filtering is done by AirM and SdevOz3

	S1<-S2<-S3<-S5<-S6<-S7<-S8<-Z1<-Z2<-Z3<-Z4<-Z5<-Z6<-Z7<-Z8<-Z9<-0

	if (rcType!="DS")
	{
		AirR <- AirM[AirM<=3.2 & SdevOz3<=2.5]
		Oz3R <- Oz3[AirM<=3.2 & SdevOz3<=2.5]
		SO2R <- SO2[AirM<=3.2 & SdevOz3<=2.5]
		MS9R <- MS9[AirM<=3.2 & SdevOz3<=2.5]
		MS8R <- MS8[AirM<=3.2 & SdevOz3<=2.5]
		TER  <- Tmp[AirM<=3.2 & SdevOz3<=2.5]
	} else
	{
		AirR <- AirM[Flag<1]
		Oz3R <- Oz3[Flag<1]
		SO2R <- SO2[Flag<1]
		MS9R <- MS9[Flag<1]
		MS8R <- MS8[Flag<1]
		TER  <- Tmp[Flag<1]
	}
	S1 = length(AirR)  # number of valid measurements

	if (S1>0)
	{
		S2 <- sum(Oz3R)
		S3 <- sum(Oz3R*Oz3R)
		S5 <- sum(SO2R)
		S6 <- sum(SO2R*SO2R)
		S7 <- sum(TER)
		Z1 <- S1
		Z2 <- sum(1/AirR)
		Z3 <- sum(1/(AirR*AirR))
		Z5 <- sum(MS9R/AirR)
		Z4 <- sum(MS9R/(AirR*AirR))
		Z8 <- sum(MS8R/AirR)
		Z7 <- sum(MS8R/(AirR*AirR))
		#Z1;Z2;Z3;Z4;Z5;Z7;Z8
		if (S1==1)
		{
			S3=0
			S6=0
			Z6=0
			Z9=0
		} else
		{
			S8 = (S1*S3-S2*S2)/S1/(S1-1)
			if (S8>=0) {S3=sqrt(S8)} else {S3=0}  # staDev Oz3
			Z6 = (Z1*Z4-Z2*Z5)/(Z1*Z3-Z2*Z2)      # ETC Oz3 today (ETD)
			if (B2<9000)
			{
				S8 = (S1*S6-S5*S5)/S1/(S1-1)
				if (S8>=0) {S6=sqrt(S8)} else {S6=0}  # staDev SO2
				Z9 = (Z1*Z7-Z2*Z8)/(Z1*Z3-Z2*Z2)      # ETC SO2 today (ETD)
			} else  # ETC(SO2) not defined
			{
				S6 = 9.9
				Z9 = 9999.9
			}
		}
		S2 = S2/S1  # mean Oz3
		S5 = S5/S1  # mean SO2
		S7 = S7/S1  # mean instrument temperature
		Z2 = Z1/Z2  # mean airmass
	}
	#S1;S2;S3;S5;S6;S7
	#Z1;Z6;DSdata[[3]][[9]];Z9;DSdata[[3]][[10]]

	# Write recalculated data in DS-data structure

	HeaderValues = list(NMeas,cDate[1],cDate[2],cDate[3],cDate[4],BrewerStr,CalStr,B1,B2,A1,A3)
	names(HeaderValues) = c("NMeas","day","mon","year","jdn","Brewer","Calib","EtcOz3","EtcSO2","AbsCoeffOz3","AbsCoeffSO2")

	Measures = list(strTime, t0, NDF, Tmp, AirM, Oz3, SdevOz3, SO2, SdevSO2, Flag)
	names(Measures) = c("TimeStr","MeasTime","NDF","InstrTemp","Airmass","Oz3","ErrOz3","SO2","ErrSO2","Flag")

	DayStatis = list(S1, NMeas, S7, Z2, S2, S5, S3, S6, Z6, Z9)
	names(DayStatis) = c("NbOzVal","NbOzTot","AvgTemp","AvgAirm","AvgOzon","AvgSO2","sdvOz3","sdvSO2","EtdOz3","EtdSO2")

	return(list(HeaderData=HeaderValues,Measurements=Measures,DayStatistics=DayStatis))


}  # end of function 'CalcNewDS'


CalcNewSL <- function (BrewerStr, CalStr, cDate, bdata, ICFType, ICFvalues,
                       StationPara)

#############################################################################
#                                                                           #
#        'CalcNewSL' recalculates SL-test values from the raw data from     #
#        the 'Bjjjyy.iii'-file (single day file)                            #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          CalcNewSL (BrewerStr, CalStr, cDate, bdata, ICFType, ICFvalues,  #
#                     StationPara)                                          #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#          BrewerStr  Brewer identifier as string (e.g. "040")              #
#          CalStr     Brewer calibration as string (e.g. "Cal16a")          #
#          cDate      date array, format={day, mon, year, jdn, leap}        #
#                                                                           #
#          bdata      structured Brewer SL-tests raw and summary data       #
#                                                                           #
#          ICFType    Instrument constants will be read from one of the     #
#                     following sources:                                    #
#                                                                           #
#               0     constants from 'inst'-values from B-file              #
#               1     constants from 'ICFjjjyy.iii'-file                    #
#               2     period constants from 'ICFCalxxx.iii'-file            #
#                                                                           #
#          ICFvalues  contents the instrument constants from the            #
#                     'ICFjjjyy.iii'-file (ICFType=1)resp. from the         #
#                     'ICFCalxxx.iii'-file (ICFType=2) resp. the            #
#                     SO2 standard absorption coefficient (ICFType=0)       #
#                                                                           #
#          StationPara    station parameters                                #
#                                                                           #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#          List (data header, single values, day statistics)                #
#                                                                           #
#                                                                           #
#        Some parts of this function were created by:                       #
#                                                                           #
#        Florian Pantillon (paf) at MeteoSuisse Payerne, 26.06.2008         #
#                                                                           #
#############################################################################

{
	# Choose source for instrument constants

	if (ICFType==0)
	{
		PZ <- bdata[[1]][[1]]   # standard station pressure [hPa]
		TC <- bdata[[1]][[4]]   # temperature coefficients
		A1 <- bdata[[1]][[5]]   # absorption coefficient O3
		A2 <- bdata[[1]][[6]]   # absorption coefficient SO2
		A3 <- bdata[[1]][[7]]   # absorption coefficient O3/SO2
		B1 <- bdata[[1]][[8]]   # extraterrestrial constant O3
		B2 <- bdata[[1]][[9]]   # extraterrestrial constant SO2
		T1 <- bdata[[1]][[10]]  # dead time [sec]
		AF <- bdata[[1]][[11]]  # filter attenuation values
		#PZ;TC;A1;A2;A3;B1;B2;T1;AF
	} else  # use instrument constants from 'ICFjjjyy.iii'- resp. 'ICFCalxxx.iii'-file
	{
		PZ <- StationPara[[3]][[4]]       # standard station pressure [hPa]
		A1 <- as.double(ICFvalues[[8]])   # absorption coefficient O3
		A2 <- as.double(ICFvalues[[9]])   # absorption coefficient SO2
		A3 <- as.double(ICFvalues[[10]])  # absorption coefficient O3/SO2
		B1 <- as.double(ICFvalues[[11]])  # extraterrestrial constant O3
		B2 <- as.double(ICFvalues[[12]])  # extraterrestrial constant SO2
		T1 <- as.double(ICFvalues[[13]])  # dead time [sec]
		AF = array()
		TC = array()
		for (i in 1:5) (TC[i] <- as.double(ICFvalues[[i+1]]))   # temperature coefficients
		for (i in 1:6) (AF[i] <- as.double(ICFvalues[[i+16]]))  # filter attenuation values
		#PZ;TC;A1;A2;A3;B1;B2;T1;AF
	}

	# Prepare and perform recalculation

	strTime <- bdata[[3]][[1]]       # time-strings [UTC]
	NDF     <- bdata[[3]][[4]]       # neutral density filter pos
	Tmp     <- bdata[[3]][[5]]       # instrument temperature
	NMeas = length (bdata[[3]][[1]]) # total number of measurements

#	for (i in 1:NMeas) t0[i] = StringToTime(strTime[i])  # UTC time [hrs]

	ZA    <- bdata[[2]][[2]]  # solar zenith angle
	p     <- bdata[[2]][[3]]  # neutral density filter pos
	TE    <- bdata[[2]][[4]]  # instrument temperature
	index <- bdata[[2]][[5]]  # raw data index
	F     <- bdata[[2]][[6]]  # raw photon counts

	recalc <- DataReduction(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,p,TE,index,F,StationPara,1)

# DataReduction <- function(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,p,TE,index,F,StationPara,isSL)

# computed <- list(MS4,MS5,MS6,MS7,MS8,MS9,Int1,sdR5,sdR6,sdInt)
# names(computed) <- c("R1","R2","R3","R4","R5","R6","Int1","sdR5","sdR6","sdInt1")

	# check wether all indices of the raw data were used for the data reduction,
	# otherwise adapt NDF, strTime, Tmp vectors

	valSL <- recalc[[1]]
	nnOrg=length(Tmp)
	nnNew=length(recalc[[1]][[6]])
	if (nnOrg!=nnNew)
	{
		NDF = array()
		strTime = array()
		Tmp = array()
		k=0
		for (i in 1:nnOrg)
		{
			if (length(grep(i,index))>0)
			{
				k=k+1
				strTime[k] = bdata[[3]][[1]][[i]]
				NDF[k] = bdata[[3]][[4]][[i]]
				Tmp[k] = bdata[[3]][[5]][[i]]
			}  # end if
		}  # next i
		NMeas = k
	}  # end if nnOrg!=nnNew

	# Daily summary statistics

	if (NMeas>0)
	{
		avgSL = array()
		redSL = array()
		kk=0
		for (k in 1:NMeas)
		{
			if ((valSL[[9]][[k]]<50) & (valSL[[8]][[k]]<100))
			{
				for (i in 1:7)
				{
					kk=kk+1
					redSL[kk] = valSL[[i]][[k]]
				}
			}
		}
		NAvg=kk/7
		dim(redSL) <- c(7,NAvg)  # reshape array

		if (NAvg>0)
		{
			for (i in 1:7)
			{
				avgSL[i] = mean(redSL[i,])
				if (NAvg>1) avgSL[i+7]=sd(redSL[i,]) else avgSL[i+7]=0
			}
		}
	}  # end if NMeas>0

	# Write recalculated data in SL-data structure

	HeaderValues = list(NMeas,cDate[1],cDate[2],cDate[3],cDate[4],BrewerStr,CalStr,"  ")
	names(HeaderValues) = c("NMeas","day","mon","year","jdn","Brewer","Calib","TimeStamp")

	Measures = list(strTime, NDF, Tmp, valSL)
	names(Measures) = c("TimeStr","NDF","InstrTemp","valuesSL")

	DayStatis = list(NAvg, min(Tmp), max(Tmp), avgSL)
	names(DayStatis) = c("NAvg", "TLo", "THi", "AvgData")

		return(list(HeaderData=HeaderValues,Measurements=Measures,DayStatistics=DayStatis))

}  # end of function 'CalcNewSL'


DataReduction <- function(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,p,TE,index,F,
                          StationPara,isSL) 
#############################################################################
#                                                                           #
#    FUNCTION DataReduction(PZ,TC,A1,A2,A3,B1,B2,T1,AF,ZA,P,TE,INDEX,F)     #
#                                                                           #
#    INPUT: MEAN PRESSURE, TEMPERATURE COEFF, 3 ABSORPTION COEFFS,          #
#           2 EXTRATERRESTRIAL CONSTANTS, DEAD TIME, ABSORPTION OF FILTERS, #
#           VECTOR OF ZENITH ANGLES [°], VECTOR OF FILTER POSITIONS,        #
#           VECTOR OF TEMPERATURES [°C], VECTOR OF MESUREMENT INDEXES,      #
#           MATRIX OF RAW PHOTON COUNTS, DataType isSL [DS=0, SL=1]         #
#                                                                           #
#    OUTPUT: LIST(INSTRUMENT CORRECTED PHOTON RATES,                        #
#                 COMPUTED TOTAL OZONE & STANDARD DEVIATION [DU],           #
#                 COMPUTED TOTAL SULPHUR DIOXIDE & STANDARD DEVIATION [DU]) #
#                                                                           #
#    ALL INPUT VECTORS MUST HAVE SAME LENGTH N                              #
#    INPUT MATRIX F MUST HAVE 6 ROWS AND N COLUMNS                          #
#                                                                           #
#        This function was essentially created by:                          #
#                                                                           #
#        Florian Pantillon (paf) at MeteoSuisse Payerne, 26.06.2008         #
#                                                                           #
#        The extensions for treating SL-tests are added by Herbert Schill   #
#                                                                           #
#############################################################################

{

	# Hard-coded parameters

	CY <- 20 # Number of Cycles
	IT <- 0.1147 # Time Factor
	BE <- c(4870,4620,4410,4220,4040) # Rayleigh Coefficients

	# Station parameters

	HOZ = StationPara[[3]][[5]]  # Barycentric height ozone layer [km]
	HAT = StationPara[[3]][[6]]  # Barycentric height atmosphere [km]
	ERA = StationPara[[3]][[7]]  # Earth radius [km]
	PSL = StationPara[[3]][[8]]  # Standard sea level pressure [hPa]

	# Computing corrected photon rates

	if (isSL==1) ctsw1 <- F[2,]  # Photon raw counts wl 1

	for (i in 2:6) F[i,] <- 2*(F[i,]-F[1,])/(CY*IT)        # Photon raw counts to rates
	F <- FF <- F[2:6,] ; for (i in 1:9) F <- FF*exp(F*T1)  # Deadtime correction
	for (i in 1:length(F)) if (F[i]<0) F[i] <- 1.e-10      # set negative values to 1E-10
	F <- log10(F)*10^4                                     # for integer arithmetic
	for (i in 1:5) F[i,] <- F[i,]+TC[i]*TE                 # Instrument temperature correction
	for (i in 1:5) F[i,] <- F[i,]+AF[p+1]                  # Neutral density filters attenuation

	if (isSL==0)
	{
		AMA <- (1/cos(asin(ERA/(ERA+HAT)*sin(ZA*pi/180))))  # Airmass through atmosphere
		for (i in 1:5) F[i,] <- F[i,]+BE[i]*AMA*PZ/PSL      # Rayleigh scattering correction
	}

	# Computing ratios

	R1 <- F[4,]-F[1,] # MS(4)
	R2 <- F[4,]-F[2,] # MS(5)
	R3 <- F[4,]-F[3,] # MS(6)
	R4 <- F[5,]-F[4,] # MS(7)
	R5 <- R1-3.2*R4   # MS(8)
	R6 <- R2-0.5*R3-1.7*R4 # MS(9)

	if (isSL==0)  # Treat DS-measurements
	{
		# Determining total Ozone and total Sulphur Dioxide (DS)

		AMO <- (1/cos(asin(ERA/(ERA+HOZ)*sin(ZA*pi/180))))  # airmass through ozone layer
		ozone <- (R6-B1)/(A1*AMO)              # MS(11)
		sulph <- (R5-B2)/(A2*A3*AMO)-ozone/A2  # MS(10)

		# Mean values by index (groups of 5 normally)

		MS8 <- MS9 <- O3 <- dO3 <- SO2 <- dSO2 <- array()
		i <- j_old <- 1
		for (j in 1:length(ozone))
		{
			if (j==length(ozone) | index[j+1]!=index[j])
			{
				MS8[i]  <- mean(R5[j_old:j])     # Double Ratio SO2
				MS9[i]  <- mean(R6[j_old:j])     # Double Ratio Oz3
				O3[i]   <- mean(ozone[j_old:j])/10  # Total O3 [DU]
				SO2[i]  <- mean(sulph[j_old:j])/10  # Total SO2 [DU]
				if ((j-j_old)>0)
				{
					dO3[i]  <- sd(ozone[j_old:j])/10  # StaDev on total O3 [DU]
					dSO2[i] <- sd(sulph[j_old:j])/10  # StaDev on total SO2 [DU]
				} else
				{
					dO3[i]=0
					dSO2[i]=0
				}
				j_old <- j+1
				i <- i+1
			}  # end if
		}  # next j

		# Check on hard-coded boundaries for O3 and SO2, scale and write values in list

		for (i in 1:length(R6))
		{
			if ((O3[i]<1000) | (O3[i]>6000)) dO3[i]=999
			if ((SO2[i]<(-150)) | (SO2[i]>150)) dSO2[i]=999
		}
		O3 <- O3/10
		dO3 <- dO3/10
		SO2 <- SO2/10
		dSO2 <- dSO2/10

		# Write values in list

		computed <- list(MS8,MS9,O3,dO3,SO2,dSO2)
		names(computed) <- c("DoubleRatioOz3","DoubleRatioSO2","TotalOz3","StaDevOz3","TotalSO2","StaDevSO2")

	} else  # Treat SL-measurements
	{
		# Mean values by index (groups of 7 normally)

		MS4 <- MS5 <- MS6 <- MS7 <- MS8 <- MS9 <- Int1 <- sdR5 <- sdR6 <- sdInt <- array()
		i <- j_old <- 1
		for (j in 1:length(R6))
		{
			if ((j==length(R6)) | (index[j+1]!=index[j]))
			{
				MS4[i]  <- mean(R1[j_old:j])     # Single Ratio R1
				MS5[i]  <- mean(R2[j_old:j])     # Single Ratio R2
				MS6[i]  <- mean(R3[j_old:j])     # Single Ratio R3
				MS7[i]  <- mean(R4[j_old:j])     # Single Ratio R4
				MS8[i]  <- mean(R5[j_old:j])     # Single Ratio R5
				MS9[i]  <- mean(R6[j_old:j])     # Single Ratio R6
				Int1[i] <- mean(ctsw1[j_old:j])  # Mean of counts wl 1
				if ((j-j_old)>0)
				{
					sdR5[i]  <- sd(R5[j_old:j])    # StaDev on single Ratio R5
					sdR6[i]  <- sd(R6[j_old:j])    # StaDev on single Ratio R6
					sdInt[i] <- sd(ctsw1[j_old:j]) # StaDev on counts wl 1
				} else
				{
					sd5[i]=0
					sd6[i]=0
					sdInt[i]=0
				}
#				if ((sdR5[i]<50) & (sdR6[i]<100))  # same filter as GW-Basic 'SLSUM.RTN'
#				{
#					j_old <- j+1
				i <- i+1
#				}
				j_old <- j+1
			}  # end if
		}  # next j

		# Write values in list

		computed <- list(MS4,MS5,MS6,MS7,MS8,MS9,Int1,sdR5,sdR6,sdInt)
		names(computed) <- c("R1","R2","R3","R4","R5","R6","Int1","sdR5","sdR6","sdInt1")

	}  # end if isSL
	return(list("Recalculated"=computed))

}  # end of function 'DataReduction'


ManuFlagDS <- function (dataIn, f2ix, flagLst)

#############################################################################
#                                                                           #
#        Function 'ManuFlagDS' sets flags accordingly to 'FlagLst' to the   #
#        DS-data in dataset 'dataIn'                                        #
#                                                                           #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        ManuFlagDS (dataIn, f2ix, flagLst)                                 #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        dataIn   Unflagged (or partially unflagged) DS dataset             #
#        flagLst  Complete list of measurements to flag/unflag              #
#        f2ix     Indices of flaglist for instrument and day                #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return   Properly flagged (or unflagged) DS dataset.               #
#                                                                           #
#############################################################################
{

	hms <- flagLst[6,f2ix]

	for (i in 1:length(hms))
	{
		n=grep (hms[i],dataIn$Measurements$TimeStr,ignore.case=FALSE)
		if (length(n)>0)
		{
			k=f2ix[i]
			dataIn$Measurements$Flag[n] = as.integer(flagLst[14,k])
		}
	}  # end for i=1:length(hms)

	return(dataIn)

}  # end of function 'ManuFlagDS'


ReadBrewerB <- function (jdn, day, mon, year, brewStr, bPath, inData)

#############################################################################
#                                                                           #
#        'ReadBrewerB' reads the raw and the summary values of Brewer       #
#        measurements from the 'B' data format:                             #
#                                                                           #
#          - 'Bjjjyy.iii'-files  (single day file)                          #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          ReadbrewerB (jdn, day, mon, year, brewer, dspath)                #
#                                                                           #
#        Input:  Date: either jdn [jjj] and year [yyyy],                    #
#                      or day [dd], month [mm] and year [yyyy],             #
#                                                                           #
#                Brewer identifier [bbb]                                    #
#                                                                           #
#                Inputfile path (may contain 'bbb', 'Calxxx', 'yyyy')       #
#                                                                           #
#                Type of data to read: direct sun meas. (inData='ds'),      #
#                                      or SL-tests (inData='sl')            #
#                                                                           #
#        Output: List (Lists of instrument constants, raw and summary       #
#                      values)                                              #
#                                                                           #
#                                                                           #
#        Some parts of this function were created by:                       #
#                                                                           #
#        Florian Pantillon (paf) at MeteoSuisse Payerne, 26.06.2008         #
#                                                                           #
#############################################################################

{

	# Replace "bbb", and "yyyy" in filepath 'dspath' by brewer and year, 
	# if occurring

	bPath = ReplaceCalInstYear (bPath, "xxxxx", brewStr, year)

	if (jdn==0)  # calculate jdn, if necessary
	{
		cDate = array()
		cDate[1] = day
		cDate[2] = mon
		cDate[3] = year
		cDate[4] = 0
		cDate[5] = 0
		cDate = ConvertDate(cDate, 1)
		jdn = cDate[4]
	}

	# Create filename, test on existence and open 'Bjjjyy.iii'-file

	if (year>=2000) {sYear=year-2000} else {sYear=year-1900}
	filename = sprintf("%s%03d%02d.%s", "B", jdn, sYear, brewStr)
	fpathname = sprintf("%s%s", bPath, filename)
	ex=file.exists(fpathname)

	if (file.exists(fpathname))
	{
		# Open and read lines of input-file, close file

		bf = file(fpathname,open="r")
		inp = readLines(bf,n=-1,ok=TRUE)
		close(bf)

		# Find data header ("dh" line)

		n=grep ("dh",inp,ignore.case=FALSE)
		if (length(n)>0)
		{
			latitude  = as.double(inp[n[1]+5])
			longitude = as.double(inp[n[1]+6])
			PZ        = as.integer(inp[n[1]+9])  # mean station pressure
		}

		# Find instrument constants ("inst" line)

		n=grep ("inst",inp,ignore.case=FALSE)
		if (length(n)>0)
		{
		TC = as.double(inp[n[1]+1:5]) # temperature coefficients
		A1 = as.double(inp[n[1]+7])   # absorption coefficients
		A2 = as.double(inp[n[1]+8])
		A3 = as.double(inp[n[1]+9])
		B1 = as.integer(inp[n[1]+10]) # extra-terrestrial constants
		B2 = as.integer(inp[n[1]+11])
		T1 = as.double(inp[n[1]+12])  # dead time
		AF = as.integer(inp[n[1]+16:21]) # filter absorption values
		pos = n[1]+22
		}

		# Iinitialization

		i <- j <- k <- l <- m <- nrat <- i_old <- 0
		p <- t_ds <- ZA_ds <- F <- TE <- index_ds <- array()
		t_sumds <- ZA_sumds <- M2 <- SO2 <- O3 <- dSO2 <- dO3 <- array()
		TT <- ND <- MS4 <- MS5 <- MS6 <- MS7 <- MS8 <- MS9 <- array()

		if (length(grep("sl",inData,ignore.case=FALSE))>0) nSeq=7 else nSeq=5

		# Find direct sun summary and raw data ("summary" resp. "ds" or "sl" lines)

		n=grep ("summary",inp,ignore.case=FALSE)
		n <- n[n>pos]
		found=length(n)
		if (found>0)
		{
			for (f in 1:found)
			{
				if (inp[n[f]+8]==inData)  # summary contains ds or sl data
				{
					nrat=0
					invalidInput=0
					for (g in 1:nSeq)
					{
						r = n[f]-19*g
						if (inp[r]==inData)  # decode previous ds or sl raw data
						{
							nrat=nrat+1
							i=i+1
							p[i] <- as.integer(inp[r+2])/64 # Filter position
							if (p[i]>5)
							{
								p[i]=0  # set invalid filter pos to zero
								invalidInput=invalidInput+1
							}
							t_ds[i] <- as.double(inp[r+3])  # UTC time [min]
							ZA_ds[i] <- zenith_angle(as.double(inp[r+3]),jdn,sYear,latitude,longitude) # compute SZA[°]
							F[(6*i-5):(6*i)] <- as.integer(inp[(r+8):(r+13)]) # Raw photon counts
						}
					}  # next g

					# decode direct sun summary data, if raw data available

					if (nrat>0)
					{
						l <- l+1
						t_sumds[l] <- inp[n[f]+1]             # UTC TIME [HH:MM:SS]
						ZA_sumds[l] <- as.double(inp[n[f]+5]) # ZENITH ANGLE [°]
						M2[l] <- as.double(inp[n[f]+6])       # AIRMASS
						TT[l] <- as.double(inp[n[f]+7])       # Instrument temperature
						ND[l] <- as.double(inp[n[f]+9])       # Neutral density filter pos'n
						MS4[l] <- as.double(inp[n[f]+10])     # single ratio #1
						MS5[l] <- as.double(inp[n[f]+11])     # single ratio #2
						MS6[l] <- as.double(inp[n[f]+12])     # single ratio #3
						MS7[l] <- as.double(inp[n[f]+13])     # single ratio #4
						MS8[l] <- as.double(inp[n[f]+14])     # double ratio #1
						MS9[l] <- as.double(inp[n[f]+15])     # double ratio #2
						SO2[l] <- as.double(inp[n[f]+16])     # TOTAL SO2 [DU]
						O3[l] <- as.double(inp[n[f]+17])      # TOTAL O3 [DU]
						if (invalidInput==0)
						{
							dSO2[l] <- as.double(inp[n[f]+24])    # STANDARD DEVIATION ON SO2
							dO3[l] <- as.double(inp[n[f]+25])     # STANDARD DEVIATION ON O3
						} else
						{
							dSO2[l] = 99.9
							dO3[l] = 99.9
						}
						for (t in (i_old+1):i)
						{
							TE[t] <- as.double(inp[n[f]+7])     # INSTRUMENT TEMPERATURE [°C]
							index_ds[t] <- l                    # MEASUREMENT INDEX
						}
						i_old <- i
					}  # end if nrat>0
				} else if (inp[n[f]+8]=="sl")  # STANDARD LAMP TEST RESULTS
				{
					m <- m+1
				}
			}  # next f
		}  # end if found>0

		# find hg data
		#n=grep ("hgscan",inp,ignore.case=FALSE)
		#k=length(n)

		infoFile = sprintf("%s%s%5d%s","  File  ", filename, l, " measurements found\n")
		cat(infoFile, file="")

		# FORMAT DATA

		if (l>0)
		{
			t_ds <- t_ds*60+strptime(paste(sYear,jdn),"%y %j",tz="GMT") # MINUTES,DAY,YEAR TO SECONDS SINCE 1970
			dim(F) <- c(6,i) # RESHAPE ARRAY
		} else
		{
			directsun=0
			summaryDS=0
		}
		# WRITE DATA IN LISTS

		parameters <- list(PZ,latitude,longitude,TC,A1,A2,A3,B1,B2,T1,AF)
		names(parameters) <- c("PZ","latitude","longitude","TC","A1","A2","A3","B1","B2","T1","AF")
		directsun <- list(t_ds,ZA_ds,p,TE,index_ds,F)
		names(directsun) <- c("UTC Time","Zenith Angle","Filter Position","Instrument Temperature",
		                      "Index","Raw Photon Counts")
		summaryDS <- list(t_sumds,ZA_sumds,M2,ND,TT,MS4,MS5,MS6,MS7,MS8,MS9,O3,dO3,SO2,dSO2)
		names(summaryDS) <- c("TimeUTC","SZA","Airmass","NDF","Temperature","SingleRatioMS4",
		                      "SingleRatioMS5","SingleRatioMS6","SingleRatioMS7","DoubleRatioMS8",
		                      "DoubleRatioMS9","Totoz","SDevTotoz","SO2","SDevSO2")
	}
	else  # file 'Bjjjyy.iii' doesen't exist
	{
		infoFile = sprintf("%s%s%s","  File  ",filename, "  not found\n")
		cat(infoFile, file="")
		parameters=0
		directsun=0
		summaryDS=0
	}  # end: if file 'Bjjjyy.iii' exists or not

	return(list("Instrument Constants"=parameters,"Raw Data"=directsun,"OriginalValues"=summaryDS))

}  # end of function 'ReadBrewerB'


ReadBrewerBpaf <- function (jdn, day, mon, year, brewStr, bPath)

#############################################################################
#                                                                           #
#        'ReadBrewerB' reads the raw and the summary values of Brewer       #
#        measurements from the 'B' data format:                             #
#                                                                           #
#          - 'Bjjjyy.iii'-files  (single day file)                          #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          ReadbrewerB (jdn, day, mon, year, brewer, dspath)                #
#                                                                           #
#        Input:  Date: either jdn [jjj] and year [yyyy],                    #
#                      or day [dd], month [mm] and year [yyyy],             #
#                                                                           #
#                Brewer identifier [bbb]                                    #
#                                                                           #
#                Inputfile path (may contain 'bbb', 'Calxxx', 'yyyy')       #
#                                                                           #
#        Output: List (Lists of instrument constants, raw and summary       #
#                      values)                                              #
#                                                                           #
#                                                                           #
#        This function was essentially created by:                          #
#                                                                           #
#        Florian Pantillon (paf) at MeteoSuisse Payerne, 26.06.2008         #
#                                                                           #
#############################################################################

{

# test
	brewStr=BrewerStr
	bPath=InputPath
# end test

	# Replace "bbb", and "yyyy" in filepath 'dspath' by brewer and year, 
	# if occurring

	bPath = ReplaceCalInstYear (bPath, "xxxxx", brewStr, year)

	if (jdn==0)  # calculate jdn, if necessary
	{
		cDate = array()
		cDate = list(day, mon, year, jdn, leap)
		names(cDate) = c("day","mon","year","jdn","leap")
		cDate = ConvertDate(cDate, 1)
		jdn = cDate[[4]][[1]]
	}

	# Create filename, test on existence and open 'Bjjjyy.iii'-file

	if (year>=2000) {sYear=year-2000} else {sYear=year-1900}
	filename = sprintf("%s%03d%02d.%s", "B", jdn, sYear, brewStr)
	fpathname = sprintf("%s%s", bPath, filename)
	ex=file.exists(fpathname)

	if (file.exists(fpathname))
	{

		bf = file(fpathname,open="r")

		# READ DATA HEADER ("VERSION=2" LINE)

		head <- ""
		while(head!="dh")
		{
			line <- scan(bf,what="character",nlines=1,quiet=TRUE)
			if ((length(line)!=0)) { head <- line[1] } else { head <- "" }
		}
		line <- scan(bf,what="character",nlines=9,quiet=TRUE)
		latitude <- as.double(line[5])
		longitude <- as.double(line[6])
		PZ <- as.integer(line[9]) # MEAN PRESSURE

		# READ INSTRUMENT CONSTANTS ("INST" LINE)

		while(head!="inst")
		{
			line <- scan(bf,what="character",nlines=1,quiet=TRUE)
			if ((length(line)!=0)) { head <- line[1] } else { head <- "" }
		}
		line <- scan(bf,what="character",nlines=27,quiet=TRUE)

		TC <- as.double(line[1:5]) # TEMPERATURE COEFFICIENTS
		A1 <- as.double(line[7]) # ABSORPTION COEFFICIENTS
		A2 <- as.double(line[8])
		A3 <- as.double(line[9])
		B1 <- as.integer(line[10]) # EXTRA-TERRESTRIAL CONSTANTS
		B2 <- as.integer(line[11])
		T1 <- as.double(line[12]) # DEAD TIME
		AF <- as.integer(line[16:21]) # ABSORPTION OF FILTERS

		# INITIALIZATION

		head <- ""
		i <- j <- k <- l <- m <- i_old <- 0
		p <- t_ds <- ZA_ds <- F <- TE <- index_ds <- array()
		t_sumds <- ZA_sumds <- M2 <- SO2 <- O3 <- dSO2 <- dO3 <- array()
		TT <- ND <- MS4 <- MS5 <- MS6 <- MS7 <- MS8 <- MS9 <- array()

		# READ ALL LINES

		while(head!="ed")
		{

			line <- scan(bf,what="character",nlines=1,quiet=TRUE)
			if ((length(line)!=0)) { head <- line[1] } else { head <- "" }

			if (head=="ds")  # DIRECT SUN RAW DATA
			{
				line <- scan(bf,what="character",nlines=18,quiet=TRUE)
				i <- i+1
				p[i] <- as.integer(line[2])/64 # FILTER POSITION
				t_ds[i] <- as.double(line[3]) # UTC TIME [MIN]
				ZA_ds[i] <- zenith_angle(as.double(line[3]),jdn,sYear,latitude,longitude) # COMPUTE ZENITH ANGLE [°]
				F[(6*i-5):(6*i)] <- as.integer(line[8:13]) # RAW PHOTON COUNTS

			} else if (head=="sl")  # STANDARD LAMP TEST RAW DATA
			{
				j <- j+1

			} else if (head=="hg")  # MERCURY CALIBRATION
			{
				k <- k+1
				line <- scan(bf,what="character",nlines=7,quiet=TRUE)

			} else if (head=="summary") 
			{
				line <- scan(bf,what="character",nlines=25,quiet=TRUE)

				if (line[8]=="ds")  # DIRECT SUN RESULTS
				{
					l <- l+1
					t_sumds[l] <- line[1]             # UTC TIME [HH:MM:SS]
					ZA_sumds[l] <- as.double(line[5]) # ZENITH ANGLE [°]
					M2[l] <- as.double(line[6])       # AIRMASS
					TT[l] <- as.double(line[7])       # Instrument temperature
					ND[l] <- as.double(line[9])       # Neutral density filter pos'n
					MS4[l] <- as.double(line[10])     # single ratio #1
					MS5[l] <- as.double(line[11])     # single ratio #2
					MS6[l] <- as.double(line[12])     # single ratio #3
					MS7[l] <- as.double(line[13])     # single ratio #4
					MS8[l] <- as.double(line[14])     # double ratio #1
					MS9[l] <- as.double(line[15])     # double ratio #2
					SO2[l] <- as.double(line[16])     # TOTAL SO2 [DU]
					O3[l] <- as.double(line[17])      # TOTAL O3 [DU]
					dSO2[l] <- as.double(line[24])    # STANDARD DEVIATION ON SO2
					dO3[l] <- as.double(line[25])     # STANDARD DEVIATION ON O3
					for (t in (i_old+1):i)
					{
						TE[t] <- as.double(line[7])     # INSTRUMENT TEMPERATURE [°C]
						index_ds[t] <- l                # MEASUREMENT INDEX
					}
					i_old <- i

				} else if (line[8]=="sl")  # STANDARD LAMP TEST RESULTS
				{
					m <- m+1
				}
			}  # end if head="ds/sl/hg/summary"
		}  # wend

		close(bf)
		infoFile = sprintf("%s%s%5d%s","  File  ", filename, l, " measurements found\n")
		cat(infoFile, file="")

		# FORMAT DATA

		if (l>0)
		{
			t_ds <- t_ds*60+strptime(paste(sYear,jdn),"%y %j",tz="GMT") # MINUTES,DAY,YEAR TO SECONDS SINCE 1970
			dim(F) <- c(6,i) # RESHAPE ARRAY
			#t_sumds <- strptime(paste(sYear,jdn,t_sumds),"%y %j %T",tz="GMT") # HH:MM:SS,JDN,YEAR TO SECONDS SINCE 1970
		} else
		{
			directsun=0
			summaryDS=0
		}
		# WRITE DATA IN LISTS

		parameters <- list(PZ,latitude,longitude,TC,A1,A2,A3,B1,B2,T1,AF)
		names(parameters) <- c("PZ","latitude","longitude","TC","A1","A2","A3","B1","B2","T1","AF")
		directsun <- list(t_ds,ZA_ds,p,TE,index_ds,F)
		names(directsun) <- c("UTC Time","Zenith Angle","Filter Position","Instrument Temperature",
		                      "Index","Raw Photon Counts")
		summaryDS <- list(t_sumds,ZA_sumds,M2,ND,TT,MS4,MS5,MS6,MS7,MS8,MS9,O3,dO3,SO2,dSO2)
		names(summaryDS) <- c("TimeUTC","SZA","Airmass","NDF","Temperature","SingleRatioMS4",
		                      "SingleRatioMS5","SingleRatioMS6","SingleRatioMS7","DoubleRatioMS8",
		                      "DoubleRatioMS9","Totoz","SDevTotoz","SO2","SDevSO2")
	}
	else  # file 'Bjjjyy.iii' doesen't exist
	{
		infoFile = sprintf("%s%s%s","  File  ",filename, "  not found\n\n")
		cat(infoFile, file="")
		parameters=0
		directsun=0
		summaryDS=0
	}  # end: if file 'Bjjjyy.iii' exists or not

	return(list("Instrument Constants"=parameters,"Raw Data"=directsun,"OriginalValues"=summaryDS))

}  # end of function 'ReadBrewerBpaf'


ReadBrewerDS <- function (jdn, day, mon, year, brewer, calib, dspath)

#############################################################################
#                                                                           #
#        'ReadBrewerDS' reads the total ozone values of Brewer              #
#        measurements from the 'DS' data format:                            #
#                                                                           #
#          - 'DSjjjyy.iii'-files  (single day file)                         #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#          ReadbrewerDS (jdn, day, mon, year, brewer, calib, dspath)        #
#                                                                           #
#        Input:  Date:   either jdn [jjj] and year [yyyy],                  #
#                        or day [dd], month [mm] and year [yyyy],           #
#                                                                           #
#                Brewer identifier [bbb]                                    #
#                                                                           #
#                Calibration series [Calxxx]                                #
#                                                                           #
#                Inputfile path (may contain 'bbb', 'Calxxx', 'yyyy')       #
#                                                                           #
#        Output: List (List of Header values, measurements and day          #
#                statistics)                                                #
#                                                                           #
#############################################################################

{

	# Replace "bbb", "Calxxx" and "yyyy" in filepath 'dspath' by brewer, 
	# calibration series and year, if occurring

	dspath = ReplaceCalInstYear (dspath, calib, brewer, year)

	cDate = array()
	cDate[1] = day
	cDate[2] = mon
	cDate[3] = year
	cDate[4] = jdn
	cDate[5] = 0
	if (jdn==0)
	{
		cDate = ConvertDate(cDate, 1)
		jdn = cDate[4]
	} else
	{
		cDate = ConvertDate(cDate, -1)
	}

	# Create filename, test on existence and open 'DSjjjyy.iii'-file

	if (year>=2000) {sYear=year-2000} else {sYear=year-1900}
	filename = sprintf("%s%03d%02d.%s", "DS", jdn, sYear, brewer)
	fpathname = sprintf("%s%s", dspath, filename)
	ex=file.exists(fpathname)

	if (ex)
	{

	ds = file(fpathname,open="r")

		# Initialization

		AvgTemp=0
		AvgAirm=0
		AvgOzon=0
		AvgSO2 =0
		sdvOz3 =0
		sdvSO2 =0
		EtdOz3 =0
		EtdSO2 =0
		NbOzTot=0
		NbOzVal=0

		hline    = array()
		NDF      = array()
		strTime  = array()
		MeasTime = array()
		InstrT   = array()
		Airmass  = array()
		Oz3Org   = array()
		ErrOz3   = array()
		SO2org   = array()
		ErrSO2   = array()
		Flag     = array()

		# Read header lines

		hline = ""
		hline = scan(ds,what="character",nlines=1,quiet=TRUE)
		if ((length(hline)>=9) & (hline[1]=="Summary"))
		{
			dsMon = hline[8]
			dsDSy = hline[9]
			for (i in 1:2) hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			StationName = hline[4]
			hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			if (length(hline)==3)
			{
				StationLat = as.double(hline[3])
			} else
			{
				occ=0
				len=nchar(hline[1])
				for (i in 1:len)	if (substr(hline[1],i,i)=="=") occ=i
				StationLat = as.double(substr(hline[1],occ+1,len))
			}
			hline = scan(ds,what="character",nlines=1,quiet=TRUE)
#			if ((length(hline)==3) | (length(hline)==6))
			if (length(hline)>=3)
			{
				StationLong = as.double(hline[3])
			} else
			{
				occ=0
				len=nchar(hline[1])
				for (i in 1:len)	if (substr(hline[1],i,i)=="=") occ=i
				StationLong = as.double(substr(hline[1],occ+1,len))
			}
			for (i in 1:2) hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			EtcOz3 = as.integer(hline[5])
			EtcSO2 = as.integer(hline[7])
			hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			AbsCoeffOz3 = as.double(hline[5])
			AbsCoeffSO2 = as.double(hline[7])

			# Read data lines

			n=0
			for (i in 1:4) hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			while (length(hline)>8)
			{
				# decode line: check on 'ds'

				if ((hline[1])=="ds")
				{
					n=n+1
					NDF[n]     = as.integer(hline[2])
					strTime[n] = hline[3]
					mHour = as.integer(substr(strTime[n],1,2))
					mMin  = as.integer(substr(strTime[n],4,5))
					mSec  = as.integer(substr(strTime[n],7,8))
					MeasTime[n] = as.double(mHour+mMin/60+mSec/3600)
					InstrT[n]  = as.integer(hline[4])
					Airmass[n] = as.double(hline[5])
					Oz3Org[n]  = as.double(hline[6])
					ErrOz3[n]  = as.double(hline[7])
					SO2org[n]  = as.double(hline[8])
					ErrSO2[n]  = as.double(hline[9])
					if (length(hline)==10)
					{
						Flag[n]= as.double(hline[10])
					} else
					{
						if (NDF[n]>5)
							{Flag[n]=1}
						else
							{Flag[n]=0}
					}
				}
				hline = scan(ds,what="character",nlines=1,quiet=TRUE)

			}  # end while

			# Read statistics, if available

			hline = scan(ds,what="character",nlines=1,quiet=TRUE)
			if (hline[1]=="Daily")  # statistics exist
			{
				AvgTemp = as.integer(hline[3])
				AvgAirm = as.double(hline[4])
				AvgOzon = as.double(hline[5])
				AvgSO2  = as.double(hline[6])
				hline = scan(ds,what="character",nlines=1,quiet=TRUE)
				sdvOz3  = as.double(hline[3])
				if (length(hline)<4) { sdvSO2=0 } else sdvSO2  = as.double(hline[4])

				hline = scan(ds,what="character",nlines=1,quiet=TRUE)
				if (length(hline)==6)
				{
					NbOzVal = as.integer(hline[4])
					NbOzTot = as.integer(hline[6])
				} else
				if (length(hline)==5)
				{
					NbOzVal = as.integer(hline[4])
					NbOzTot = as.integer(substr(hline[5],2,4))
				} else
				if (length(hline)==4)
				{
					occ=0
					len=nchar(hline[4])
					for (i in 1:len) if (substr(hline[4],i,i)=="/") occ=i
					NbOzVal = as.integer(substr(hline[4],1,occ-1))
					NbOzTot = as.integer(substr(hline[4],occ+1,len))
				}

				for (i in 1:2) hline = scan(ds,what="character",nlines=1,quiet=TRUE)
				if (length(hline)>4)
				{
					EtdOz3  = as.double(hline[5])
					EtdSO2  = as.double(hline[9])
				} else
				{
					EtdOz3  = 0
					EtdSO2  = 0
				}
			}  # end if hline[1]=="Daily"
		} else # end if hline[1]=="Summary"
		{
			infoFile = sprintf("%s%s%s","  File  ",filename, "  invalid data format\n\n")
			cat(infoFile, file="")
		}

		close(ds)
		infoFile = sprintf("%s%s%5d%s%4d\n","    File  ",filename, NbOzVal, " valid measurements of total", n)
		cat(infoFile, file="")

		# Write data in lists

		HeaderValues = list(n, day, mon, year, jdn, brewer, calib, EtcOz3, EtcSO2, 
		                    AbsCoeffOz3, AbsCoeffSO2, StationName, StationLat, StationLong)
		names(HeaderValues) = c("NMeas","day","mon","year","jdn","Brewer","Calib","EtcOz3","EtcSO2",
		                        "AbsCoeffOz3","AbsCoeffSO2","StationName","StationLatitude","StationLongitude")

		Measures = list(strTime, MeasTime, NDF, InstrT, Airmass, Oz3Org, ErrOz3, SO2org, ErrSO2, Flag)
		names(Measures) = c("TimeStr","MeasTime","NDF","InstrTemp","Airmass","Oz3Org","ErrOz3","SO2org","ErrSO2","Flag")

		DayStatis = list(NbOzVal, NbOzTot, AvgTemp, AvgAirm, AvgOzon, AvgSO2, sdvOz3, sdvSO2, EtdOz3, EtdSO2)
		names(DayStatis) = c("NbOzVal","NbOzTot","AvgTemp","AvgAirm","AvgOzon","AvgSO2","sdvOz3","sdvSO2","EtdOz3","EtdSO2")

	}
	else  # file 'DSjjjyy.iii' doesen't exist
	{
		infoFile = sprintf("%s%s%s\n","    File  ",filename, "  not found")
		cat(infoFile, file="")
		HeaderValues=0
		Measures=0
		DayStatis=0
	}  # end: if file 'DSjjjyy.iii' exists or not

	return(list(HeaderData=HeaderValues,Measurements=Measures,DayStatistics=DayStatis))

}  # end of function 'ReadBrewerDS'


ReduceDS <- function  (wl, FlagingType, AutoflagDRval, RDiffDRvalLim, dataDS)
#############################################################################
#                                                                           #
#        'ReduceDS' reduces the complete DS dataset of a Brewer of one day  #
#        to the values needed for automated flagging: only the desired      #
#        wavelength (O3 or SO2), measurements already flagged are omitted   #
#        (FlagingType=1) or the flags are set back to zero (FlagingType=0); #
#        if AutoflagDRval=true, measurements are filtered accordingly to    #
#        the SDev limits                                                    #                                                                           #
#                                                                           #                                                                           #
#        Output: List (List of Header values, List of measurements)         #
#                                                                           #
#############################################################################

{

	if (wl=="O3") {w=1} else {w=2}

	Index    = array()
	MessTime = array()
	OzVal    = array()

	dRlim = RDiffDRvalLim[w]
	n1 = dataDS$HeaderData$NMeas
	f  = 0
	for (n in 1:n1)
	{
		flag = dataDS$Measurements$Flag[n]
		if ((flag==0) | (FlagingType==0))
		{
			if (wl=="O3")
			{
				RR = dataDS$Measurements$Oz3Org[n]
				dR = dataDS$Measurements$ErrOz3[n]
			} else  # SO2 to be flagged
			{
				RR = dataDS$Measurements$SO2Org[n]
				dR = dataDS$Measurements$ErrSO2[n]
			}
			
			if ((RR>0) | (wl=="SO2"))
			{
				if ((AutoflagDRval==FALSE) | ((AutoflagDRval==TRUE) & (dR<=dRlim)))
				{
					f=f+1
					Index[f]=n
					MessTime[f] = dataDS$Measurements$MeasTime[n]
					OzVal[f] = RR
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

}  # end of function 'ReduceDS'


SetFlagsDS <- function (wl, dataIn, dataRed, flaggingT, maxAirmass, minSun)

#############################################################################
#                                                                           #
#        Function 'SetFlagsDS' compares an unflagged (or partially unflag-  #
#        ged) DS dataset 'dataIn' with a reduced dataset 'dataRed'; all     #
#        records which apear in 'dataIn' but not in 'dataRed' are flagged   #
#        as follows:                                                        #
#                                                                           #
#        - high airmass (Airmass >  maxAirmass)     -> flag=2               #
#                                                                           #
#        - mid airmass  (Airmass >= maxAirmass-0.5)                         #
#                          & NDF <= NDFFmin-1       -> flag=1               #
#                          & NDF >  NDFFmin-1       -> flag=2               #
#                                                                           #
#        - low airmass  (Airmass <  maxAirmass-0.5)                         #
#                          & NDF <  NDFmin          -> flag=1               #
#                          & NDF >= NDFmin          -> flag=4               #
#                                                                           #
#        - high StaDev, measurement error           -> flag=4               #
#                                                                           #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#        SetFlagsDS (wl, dataIn, dataRed, flaggingT, maxAirmass, minSun))   #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#        wl       Wavelength (O3 or SO2)                                    #
#        dataIn   Unflagged (or partially unflagged) DS dataset             #
#        dataRed  Unflagged reduced dataset of wavelength 'wl'              #
#                 (if dataRed=0, flag all data in unflagged dataset)        #
#        flaggingT   Type of flagging/unflagging (see below)                #
#        maxAirmass  Maximal airmas for filtering                           #
#        minSun      Minimal NDF-value [0..5] for filtering                 #
#                                                                           #
#        status of 'flaggingT':                                             #
#                                                                           #
#          0  reset all flags                                               #
#          1  set automatic flags and write values to flaglist file         #
#          2  set/reset manual flags/unflags accordingly to flaglist file   #
#                                                                           #
#        Output:                                                            #
#                                                                           #
#        return   Properly flagged (or unflagged) DS dataset, including     #
#                 the data for the flaglist.                                #
#                                                                           #
#############################################################################
{

	if (wl=="O3") {w=1} else {w=2}

	nOrg = dataIn$HeaderData$NMeas

	if (flaggingT!=0)
	{
		strTime  = array()
		MeasTime = array()
		NDF      = array()
		InstrT   = array()
		Airmass  = array()
		Ozone    = array()
		sdOz3    = array()
		SO2      = array()
		sdSO2    = array()
		flag     = array()
		f=0

		if (dataRed[[1]][[4]][[1]]==0)  # all measurements invalid: flag them
		{
			RedIx = array()
			for (i in 1:nOrg) RedIx[i]=0
		} else
		{ RedIx <- dataRed$Measurements$Index }

		for (i in 1:nOrg)
		{
			if (length(RedIx[RedIx==i])==0)  # measurement is not in reduced dataset
			{
				ndf=dataIn$Measurements$NDF[i]
				My=dataIn$Measurements$Airmass[i]
				if (My>maxAirmass)
					flagA=2 else
					if (My<(maxAirmass-0.5))  # low airmass
					{
						if (ndf<minSun) flagA=1 else flagA=4
					} else  # middle airmass: Airmass>=maxAirmass-0.5
					{
						if (ndf>(minSun-1)) flagA=2 else flagA=1
					}
				dataIn$Measurements$Flag[i]=flagA

				# add flagged measurement values to intermediary flaglist
				#
				# dd	mm	jdn	Instr	nf	hh:mm:ss	time	temp	my	Ozon	sd/sun	SO2	sd	A	C	D	Remark

				f=f+1
				strTime[f]  = dataIn$Measurements$TimeStr[i]
				MeasTime[f] = dataIn$Measurements$MeasTime[i]/24
				NDF[f]      = dataIn$Measurements$NDF[i]
				InstrT[f]   = dataIn$Measurements$InstrTemp[i]
				Airmass[f]  = dataIn$Measurements$Airmass[i]
				Ozone[f]    = dataIn$Measurements$Oz3Org[i]
				sdOz3[f]    = dataIn$Measurements$ErrOz3[i]
				SO2[f]      = dataIn$Measurements$SO2org[i]
				sdSO2[f]    = dataIn$Measurements$ErrSO2[i]
				flag[f]     = dataIn$Measurements$Flag[i]
				#strTime;MeasTime;NDF;InstrT;Airmass;Ozone;sdOz3;SO2;sdSO2;flag

			}  # end if
		}  # end for i=1:nOrg

		# Write flaglist data in list

		flagList = list(strTime, MeasTime, NDF, InstrT, Airmass, Ozone, sdOz3, 
		                SO2, sdSO2, flag)
		names(flagList) = c("TimeStr", "MeasTime", "NDF", "InstrTemp", "Airmass", 
		                    "Ozone", "sdOz3", "SO2", "sdSO2", "Flags")

	} else  # flaggingT=0: unflag all flags
	{
		dataIn$Measurements$Flag[]=0
		flagList = 0
	}

	dataFlagged = list(dataIn, flagList)
	names(dataFlagged) = c("dataDS", "flagList")
	return(dataFlagged)

}  # end of function 'SetFlagsDS'


WriteBrewerDS <- function (DSdata, DSpath, StationPara)

#############################################################################
#                                                                           #
#        'WriteBrewerDS' writes the total ozone values of Brewer            #
#        measurements into the 'DS' data format:                            #
#                                                                           #
#          - 'DSjjjyy.iii'-files  (single day file)                         #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#         WriteBrewerDS(DSdata, DSpath, StationPara)                        #
#                                                                           #
#        Input:  List (List of Header values, measurements and day          #
#                statistics)                                                #
#                                                                           #
#                Outputfile path (may contain 'bbb', 'Calxxx', 'yyyy')      #
#                                                                           #
#                Station parameters (name, latitude, longitude, ...)        #
#                (may be empty, parameters then read from 'Dsdata')         #
#                                                                           #
#############################################################################

{

	# Replace "bbb", "Calxxx" and "yyyy" in filepath 'dspath' by brewer, 
	# calibration series and year, if occurring

	day  = DSdata[[1]][[2]]
	mon  = DSdata[[1]][[3]]
	year = DSdata[[1]][[4]]
	jdn  = DSdata[[1]][[5]]
	CalStr = DSdata[[1]][[7]]
	BrewerStr = DSdata[[1]][[6]]

	mmStr=substr("YYYJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",mon*3+1,mon*3+3)

	DSpath = ReplaceCalInstYear (DSpath, CalStr, BrewerStr, year)

	# Create filename and open 'DSjjjyy.iii'-file for write

	if (year>=2000) {sYear=year-2000} else {sYear=year-1900}
	filename = sprintf("%s%03d%02d.%s", "DS", jdn, sYear, BrewerStr)
	fpathname = sprintf("%s%s", DSpath, filename)
	ds = file(fpathname,open="w")

	# set station parameters from proper origin

	if (length(StationPara)==1)
	{
		StaName = DSdata$HeaderData$StationName
		StaLat  = DSdata$HeaderData$StationLatitude
		StaLong = DSdata$HeaderData$StationLongitude
	} else
	{
		StaName = StationPara[[2]][[1]]
		StaLat  = StationPara[[3]][[2]]
		StaLong = StationPara[[3]][[1]]
	}

	# write header

	outl = sprintf ("%s", "Summary of Brewer direct sun measurements for ")
	outl = sprintf ("%s%s %2.2d%s%02d\n\n", outl, mmStr, day, "/", sYear)
	cat (outl, file=ds)
	outl = sprintf ("%s%s", "Measurements made at ", StaName)
	outl = sprintf ("%s%s%s%s%s\n", outl, "  with instrument # ", BrewerStr, "  ", DSdata[[1]][[7]])
	cat (outl, file=ds)
	outl = sprintf ("%s%8.4f\n", "        Latitude  = ", StaLat)
	cat (outl, file=ds)
	outl = sprintf ("%s%8.4f%s%s\n\n", "        Longitude = ", StaLong, "     ", DSdata[[1]][[15]])
	cat (outl, file=ds)
	outl = sprintf ("%s", "        ETC Values    (O3/SO2) =")
	outl = sprintf ("%s%6.0f%s%6.0f\n", outl, DSdata[[1]][[8]], "  /", DSdata[[1]][[9]])
	cat (outl, file=ds)
	outl = sprintf ("%s", "        O3 Absorption (O3/SO2) =")
	outl = sprintf ("%s%8.4f%s%8.4f\n\n", outl, DSdata[[1]][[10]],"  /", DSdata[[1]][[11]])
	cat (outl, file=ds)
	outl = sprintf ("%s", "Type Time(UTC)  Temp  Airmass    ")
	outl = sprintf ("%s%s\n\n", outl, "Ozone   Error     SO2   Error  Flag")
	cat (outl, file=ds)

	# encode and write measurement lines

	for (i in 1:DSdata[[1]][[1]])
	{
		outl = sprintf ("%s%2d%s%s", "ds", DSdata[[2]][[3]][[i]], "  ", DSdata[[2]][[1]][[i]])
		outl = sprintf ("%s%5d%9.3f", outl, DSdata[[2]][[4]][[i]], DSdata[[2]][[5]][[i]])
		outl = sprintf ("%s%10.1f%8.1f", outl, DSdata[[2]][[6]][[i]], DSdata[[2]][[7]][[i]])
		outl = sprintf ("%s%8.1f%8.1f", outl, DSdata[[2]][[8]][[i]], DSdata[[2]][[9]][[i]])
		outl = sprintf ("%s%6d\n", outl, DSdata[[2]][[10]][[i]])
		cat (outl, file=ds)
	}  # end for i=1..n

	# write statistics

	if (DSdata[[3]][[1]]>1)
	{
		outl = sprintf ("\n%s%5.0f%9.3f", "   Daily means", DSdata[[3]][[3]], DSdata[[3]][[4]])
		outl = sprintf ("%s%10.1f%16.1f\n", outl, DSdata[[3]][[5]], DSdata[[3]][[6]])
		cat (outl, file=ds)
		outl = sprintf ("%s%17.1f%16.1f\n", "   Standard deviation", DSdata[[3]][[7]], DSdata[[3]][[8]])
		cat (outl, file=ds)
		if (DSdata[[3]][[2]]>99)
			{ outl = sprintf ("%s%21i%s%4i\n\n", "   Number of obs", DSdata[[3]][[1]], " /", DSdata[[3]][[2]]) } else
			{ outl = sprintf ("%s%21i%s%3i\n\n", "   Number of obs", DSdata[[3]][[1]], " /", DSdata[[3]][[2]]) }
		cat (outl, file=ds)
		outl = sprintf ("%s%9.1f%s%9.1f\n", "   Today's Ozone ETC =", DSdata[[3]][[9]], "  SO2 ETC =", DSdata[[3]][[10]])
	} else
	{
		outl = sprintf ("\n%s\n", "*** insufficient number of valid data points ***")
	}  # end if n>0
	cat (outl, file=ds)

	close(ds)  # close DS-file

}  # end of function 'WriteBrewerDS'


WriteBrewerSL <- function (SLdata, SLpath, StationPara)

#############################################################################
#                                                                           #
#        'WriteBrewerSL' writes the Standard Lamp test values of Brewer     #
#        measurements into the 'SL' data format:                            #
#                                                                           #
#          - 'SLjjjyy.iii'-files  (single day file)                         #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#         WriteBrewerSL(SLdata, SLpath, StationPara)                        #
#                                                                           #
#        Input:  List (List of Header values, SL-test values and day        #
#                statistics)                                                #
#                                                                           #
#                Outputfile path (may contain 'bbb', 'Calxxx', 'yyyy')      #
#                                                                           #
#                Station parameters (name, latitude, longitude, ...)        #
#                (may be empty, parameters then read from 'Dsdata')         #
#                                                                           #
#############################################################################

{

	# Replace "bbb", "Calxxx" and "yyyy" in filepath 'dspath' by brewer, 
	# calibration series and year, if occurring

	day  = SLdata[[1]][[2]]
	mon  = SLdata[[1]][[3]]
	year = SLdata[[1]][[4]]
	jdn  = SLdata[[1]][[5]]
	BrewerStr = SLdata[[1]][[6]]
	CalStr = SLdata[[1]][[7]]
	TimeStamp = SLdata[[1]][[8]]

	mmStr=substr("YYYJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",mon*3+1,mon*3+3)

	SLpath = ReplaceCalInstYear (SLpath, CalStr, BrewerStr, year)

	# Create filename and open 'SLjjjyy.iii'-file for write

	if (year>=2000) {sYear=year-2000} else {sYear=year-1900}
	filename = sprintf("%s%03d%02d.%s", "SL", jdn, sYear, BrewerStr)
	fpathname = sprintf("%s%s", SLpath, filename)
	ds = file(fpathname,open="w")

	# set station parameters from proper origin

	if (length(StationPara)==1)
	{
		StaName = SLdata$HeaderData$StationName
		StaLat  = SLdata$HeaderData$StationLatitude
		StaLong = SLdata$HeaderData$StationLongitude
	} else
	{
		StaName = StationPara[[2]][[1]]
		StaLat  = StationPara[[3]][[2]]
		StaLong = StationPara[[3]][[1]]
	}

	# write header

	outl = sprintf ("%s", "Summary of Brewer standard lamp measurements for ")
	outl = sprintf ("%s%s %2.2d%s%02d %s%03d%s\n\n", outl, mmStr, day, "/", sYear, "(", jdn, ")")
	cat (outl, file=ds)
	outl = sprintf ("%s%s", "Measurements made at ", StaName)
	outl = sprintf ("%s%s%s%s%s\n", outl, "  with instrument # ", BrewerStr, "  ", TimeStamp)
	cat (outl, file=ds)
	outl = sprintf ("%s%8.4f%s%8.4f\n\n", "    Latitude = ", StaLat,"    Longitude = ", StaLong)
	cat (outl, file=ds)
	outl = sprintf ("%s\n\n", "Type Time(UTC) Temp  R1    R2    R3    R4    R5  SDR5  R6  SDR6     F1    SDF1")
	cat (outl, file=ds)

	# encode and write SL-test lines

	for (i in 1:SLdata[[1]][[1]])
	{
		outl = sprintf ("%s%2d%s%s", "sl", SLdata[[2]][[2]][[i]], "  ", SLdata[[2]][[1]][[i]])         # ndf, timeStr
		outl = sprintf ("%s%4d", outl, SLdata[[2]][[3]][[i]])                                          # Temp
		for (k in 1:5) outl = sprintf ("%s%6.0f", outl, SLdata[[2]][[4]][[k]][[i]])                    # R1..R5
		outl = sprintf ("%s%4.0f%6.0f", outl, SLdata[[2]][[4]][[8]][[i]], SLdata[[2]][[4]][[6]][[i]])  # sdR5, R6
		outl = sprintf ("%s%4.0f%10.0f", outl, SLdata[[2]][[4]][[9]][[i]], SLdata[[2]][[4]][[7]][[i]]) # sdR6, F1
		outl = sprintf ("%s%7.0f\n", outl, SLdata[[2]][[4]][[10]][[i]])                                # sdF1
		cat (outl, file=ds)
	}  # end for i=1..n

	# write statistics

	if (SLdata[[3]][[1]]>1)
	{
		outl = sprintf ("\n%s", "    Daily means:  ")
		for (k in 1:5) outl = sprintf ("%s%6.0f", outl, SLdata[[3]][[4]][[k]])                     # avgR1..avgR5
		outl = sprintf ("%s%10.0f%14.0f\n", outl, SLdata[[3]][[4]][[6]], SLdata[[3]][[4]][[7]])    # avgR6, avgF1
		cat (outl, file=ds)
		outl = sprintf ("%s", "    Standard dev: ")
		for (k in 8:12) outl = sprintf ("%s%6.0f", outl, SLdata[[3]][[4]][[k]])                    # sdR1..sdR5
		outl = sprintf ("%s%10.0f%14.0f\n", outl, SLdata[[3]][[4]][[13]], SLdata[[3]][[4]][[14]])  # sdR6, sdF1
		cat (outl, file=ds)
		outl = sprintf ("%8i /%2i%s\n", SLdata[[1]][[1]], SLdata[[3]][[1]], "  observations.")
		cat (outl, file=ds)
	} else
	{
		outl = sprintf ("\n%s\n", "*** insufficient number of valid data points ***")
	}  # end if n>0

	close(ds)  # close SL-file

}  # end of function 'WriteBrewerSL'


WriteDayozDS <- function (df, DSdata)

#############################################################################
#                                                                           #
#        'WriteDayozDS' writes the daily means of O3 and SO2 values of      #
#        Brewer measurements into the 'Dayoz' data format:                  #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#         WriteDayozDS(df, DSdata)                                          #
#                                                                           #
#        Input:  df  Opened dayoz file to write to                          #
#                                                                           #
#                DSdata (List of Header values, single measurements         #
#                and day statistics)                                        #
#                                                                           #
#############################################################################

{
	# write statistics

	if (DSdata[[3]][[1]]>0)
	{
		outl = sprintf (" %s%s%04d%02d%02d", DSdata[[1]][[6]], " M ", DSdata[[1]][[4]], DSdata[[1]][[3]], DSdata[[1]][[2]])
		outl = sprintf ("%s%8.2f%6.2f%4d", outl, DSdata[[3]][[5]], DSdata[[3]][[7]], DSdata[[3]][[1]])
		outl = sprintf ("%s%8.2f%6.2f%4d\n", outl, DSdata[[3]][[6]], DSdata[[3]][[8]], DSdata[[3]][[1]])
		cat (outl, file=df)
	}  # end if n>0
	return(df)

}  # end of function 'WriteDayozDS'


WriteAvgSL <- function (df, year, doy, nData, SLdata)

#############################################################################
#                                                                           #
#        'WriteAvgSL' writes the daily means of the SL-test values of the   #
#        Brewer into the 'SLOavg' data format:                              #
#                                                                           #
#        Function call:                                                     #
#                                                                           #
#         WriteAvgSL(df, year, doy, nData, SLdata)                          #
#                                                                           #
#        Input:  df  Opened avgSL file to write to                          #
#                                                                           #
#                year, doy, nData                                           #
#                                                                           #
#                SLdata (List of Header values, single measurements         #
#                        and day statistics)                                #
#                                                                           #
#############################################################################

{
	# write statistics

	yy = year-1900
	if (yy>100) yy=yy-100

	if (nData>0)
	{
		outl = sprintf ("%03d%2d%3d%3d%3d", doy, yy, SLdata[[3]][[2]], SLdata[[3]][[3]], SLdata[[3]][[1]])
		for (k in 1:6) outl = sprintf ("%s%6.0f", outl, SLdata[[3]][[4]][[k]])  # avgR1..avgR6
		outl = sprintf ("%s%8.0f", outl, SLdata[[3]][[4]][[7]])                 # avgF1
		for (k in 8:13) outl = sprintf ("%s%4.0f", outl, SLdata[[3]][[4]][[k]]) # sdR1..sdR6
		outl = sprintf ("%s%6.0f\n", outl, SLdata[[3]][[4]][[14]])              # sdF1
	} else
		outl = sprintf ("%03d%2d\n", doy, yy)
	{
	}  # end if nData:0
	cat (outl, file=df)

	return(df)

}  # end of function 'WriteAvgSL'


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

zenith_angle <- function(t0,day,year,latitude,longitude) {

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


#### end of library 'TreatBrewerData.R' #####################################
