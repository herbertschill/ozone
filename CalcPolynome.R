#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  StatisRadMax.R                              #
#                                                                           #
#                                                                           #
#        Creation Date       :  07.11.2019                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD-WRC                   #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'StatisRadMax.R' reads the (direct, global, diffuse) radiation     #
#        data from the input files at the time of true noon and the time    #
#        and value of the maximal value of the day over a period.           #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'StatisRadMax.ini'.                                                #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\LKO\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          CalcTrueNoon, ConvertDate, LeapYear 
#
#  ReadInstTables.R    ReadConstantsFile, ReplaceCalInstYear


statusOK <- 0

# Open the ini-file 'RecalcAE.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=7
iniParams = ReadIniFile(Nparams, "StatisRadMax.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Read the values for the station from the station list file

	StationPara = ReadStationsFile (iniPar[4],"DAV")
	StaLong  = StationPara[[3]][[1]]

	# Treat desired period

	yearA = as.integer(iniPar[2])
	yearE = as.integer(iniPar[3])

	for (year in yearA:yearE)
	{
		# set proper pathes for input files; open yearly output file and write header

		PathRad = ReplaceCalInstYear (iniPar[5], "", "", year)
		PathRad = sprintf("%s%s", PathRad, "\\")
		dpathname = ReplaceCalInstYear (iniPar[7], "", "", year)
		ds = file(dpathname,open="w")
		outl = sprintf ("%s", " jdn  TrueNoon  RadVal  TimeRMax  RadMax\n")
		                      " 365 11.393983    7.87 11.958333  219.24\n"
		cat(outl, file=ds)

		# Treat days of year

		leap = LeapYear(year)
		jdn1=1
		if (leap) jdn2=366 else jdn2=365
		for (jdn in jdn1:jdn2)
		{
			cDate = array()
			cDate[3] = year
			cDate[4] = jdn
			cDate[5] = leap
			cDate = ConvertDate(cDate, -1)
			day = cDate[1]
			mon = cDate[2]

			# Get time of true noon; trueNoon-list with: TimeHMax [hr], HMax "hh:mm:ss", SunheiMax

			TN = CalcTrueNoon (day, mon, year, StaLong)

			# Create inputfile name

			jyStr = sprintf("%03d%04d", jdn, year)
			filename = sub("jjjyyyy", jyStr, iniPar[6])
			fpathname = sprintf("%s%s", PathRad, filename)

			######################################################

			pathname = "C:\\LKO\\Klima\\Radiation\\"
			filename = "Davos_MaxRad_Statistics_2010-19.txt"
			fpathname = sprintf("%s%s", pathname, filename)
			met = file(fpathname,open="r")
			hline = scan(met, what="character", nlines=2, quiet=TRUE)
			drDat = scan(met, what="character", nlines=-1, quiet=TRUE)
			close(met)
			cc=2
			dim(drDat) = c(cc,length(drDat)/cc)
			dJDN<-as.single(drDat[1,])
			dRad<-as.single(drDat[2,])
			dr<-data.frame(x=dJDN,y=dRad)

			## An example of polynomial regression of multiple degree

			require(stats); require(graphics)
			plot(dRad, xlab = "DOY", ylab = "MaxRad (Wm-2)",las = 1)
			d <- seq(1, 365, length.out = 365)  # nods (Stützstellen)

			for(degree in 3:6)
			{
				degree=6
				fm <- lm(dRad ~ poly(dJDN, degree), data = dr)
				assign(paste("dr", degree, sep = "."), fm)
				dp6 <- predict(fm, data.frame(dJDN))
				dy<-as.single(dp6)  # 1-dim vector with poly6-values at nods
				lines(dJDN, dp6, col = 8-degree)
				# degree color
				#   3    5 emeraude
				#   4    4 green
				#   5    3 blue
				#   6    2 red
			}

			filename = "Davos_MaxRad_Statistics_2010-19_Poly6.txt"
			fpathname = sprintf("%s%s", pathname, filename)
			met = file(fpathname,open="w")
			outl = sprintf ("%s", " jdn	MaxRad	RadMax\n")
			cat(outl, file=met)
			for(i in 1:365)
			{
				outl = sprintf ("%4d%9.2f%11.4f%s", dJDN[i], dRad[i], dy[i], "\n")
				cat(outl, file=met)
			}
			close(met)

			## An example of polynomial regression from R-Reference_Manual

			plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
			las = 1, xlim = c(0, 25))
			d <- seq(0, 25, length.out = 200)
			for(degree in 1:4) {
			fm <- lm(dist ~ poly(speed, degree), data = cars)
			assign(paste("cars", degree, sep = "."), fm)
			lines(d, predict(fm, data.frame(speed = d)), col = degree)
			}
			anova(cars.1, cars.2, cars.3, cars.4)
			
			#od <- options(digits = 5) # avoid too much visual clutter
			pdim=6
			z <- poly(dRad, pdim)
			######################################################

			if (file.exists(fpathname))  # open input file and read data
			{
				met = file(fpathname,open="r")
				hline = scan(met, what="character", nlines=1, quiet=TRUE)
				drDat = scan(met, what="character", nlines=-1, quiet=TRUE)
				close(met)
				dim(drDat) = c(3,length(drDat)/3)

				# Find data at true noon and maximal value and its time

				n=grep ("0.",drDat[2,])
				if (length(n)>1)
				{
					drTime <- as.single(drDat[1,n[1]:n[length(n)]])
					drRVal <- as.single(drDat[2,n[1]:n[length(n)]])
					dtAM <- (drTime[drTime<TN$TimeHR])
					i=length(dtAM)
					if (i<length(n)) RadTrueNoon = (drRVal[i-1]+drRVal[i])/2 else RadTrueNoon=0

					ix = match (max(drRVal), drRVal)
					RadMax = drRVal[ix]
					TimeMax = drTime[ix]

					outl = sprintf (" %03d%10.6f%8.2f%10.6f%8.2f%s", jdn, TN$TimeHR, RadTrueNoon, TimeMax, RadMax, "\n")
					cat(outl, file=ds)
				}
				else  # no usable data found in file
				{
				infoFile = sprintf("%s%s%s","\n  File  ",filename, "  no usable data found")
				cat (infoFile, file="")
				outl = sprintf (" %03d%s", jdn, "\n")
				cat(outl, file=ds)
				}

			}
			else  # input-file of the day doesen't exist
			{
				infoFile = sprintf("%s%s%s","\n  File  ",filename, "  not found")
				cat (infoFile, file="")
				outl = sprintf (" %03d%s", jdn, "\n")
				cat(outl, file=ds)
			}  # end if

		}  # for jdn=jdn1:jdn2

		close(ds)  # close the output file of the year

	}  # for year=yearA:yearE

}  # end if iniOk

#### end of program 'StatisRadMax.R' ############################################
