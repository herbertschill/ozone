#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  ReduceRad2SunDur.R                          #
#                                                                           #
#                                                                           #
#        Creation Date       :  14.01.2020                                  #
#                                                                           #
#        Last Modifications  :  13.07.2020                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD-WRC                   #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ReduceRad2SunDur.R' reads the direct and global 1-min radiation   #
#        data and the 10-min sunshine and rainfall data from the input      #
#        files, converts the direct radiation to sun duration and reduces   #
#        the 1-min to 10-min data; direct rad, global rad, sun duration,    #
#        sunshine and rain are written as 10-min-values to the output file, #
#        if desired. Daily sums or avgs are calculated, if desired, and     #
#        written to a yearly statistical file.                              #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'ReduceRad2SunDur.ini'.                                            #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 13.07.2020 by Herbert Schill:                                    #
#                                                                           #
#          - introduce 'nHeadVQ' for number of header lines in VQBA-files,  #
#            adapt for new VQBA-format since 06.07.2020                     #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          CheckConvertDate, ConvertDate, LeapYear, 
#                      ReplaceCalInstYear, ReplaceDayMonthYear
#
#  ReadInstTables.R    ReadIniFile


statusOK <- 0

# Open the ini-file 'ReduceRad2SunDur.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

nHeadVQ=3
Nparams=16
iniParams = ReadIniFile(Nparams, "ReduceRad2SunDur.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]

	# Check desired parameters for output

	outRec = array()
	nout = strsplit(iniPar[13],",")
	nop  = length(nout[[1]])
	for (n in 1:nop)  outRec[n] = nout[[1]][[n]]
	dirRadLimit = as.double(iniPar[15])
	makeStat = as.integer(iniPar[16])

	# Create standard output time record "hh mm"

	outHM = array()
	t=1
	for (h in 0:23)
	{
		for (m in 0:5)
		{
		outHM[t]=sprintf(" %02d %02d", h, m*10)
		t=t+1
		}
	}

	# Check date format and treat desired period

	cDate = array()
	cDate = CheckConvertDate(iniPar[2])
	dayA  = cDate[1]
	monA  = cDate[2]
	yearA = cDate[3]
	cDate = array()
	cDate = CheckConvertDate(iniPar[3])
	dayE  = cDate[1]
	monE  = cDate[2]
	yearE = cDate[3]

	# Treat years; set proper pathes for input files

	for (year in yearA:yearE)
	{
	# year=yearA
		PathDirRad = ReplaceCalInstYear (iniPar[4], "", "", year)
		PathDirRad = sprintf("%s%s", PathDirRad, "\\")
		PathGloRad = ReplaceCalInstYear (iniPar[5], "", "", year)
		PathGloRad = sprintf("%s%s", PathGloRad, "\\")
		PathRain = ReplaceCalInstYear (iniPar[6], "", "", year)
		PathRain = sprintf("%s%s", PathRain, "\\")
		PathOutput = ReplaceCalInstYear (iniPar[10], "", "", year)
		PathOutput = sprintf("%s%s", PathOutput, "\\")
		opName0 = ReplaceCalInstYear (iniPar[11], "", iniPar[14], year)

		leap = LeapYear(year)
		mon1=1
		mon2=12
		if (year==yearA) mon1=monA
		if (year==yearE) mon2=monE

		# Open daily statistics output file, if desired, write header

		if (makeStat>0)
		{
			statName = ReplaceCalInstYear (iniPar[12], "", iniPar[14], year)
			opPathname = sprintf("%s%s", PathOutput, statName)
			st = file(opPathname,open="w")
			outl = sprintf(" dd.mm.yyyy (doy)   GR10   DR10   SD10   SDUR   RAIN\n\n")
			cat (outl, file=st)
		}

		# Treat months; set proper days in month 'mdays'

		for (mon in mon1:mon2)
		{
		# mon=mon1
			if (leap==1) 
				{mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
				{mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)}

			day1=1
			day2=mdays
			if (mon==monA) day1=dayA
			if (mon==monE) day2=dayE
			
			# Treat days in month

			for (day in day1:day2)
			{
			# day=day1

				cDate = array()
				cDate[1] = day
				cDate[2] = mon
				cDate[3] = year
				cDate = ConvertDate(cDate, 1)
				doy = cDate[4]

				# Create inputfiles and outputfile names; check on input files

				drFilename = ReplaceDayMonthYear(iniPar[7], day, mon, year)
				drPathname = sprintf("%s%s", PathDirRad, drFilename)
				grFilename = ReplaceDayMonthYear(iniPar[8], day, mon, year)
				grPathname = sprintf("%s%s", PathGloRad, grFilename)
				vqFilename = ReplaceDayMonthYear(iniPar[9], day, mon, year)
				vqPathname = sprintf("%s%s", PathRain, vqFilename)

				nf=0
				if (file.exists(drPathname))  nf=nf+1
				if (file.exists(grPathname))  nf=nf+1
				if (file.exists(vqPathname))  nf=nf+1
				if (nf>0)
				{
					infoFile = sprintf("%s%02d.%02d.%04d  (%03d)%s","    Process data for  ",day, mon, year, doy, "\n")
					cat (infoFile, file="")
					filename = ReplaceDayMonthYear(opName0, day, mon, year)
					opPathname = sprintf("%s%s", PathOutput, filename)
					op = file(opPathname,open="w")

					dr10 = array()
					gr10 = array()
					sd10  = array()
					drVec = array()
					grVec = array()
					rrVec = array()
					sdVec = array()
					ssVec = array()

					if (file.exists(drPathname))  # read 1-min direct radiation data
					{
						dr = file(drPathname,open="r")
						hline = scan(dr, what="character", nlines=1, quiet=TRUE)
						drDat = scan(dr, what="character", nlines=-1, quiet=TRUE)
						close(dr)
						dim(drDat) = c(3,length(drDat)/3)
						drHr <- as.double(drDat[1,drDat[2,]!="NaN"])
						drVec<- as.double(drDat[2,drDat[2,]!="NaN"])
						drLen=length(drHr)

						# convert direct radiation to sunshine

						for (i in 1:length(drVec))
						{
							if (drVec[i]>=dirRadLimit) sdVec[i]=1 else sdVec[i]=0
						}
					} else  # file doesen't exist
					{
						drLen=0
						dr10[1:144]=0
						sd10[1:144]=0
						infoFile = sprintf("%s%s%s","    File  ",drPathname, "  not found\n")
						cat (infoFile, file="")
					}  # end if

					if (file.exists(grPathname))  # read 1-min global radiation data
					{
						gr = file(grPathname,open="r")
						hline = scan(gr, what="character", nlines=1, quiet=TRUE)
						grDat = scan(gr, what="character", nlines=-1, quiet=TRUE)
						close(gr)
						dim(grDat) = c(3,length(grDat)/3)
						grVec<- as.double(grDat[2,grDat[2,]!="NaN"])
						grHr <- as.double(grDat[1,grDat[2,]!="NaN"])
						grLen=length(grHr)
					} else  # file doesen't exist
					{
						grLen=0
						gr10[1:144]=0
						infoFile = sprintf("%s%s%s","    File  ",grPathname, "  not found\n")
						cat (infoFile, file="")
					}  # end if

					if (file.exists(vqPathname))  # read rain and sundur data from SMN station
					{
						vq = file(vqPathname,open="r")
						hline = scan(vq, what="character", nlines=nHeadVQ, quiet=TRUE)
						vqDat = scan(vq, what="character", nlines=-1, quiet=TRUE)
						close(vq)
						dim(vqDat) = c(8,length(vqDat)/8)
						rrVec<- as.double(vqDat[8,vqDat[1,]==iniPar[14]])
						ssVec<- as.integer(vqDat[7,vqDat[1,]==iniPar[14]])
						vqHM <- substr(vqDat[2,vqDat[1,]==iniPar[14]],9,13)
						vqHr =  as.double(substr(vqHM,1,2)) + as.double(substr(vqHM,3,4))/60
						vqLen=length(vqHr)
						for (m in 1:vqLen)
						{
							if (rrVec[m]>9999) rrVec[m]=9999.9
							if (ssVec[m]>9999) ssVec[m]=9999.9
						}
					} else  # file doesen't exist
					{
						vqLen=0
						rrVec[1:144]=0
						ssVec[1:144]=0
						infoFile = sprintf("%s%s%s","    File  ",vqPathname, "  not found\n")
						cat (infoFile, file="")
					}  # end if

					# reduce 1-min data to 10-min data

					for (m in 1:144)
					{
						t0 = (m-2)/6
						t1 = (m-1)/6
						if (drLen>0)
						{
							drs <- drVec[drHr>t0 & drHr<=t1]
							sds <- sdVec[drHr>t0 & drHr<=t1]
							if (length(drs)>0) dr10[m]=mean(drs) else dr10[m]=0
							if (length(sds)>0) sd10[m]=sum(sds) else sd10[m]=0
						}
						if (grLen>0)
						{
							grs <- grVec[grHr>t0 & grHr<=t1]
							if (length(grs)>0) gr10[m]=mean(grs) else gr10[m]=0
						}
					}  # end for m=1:144

					# create daily means (dr10, gr10) or sums (sd10, ssVec, rrVec) if desired,
					# and write them to daily statistics output file

					if (makeStat>0)
					{
						outl = sprintf(" %02d.%02d.%04d (%03d)", day, mon, year, doy)
						if (grLen>0)
						{
							gr10avg = mean(grVec)
							outl = sprintf("%s%7.1f", outl, gr10avg)
						} else
						{
							outl = sprintf("%s%s", outl, "      /")
						}
						if (drLen>0)
						{
							dr10avg = mean(drVec)
							sd10sum = sum(sd10)
							outl = sprintf("%s%7.1f%7.0f", outl, dr10avg, sd10sum)
						} else
						{
							outl = sprintf("%s%s", outl, "      /      /")
						}
						if (vqLen>0)
						{
							if (max(ssVec)>20)
							{
								outl = sprintf("%s%s", outl, "      /")
							}else
							{
								ssVecSum = sum(ssVec)
								outl = sprintf("%s%7.0f", outl, ssVecSum)
							}
							if (max(rrVec)>120)
							{
								outl = sprintf("%s%s%s", outl, "      /", "\n")
							}else
							{
								rrVecSum = sum(rrVec)
								outl = sprintf("%s%7.1f%s", outl, rrVecSum, "\n")
							}
						} else
						{
							outl = sprintf("%s%s%s", outl, "      /      /", "\n")
						}
						cat (outl, file=st)
					}  # end if makeStat>0

					# write data to output file (menu: "GR10" "DR10" "SD10" "SDUR" "RAIN")

					for (m in 1:144)
					{
						outl <- outHM[m]
						for (i in 1:nop)
						{
							if (match(outRec[i],"GR10",nomatch=0)) outl = sprintf("%s%7.1f",outl, gr10[m])
							if (match(outRec[i],"DR10",nomatch=0)) outl = sprintf("%s%7.1f",outl, dr10[m])
							if (match(outRec[i],"SD10",nomatch=0)) outl = sprintf("%s%7.1f",outl, sd10[m])
							if (match(outRec[i],"SDUR",nomatch=0)) outl = sprintf("%s%7.1f",outl, ssVec[m])
							if (match(outRec[i],"RAIN",nomatch=0)) outl = sprintf("%s%7.1f",outl, rrVec[m])
						}
						outl = sprintf("%s%s",outl, "\n")
						cat (outl, file=op)
					}  # end for m=1..144
					close(op)  # close the output file of the day
				}
				else  # input data of the day doesen't exist
				{
					infoFile = sprintf("%s%02d.%02d.%04d%s","    No input data found for  ",day, mon, year, "\n")
					cat (infoFile, file="")
				}  # end if nf:0

			}  # for day=day1:day2

		}  # for mon=mon1:mon2

		if (makeStat>0) close(st)  # close the daily statistics output file
	}  # for year=yearA:yearE

}  # end if iniOk

#### end of program 'ReduceRad2SunDur.R' ####################################
