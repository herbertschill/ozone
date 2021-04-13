#############################################################################
#                                                                           #
#                                                                           #
#                            BREWER DATA	 HANDLING                         #
#                                                                           #
#                                                                           #
#        Module Name         :  InstrConstHistoryDWH.R                      #
#                                                                           #
#                                                                           #
#        Creation Date       :  03/12/2010                                  #
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
#        'InstrConstHistoryDWH.R' reads a series of instrument constant     #
#        fiels 'ICFjjjyy.bbb' and writes selected constants of them to      #
#        the file 'InstrConstHistoryDWH.bbb'.                               #
#                                                                           #
#        The parameters are read from the file 'InstrConstHistoryDWH.ini'.  #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


# setwd("C:\\LKO\\Programs\\R")

source("ReadDobsTables.R")

statusOK <- 0

# Reads the path-file 'InstrConstHistoryDWH.pth' with the adress of the
# ini-file 'InstrConstHistoryDWH.ini' and reads the initialization parameters
# from the latter (Nparams designs the number of parameters to read from
# ini-file)

Nparams=8
iniParams = ReadIniFile(Nparams, "InstrConstHistoryDWH")
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Initialize values

	FirstYear = as.integer(iniParams[[2]][[2]])
	LastYear  = as.integer(iniParams[[2]][[3]])
	BrewStr   = iniParams[[2]][[4]]
	InPath    = iniParams[[2]][[5]]
	OutPath   = iniParams[[2]][[6]]
	ParamList = iniParams[[2]][[7]]
	NNN       = as.integer(iniParams[[2]][[8]])-1
	maxConst  = nchar(ParamList)
	vv  = array()

	# create output-filename and open file for write

	opathname = sprintf("%s%s%s", OutPath, "InstrConstHistoryDWH.", BrewStr)
	df = file(opathname,open="w")

	# proceed years

	for (year in FirstYear:LastYear)
	{
		if (year<2000)
			SYear=year-1900
		else
			SYear=year-2000

		JDN=0
		if (year/4==floor(year/4))
			leap=1
		else
			leap=0

		# Treat months; set proper days in month 'mdays'

		for (mon in 1:12)
		{
			if (leap==1) 
				mdays = switch(mon, 31,29,31,30,31,30,31,31,30,31,30,31)
			else
				mdays = switch(mon, 31,28,31,30,31,30,31,31,30,31,30,31)

			# Treat days in month

			for (day in 1:mdays)
			{
				JDN = JDN+1

				# create input-filename and open 'ICFjjjyy.bbb'-file if existing

				ipathname = sprintf("%s%s%03i%02i%s%s", InPath, "ICF", JDN, SYear, ".", BrewStr)
				ex=file.exists(ipathname)
				if (ex)
				{
					cf = file(ipathname,open="r")

					# create date 'yyyymmdd', set line number

					YMDstr = sprintf("%04i%02i%02i", year, mon, day)
					NNN = NNN+1

					# read desired constants from 'ICFjjjyy.bbb'-file
					#
					# cc  vv[cc]       constant
					#
					#  1  0            Ozone temperature coefficient slit 1 (tx2)
					#  2  -0.4968      Ozone temperature coefficient slit 2 (tx3)
					#  3  -0.8072      Ozone temperature coefficient slit 3 (tx4)
					#  4  -1.3838      Ozone temperature coefficient slit 4 (tx5)
					#  5  -2.8861      Ozone temperature coefficient slit 5 (tx6)
					#  6  .3434        Ozone on ozone ratio 
					#  7  2.35         SO2 on SO2 ratio
					#  8  1.1426       Ozone on SO2 ratio
					#  9  3170         ETC on ozone ratio
					# 10  3220         ETC on SO2 ratio
					# 11  .00000004    Dead time [seconds]
					# 12  0            Neutral density of filter 0 (divide by 10**4 to get attenuation)
					# 13  5000         Neutral density of filter 1 (divide by 10**4 to get attenuation)
					# 14  10000        Neutral density of filter 2 (divide by 10**4 to get attenuation)
					# 15  15000        Neutral density of filter 3 (divide by 10**4 to get attenuation)
					# 16  20000        Neutral density of filter 4 (divide by 10**4 to get attenuation)
					# 17  25000        Neutral density of filter 5 (divide by 10**4 to get attenuation)

					cc=0
					for (i in 1:maxConst)
					{
						line = scan(cf,what="character",nlines=1,quiet=TRUE)
						desi = substr(ParamList,i,i)
						if (desi=="1")
						{
							cc=cc+1
							vv[cc]=as.double(line[1])
						}
					}  # for i=1..maxConst

					# calculate temperature coefficients TX[x]

					tx0 = vv[1]-vv[4] -3.2*(vv[4]-vv[5])
					tx1 = vv[2]-vv[4] -0.5*(vv[3]-vv[4]) -1.7*(vv[4]-vv[5])
					tx7 = 0

					# create output line and write it to 'InstrConstHistoryDWH.bbb'
					#
					#  vv[cc], cc= 9,10,6,7,8,tx0,1,2,3,4,5,tx6,tx7,11,12,13,14,15,16,17

					outl = sprintf("%4i  %s%6.0f%6.0f%8.4f%8.4f%8.4f", NNN, YMDstr, vv[9], vv[10], vv[6], vv[8], vv[7])
					outl = sprintf("%s%10.5f%10.5f%10.5f%10.5f%10.5f", outl, tx0, tx1, vv[1], vv[2], vv[3])
					outl = sprintf("%s%10.5f%10.5f%10.5f%12.9f", outl, vv[4], vv[5], tx7, vv[11])
					outl = sprintf("%s%6.0f%6.0f%6.0f%6.0f%6.0f%6.0f", outl, vv[12], vv[13], vv[14], vv[15], vv[16], vv[17])
					outl = sprintf ("%s%s", outl, "\n")
					cat (outl, file=df)
					close(cf)

					}  # end if 'ex'
			}  # for day=1..mdays
		}  # for mon=1..12
	}  # for year=FirstYear..LastYear

	close(df)

}  # end if 'iniOk'


# end of program 'InstrConstHistoryDWH.R'