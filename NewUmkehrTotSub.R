#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  NewUmkehrTotSub.R                           #
#                                                                           #
#                                                                           #
#        Creation Date       :  27.10.2010                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               24.03.2016                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert                              #
#                                                                           #
#        Developing System   :  R 2.5.0    (2009)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'NewUmkehrTotSub.R' contains functions for the treatment of        #
#        halfday total ozone values of Dobson measurements:                 #
#                                                                           #
#         - CompleteYear                                                    #
#         - DecodeHdLine                                                    #
#         - InterpolateDays                                                 #
#         - WriteNewtot                                                     #
#                                                                           #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'NewUmkehrTotoz.ini'.                         #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 24.03.2016 by Herbert Schill:                                    #
#                                                                           #
#          Replace 'as.real' by 'as.double'                                 #
#                                                                           #
#############################################################################



CompleteYear <- function (nt, djdn, year, leap, loc)

#############################################################################
#                                                                           #
#        Fills the output file "NEWTOT_yyyy.iii" (nt) with empty lines      #
#        at the begin or the end of the year:                               #
#                                                                           #
#         ddmmyyh 999  3                                                    #
#                                                                           #
#        Function call: CompleteYear (nt, djdn, year, loc)                  #
#                                                                           #
#        Input:  nt  : open output file "NEWTOT_yyyy.iii"                   #
#                djdn: first or last julian day with proper data            #
#                year: year                                                 #
#                leap: leap year=1, else 0                                  #
#                loc : location to complete: -1=begin, 1=end of file        #
#                                                                           #
#        Output: nt: open output file "NEWTOT_yyyy.iii"                     #
#                                                                           #
#############################################################################

{

# Write line with daily and halfday statistics for wl=AD (AM=1, PM=2) on 'nt'

dd=mm=0
syear = year-1900
if  (syear>99)  syear=syear-100
ozA=ozP=999
qfA=qfP=3

if (loc<0)
{
	ja = 1
	je = djdn-1
}
else
{
	ja = djdn+1
	if (leap)
		je = 366
	else
		je = 365
}

for (jj in ja:je)
{
	xDate = list(dd, mm, year, jj, leap)
	xDate = ConvertDate (xDate, -1)
	day  = as.integer(xDate[1])
	mon  = as.integer(xDate[2])
	xData = list(day,mon,year,jj,ozA,ozP,qfA,qfP)
	nt = WriteNewtot (xData, nt)
}

return(nt)

}  # end of function 'CompleteYear'


DecodeHdLine <- function (hline, leap)

#############################################################################
#                                                                           #
#  Decodes a line from the halfday data file "HDOZON_yyyy.iii" and writes   #
#  the values to the list 'cdata', which is returned.                       #
#                                                                           #
#############################################################################

{
dateStr1 = hline[1]
year = as.integer(substr(dateStr1,1,4))
mon  = as.integer(substr(dateStr1,5,6))
day  = as.integer(substr(dateStr1,7,8))
OzAM = as.double(hline[4])
qfAM = as.integer(hline[5])
OzPM = as.double(hline[6])
qfPM = as.integer(hline[7])

# set proper julian day of the year

if (leap==0) 
	jdn = switch(mon, 0,31,59,90,120,151,181,212,243,273,304,334) + day
else
	jdn = switch(mon, 0,31,60,91,121,152,182,213,244,274,305,335) + day

# Set proper halfday ozone and quality flag

if ((qfAM>0) & (qfPM>0))
{
	qfAM = 0
	qfPM = 0
}
else if ((qfAM>0) & (qfPM==0))
{
	qfAM = 0
	qfPM = 1
	OzPM = OzAM
}
else if ((qfAM==0) & (qfPM>0))
{
	qfAM = 1
	qfPM = 0
	OzAM = OzPM
}
else if ((qfAM==0) & (qfPM==0))
{
	qfAM = 2
	qfPM = 2
	OzAM = 999
	OzPM = 999
}

# Write data in list

cdata = list(day, mon, year, jdn, OzAM, OzPM, qfAM, qfPM)
names(cdata) = c("day","mon","year","jdn","OzAM","OzPM","qfAM","qfPM")

return (cdata)

}  # end of function 'DecodeHdLine'


InterpolateDays <- function (nn, cdata1, cdata2)

#############################################################################
#                                                                           #
#  Interpolates the ozone values and set the proper quality flag for a      #
#  given day between two days with existing ozone; if time gap is bigger    #
#  than 10 days, OzAM/PM=999 and qf=3.                                      #
#                                                                           #
#  cdataX {day, mon, year, jdn, OzAM, OzPM, qfAM, qfPM} is returned.        #
#                                                                           #
#############################################################################

{

# get proper jdn and date

year = as.integer(cdata1[3])
jdn1 = as.integer(cdata1[4])
jdn2 = as.integer(cdata2[4])

jdnN = jdn2-jdn1
jdnX = jdn1+nn
day=mon=leap=0

cDateX = list(day, mon, year, jdnX, leap)
names(cDateX) = c("day","mon","year","jdn","leap")

cDateX=ConvertDate(cDateX, -1)
dd = as.integer(cDateX[1])
mm = as.integer(cDateX[2])

	# do interpolation (use daily means)

if (jdnN<=10)
{
	ozoAM = as.double(cdata1[5])
	ozoPM = as.double(cdata1[6])
	ozo1 = (ozoAM+ozoPM)/2
	ozoAM = as.double(cdata2[5])
	ozoPM = as.double(cdata2[6])
	ozo2 = (ozoAM+ozoPM)/2
	ozo1 = ozo1 + (ozo2-ozo1)*nn/(jdnN)
	ozo2 = ozo1
	qf1=qf2=2
}
else
{
	ozo1=ozo2=999
	qf1=qf2=3
}

# Write data in list

cdataX = list(dd, mm, year, jdnX, ozo1, ozo2, qf1, qf2)
names(cdataX) = c("day","mon","year","jdn","OzAM","OzPM","qfAM","qfPM")

return (cdataX)

}  # end of function 'InterpolateDays'


WriteNewtot <- function (cdata, nt)

#############################################################################
#                                                                           #
#        'WriteNewtot' writes the halfday values (mean, quality flag)       #
#        of total ozone values of Dobson measurements (Wavelength=AD)       #
#        to the output file "NEWTOT_yyyy.iii" (nt).                         #
#                                                                           #
#        Desired Input/Output parameters, pathnames and filenames are       #
#        read from ASCII-file 'NewUmkehrTotoz.ini'.                         #
#                                                                           #
#        Function call: WriteNewtot (cdata, nt)                             #
#                                                                           #
#        Input:  cdata: list of measurements                                #
#                nt: open output file "NEWTOT_yyyy.iii"                     #
#                                                                           #
#        Output: nt: open output file "NEWTOT_yyyy.iii"                     #
#                                                                           #
#############################################################################

{

# Write line with daily and halfday statistics for wl=AD (AM=1, PM=2) on 'nt'

day = as.integer(cdata[1])
mon = as.integer(cdata[2])
year = as.integer(cdata[3])
syear = year-1900
if  (syear>99)  syear=syear-100
OzAM  = as.double(cdata[5])
OzPM  = as.double(cdata[6])
iOzAM = as.integer(OzAM + 0.5)
iOzPM = as.integer(OzPM + 0.5)
qfAM = as.integer(cdata[[7]])
qfPM = as.integer(cdata[[8]])

outl   = sprintf ("%s%02i%02i%02i%s%4i%3i%s", " ", day, mon, syear, "0", iOzAM, qfAM, "\n")
cat (outl, file=nt)
outl   = sprintf ("%s%02i%02i%02i%s%4i%3i%s", " ", day, mon, syear, "1", iOzPM, qfPM, "\n")
cat (outl, file=nt)

return(nt)

}  # end of function 'WriteNewtot'

