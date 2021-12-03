#############################################################################
#                                                                           #
#                                                                           #
#                            FILE HANDLING                                  #
#                                                                           #
#                                                                           #
#        Module Name         :  RenameFigFiles.R                            #
#                                                                           #
#                                                                           #
#        Creation Date       :  28.07.2021                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               11.08.2021                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD/WRC                   #
#        Modifications by    :     "      "                                 #
#                                                                           #
#        Developing System   :  R 3.5.2    (2019)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RenameFigFiles.R' renames and moves a number of ozone rough data  #
#        quality check files from the source-directory to the destination-  #
#        directories. Default file and path names are hardcoded.            #
#                                                                           #
#        Possible infile names are: -'fig_o3.jpg'                           #
#                                   -'fig_o3DD.jpg'        (DD=day [01..31] #
#                                   -'fig_o3_ratios.jpg'                    #
#                                   -'fig_o3_ratiosDD.jpg' (DD=day [01..31] #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#        - 11.08.2021 by Herbert Schill:                                    #
#                                                                           #
#          - modify info-message after moving procedure                     #
#                                                                           #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")

#  from:               import:
#
#  DateZeit.R          LeapYear


#  Hard-coded parameters

inPath = "C:\\PMOD\\"
outPathO3 = "C:\\PMOD\\Ozone\\O3DataCheck\\"
outPathRatio = "C:\\PMOD\\Ozone\\O3DataCheck\\Ratios\\"
inNameO3 = "fig_o3"
outNameO3 = "O3_data_check_"
NameRatio = "fig_o3_ratios"

fd=as.character(Sys.time(),TZ="GMT")  # "yyyy-mm-dd hh:mm:ss"
yr = as.integer(substr(fd,1,4))
Day = as.integer(substr(fd,9,10))-1
Mon = as.integer(substr(fd,6,7))
if (Day==0)
{
	Mon = Mon-1
	if (Mon==0)
	{
		Day=31
		Mon=12
		yr=yr-1
	} else
	{
		if (LeapYear(yr)==1) 
			{Day = switch(Mon, 31,29,31,30,31,30,31,31,30,31,30,31)} else
			{Day = switch(Mon, 31,28,31,30,31,30,31,31,30,31,30,31)}
	}
}  # end if Day=0

for (d in 0:31)
{
	if (d==0)
	{
		aNameO3 = sprintf("%s%s%s", inPath, inNameO3, ".jpg")
		aNameRatio = sprintf("%s%s%s", inPath, NameRatio, ".jpg")
	} else
	{
		aNameO3 = sprintf("%s%s%02d%s", inPath, inNameO3, d, ".jpg")
		aNameRatio = sprintf("%s%s%02d%s", inPath, NameRatio, d, ".jpg")
		Day=d
	}

	if (file.exists(aNameO3))
	{
		oNameO3 = sprintf("%s%s%04d%02d%02d%s", outPathO3, outNameO3, yr, Mon, Day, ".jpg")
		cstat = file.copy(aNameO3, oNameO3, overwrite=TRUE, copy.date=TRUE)
		if (cstat)
		{
			cstat = file.remove(aNameO3)
			infoFile = sprintf("%s%s%s%s%s%s%s", "\n  File '", inNameO3, "' moved from '", inPath, "' to '", oNameO3, "'\n")
			cat(infoFile, file="")
		} else
		{
			infoFile = sprintf("%s%s%s", "\n  Move of file ", aNameO3, " failed\n")
			cat(infoFile, file="")
		}  # end if cstat
	}  # end if file.exists(aNameO3)

	if (file.exists(aNameRatio))
	{
		oNameRatio = sprintf("%s%s%02d%s", outPathRatio, NameRatio, Day, ".jpg")
		cstat = file.copy(aNameRatio, oNameRatio, overwrite=TRUE, copy.date=TRUE)
		if (cstat)
		{
			cstat = file.remove(aNameRatio)
			infoFile = sprintf("%s%s%s%s%s%s%s", "\n  File '", NameRatio, "' moved from '", inPath, "' to '", outPathRatio, "'\n")
			cat(infoFile, file="")
		} else
		{
			infoFile = sprintf("%s%s%s", "\n  Move of file ", oNameRatio, " failed\n")
			cat(infoFile, file="")
		}  # end if cstat
	}  # end if file.exists(aNameRatio)
}  # next d

# end of program 'RenameFigFiles.R'