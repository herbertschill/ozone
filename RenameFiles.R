#############################################################################
#                                                                           #
#                                                                           #
#                            FILE HANDLING                                  #
#                                                                           #
#                                                                           #
#        Module Name         :  RenameFiles.R                               #
#                                                                           #
#                                                                           #
#        Creation Date       :  15.11.2021                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD/WRC                   #
#        Modifications by    :     "      "                                 #
#                                                                           #
#        Developing System   :  R 4.1.1    (2021)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'RenameFiles.R' renames and copies a number of files including a   #
#        date-string of format 'jjj.yyyy' from the source-directory to the  #
#        destination-directory.                                             #
#                                                                           #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'RenameFiles.ini'.                                                 #
#                                                                           #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("DateZeit.R")
source("ReadInstTables.R")

#  from:               import:
#
#  DateZeit.R          LeapYear
#  ReadInstTables.R    ReadIniFile 


# Open the ini-file 'RenameFiles.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=8
iniParams = ReadIniFile(Nparams, "RenameFiles.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
  # Redefine ini-parameter vector
  
  iniPar = array()
  npar = iniParams[[1]][[1]]
  for (n in 1:npar)  iniPar[n] = iniParams[[2]][[n]]
  doy1 = as.integer(substr(iniPar[2],1,3))
  doy2 = as.integer(substr(iniPar[3],1,3))
  year = as.integer(substr(iniPar[2],5,8))
  dobson = iniPar[4]
  inHead = iniPar[5]
  outHead= iniPar[6]
  inpath = iniPar[7]
  outpath= iniPar[8]
  
  for (dd in doy1:doy2)
  {
    #dd=doy1
    inFilename = sprintf("%s%3.3d%4.4d.%s", inHead, dd, year, dobson)
    inName = sprintf("%s%s", inpath, inFilename)
    outFilename = sprintf("%s%3.3d%4.4d.%s", outHead, dd, year, dobson)
    outName = sprintf("%s%s", outpath, outFilename)
    
    if (file.exists(inName))
    {
      file.copy(inName, outName, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)      
    }
  }
}  # end if iniOk
      
# end of program 'RenameFiles.R'