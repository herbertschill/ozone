#############################################################################
#                                                                           #
#                                                                           #
#                            METEOROLOGIGAL OBSERVATIONS                    #
#                                                                           #
#                                                                           #
#        Module Name         :  AstaConvert.R                               #
#                                                                           #
#                                                                           #
#        Creation Date       :  07.09.2010                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               28.08.2019                                  #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   Meteoswiss                 #
#        Modifications by    :  SCHILL Herbert   PMOD-WRC                   #
#                                                                           #
#        Developing System   :  R 3.4.3    (2018)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'AstaConvert.R' reads either the combined Asta datafile            #
#        'Djjjyyyy.DAT', splits its contents and writes them to the         #
#        separated data files 'Djjjyyyy.ARO' and 'Djjjyyyy.TSC', or         #
#        reads the single datafile 'Ayyyyjjj.DAT' resp. 'Tyyyyjjj.DAT'      #
#        reformatts the contents and writes it to the data file             #
#        'Djjjyyyy.ARO' or 'Djjjyyyy.TSC'.                                  #
#                                                                           #
#        If necessary, the hour values of 'Djjjyyyy.DAT' can be corrected;  #
#        the corrected data are written to 'Djjjyyyy.COR'                   #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#        - 28.08.2019 by Herbert Schill:                                    #
#                                                                           #
#          - expand for treatment of 'Ayyyyjjj.DAT' resp. 'Tyyyyjjj.DAT'    #
#            files                                                          #
#                                                                           #
#############################################################################


setwd("C:\\PMOD\\Programs\\R")
Sys.setenv(TZ="UTC")

source("ReadInstTables.R")

#  from:               import:
#
#  ReadInstTables.R    ReadIniFile


statusOK <- 0

# Open the ini-file 'AstaConvert.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=8
iniParams = ReadIniFile(Nparams, "AstaConvert.ini", 0)
iniOk = iniParams[[1]][[1]]

if (iniOk)  # Continue, if ini file was properly read
{
	# Redefine ini-parameter vector

	iniPar = array()
	npar = iniParams[[1]][[1]]
	for (n in 1:Nparams)  iniPar[n] = iniParams[[2]][[n]]

	InYear  = as.integer(iniPar[2])
	JDN     = as.integer(iniPar[3])
	jdnStr  = iniPar[3]
	Station = iniPar[4]
	corrD   = as.integer(iniPar[8])

	InYear;JDN;jdnStr;Station;corrD

	# create filenames and open files for read/write

	if  (Station=="ARO")  dpathname = sprintf("%s%s%s%s%s", iniPar[5], "A", InYear, jdnStr, ".DAT") else
	if  (Station=="TSC")  dpathname = sprintf("%s%s%s%s%s", iniPar[5], "T", InYear, jdnStr, ".DAT") else
	dpathname = sprintf("%s%s%s%s%s", iniPar[5], "D", jdnStr, InYear, ".DAT")

	if (file.exists(dpathname))
	{
		df = file(dpathname,open="r")
		if (Station=="ARO")
		{
			apathname = sprintf("%s%s%s%s%s", iniPar[6], "D", InYear, jdnStr, ".ARO")
			af = file(apathname,open="w")
			mm=19
		} else
		if (Station=="TSC")
		{
			tpathname = sprintf("%s%s%s%s%s", iniPar[7], "D", InYear, jdnStr, ".TSC")
			tf = file(tpathname,open="w")
			mm=14
		} else
		{
			apathname = sprintf("%s%s%s%s%s", iniPar[6], "D", jdnStr, InYear, ".ARO")
			tpathname = sprintf("%s%s%s%s%s", iniPar[7], "D", jdnStr, InYear, ".TSC")
			af = file(apathname,open="w")
			tf = file(tpathname,open="w")
			mm=27
		}

		if (corrD!=0)
		{
			cpathname = sprintf("%s%s%s%s%s", iniPar[5], "D", jdnStr, InYear, ".COR")
			cf = file(cpathname,open="w")
		}

		k=0
		dv = array()
		mv = array()

		if (Station=="DAT")  # proceed the lines of 'Dyyyyjjj.DAT'
		{
			#   content of mv array, if Station='DAT'
			#          date + time   station 'ARO'                                     station 'TSC'
			#   i      1    2   3    4  5   6  7   8   9  10 11  12  13 14  15 16  17  18 19  20 21 22 23  24  25  26 27
			#   mv[i]  yyyy,jjj,hhmm,ff,ddd,fx,rff,rdd,tt,uu,glr,lux,sd,ppp,rr,uvm,uvb,ff,ddd,fx,tt,uu,glr,oz1,uvb,ty,oz2

			while (nchar(mv[1])>3)
			{
				ayear = as.integer(mv[1])
				ajdn  = as.integer(mv[2])
				ahhmm = as.integer(mv[3])
				ahour = floor(ahhmm/100)
				amin  = ahhmm-ahour*100

				# special case (global rad threshold)
				
				if (abs(as.single(mv[11]))<0.01) mv[11]=0.0
				
				# write corrected time and measurement values on file 'Djjjyyyy.COR' if desired

				if (corrD!=0)
				{
					ahour = ahour+corrD
					if (ahour<0)
					{
						ahour = ahour+24
						ajdn  = ajdn-1
					}
					if (ahour>24)
					{
						ahour = ahour-24
						ajdn  = ajdn+1
					}
					if (ahour==0)
						outl = sprintf ("%i,%i,%i,", ayear, ajdn, amin)
					else
						outl = sprintf ("%i,%i,%i%02i,", ayear, ajdn, ahour, amin)
					for (k in 4:26)	outl = sprintf ("%s%g,", outl, mv[k])
					outl = sprintf ("%s%g%s", outl, mv[27], "\n")
					cat (outl, file=cf)
				}

				# unit correction for some items

				mv[4] = as.single(mv[4])/10
				mv[6] = as.single(mv[6])/10
				mv[7] = as.single(mv[7])/10
				mv[15] = as.single(mv[15])/10
				if (mv[16]<0.0) mv[16]=0.0

				if ((ahhmm>5) | (k>3))
				{
					k=k+1
					if ((ahhmm==0) & (k>3))
					{
						ahour = 24
						amin  = 0
					}

					#  output-format ARO-file
					#  hh,mm,ff,  ddd,fx,  rff, rdd,tt,   uu,   glr,    lux,  sd, ppp,   rr,  uvm,  uvb
					#  00,10,00.5,033,03.1,00.0,072,+17.7,053.8,-0001.7,00.00,000,0814.7,00.0,000.0,000.3
					#  11,50,03.4,054,07.3,02.4,075,+09.7,099.7,+0018.3,00.19,002,0812.1,08.9,000.1,001.5
					#  00,00,31.6,171,88.0,21.0,047,+18.5,047.0,-0002.8,00.00,646,0815.0,00.0,-00.0,000.4

					outl = sprintf ("%02i,%02i,", ahour, amin)
					outl = sprintf ("%s%04.1f,%03.0f,%04.1f,%04.1f,%03.0f,%+05.1f,%05.1f,", outl, mv[4],mv[5],mv[6],mv[7],mv[8],mv[9],mv[10])
					outl = sprintf ("%s%+07.1f,%05.2f,%03.0f,%06.1f,%04.1f,%05.1f,%05.1f", outl, mv[11],mv[12],mv[13],mv[14],mv[15],mv[16],mv[17])
					outl = sprintf ("%s%s", outl, "\n")
					cat (outl, file=af)

					#  output-format TSC-file
					#  hh,mm,ff,  ddd,fx,  tt,   uu,   glr,   oz1,  oz2,  uvb,   ty,  res
					#  00,10,05.6,201,10.1,+17.3,044.1,0003.7,033.3,034.6,0000.0,46.0,99.9
					#  14,10,01.2,320,02.2,+10.2,097.1,0262.0,042.0,042.8,0073.0,46.1,99.9

					outl = sprintf ("%02i,%02i,", ahour, amin)
					outl = sprintf ("%s%04.1f,%03.0f,%04.1f,%+05.1f,%05.1f,", outl, mv[18],mv[19],mv[20],mv[21],mv[22])
					outl = sprintf ("%s%06.1f,%05.1f,%05.1f,%06.1f,%04.1f,%s", outl, mv[23],mv[24],mv[27],mv[25],mv[26],"99.9")
					outl = sprintf ("%s%s", outl, "\n")
					cat (outl, file=tf)
				}
				mv = scan(df,what="numeric",sep=",",nlines=-1,quiet=TRUE)

			}  # end while

		} else  # proceed the lines of 'Ayyyyjjj.DAT' resp. 'Tjjjyyyy.DAT'
		{
			#   content of mdataLst array, if Station='ARO'
			#                  sta date jdn time
			#   n              1   2    3   4    5  6   7  8   9   10 11 12  13  14 15  16 17  18  19
			#   mdataLst[n,k]  iii,yyyy,jjj,hhmm,ff,ddd,fx,rff,rdd,tt,uu,glr,lux,sd,ppp,rr,uvm,uvb,0

			#   content of mdataLst array, if Station='TSC'
			#                  sta date jdn time
			#   n              1   2    3   4    5  6   7  8  9  10  11  12  13 14
			#   mdataLst[n,k]  iii,yyyy,jjj,hhmm,ff,ddd,fx,tt,uu,glr,oz1,uvb,ty,oz2

			mdataLst = scan(df,what="numeric",sep=",",nlines=-1,quiet=TRUE)
			nn = length(mdataLst)/mm
			dim(mdataLst) = c(mm,nn)

			for (k in 1:nn)
			{
				#k=1
				ayear = as.integer(mdataLst[2,k])
				ajdn  = as.integer(mdataLst[3,k])
				ahhmm = as.integer(mdataLst[4,k])
				ahour = floor(ahhmm/100)
				amin  = ahhmm-ahour*100

				if ((ahhmm==0) & (k>3))
				{
					ahour = 24
					amin  = 0
				}
					for (n in 5:mm)  dv[n] = as.single(mdataLst[n,k])

				if (Station=="ARO")
				{
					# unit correction for some items

					dv[5] = dv[5]/10
					dv[7] = dv[7]/10
					dv[8] = dv[8]/10
					dv[16] = dv[16]/10
					if (dv[17]<0.0) dv[17]=0.0

					#  output-format ARO-file
					#  hh,mm,ff,  ddd,fx,  rff, rdd,tt,   uu,   glr,    lux,  sd, ppp,   rr,  uvm,  uvb
					#  00,10,00.5,033,03.1,00.0,072,+17.7,053.8,-0001.7,00.00,000,0814.7,00.0,000.0,000.3
					#  11,50,03.4,054,07.3,02.4,075,+09.7,099.7,+0018.3,00.19,002,0812.1,08.9,000.1,001.5

					outl = sprintf ("%02i,%02i,", ahour, amin)
					outl = sprintf ("%s%04.1f,%03.0f,%04.1f,%04.1f,%03.0f,%+05.1f,%05.1f,", outl, dv[5],dv[6],dv[7],dv[8],dv[9],dv[10], dv[11])
					outl = sprintf ("%s%+07.1f,%05.2f,%03.0f,%06.1f,%04.1f,%05.1f,%05.1f%s", outl,dv[12],dv[13],dv[14],dv[15],dv[16],dv[17],dv[18],"\n")
					cat (outl, file=af)

				} else  # Station=="TSC"
				{
					# unit correction for uvb

					dv[12] = dv[12]*1000

					#  output-format TSC-file
					#  hh,mm,ff,  ddd,fx,  tt,   uu,   glr,   oz1,  oz2,  uvb,   ty,  res
					#  00,10,05.6,201,10.1,+17.3,044.1,-003.7,033.3,034.6,0000.0,46.0,99.9
					#  14,10,01.2,320,02.2,+10.2,097.1,0262.0,042.0,042.8,0073.0,46.1,99.9

					outl = sprintf ("%02i,%02i,", ahour, amin)
					outl = sprintf ("%s%04.1f,%03.0f,%04.1f,%+05.1f,%05.1f,", outl, dv[5],dv[6],dv[7],dv[8],dv[9])
					outl = sprintf ("%s%06.1f,%05.1f,%05.1f,%06.1f,%04.1f,%s%s", outl, dv[10],dv[11],dv[14],dv[12],dv[13],"99.9","\n")
					cat (outl, file=tf)
				}

			}  # end for k in 1:nn
		}  # end if Station:'DAT'

		close(df)  # close input file 'Dyyyyjjj.DAT' RESP.  'Djjjyyyy.DAT'
		if (Station=="ARO")  # close output file 'Dyyyyjjj.ARO'
		{
			close(af)
			infoFile = sprintf("%s%s%s%s%s","  File  ",dpathname, "  converted to\n        ",apathname, "\n")
		} else
		if (Station=="TSC")  # close output file 'Dyyyyjjj.TSC'
		{
			close(tf)
			infoFile = sprintf("%s%s%s%s%s","  File  ",dpathname, "  converted to\n        ",tpathname, "\n")
		} else
		{
			close(af)  # close output file 'Djjjyyyy.ARO'
			close(tf)  # close output file 'Djjjyyyy.TSC'
			infoFile = sprintf("%s%s%s%s  %s%s","  File  ",dpathname, "  converted to\n  Files  ",apathname,tpathname, "\n")
			if (corrD!=0)	close(cf)  # close output file 'Djjjyyyy.COR'
		}
		cat(infoFile, file="")

	} else  # file dpathname does not exist
	{
		infoFile = sprintf("%s%s%s","  File  ",dpathname, "  not found !\n")
		cat(infoFile, file="")
	}  # 	end if file.exists(dpathname)

}  # end if iniOk

# end of program 'AstaConvert.R'
