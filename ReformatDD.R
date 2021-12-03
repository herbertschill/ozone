#############################################################################
#                                                                           #
#                                                                           #
#                            TOTAL OZONE RECALCULATION                      #
#                                                                           #
#                                                                           #
#        Program Name        :  ReformatDD.R                                #
#                                                                           #
#                                                                           #
#        Creation Date       :  06.09.2021                                  #
#                                                                           #
#        Last Modifications  :                                              #
#                               none                                        #
#                                                                           #
#                                                                           #
#        Author              :  SCHILL Herbert   PMOD-WRC                   #
#        Modifications by    :                                              #
#                                                                           #
#        Developing System   :  R 3.5.2    (2018)                           #
#                                                                           #
#        Short Description   :                                              #
#                                                                           #
#        'ReformatDD' reformatts the values of Dobson measurement in        #
#        'Djjjyyyy.iii'-files coming from an unix-source to the data        #
#        format read by Martin Stanek's program 'O3Dobson'.                 #
#                                                                           #
#        All necessary initial information is read from the  ASCII-file     #
#        'ReformatDD.ini'.                                                  #
#                                                                           #
#        Input:                                                             #
#                                                                           #
#          - 'Djjjyyyy.iii'-files  (single day file)                        #
#                                                                           #
#        Output: Corrected D-files                                          #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################

setwd("C:\\PMOD\\Programs\\R")

source("ReadInstTables.R")  #  import: ReadIniFile


# Open the ini-file 'ReformatDD.ini' and read the initialization parameters
# (Nparams designs the number of parameters to read from ini-file)

Nparams=8
iniParams = ReadIniFile(Nparams, "ReformatDD.ini", 0)
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
		fpathname = sprintf("%s%s", inpath, inFilename)

		if (file.exists(fpathname))
		{
			inSets = array()
			infile = file(fpathname,open="r")
			inSets = scan(infile,what="character",nlines=-1,sep="\n",quiet=TRUE)

			close(infile)
			infoFile = sprintf("%s%s%s","    File  ",inFilename, "  ok\n")
			cat(infoFile, file="")

			# Initialization and data aggregation

			header = array()
			dNset  = array()
			RNtab  = array()
			Zpoly  = array()
			EmpCor = array()

			# read header and tables

			header<- inSets[1:17]
			n=grep ("dN",inSets,ignore.case=FALSE)
			dNset<- inSets[(n[1]+1):(n[1]+3)]
			n=grep ("NTable",inSets,ignore.case=FALSE)
			RNtab<- inSets[(n[1]+1):(n[1]+93)]
			n=grep ("Zpoly",inSets,ignore.case=FALSE)
			Zpoly<- inSets[(n[1]+1):(n[1]+60)]
			n=grep ("EmpCor",inSets,ignore.case=FALSE)
			EmpCor<- inSets[(n[1]+1):(n[1]+18)]

			# get positions of the measurements

			nal = grep ("ADADA",inSets,ignore.case=FALSE)
			lal = length(nal)

			mlen=40
			vRec   = array(1:(lal*mlen))
			j=1
			for (i in 1:lal)
			{
				vRec[j:(j+mlen-1)] <- inSets[(nal[i]-2):(nal[i]+mlen-3)]
				j=j+mlen
			}

			# Reshape measurement vector

			mRec<-vRec
			dim(mRec) = c(mlen,lal)

			# open output file and write data in proper format to it

			outFilename = sprintf("%s%3.3d%4.4d.%s", outHead, dd, year, dobson)
			outPathname = sprintf("%s%s", outpath, outFilename)
			odd = file(outPathname,open="w")

			outl = sprintf ("%s","")
			for (i in 1:length(header)) outl = sprintf ("%s%s\n",outl, header[i])
			cat(outl, file=odd)
			outl = sprintf ("%s","dN\n")
			for (i in 1:length(dNset)) outl = sprintf ("%s%s\n",outl, dNset[i])
			cat(outl, file=odd)
			outl = sprintf ("%s","NTable\n")
			cat(outl, file=odd)
			w=length(RNtab)/3
			for (j in 1:3)
			{
				outl = sprintf ("%s","")
				i1=(j-1)*w+1
				i2=j*w
				for (i in i1:i2) outl = sprintf ("%s%s\r",outl, RNtab[i])
				outl = sprintf ("%s\n",outl)
				cat(outl, file=odd)
			}
			outl = sprintf ("%s","Zpoly\n")
			cat(outl, file=odd)
			outl = sprintf ("%s","")
			for (i in 1:10) outl = sprintf ("%s%s\r",outl, Zpoly[i])
			outl = sprintf ("%s\n",outl)
			for (i in 11:20) outl = sprintf ("%s%s\r",outl, Zpoly[i])
			outl = sprintf ("%s\n",outl)
			cat(outl, file=odd)
			for (j in 1:10)
			{
				outl = sprintf ("%s","")
				i1=(j-1)*4+21
				i2=j*4+20
				for (i in i1:i2) outl = sprintf ("%s%s\r",outl, Zpoly[i])
				outl = sprintf ("%s\n",outl)
				cat(outl, file=odd)
			}
			outl = sprintf ("%s","EmpCor\n")
			for (i in 1:3) outl = sprintf ("%s%s\r",outl, EmpCor[i])
			outl = sprintf ("%s\n",outl)
			for (i in 4:6) outl = sprintf ("%s%s\r",outl, EmpCor[i])
			outl = sprintf ("%s\n",outl)
			for (i in 7:13) outl = sprintf ("%s%s\r",outl, EmpCor[i])
			outl = sprintf ("%s\n",outl)
			for (i in 14:18) outl = sprintf ("%s%s\r",outl, EmpCor[i])
			outl = sprintf ("%s\n\n",outl)
			cat(outl, file=odd)

			for (j in 1:lal)
			{
				outl = sprintf ("%s","")
				for (i in 1:mlen) outl = sprintf ("%s%s\n",outl, mRec[i,j])
				outl = sprintf ("%s\n",outl)
				cat(outl, file=odd)
			}

			close(odd)

		} else  # file 'XXXjjjyyyy.iii' doesen't exist
		{
			infoFile = sprintf("%s%s%s","    File  ",inFilename, "  not found\n")
			cat(infoFile, file="")

		}  # end: if file 'Djjjyyyy.iii' exists or not

	}  # next dd
} else
{
	infoFile = sprintf("%s","\n  Ini-File 'ReformatDD.ini' not found\n")
	cat(infoFile, file="")
}

# end of program 'ReformatDD'


