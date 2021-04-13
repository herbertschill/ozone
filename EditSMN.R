#############################################################################
#                                                                           #
#                                                                           #
#                            METEOROLOGIGAL OBSERVATIONS                    #
#                                                                           #
#                                                                           #
#        Module Name         :  EditSMN.R                                   #
#                                                                           #
#                                                                           #
#        Creation Date       :  05/11/2010                                  #
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
#        Allows to edit SMN files. Entries have to be made manually in      #
#        the program.                                                       #
#                                                                           #
#                                                                           #
#        Modification history:                                              #
#                                                                           #
#                                                                           #
#############################################################################


# setwd("C:\\LKO\\Programs\\R")

statusOK <- 0

# create filenames and open files for read/write

inpath = "C:\\LKO\\Klima\\Asta\\SMN\\"
infile = "Ruenenberg_GRSD_2010_1005.txt"
outfile = "Ruenenberg_GRSD_2010_1005c.txt"
inpathname = sprintf("%s%s", inpath, infile)
ex=file.exists(inpathname)

if (ex)
{
	df = file(inpathname,open="r")
	outpathname = sprintf("%s%s", inpath, outfile)
	af = file(outpathname,open="w")

	# read and write header lines

	for (i in 1:11)
	{
		header = readLines(df, n=1)
		header = sprintf ("%s%s", header, "\n")
		cat (header, file=af)
	}

	# proceed the data lines of infile

	mv = array()
	mv = scan(df,what="numeric",nlines=1,quiet=TRUE)

	while (nchar(mv[2])>3)
	{
		sta = as.integer(mv[1])
		ayr = as.integer(mv[2])
		amn = as.integer(mv[3])
		ady = as.integer(mv[4])
		ahh = as.integer(mv[5])
		amm = as.integer(mv[6])
		sdur = as.integer(mv[7])
		sdcl = as.integer(mv[8])
		grad = as.integer(mv[9])

		sta;ayr;amn;ady;ahh;amm;sdur;grad
		outl = sprintf ("%5i%5i%3i%3i%3i%3i%6i%6i%s", sta, ayr, amn, ady, ahh, amm, sdur, grad, "\n")
		cat (outl, file=af)

		mv = scan(df,what="numeric",nlines=1,quiet=TRUE)

	}  # end while

	close(df)  # close input file
	close(af)  # close output file

}  # 	end if file.exists(dpathname)


# end of program 'EditSMN.R'