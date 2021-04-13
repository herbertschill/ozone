setwd("/home/pay/users/sch/LKO/Programs/Tabelle/");


# Test with the input from DWH =============================================


# Run the following command for directly working in the 'tcsh' window with the database on the SQL level:
#
# sqlplus meteoch/dwh@dwhdepl 

# For working with the SQL in R,  in window 'tcsh' first:
#
# source /opt/local/opkg/share/oracle/product/10.2.0/env_oracle.csh
#
# ... then start R

# Load the 'dwhget'-library into R:

library(dwhget)

ora <- Oracle(fetch.default.rec=50000);
con <- dbConnect(ora, user = "meteoch", password="dwh", db="DWHDEVT");# <<<< DB used
db_table <- dbListTables(con); # <<<<< liste des tables de la DB

tabOz <- db_table[708]; # T_WORK_OZONE_RAW_DATA
dbOz <- "WORK1";
dbt <- paste(dbOz,".",tabOz,sep="");

# Recherche des blocs de mesure d'une journee pour un Brewer:

sqlcmd <-paste("",
"SELECT DISTINCT",
" TO_CHAR(O.SET_REFERENCE_TS,'YYYYMMDDHH24MISS') AS tref, ",
" O.PARAMETER_ID AS par,",
" O.VALUE_NU AS data ",
"FROM ",dbt," O ",
"WHERE ",
"(TO_CHAR(O.SET_REFERENCE_TS,'YYYYMMDD') LIKE '20080725') AND ",
"(O.PARAMETER_ID = 3270 OR O.PARAMETER_ID between 3277 AND 3283) AND ",
"(O.INSTRUMENT_ID = 40)",
" ORDER BY 1, 2",sep="");
time.ref <- dbGetQuery(con, statement=sqlcmd);

cond <- time.ref$PAR==3270;  FILTRE <- time.ref$DATA[cond];
cond <- time.ref$PAR==3277;  SZA <- time.ref$DATA[cond]
cond <- time.ref$PAR==3278;  MU <- time.ref$DATA[cond]
cond <- time.ref$PAR==3279;  T <- time.ref$DATA[cond]
cond <- time.ref$PAR==3281;  O3 <- time.ref$DATA[cond]
cond <- time.ref$PAR==3283;  SDO3 <- time.ref$DATA[cond]
Tref <- as.POSIXct(strptime(time.ref$TREF[cond],format="%Y%m%d%H%M%S"),"UTC");

#-------------------------------------------------------------------------------------------
#
# the following commands describe and list validity and remarks for the existing RN-tables

tabOz <- db_table[619]; # T_DOBSON_RN_REF
dbOz <- "WORK1";
dbt <- paste(dbOz,".",tabOz,sep="");

# the following 8 lines show the original command used in the SQL environment:

select
	DOBSON_RN_ID as RN_Id,
	TO_CHAR(VALID_SINCE_DT,'YYYY MM DD ') "VALID_SINCE_DT",
	REMARKS_TX as Remarks
from
	WORK1.T_DOBSON_RN_REF
order by
	RN_Id;

# the following 7 lines show the construction of the SQL command:

sqlcmd <-paste(
" SELECT",
" a.DOBSON_RN_ID AS RN_Id,",
" TO_CHAR(a.VALID_SINCE_DT,'YYYY MM DD') AS Valid_since,",
" a.REMARKS_TX AS Remarks",
" FROM WORK1.T_DOBSON_RN_REF a",
" ORDER BY RN_Id",sep="");

bloc <- dbGetQuery(con, statement=sqlcmd);


sqlcmd <-paste("SELECT DOBSON_RN_ID FROM WORK1.T_DOBSON_RN_REF;",sep="");

bloc <- dbGetQuery(con, statement=sqlcmd);

#-------------------------------------------------------------------------------------------


# recherche d'un bloc de mesures spécifique dans la journée:

sqlcmd <-paste("",
"SELECT ",
" TO_CHAR(O.REFERENCE_TS,'YYYYMMDDHH24MISS') AS t,",
" O.PARAMETER_ID AS par,",
" O.VALUE_NU as data",
" FROM ",
dbt," O ",
"WHERE ",
"(TO_CHAR(O.SET_REFERENCE_TS,'YYYYMMDDHH24MI') LIKE ",time.ref$TREF[10],") AND ",
"(O.PARAMETER_ID between 3271 AND 3276) AND ",
"(O.INSTRUMENT_ID = 40) ",
"ORDER BY 1,2 ",sep="");
bloc <- dbGetQuery(con, statement=sqlcmd);
F <- array(NA,dim=c(5,6));
for (i in 1:6){cond <- bloc$PAR==(3270+i);  F[,i] <- bloc$DATA[cond]};
Time <- bloc$T[cond];
blocT <- mean(as.POSIXct(strptime(Time,format="%Y%m%d%H%M%S"),"UTC"))

sqlcmd <-paste("",
"SELECT ",
" TO_CHAR(O.SET_REFERENCE_TS,'YYYYMMDDHH24MISS') AS tref,",
" O.PLAUS_INFO_NR AS fl,",
" O.PARAMETER_ID AS par,",
" TO_CHAR(O.REFERENCE_TS,'YYYYMMDDHH24MISS') AS t,",
" O.VALUE_NU as data",
" FROM ",
dbt," O ",
"WHERE ",
"(O.SET_REFERENCE_TS LIKE TO_DATE('20080725164559','YYYYMMDDHH24MISS')) AND ",
"(O.PARAMETER_ID between 3271 AND 3276) AND ",
"(O.INSTRUMENT_ID = 40) ",
"ORDER BY 1,4,3 ",sep="");

res <- dbGetQuery(con, statement=sqlcmd);res

plot(Tref[SDO3<2.5],O3[SDO3<2.5],ylab="O3 [DU]", xlab="Time [s]",pch=15);
lines(Tref[SDO3<2.5],O3[SDO3<2.5],type="l",col=2);
segments(Tref[SDO3<2.5],(O3-SDO3)[SDO3<2.5],Tref[SDO3<2.5],(O3+SDO3)[SDO3<2.5]);
grid();

plot(Tref,MU,ylim=c(0,10));lines(Tref,FILTRE/64,col=2);lines(Tref,T/10, type="b",col=3);grid();lines(Tref,SZA/10,col=4);


# Recherche des paramètres pour le calcul de l'ozone: table 610:620 ; 
# 624 = RN_factor; 

sqlcmd <-paste("ALTER SESSION SET CURRENT_SCHEMA=","\"DWH1\"",sep=""); 
dbSendQuery(con, statement=sqlcmd);

db_table <- dbListTables(con); # <<<<< liste des tables de la DB

RN <- db_table[which(db_table== "T_DOBSON_RN_FACTOR")][1]
dbOz <- "DWH1";
dbt <- paste(dbOz,".",RN,sep="");

dbListFields(con=con,name=RN)
dbReadTable(con,dbt,row.names="R_VALUE_NU")

DC <- db_table[which(db_table== "T_DOBSON_CONSTANT")][1];
dbOz <- "DWH1";
dbt <- paste(dbOz,".",DC,sep="");

dbListFields(con=con,name=DC)
df_constant <- dbReadTable(con,dbt,row.names="DOBSON_CONSTANT_ID")

dbGetQuery(con, paste("select * from DWH1.T_DOBSON_CONSTANT"));
sqlcmd <- paste("select * from DWH1.T_DOBSON_CONSTANT where INSTRUMENT_ID = 5740");
sqlcmd <- paste("select * from",dbt,"where INSTRUMENT_ID = 5740");
dbGetQuery(con, sqlcmd);

# Test: une commande qui passe dans le sql direct est facilement convertie: ne pas mettre de ";" en fin de sql et 
# adapter le nom de la table avec DWH1.t_....

sqlcmd <- paste(
" select ",
" DOBSON_CONSTANT_ID as Cst,",
" VARIABLE_ID as VARid,",
" VALUE_NU as val,",
" substr(REMARKS_TX,1,20) as rem,",
" INSTRUMENT_ID as Inst ",
" from ",
" DWH1.t_dobson_constant ",
" where ",
" INSTRUMENT_ID =5740 or INSTRUMENT_ID is null ",
" order by DOBSON_CONSTANT_ID",sep="");

dbGetQuery(con, sqlcmd);



# http://cran.r-project.org/doc/manuals/R-data.pdf
