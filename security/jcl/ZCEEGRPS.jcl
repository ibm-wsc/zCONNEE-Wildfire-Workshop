//*---------------------------------------------------------*/
//*   ADD GROUPS AND CONNNECT USERS                         */
//*---------------------------------------------------------*/
//RACF     EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
ADDGROUP GMADMIN OMVS(AUTOGID)
ADDGROUP GMINVOKE  OMVS(AUTOGID)
CO FRED GROUP(GMADMIN)
CO USER1 GROUP(GMINVOKE)
