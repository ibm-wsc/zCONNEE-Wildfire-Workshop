//*---------------------------------------------------------*/
//*   Define personal certificate and key ring              */
//*---------------------------------------------------------*/
//RACF     EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *

RACDCERT ID(USER1) ADDRING(Liberty.KeyRing)

RACDCERT ID(USER1) CONNECT(LABEL('USER1') +
         RING(Liberty.KeyRing) DEFAULT)

RACDCERT ID(USER1) CONNECT(CERTAUTH LABEL('Liberty CA') +
         RING(Liberty.KeyRing))

PERMIT IRR.DIGTCERT.LISTRING +
  CLASS(FACILITY) ID(USER1) ACCESS(READ)

PERMIT IRR.DIGTCERT.LIST +
  CLASS(FACILITY) ID(USER1) ACCESS(READ)

SETR RACLIST(DIGTCERT DIGTRING FACILITY) REFRESH

CONNECT LIBSERV GROUP(ZCEEUSRS)

CONNECT LIBSERV GROUP(GMINVOKE)
/*
