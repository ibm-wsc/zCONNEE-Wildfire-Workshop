//ZCEETLSX JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//*---------------------------------------------------------*/
//*   DEFINE CA AND SERVER CERTIFICATES                     */
//*---------------------------------------------------------*/
//RACF     EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *

RACDCERT ID(FRED) EXPORT(LABEL('FRED')) -
  DSN('USER1.FRED.P12') FORMAT(PKCS12DER) PASSWORD('secret')

RACDCERT ID(USER1) EXPORT(LABEL('USER1')) -
  DSN('USER1.USER1.P12') FORMAT(PKCS12DER) PASSWORD('secret')

RACDCERT CERTAUTH EXPORT(LABEL('Liberty CA')) -
  DSN('USER1.CERTAUTH.PEM')
/*
