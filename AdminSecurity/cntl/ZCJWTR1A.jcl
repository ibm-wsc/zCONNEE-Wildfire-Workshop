//JOHNSON1 JOB 'JWT Cert',CLASS=A,MSGCLASS=H,                           JOB19642
// NOTIFY=&SYSUID,TIME=1440,REGION=0M
//*
//* Use RACF to define certificate that will be used to sign JWT
//*
//GO EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD SYSOUT=*
//SYSTSIN DD *
 /* Delete previous efforts for the OP Server

 RACDCERT DELETE(LABEL('JWT-Signer')) ID(ATSSERV)
 RACDCERT ID(ATSSERV) DELRING(JWT.KeyRing)

 /* Add a certificate to sign JWT created in the OpenId Provider

 racdcert id(atsserv) add('USER1.JWT.CERT.P12') withlabel('JWT-Signer')+
 trust password('secret')

 /* Create a key ring for JWT Signing cert

 RACDCERT ADDRING(JWT.KeyRing) ID(ATSSERV)

 /* Connect JWT Signing certificate to keyring

 RACDCERT ID(ATSSERV) CONNECT +
 (ID(ATSSERV) RING(JWT.KeyRing) LABEL('JWT-Signer'))

 SETR RACLIST(digtcert) REFRESH

 RACDCERT LISTRING(JWT.KeyRing) ID(ATSSERV)

