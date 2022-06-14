//JOHNSON3 JOB 'JWT Certs',CLASS=A,MSGCLASS=H,                          JOB19642
// NOTIFY=&SYSUID,TIME=1440,REGION=0M
//GO EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD SYSOUT=*
//SYSTSIN DD *
 /* Delete previous efforts for the OP Server

 RACDCERT DELETE(LABEL('OpenIdProv-CertAuth'))  CERTAUTH
 RACDCERT DELETE(LABEL('OpenIdProv-Server')) ID(ATSSERV)
 RACDCERT ID(ATSSERV) DELRING(OpServer.KS)
 SETROPTS RACLIST(DIGTCERT) REFRESH

 /* Create certificates for the OP server

 /* Create a certificate to sign the certificates

 RACDCERT CERTAUTH ADD('USER1.PROVIDER.CERTAUTH.CERT.P12') +
 withlabel('OpenIdProv-CertAuth') +
 trust password('secret')

 /* Create a certificate to be server side cert in the OpenId Provider

 RACDCERT ID(ATSSERV) ADD('USER1.PROVIDER.SERVER.CERT.P12') +
 withlabel('OpenIdProv-Server') +
 trust password('secret')

 /* Create a key ring for the userid the server runs under

 RACDCERT ADDRING(OpServer.KS) ID(ATSSERV)

 /* Connect Cert Auth certificate to user keyring

 RACDCERT ID(ATSSERV) CONNECT +
 ( LABEL('OpenIdProv-CertAuth') RING(OpServer.KS) CERTAUTH)

 RACDCERT ID(ATSSERV) CONNECT +
 (ID(ATSSERV) RING(OpServer.KS) LABEL('OpenIdProv-Server'))

 /* Delete previous efforts for the RP Server

 RACDCERT DELETE(LABEL('Replying-CertAuth'))  CERTAUTH
 RACDCERT DELETE(LABEL('Replying-Server')) ID(ATSSERV)
 RACDCERT ID(ATSSERV) DELRING(RpServer.KS)

 /* Create certificates for the RP server

 /* Create a certificate to sign the certificates

 RACDCERT CERTAUTH ADD('USER1.REPLYING.CERTAUTH.CERT.P12') +
 WITHLABEL('Replying-CertAuth') +
 trust password('secret')

 /* Create a certificate to be server side cert in the Replying Server

 RACDCERT ID(ATSSERV) ADD('USER1.REPLYING.SERVER.CERT.P12') +
 WITHLABEL('Replying-Server') +
 trust password('secret')

 /* Create a key ring for the userid the server runs under

 RACDCERT ADDRING(RpServer.KS) ID(ATSSERV)

 /* Connect Cert Auth certificate to user keyring

 RACDCERT ID(ATSSERV) CONNECT +
 ( LABEL('Replying-CertAuth') RING(RpServer.KS) CERTAUTH)

 RACDCERT ID(ATSSERV) CONNECT +
 (ID(ATSSERV) RING(RpServer.KS) LABEL('Replying-Server'))

 SETR RACLIST(digtcert) REFRESH

 RACDCERT LISTRING(OpServer.KS) ID(ATSSERV)
 RACDCERT LISTRING(RpServer.KS) ID(ATSSERV)
//
